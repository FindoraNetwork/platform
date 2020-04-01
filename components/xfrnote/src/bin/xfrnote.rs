//! A basic chat application demonstrating libp2p and the mDNS and floodsub protocols.
//! The application was modified to send a zei Xfrnote when either participant types
//! "xfrnote"
//!
//! Using two terminal windows, start two instances. If you local network allows mDNS,
//! they will automatically connect. Type a message in either terminal and hit return: the
//! message is sent and printed in the other terminal. Close with Ctrl-c.
//!
//! You can of course open more terminal windows and add more participants.
//! Dialing any of the other peers will propagate the new participant to all
//! chat members and everyone will receive all messages.
//!
//! # If they don't automatically connect
//!
//! If the nodes don't automatically connect, take note of the listening address of the first
//! instance and start the second with this address as the first argument. In the first terminal
//! window, run:
//!
//! ```sh
//! cargo run --bin xfrnote
//! ```
//!
//! It will print the PeerId and the listening address, e.g. `Listening on
//! "/ip4/0.0.0.0/tcp/24915"`
//!
//! In the second terminal window, start a new instance of the example with:
//!
//! ```sh
//! cargo run --bin xfrnote -- /ip4/127.0.0.1/tcp/24915
//! ```
//!
//! The two nodes then connect.

use async_std::{io, task};
use futures::{future, prelude::*};
use itertools::Itertools;
use libp2p::{
  floodsub::{self, Floodsub, FloodsubEvent},
  identity,
  mdns::{Mdns, MdnsEvent},
  swarm::NetworkBehaviourEventProcess,
  Multiaddr, NetworkBehaviour, PeerId, Swarm,
};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use std::{
  error::Error,
  task::{Context, Poll},
};
use zei::xfr::sig::XfrKeyPair;
use zei::xfr::structs::{AssetRecord, AssetRecordTemplate, XfrNote};
// use zei::xfr::structs::{AssetRecordTemplate, AssetRecord};
use zei::xfr::asset_record::AssetRecordType;
use zei::xfr::lib::gen_xfr_note;

pub fn make_xfr_note() -> XfrNote {
  let mut prng: ChaChaRng;
  prng = ChaChaRng::from_seed([0u8; 32]);
  let asset_type = [0u8; 16];
  let inputs_amounts = [(10u64, asset_type),
                        (10u64, asset_type),
                        (10u64, asset_type)];
  let outputs_amounts = [(1u64, asset_type),
                         (2u64, asset_type),
                         (3u64, asset_type),
                         (24u64, asset_type)];

  let mut inputs = vec![];
  let mut outputs = vec![];

  let mut inkeys = vec![];
  let mut in_asset_records = vec![];

  let asset_record_type = AssetRecordType::NonConfidentialAmount_NonConfidentialAssetType;

  for x in inputs_amounts.iter() {
    let keypair = XfrKeyPair::generate(&mut prng);
    let asset_record = AssetRecordTemplate::with_no_asset_tracking(x.0,
                                                                   x.1,
                                                                   asset_record_type,
                                                                   keypair.get_pk_ref().clone());

    inputs.push(AssetRecord::from_template_no_identity_tracking(&mut prng, &asset_record).unwrap());

    in_asset_records.push(asset_record);
    inkeys.push(keypair);
  }

  for x in outputs_amounts.iter() {
    let keypair = XfrKeyPair::generate(&mut prng);

    let ar = AssetRecordTemplate::with_no_asset_tracking(x.0,
                                                         x.1,
                                                         asset_record_type,
                                                         keypair.get_pk_ref().clone());
    let output = AssetRecord::from_template_no_identity_tracking(&mut prng, &ar).unwrap();
    outputs.push(output);
  }

  gen_xfr_note(&mut prng,
               inputs.as_slice(),
               outputs.as_slice(),
               inkeys.iter().map(|x| x).collect_vec().as_slice()).unwrap()
}

fn main() -> Result<(), Box<dyn Error>> {
  env_logger::init();

  // Creating an identity Keypair for the local node, obtaining the local PeerId from the PublicKey.
  // Create a random PeerId
  let local_key = identity::Keypair::generate_ed25519();
  let local_peer_id = PeerId::from(local_key.public());
  println!("Local peer id: {:?}", local_peer_id);

  // Creating an instance of a base Transport, e.g. TcpConfig, upgrading it with all the desired protocols,
  // such as for transport security and multiplexing. In order to be usable with a Swarm later, the Output
  // of the final transport must be a tuple of a PeerId and a value whose type implements StreamMuxer (e.g. Yamux).
  // The peer ID must be the identity of the remote peer of the established connection, which is usually
  // obtained through a transport encryption protocol such as secio that authenticates the peer. See the
  // implementation of build_development_transport for an example.
  // Set up a an encrypted DNS-enabled TCP Transport over the Mplex and Yamux protocols
  let transport = libp2p::build_development_transport(local_key)?;

  // Create a Floodsub topic
  let floodsub_topic = floodsub::Topic::new("chat");

  // Creating a struct that implements the NetworkBehaviour trait and combines all the desired network behaviours,
  // implementing the event handlers as per the desired application's networking logic.
  // We create a custom network behaviour that combines floodsub and mDNS.
  // In the future, we want to improve libp2p to make this easier to do.
  // Use the derive to generate delegating NetworkBehaviour impl and require the
  // NetworkBehaviourEventProcess implementations below.
  #[derive(NetworkBehaviour)]
  struct MyBehaviour {
    floodsub: Floodsub,
    mdns: Mdns,

    // Struct fields which do not implement NetworkBehaviour need to be ignored
    #[behaviour(ignore)]
    #[allow(dead_code)]
    ignored_member: bool,
  }

  impl NetworkBehaviourEventProcess<FloodsubEvent> for MyBehaviour {
    // Called when `floodsub` produces an event.
    fn inject_event(&mut self, message: FloodsubEvent) {
      if let FloodsubEvent::Message(message) = message {
        println!("Received: '{:?}' from {:?}",
                 String::from_utf8_lossy(&message.data),
                 message.source);
      }
    }
  }

  impl NetworkBehaviourEventProcess<MdnsEvent> for MyBehaviour {
    // Called when `mdns` produces an event.
    fn inject_event(&mut self, event: MdnsEvent) {
      match event {
        MdnsEvent::Discovered(list) => {
          for (peer, _) in list {
            self.floodsub.add_node_to_partial_view(peer);
          }
        }
        MdnsEvent::Expired(list) => {
          for (peer, _) in list {
            if !self.mdns.has_node(&peer) {
              self.floodsub.remove_node_from_partial_view(&peer);
            }
          }
        }
      }
    }
  }

  // Instantiating a Swarm with the transport, the network behaviour and the local peer ID from the previous steps.

  // Create a Swarm to manage peers and events
  let mut swarm = {
    let mdns = Mdns::new()?;
    let mut behaviour = MyBehaviour { floodsub: Floodsub::new(local_peer_id.clone()),
                                      mdns,
                                      ignored_member: false };

    behaviour.floodsub.subscribe(floodsub_topic.clone());
    Swarm::new(transport, behaviour, local_peer_id)
  };

  // Reach out to another node if specified
  if let Some(to_dial) = std::env::args().nth(1) {
    let addr: Multiaddr = to_dial.parse()?;
    Swarm::dial_addr(&mut swarm, addr)?;
    println!("Dialed {:?}", to_dial)
  }

  // Read full lines from stdin
  let mut stdin = io::BufReader::new(io::stdin()).lines();

  // Listen on all interfaces and whatever port the OS assigns
  Swarm::listen_on(&mut swarm, "/ip4/0.0.0.0/tcp/0".parse()?)?;

  // Kick it off
  let mut listening = false;
  // Create an xfr note
  let xfr_note = make_xfr_note();
  let xfr_note_string = serde_json::to_string(&xfr_note).unwrap();

  task::block_on(future::poll_fn(move |cx: &mut Context| {
                   loop {
                     match stdin.try_poll_next_unpin(cx)? {
                       Poll::Ready(Some(line)) => {
                         if line == "xfrnote" {
                           swarm.floodsub
                                .publish(floodsub_topic.clone(), xfr_note_string.as_bytes())
                         } else {
                           swarm.floodsub
                                .publish(floodsub_topic.clone(), line.as_bytes())
                         }
                       }
                       Poll::Ready(None) => panic!("Stdin closed"),
                       Poll::Pending => break,
                     }
                   }
                   loop {
                     match swarm.poll_next_unpin(cx) {
                       Poll::Ready(Some(event)) => println!("{:?}", &event),
                       Poll::Ready(None) => return Poll::Ready(Ok(())),
                       Poll::Pending => {
                         if !listening {
                           for addr in Swarm::listeners(&swarm) {
                             println!("Listening on {:?}", addr);
                             listening = true;
                           }
                         }
                         break;
                       }
                     }
                   }
                   Poll::Pending
                 }))
}
