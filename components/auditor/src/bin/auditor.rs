//! A program that tests consensus on the value of a signed ledger state
//!
//! Using N terminal windows, start N instances. If you local network allows mDNS,
//! they will automatically connect. To demonstrate that consensus failure works,
//! we "poison" the received ledger value using the --poison option
//!
//! ```sh
//! cargo run --bin auditor -- --poison
//! ```
//!
//! # If they don't automatically connect
//!
//! If the nodes don't automatically connect, take note of the listening address of the first
//! instance and start the second with this address as the first argument. In the first terminal
//! window, run:
//!
//! ```sh
//! cargo run --bin auditor
//! ```
//!
//! It will print the PeerId and the listening address, e.g. `Listening on
//! "/ip4/0.0.0.0/tcp/24915"`
//!
//! In the second terminal window, start a new instance of the example with:
//!
//! ```sh
//! cargo run --bin auditor -- /ip4/127.0.0.1/tcp/24915
//! ```
//!
//! The two nodes then connect.

use async_std::{io, task};
use attohttpc;
use clap::{App, Arg, ArgMatches};
use cryptohash::sha256::Digest as BitDigest;
use futures::{future, prelude::*};
use libp2p::{
  floodsub::{self, Floodsub, FloodsubEvent},
  identity,
  mdns::{Mdns, MdnsEvent},
  swarm::NetworkBehaviourEventProcess,
  Multiaddr, NetworkBehaviour, PeerId, Swarm,
};
use serde_derive::{Deserialize, Serialize};
use std::{
  collections::{HashMap, HashSet},
  error::Error,
  task::{Context, Poll},
  thread,
  time::Duration,
};
use utils::{protocol_host, QUERY_PORT};
use zei::xfr::sig::{XfrPublicKey, XfrSignature};

#[derive(Debug, Deserialize, Serialize, Clone, Eq, PartialEq)]
struct KeyAndState {
  pub public_key: XfrPublicKey,
  pub global_state: (BitDigest, u64, XfrSignature),
}

fn poison(v: u64) -> u64 {
  if v == 0 {
    1
  } else {
    v - 1
  }
}

fn parse_args() -> ArgMatches<'static> {
  App::new("Ledger Auditor").version("0.1.0")
                            .author("Brian Rogoff <brian@findora.org>")
                            .about("Auditor consensus on ledger signed commitments")
                            .arg(Arg::with_name("time").short("t")
                                                       .long("time")
                                                       .takes_value(true)
                                                       .help("time between broadcasts"))
                            .arg(Arg::with_name("poison").short("p")
                                                         .long("poison")
                                                         .help("mess up a commitment"))
                            .arg(Arg::with_name("dial").short("d")
                                                       .takes_value(true)
                                                       .help("address to dial"))
                            .get_matches()
}

struct ConsensusState {
  this_state: KeyAndState,
  matches: HashSet<PeerId>,
  mismatches: HashMap<PeerId, KeyAndState>,
}

fn main() -> Result<(), Box<dyn Error>> {
  env_logger::init();

  let args = parse_args();
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
  let floodsub_topic = floodsub::Topic::new("ledger auditor");

  // Read signed commitment from ledger
  let (protocol, host) = protocol_host();
  let resp_gs =
    attohttpc::get(&format!("{}://{}:{}/global_state", protocol, host, QUERY_PORT)).send()?;
  let (comm, idx, sig): (BitDigest, u64, XfrSignature) =
    serde_json::from_str(&resp_gs.text()?[..]).unwrap();
  let idx = if args.is_present("poison") {
    poison(idx)
  } else {
    idx
  };
  println!("Got ({:?}, {}, {:?}) from global_state", comm, idx, sig);
  // Read signed commitment from ledger
  let resp_pk =
    attohttpc::get(&format!("{}://{}:{}/public_key", protocol, host, QUERY_PORT)).send()?;
  let pk: XfrPublicKey = serde_json::from_str(&resp_pk.text()?[..]).unwrap();
  println!("Got {:?} from public_key", pk);
  match pk.verify(&serde_json::to_vec(&(comm, idx)).unwrap(), &sig) {
    Ok(()) => println!("Verification succeeded"),
    Err(zei_err) => println!("Verification failed with error = {}", zei_err),
  };

  let key_and_state: KeyAndState = KeyAndState { public_key: pk,
                                                 global_state: (comm, idx, sig) };
  let ks_str = serde_json::to_string(&key_and_state).unwrap();

  let consensus_state = ConsensusState { this_state: key_and_state,
                                         matches: HashSet::new(),
                                         mismatches: HashMap::new() };
  // Creating a struct that implements the NetworkBehaviour trait and combines all the desired network behaviours,
  // implementing the event handlers as per the desired application's networking logic.
  // We create a custom network behaviour that combines floodsub and mDNS.
  // In the future, we want to improve libp2p to make this easier to do.
  // Use the derive to generate delegating NetworkBehaviour impl and require the
  // NetworkBehaviourEventProcess implementations below.
  #[derive(NetworkBehaviour)]
  struct AuditorBehaviour {
    floodsub: Floodsub,
    mdns: Mdns,
    // Struct fields which do not implement NetworkBehaviour need to be ignored
    #[behaviour(ignore)]
    #[allow(dead_code)]
    consensus_state: ConsensusState,
  }

  impl NetworkBehaviourEventProcess<FloodsubEvent> for AuditorBehaviour {
    // Called when `floodsub` produces an event.
    fn inject_event(&mut self, message: FloodsubEvent) {
      if let FloodsubEvent::Message(message) = message {
        let msg_str = String::from_utf8_lossy(&message.data);
        let received = serde_json::from_str::<KeyAndState>(&msg_str);
        match received {
          Ok(ks) => {
            if ks == self.consensus_state.this_state {
              if !self.consensus_state.matches.contains(&message.source) {
                self.consensus_state.matches.insert(message.source.clone());
                println!("Received matching signed commitment from {:?}",
                         message.source);
              }
            } else {
              if !self.consensus_state
                      .mismatches
                      .contains_key(&message.source)
              {
                self.consensus_state
                    .mismatches
                    .insert(message.source.clone(), ks.clone());
                println!("Received nonmatching signed commitment \n{:?}\nfrom {:?}",
                         ks, message.source)
              }
            }
          }
          _ => {
            println!("Received: '{:?}' from {:?}", msg_str, message.source);
          }
        }
      }
    }
  }

  impl NetworkBehaviourEventProcess<MdnsEvent> for AuditorBehaviour {
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
    let mut behaviour = AuditorBehaviour { floodsub: Floodsub::new(local_peer_id.clone()),
                                           mdns,
                                           consensus_state };

    behaviour.floodsub.subscribe(floodsub_topic.clone());
    Swarm::new(transport, behaviour, local_peer_id)
  };

  // Reach out to another node if specified
  if let Some(to_dial) = args.value_of("dial") {
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
  let mut secs: u64 = 3;
  if let Some(time) = args.value_of("time") {
    let new_time: u64 = time.parse()?;
    secs = new_time;
  }
  let pause_time = Duration::from_secs(secs);

  task::block_on(future::poll_fn(move |cx: &mut Context| {
                   loop {
                     match stdin.try_poll_next_unpin(cx)? {
                       Poll::Ready(Some(line)) => {
                         swarm.floodsub
                              .publish(floodsub_topic.clone(), line.as_bytes())
                       }
                       Poll::Ready(None) => panic!("Stdin closed"),
                       Poll::Pending => break,
                     }
                   }
                   loop {
                     match swarm.poll_next_unpin(cx) {
                       Poll::Ready(Some(event)) => {
                         println!("{:?}", event);
                       }
                       Poll::Ready(None) => return Poll::Ready(Ok(())),
                       Poll::Pending => {
                         if !listening {
                           for addr in Swarm::listeners(&swarm) {
                             println!("Listening on {:?}", addr);
                           }
                           listening = true;
                           println!("********************************************************************************\n");
                         }
                         thread::sleep(pause_time);
                         swarm.floodsub
                              .publish(floodsub_topic.clone(), ks_str.as_bytes());
                         break;
                       }
                     }
                   }
                   Poll::Pending
                 }))
}
