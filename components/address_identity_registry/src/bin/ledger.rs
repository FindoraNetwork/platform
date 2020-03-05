//#![deny(warnings)]
// Copyright 2019 © Findora. All rights reserved.
/// Command line executable to exercise functions related to credentials
#[macro_use]
extern crate lazy_static;

use async_std::{io, task};
use clap;
use clap::{App, Arg, ArgMatches};
use colored::*;
use cryptohash::sha256;
use futures::{future, prelude::*};
use hex;
use libp2p::{
  floodsub::{self, Floodsub, FloodsubEvent},
  identity,
  mdns::{Mdns, MdnsEvent},
  swarm::NetworkBehaviourEventProcess,
  Multiaddr, NetworkBehaviour, PeerId, Swarm,
};
use log::{info, warn};
use rand_chacha::ChaChaRng;
use rand_core::SeedableRng;
use rustyline::error::ReadlineError;
use rustyline::Editor;
// use sha2::{Digest, Sha256};
use air::AIR;
use serde::{Deserialize, Serialize};
use sha256::DIGESTBYTES;
use std::collections::HashMap;
use std::path::Path;
use std::{
  error::Error,
  task::{Context, Poll},
};
use zei::api::anon_creds::{
  ac_commit, ac_keygen_issuer, ac_keygen_user, ac_open_commitment, ac_sign, ac_verify,
  ACCommitment, ACCommitmentKey, ACIssuerPublicKey, ACIssuerSecretKey, ACPoK, ACSignature,
  ACUserPublicKey, ACUserSecretKey, Credential,
};

fn main() -> Result<(), Box<dyn Error>> {
  env_logger::init();

  // Create a random PeerId
  let local_key = identity::Keypair::generate_ed25519();
  let local_peer_id = PeerId::from(local_key.public());
  println!("Local peer id: {:?}", local_peer_id);

  // Set up a an encrypted DNS-enabled TCP Transport over the Mplex and Yamux protocols
  let transport = libp2p::build_development_transport(local_key)?;

  // Create a Floodsub topic
  let floodsub_topic = floodsub::Topic::new("chat");

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
  task::block_on(future::poll_fn(move |cx: &mut Context| {
                   loop {
                     match stdin.try_poll_next_unpin(cx)? {
                       Poll::Ready(Some(line)) => {
                         swarm.floodsub
                              .publish(floodsub_topic.clone(), format!("ledger sends: {}", line).as_bytes())
                       },
                       Poll::Ready(None) => panic!("Stdin closed"),
                       Poll::Pending => break,
                     }
                   }
                   loop {
                     match swarm.poll_next_unpin(cx) {
                       Poll::Ready(Some(event)) => println!("{:?}", event),
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
