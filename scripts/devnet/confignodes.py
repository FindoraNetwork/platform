#!/usr/bin/env python2

import os
import subprocess
import toml

devnet = os.path.join(os.environ['LEDGER_DIR'], "devnet")
localhost = "127.0.0.1"
base_url = "tcp://127.0.0.1:"
base_port_node = 26000
base_port_abci = 8600
blocks_interval = "10s"
timeout_commit = "15s"
toml_string = """
abci_host = "127.0.0.1"
abci_port = "26008"

tendermint_host = "127.0.0.1"
tendermint_port = "26007"

submission_host = "127.0.0.1"
submission_port = "8609"

ledger_host = "127.0.0.1"
ledger_port = "8608"
"""

# toml set
def set_toml(config_toml, field, value):
    args = ['toml', 'set', '--toml-path', config_toml, field, value]
    process = subprocess.Popen(args, stdout=subprocess.PIPE)
    process.wait()


# set persistent peers
def set_persistent_peers(config_toml, contents, i):
    peers = contents["p2p"]["persistent_peers"].split(",")
    peers_new = []
    for j, peer in enumerate(peers):
        if i == j:  # skip self
            continue
        p2p_laddr_j = str(base_port_node + 10 * j + 6)
        peer_id = peer.split("@")[0]
        peers_new.append("{}@{}:{}".format(peer_id, localhost, p2p_laddr_j))
    peers = ",".join(peers_new)
    set_toml(config_toml, "p2p.persistent_peers", peers)


# set ip addresses
def set_ip_addresses(config_toml, contents, i):
    # abci_proxy, rpc, p2p
    """ e.g.
    tcp://127.0.0.1:26008
    tcp://127.0.0.1:26007
    tcp://127.0.0.1:26006
    """
    proxy_app = base_url + str(base_port_node + 10 * i + 8)
    rpc_laddr = base_url + str(base_port_node + 10 * i + 7)
    p2p_laddr = base_url + str(base_port_node + 10 * i + 6)
    contents['proxy_app'] = proxy_app
    contents['rpc']['laddr'] = rpc_laddr
    contents['p2p']['laddr'] = p2p_laddr
    set_toml(config_toml, 'proxy_app', proxy_app)
    set_toml(config_toml, 'rpc.laddr', rpc_laddr)
    set_toml(config_toml, 'p2p.laddr', p2p_laddr)


# set time out
def set_commit_timeout(config_toml, contents):
    set_toml(config_toml, "consensus.create_empty_blocks_interval",
             blocks_interval)
    set_toml(config_toml, "consensus.timeout_commit", timeout_commit)


# gen abci.toml
def gen_abci_toml(abci_toml, contents, i):
    # create abci.toml
    with open(abci_toml, 'w+') as f:
        f.write(toml_string)
    
    # abci_port, tendermint_port, submission_port, ledger_port
    """ e.g.
    26008
    26007
    8609
    8608
    """
    abci_port = contents["proxy_app"].split(":")[2]
    tendermint_port = contents["rpc"]["laddr"].split(":")[2]
    submission_port = str(base_port_abci + 10 * i + 9)
    ledger_port = str(base_port_abci + 10 * i + 8)

    # set ports
    set_toml(abci_toml, "abci_port", abci_port)
    set_toml(abci_toml, "tendermint_port", tendermint_port)
    set_toml(abci_toml, "submission_port", submission_port)
    set_toml(abci_toml, "ledger_port", ledger_port)

if __name__ == "__main__":
    # list nodes in devnet
    nodes = os.listdir(devnet)
    nodes.sort()

    # modify config.toml in each node
    for i, node in enumerate(nodes):
        # load toml
        config_toml = os.path.join(devnet, node, "config", "config.toml")
        contents = toml.load(config_toml)

        # modify ip addresses
        set_ip_addresses(config_toml, contents, i)

        # modify peers
        set_persistent_peers(config_toml, contents, i)

        # modify commit timeout
        set_commit_timeout(config_toml, contents)

        # create abci.toml
        abci_toml = os.path.join(devnet, node, "abci", "abci.toml")
        gen_abci_toml(abci_toml, contents, i)

    exit(0)
