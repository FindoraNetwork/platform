#!/usr/bin/env python

import os
import subprocess
import toml

devnet = os.path.join(os.environ['FIN_DEBUG'], "devnet")
localhost = "0.0.0.0"
base_url = "tcp://0.0.0.0:"
base_port_node = 26650
base_port_abci = 8660
base_port_evm = 8540
blocks_interval = "0s"
timeout_commit = "15s"
if os.getenv('BLOCK_INTERVAL') != None:
    timeout_commit = "{}s".format(os.environ['BLOCK_INTERVAL'])
toml_string = """
abci_host = "0.0.0.0"
abci_port = "26008"

tendermint_host = "0.0.0.0"
tendermint_port = "26657"

submission_port = "8669"

ledger_port = "8668"

evm_http_port = "8545"
evm_ws_port = "8546"
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
    # rpc, p2p
    """ e.g.
    tcp://0.0.0.0:26658
    tcp://0.0.0.0:26657
    tcp://0.0.0.0:26656
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

# set time out
def set_tx_index(config_toml, contents):
    set_toml(config_toml, "tx_index.index_all_keys", "True")

# gen abci.toml
def gen_abci_toml(abci_toml, contents, i):
    # create abci.toml
    with open(abci_toml, 'w+') as f:
        f.write(toml_string)
    
    # tendermint_port, submission_port, ledger_port, evm_http_port, evm_ws_port
    """ e.g.
    26657
    8669
    8668
    8545
    8546
    """
    abci_port = contents["proxy_app"].split(":")[2]
    tendermint_port = contents["rpc"]["laddr"].split(":")[2]
    submission_port = str(base_port_abci + 10 * i + 9)
    ledger_port = str(base_port_abci + 10 * i + 8)
    evm_http_port = str(base_port_evm + 10 * i + 5)
    evm_ws_port = str(base_port_evm + 10 * i + 6)

    # set ports
    set_toml(abci_toml, "abci_port", abci_port)
    set_toml(abci_toml, "tendermint_port", tendermint_port)
    set_toml(abci_toml, "submission_port", submission_port)
    set_toml(abci_toml, "ledger_port", ledger_port)
    set_toml(abci_toml, "evm_http_port", evm_http_port)
    set_toml(abci_toml, "evm_ws_port", evm_ws_port)

if __name__ == "__main__":
    # list nodes in devnet
    dirs = os.listdir(devnet)
    nodes = list(filter(lambda d : d.startswith('node'), dirs))
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

        # modify tx indexing
        set_tx_index(config_toml, contents)

        # create abci.toml
        abci_toml = os.path.join(devnet, node, "abci", "abci.toml")
        gen_abci_toml(abci_toml, contents, i)

    exit(0)
