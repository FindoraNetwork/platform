import os

# environments
local = "http://127.0.0.1"
mainnet = "https://prod-mainnet.prod.findora.org"
testnet = "https://prod-testnet.prod.findora.org"
mock = "https://dev-mainnetmock.dev.findora.org"
qa01 = "https://dev-qa01.dev.findora.org"
qa02 = "https://dev-qa02.dev.findora.org"

KNOWN_ENVS = {'local': local, 'mainnet': mainnet,
              'testnet': testnet, 'mock': mock, 'qa01': qa01, 'qa02': qa02}

# local devnet
fin_debug = os.getenv('FIN_DEBUG')
devnet = os.path.join(fin_debug, "devnet")
node0 = os.path.join(devnet, "node0")
node1 = os.path.join(devnet, "node1")
