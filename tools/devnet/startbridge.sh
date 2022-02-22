#!/usr/bin/env bash

SRC_GATEWAY=http://localhost:8545/

SRC_ADDR="0x6143836854C15D4397dd9fd022Bdc989a1D583da","0xfD38a5DD37bE755b05a0cFeBE8C98538F0C4E4F1","0xE46ed0256692f3D2b755c10109b60Cec9cf87512"
SRC_PK="850d6861470381e8553991bd6a4f331f8acc317e8af1cba223492d9b7dd5ca25"

SRC_BRIDGE="0x3Fc97AB0965eAe6F59a7D3AF65A20EcD9144B74e"
SRC_HANDLER="0x6930a96496eBbD4F2Dd3d8fcdFf979e204873e37"

SRC_TOKEN="0x0000000000000000000000000000000000001000"
RESOURCE_ID_FRA="0x000000000000000000000000000000c76ebe4a02bbc34786d860b355f5000000"


cb-sol-cli --url $SRC_GATEWAY --privateKey $SRC_PK --gasPrice 10000000000 deploy \
    --bridge --erc20Handler \
    --relayers $SRC_ADDR \
    --relayerThreshold 3 \
    --expiry 100 \
    --chainId 0

cb-sol-cli --url $SRC_GATEWAY --privateKey $SRC_PK --gasPrice 10000000000 bridge register-resource \
    --bridge $SRC_BRIDGE \
    --handler $SRC_HANDLER \
    --resourceId $RESOURCE_ID_FRA \
    --targetContract $SRC_TOKEN