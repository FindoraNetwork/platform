import os
import sys
import time
import json
import requests

from web3 import Web3, HTTPProvider
from .constants import *
from .config import *


def rpc_call(url, method, params):
    # tendermint rpc
    payload = {
        "method": method,
        "params": params,
        "id": 123,
        "jsonrpc": "2.0"
    }

    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json",
    }

    try:
        response = requests.request("POST", url, data=json.dumps(payload), headers=headers)
    except:
        print("{}rpc_call failed, url: {}{}".format(FAIL, url, ENDC))
        return {'code': -1, 'log': 'rpc_call failed, url: {}'.format(url)}

    if response.status_code != 200:
        print("rpc_call failed, status code: {}".format(response.status_code))
        return {'code': -2, 'log': 'rpc_call failed, status code: {}'.format(response.status_code)}

    resp = json.loads(response.text)
    if "result" in resp:
        return resp["result"]
    else:
        return {'code': -3, 'log': 'no result'}


def broadcast_sync(url, tx):
    return rpc_call(url, 'broadcast_tx_sync', {"tx": tx})


def broadcast_async(url, tx):
    return rpc_call(url, 'broadcast_tx_async', {"tx": tx})


def broadcast_commit(url, tx):
    return rpc_call(url, 'broadcast_tx_commit', {"tx": tx})


def broadcast(url, tx, mode=TxSync):
    if mode == TxSync:
        return broadcast_sync(url, tx)
    elif mode == TxAsync:
        return broadcast_async(url, tx)
    elif mode == TxCommit:
        return broadcast_commit(url, tx)
    else:
        return broadcast_sync(url, tx)


def get_height(url):
    w3 = Web3(HTTPProvider(url))
    try:
        return w3.eth.blockNumber
    except:
        print("{}w3.eth.blockNumber failed.{}".format(WARNING, ENDC))
        return None


def wait_until(url, call, predict, poll=1.0, delay=2.0):
    # keep calling until condition is met
    fail, met = False, False
    while (not fail) and (not met):
        resp = call(url)
        if resp is None:
            time.sleep(poll)
            continue

        # return if prediction fail
        fail, met = predict(resp)
        if fail:
            break
        time.sleep(poll)

    # delay a bit
    time.sleep(delay)
    return resp


def wait_until_height_begin(url, height, strict_mode=False):
    """
        Parameters:
            url (str): url of EVM
            height (int): wait until the beginning of this height
            strict_mode (boolean): wait for the exact height or fails otherwise.
        Returns:
            (int, boolean): returns current height and status(succeed or failed)
    """
    # wait until a specific block begin(not end or commit)
    height = height-1

    # polling and wait
    def until(cur):
        fail = False
        arrives = False
        if height < cur:
            if strict_mode:
                print("{}Height {} already passed, current height is {}{}".format(WARNING, height, cur, ENDC))
                fail, arrives = (True, True)
            else:
                print("{}Height {} already passed, current height is {}. It's okay{}".format(WARNING, height+1, cur, ENDC))
                fail, arrives = (False, True)
        else:
            fail, arrives = (False, height == cur)
        return fail, arrives

    return wait_until(url, get_height, until, 1.0, 2.0)


def wait_for(url, blocks):
    # wait for some blocks
    cur_height = get_height(url)
    wait_until_height_begin(cur_height+blocks)


def env_to_url(name):
    # convert environment name to an endpoint
    if name.lower() not in KNOWN_ENVS:
        print('{}unknown environment: {}{}'.format(FAIL, name.lower(), ENDC))
        print('Environments supported: local, mainnet, testnet, mock, qa01, qa02')
        exit(5)
    else:
        return KNOWN_ENVS[name]


def to_web3_url(url):
    return '{}:8545'.format(url)


def to_abci_url(url):
    return '{}:26657'.format(url)
