#!/usr/bin/env python3

from sdk import *
import argparse
import subprocess
import os

# Environment variables
bin_path = ''


def private_key_to_address(private_key, w3):
    account = w3.eth.account.privateKeyToAccount(private_key)
    return account.address


def get_fra_balance(sec_key):
    out_str = subprocess.check_output([bin_path + '/fn', 'account', '-s', sec_key])
    parsed = out_str.split()
    return int(parsed[1])


def get_fra_anon_balance(anon_keys_path, commitments):
    out_str = subprocess.check_output(
        [bin_path + '/fn', 'anon-balance', '--anon-keys', anon_keys_path, '--commitments', commitments])
    parsed = out_str.split()
    return int(parsed[1])


def get_asset_balance(sec_key, asset):
    out_str = subprocess.check_output([bin_path + '/fn', 'account', '-s', sec_key, '--asset', asset])
    parsed = out_str.split()
    return int(parsed[1])


def get_asset_anon_balance(anon_keys_path, commitments, asset):
    out_str = subprocess.check_output(
        [bin_path + '/fn', 'anon-balance', '--anon-keys', anon_keys_path, '--commitments', commitments,
         '--asset', asset])
    parsed = out_str.split()
    return int(parsed[1])


def get_erc20_balance(address, url):
    w3 = Web3(HTTPProvider(to_web3_url(url)))
    try:
        return w3.eth.get_balance(address)
    except:
        print("{}w3.eth.get_balance failed.{}".format(WARNING, ENDC))
        return None


def get_balance(arguments):
    balance = ""
    addr = arguments['addr']
    sec_key = arguments['sec_key']
    asset = arguments['asset']
    if addr is not None:
        balance = get_erc20_balance(addr, arguments['url'])
    elif sec_key is not None:
        if asset is not None:
            balance = get_asset_balance(sec_key, asset)
        else:
            balance = get_fra_balance(sec_key)
    print('{}balance is {} {}'.format(OKBLUE, balance, ENDC))
    return balance


def get_anon_balance(arguments):
    anon_keys_path = arguments['anon_keys']
    commitments = arguments['commitments']
    asset = arguments['asset']

    balance = ""
    if asset is not None:
        balance = get_asset_anon_balance(anon_keys_path, commitments, asset)
    else:
        balance = get_fra_anon_balance(anon_keys_path, commitments)
    print('{}balance is {} {}'.format(OKBLUE, balance, ENDC))
    return balance


def verify_balance(arguments):
    expected_balance = arguments['amount']
    balance = get_balance(arguments)
    if int(expected_balance) != int(balance):
        print("{}balance doesn't match expected value{}".format(FAIL, ENDC))
        exit(-1)
    else:
        print("{}Balance Verified successfully{}".format(OKGREEN, ENDC))


def verify_anon_balance(arguments):
    expected_balance = arguments['amount']
    balance = get_anon_balance(arguments)
    if int(expected_balance) != int(balance):
        print("{}balance{} doesn't match expected value{} {}".format(FAIL, balance, expected_balance, ENDC))
        exit(-1)
    else:
        print("{}Balance Verified successfully{}".format(OKGREEN, ENDC))


def transfer(arguments):
    w3 = Web3(HTTPProvider(to_web3_url(arguments['url'])))
    address = private_key_to_address(arguments['from_priv_key'], w3)
    signed_txn = w3.eth.account.sign_transaction(dict(
        nonce=w3.eth.get_transaction_count(address),
        gas=100000,
        gasPrice=10000000000,
        to=arguments['to_addr'],
        value=int(arguments['amount']),
        data=b'',
        chainId=w3.eth.chainId,
    ),
        arguments['from_priv_key'],
    )
    txn_hash = w3.eth.send_raw_transaction(signed_txn.rawTransaction)
    print("Transaction Hash: ", txn_hash.hex())
    return


if __name__ == "__main__":
    # Get environment variables
    try:
        bin_path = os.environ['BIN']
    except:
        print("error getting environment variables")
        exit(-1)

    # initialize commands
    cmd_list = {'balance': get_balance, 'verify-balance': verify_balance,
                'verify-anon-balance': verify_anon_balance, 'transfer': transfer}

    # Initialize Parser
    parser = argparse.ArgumentParser()
    parser.add_argument('--url', default=local, help='web3 rpc endpoint')
    subparsers = parser.add_subparsers(dest='subparsers', help='sub-command help')

    # Initialize Balance Parser
    parser_balance = subparsers.add_parser('balance', help='query balance of an address')
    parser_balance.add_argument('--addr', help='address to query')
    parser_balance.add_argument('--sec-key', help='secret key of FRA account')
    parser_balance.add_argument('--asset', help='asset to query balance of')

    # Initialize Verify-Balance Parser
    parser_verify_balance = subparsers.add_parser('verify-balance', help='verify balance of an address')
    parser_verify_balance.add_argument('--addr', help='address to query')
    parser_verify_balance.add_argument('--sec-key', help='secret key of FRA account')
    parser_verify_balance.add_argument('--amount', default='0', help='expected balance for given address')
    parser_verify_balance.add_argument('--asset', help='asset to query balance of')

    # Initialize Verify-Balance Parser
    parser_verify_anon_balance = subparsers.add_parser('verify-anon-balance', help='verify anon balance of a anon key '
                                                                                   'and commitment')
    parser_verify_anon_balance.add_argument('--anon-keys', help='path of anon key JSON file')
    parser_verify_anon_balance.add_argument('--commitments', help='comma separated list of commitments')
    parser_verify_anon_balance.add_argument('--amount', default='0', help='expected balance for given address')
    parser_verify_anon_balance.add_argument('--asset', help='asset to query balance of')

    # Initialize Transfer Parser
    parser_transfer = subparsers.add_parser('transfer', help='transfer funds between two erc20 addresses')
    parser_transfer.add_argument('--from_priv_key', help='address to transfer funds from')
    parser_transfer.add_argument('--to_addr', help='address to transfer funds to')
    parser_transfer.add_argument('--amount', help='amount of funds to transfer')

    # Execute Command
    args = vars(parser.parse_args())
    cmd_list[args['subparsers']](args)
