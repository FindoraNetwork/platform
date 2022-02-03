#!/usr/bin/env python3

from sdk import *
import argparse
import subprocess


def get_fra_balance(sec_key):
    out_str = subprocess.check_output(['fn', 'account', '-s', sec_key])
    parsed = out_str.split()
    return int(parsed[1])


def get_erc20_balance(address):
    w3 = Web3(HTTPProvider(to_web3_url(local)))
    try:
        return w3.eth.get_balance(address)
    except:
        print("{}w3.eth.get_balance failed.{}".format(WARNING, ENDC))
        return None


def get_balance(arguments):
    balance = ""
    addr = arguments['addr']
    sec_key = arguments['sec_key']
    if addr is not None:
        balance = get_erc20_balance(addr)
    elif sec_key is not None:
        balance = get_fra_balance(sec_key)
    # print('{}balance of {} is {}{}'.format(OKBLUE, account, balance, ENDC))
    return balance


def verify_balance(arguments):
    expected_balance = arguments['amount']
    balance = get_balance(arguments)
    if int(expected_balance) != int(balance):
        print("{}balance doesn't match expected value{}".format(FAIL, ENDC))
        exit(-1)
    else:
        print("{}Balance Verified successfully{}".format(OKGREEN, ENDC))


# def transfer(arguments):
#     w3 = Web3(HTTPProvider(to_web3_url(local)))
#     signed_txn = w3.eth.account.sign_transaction(dict(
#             nonce=w3.eth.get_transaction_count(arguments['from_addr']),
#             maxFeePerGas=3000000000,
#             maxPriorityFeePerGas=2000000000,
#             gas=100000,
#             to=arguments['to_addr'],
#             value=int(arguments['amount']),
#             data=b'',
#             type=2,  # (optional) the type is now implicitly set based on appropriate transaction params
#             chainId=1,
#         ),
#         arguments['from_priv_key'],
#     )
#     w3.eth.send_raw_transaction(signed_txn.rawTransaction)
#     return


if __name__ == "__main__":
    # initialize commands
    cmd_list = {'balance': get_balance, 'verify-balance': verify_balance}  # 'transfer': transfer

    # Initialize Parser
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest='subparsers', help='sub-command help')

    # Initialize Balance Parser
    parser_balance = subparsers.add_parser('balance', help='query balance of an address')
    parser_balance.add_argument('--addr', help='address to query')
    parser_balance.add_argument('--sec-key', help='secret key of FRA account')

    # Initialize Verify-Balance Parser
    parser_verify_balance = subparsers.add_parser('verify-balance', help='verify balance of an address')
    parser_verify_balance.add_argument('--addr', help='address to query')
    parser_verify_balance.add_argument('--sec-key', help='secret key of FRA account')
    parser_verify_balance.add_argument('--amount', default='0', help='expected balance for given address')

    # # Initialize Transfer Parser
    # parser_transfer = subparsers.add_parser('transfer', help='transfer funds between two erc20 addresses')
    # parser_transfer.add_argument('--from_addr', help='address to transfer funds from')
    # parser_transfer.add_argument('--from_priv_key', help='address to transfer funds from')
    # parser_transfer.add_argument('--to_addr', help='address to transfer funds to')
    # parser_transfer.add_argument('--amount', help='amount of funds to transfer')

    # Execute Command
    args = vars(parser.parse_args())
    cmd_list[args['subparsers']](args)
