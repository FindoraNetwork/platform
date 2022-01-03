#!/usr/bin/env python3

import getopt
import sys
import os
import base64
from sdk import *


def usage():
    """
        Dump   example: python3 tools/regression/replay.py -d -f /Users/charlie/test/devnet/node0/abcid.log -h 1
        Replay example: python3 tools/regression/replay.py -e local -f /Users/charlie/test/devnet/node0/txns_1_to_27.json -h 1
    """
    print('{}usage-1(replay)  :{} replay.py -e <environment(default: local)> -f <txn_file> -h <height(default: 1)> -s <height_stop(default: no stop)>'.format(WARNING, ENDC))
    print('{}usage-2(log dump):{} replay.py -d -f <log_file> -h <height(default: 1)> -s <height_stop(default: no stop)>'.format(WARNING, ENDC))
    sys.exit(1)


def check_params(env, file, height, height_stop):
    # Check url
    url = env_to_url(env)

    # Check height
    if height < 1:
        print('{}invalid height: {}{}'.format(FAIL, height, ENDC))
        sys.exit(2)

    if height_stop < 0:
        print('{}invalid height_stop: {}{}'.format(FAIL, height_stop, ENDC))
        sys.exit(3)

    # Check file
    if not os.path.isfile(file):
        print('{}file not found: {}{}'.format(FAIL, file, ENDC))
        sys.exit(4)

    return url


def parse_args():
    # Parse arguments
    try:
        # Define the getopt parameters
        argv = sys.argv[1:]
        opts, args = getopt.getopt(argv, 'de:f:h:s:', ['dump', 'environment', 'file', 'height', 'height_stop'])
        if len(opts) < 2:
            usage()
    except getopt.GetoptError:
        usage()

    # Iterate the options and get the corresponding values
    dump = False
    env = "local"
    file = ""
    height = 1
    height_stop = 0
    for opt, arg in opts:
        if opt in ("-d", "--dump"):
            dump = True
        if opt in ("-e", "--environment"):
            env = arg
        if opt in ("-f", "--file"):
            file = arg
        if opt in ("-h", "--height"):
            height = int(arg)
        if opt in ("-s", "--stop"):
            height_stop = int(arg)

    # check and print params
    url = check_params(env, file, height, height_stop)
    if dump:
        print('==================parameters for txn dump=====================')
        print('{}File  : {}{}'.format(OKGREEN, file, ENDC))
        print('{}Height: {}{}'.format(OKGREEN, height, ENDC))
    else:
        print('==================parameters for txn replay===================')
        print('{}URL   : {}{}'.format(OKGREEN, url, ENDC))
        print('{}File  : {}{}'.format(OKGREEN, file, ENDC))
        print('{}Height: {}{}'.format(OKGREEN, height, ENDC))

    # return
    return (dump, url, file, height, height_stop)


def log_print(file, msg, flush=True):
    print(msg)
    file.write(msg + "\n")
    if flush:
        file.flush()


def parse_tx_result(file, result):
    if result['code'] == 0:
        log_print(file, '    Transaction sent, hash: {}'.format(result['hash']))
        return True
    else:
        log_print(file, '    Transaction fail, log: {}'.format(result['log']))
        return False


def replay(url, file, height, height_stop=0, strict_mode=False):
    # load transactions
    with open(file) as json_file:
        # load transactions
        transactions = json.load(json_file)
        if height_stop == 0:
            transactions = {int(k): v for k, v in transactions.items() if height <= int(k)}
        else:
            transactions = {int(k): v for k, v in transactions.items() if height <= int(k) <= height_stop}
        if len(transactions) == 0:
            print("{}Replay finished. No transactions loaded.{}".format(WARNING, height, ENDC))
            exit(-1)

        # replay log file
        logfile = open(os.path.join(os.path.dirname(file), "replay.log"), 'a')
        log_print(logfile, '\n==================replaying transactions=========================')

        # replay transactions
        tx_count = 0
        for height, txns_base64 in transactions.items():
            # wait until height arrives
            pending_height = wait_until_height_begin(to_web3_url(url), height, strict_mode) + 1
            if pending_height != height and strict_mode:
                print("{}Replay failed. Target height: {}, current height: {}{}".format(FAIL, height, pending_height, ENDC))
                exit(-1)

            # broadcast transactions on this height
            log_print(logfile, 'Height: {}___{}'.format(pending_height, height))
            for tx_base64 in txns_base64:
                tx_base64_bytes = tx_base64.encode("ascii")
                tx_string_bytes = base64.b64decode(tx_base64_bytes)
                tx_string = tx_string_bytes.decode("ascii")
                result = broadcast(to_abci_url(url), tx_base64, TxSync)
                if parse_tx_result(logfile, result):
                    tx_count += 1
                else:
                    exit(-1)

        print("{}Replay finished. {} transactions sent.{}".format(OKGREEN, tx_count, ENDC))


if __name__ == "__main__":
    # parse arguements
    dump, url, file, height, height_stop = parse_args()

    # dump or replay
    if dump:
        abcilog = ABCILog(file)
        abcilog.dump_txns(height, height_stop)
    else:
        replay(url, file, height, height_stop, False)

    exit(0)
