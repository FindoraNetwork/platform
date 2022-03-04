import os
import re
import json
from .constants import *


class ABCILog:
    def __init__(self, file):
        self.file = file
        self.path = os.path.dirname(file)

    def dump_txns(self, height_begin, height_stop):
        """
        Parameters:
            height_begin (int): the begining block (inclusive) to extract transactions
            height_end (int): the ending block (inclusive) to stop extraction. 0 means extract all transactions.
        """
        txns_found = {}
        txns_total = 0
        with open(self.file, 'r', encoding='utf-8') as logs:
            cur_txns = []
            cur_height = 0
            for line in logs:
                # Extract transaction if available
                match_txn = re.search('try_tx_catalog: "(.*)"', line)
                if match_txn:
                    tx_base64 = match_txn.group(1)
                    cur_txns.append(tx_base64)
                    continue

                # Extract height of commit
                match_commit = re.search('app_hash_commit: (.*)_(.*), height: (.*)', line)
                if match_commit:
                    cur_height = int(match_commit.group(3))

                    # Wait until block (height_begin-1) gets committed
                    if cur_height < height_begin:
                        cur_txns = []
                        continue

                    # Take transactions of this block
                    if len(cur_txns) > 0:
                        print("{}Block {}: scanned {} transactions{}".format(OKGREEN, cur_height, len(cur_txns), ENDC))
                        txns_total += len(cur_txns)
                        txns_found[cur_height] = cur_txns
                        cur_txns = []

                    # Stop scanning logs if height_stop arrives
                    if cur_height == height_stop:
                        break

        # the actual stop height
        height_stop = cur_height

        # return if no transaction found
        if txns_total == 0:
            print("{}No transactions found{}".format(WARNING, ENDC))
            return

        # dump transactions to json file
        txns_json = self.json_file(height_begin, height_stop)
        with open(txns_json, 'w') as f_out:
            self.json_dump(txns_found, f_out)
        print("{}{} transactions dumped into file: {}{}".format(OKGREEN, txns_total, txns_json, ENDC))

    def json_file(self, height_begin, height_stop):
        return os.path.join(self.path, "txns_{}_to_{}.json".format(height_begin, height_stop))

    def json_dump(self, txns, f_out):
        json.dump(txns, f_out, indent=4)
