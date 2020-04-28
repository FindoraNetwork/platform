import json
import sys

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        print('txn')
        for line in f:
            js = json.loads(line.strip())
            data = json.dumps(js["block"][0])
            print('\'', data, '\'', sep='')
