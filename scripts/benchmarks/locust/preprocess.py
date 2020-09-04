import json
import sys

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        for line in f:
            js = json.loads(line.strip())
            js["block"][0]["seq_id"] = 0
            print(json.dumps(js["block"][0]))
#            print('\'', data, '\'', sep='')
