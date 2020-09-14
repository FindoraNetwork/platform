import os

from json import dumps, loads
from locust import HttpUser, TaskSet, task, constant

headers = {'content-type': 'application/json'}

class QuickstartUser(HttpUser):
    wait_time = constant(0)
    def on_start(self):
        """ on_start is called when a Locust start before any task is scheduled """
        self.js = []
        self.curr = 0
        self.count = 0
        self.batch_size = int(os.environ.get('BATCH_SIZE', 1))
        with open(os.environ.get('LOG_FILE', 'test_txnlog_full')) as log_file:
            for line in log_file:
                self.js.append(line.strip())
                self.count += 1
        print('UserBehavior: count = {}, json[0]={}'.format(self.count, self.js[0]))

    def on_stop(self):
        """ on_stop is called when a Locust stop after every task has been scheduled """
        self.client.post(":8669/force_end_block")

    @task(1)
    def global_state(self):
        self.client.get(":8668/global_state")

    @task(1)
    def public_key(self):
        self.client.get(":8668/public_key")

    @task(10)
    def txn_from_file(self):
        self.client.post(":8669/submit_transaction", data=self.js[self.curr], headers=headers)
        self.curr += 1
        if self.curr % self.batch_size == 0:
            self.client.post(":8669/force_end_block")
        if self.curr >= self.count - 1:
            self.curr = 0
