import os

from json import dumps, loads
from locust import HttpLocust, TaskSet, task, constant

class UserBehavior(TaskSet):
    def on_start(self):
        """ on_start is called when a Locust start before any task is scheduled """
        self.js = []
        self.curr = 0
        self.count = 0
        with open(os.environ.get('LOG_FILE', 'test_txnlog_full') as log_file:
            for line in log_file:
                self.js.append(line.strip())
                self.count += 1
        print('UserBehavior: count = {}, json[0]={}'.format(self.count, self.js[0]))

    @task(1)
    def global_state(self):
        self.client.get(":8668/global_state")

    @task(1)
    def public_key(self):
        self.client.get(":8668/public_key")

    @task(1)
    def submit_txn(self):
        headers = {'content-type': 'application/json'}
        self.client.post(":8669/submit_transaction", data=self.js[self.curr], headers=headers)
        self.client.post(":8669/force_end_block")
        self.curr += 1
        if self.curr >= self.count - 1:
            self.curr = 0

class WebsiteUser(HttpLocust):
  task_set = UserBehavior
  wait_time = constant(0)
