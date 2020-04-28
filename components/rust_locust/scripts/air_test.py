import os

from json import dumps, loads
from locust import HttpLocust, TaskSet, task, constant
from rust_locust.air import AIR

headers = {'content-type': 'application/json'}

class UserBehavior(TaskSet):
    def on_start(self):
        self.air = AIR()
        """ on_start is called when a Locust start before any task is scheduled """
        self.air_count = 0

    @task(1)
    def air_txn(self):
        txn = self.air.make_assign_txn()
        self.client.post(":8669/submit_transaction", data=txn, headers=headers)
        self.client.post(":8669/force_end_block")
        self.air_count += 1

class WebsiteUser(HttpLocust):
    task_set = UserBehavior
    wait_time = constant(0)
