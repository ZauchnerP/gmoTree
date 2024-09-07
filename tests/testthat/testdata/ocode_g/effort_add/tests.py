from . import *
import random
import time

class PlayerBot(Bot):

    def play_round(self):
        if self.session.vars.get("DoEffortTask", True):
            # Intro
            time.sleep(2)  # Sleep for x seconds
            yield Intro

            # TODO: Find way to properly test live-pages with bots

            # Task
            # while not self.player.final_round:
            # if self.round_number == 1:
            #     yield Submission(Task, {"user_total": 22},
            #                      check_html=False)
            # else:
            #     yield Submission(Task, {"user_total": random.randrange(1, 20, 1)},
            #                      check_html=False)

            time.sleep(2)  # Sleep for x seconds
            yield Submission(Task, {"user_total": random.randrange(1, 20, 1)}, check_html=False)

            # Result
            time.sleep(2)  # Sleep for x seconds
            yield Results
