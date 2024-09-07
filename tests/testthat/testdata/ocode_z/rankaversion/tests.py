from . import pages
from ._builtin import Bot
from .models import Constants
from otree.api import Submission
import random
import time

class PlayerBot(Bot):

    def play_round(self):
        if self.player.choicetype == "noRR_needSat":
            prob_acc = 0.75
        elif self.player.choicetype == "RR_needSat":
            prob_acc = 0.65
        elif self.player.choicetype == "noRR_needy":
            prob_acc = 0.85
        elif self.player.choicetype == "RR_needy":
            prob_acc = 0.80
        else:
            prob_acc = 0  # Shouldnt happen

        # "YourRole"
        time.sleep(0)
        if self.round_number == 1:
            yield pages.YourRole

        # "Task"
        time.sleep(0)
        if self.participant.vars["kickout"] != 1 and \
                self.player.role() == "dictator" and \
                self.round_number <= self.participant.vars["RRrounds"]:
            if self.player.is_trick == 1:
                yield Submission(pages.Task, {
                    "acceptance": 1})
            elif self.player.is_trick == 2:
                yield Submission(pages.Task, {
                    "acceptance": 0})
            else:
                yield Submission(pages.Task, {
                    "acceptance": random.choices([0, 1], weights=[1-prob_acc, prob_acc])[0]
                })

        # "Waitpage" is not a real Waitpage and has to be klicked
        time.sleep(0)
        if self.participant.vars["kickout"] != 1 and \
                self.player.role() == "dictator" and \
                self.round_number < self.participant.vars["RRrounds"]:
            yield pages.TaskWaitPage
