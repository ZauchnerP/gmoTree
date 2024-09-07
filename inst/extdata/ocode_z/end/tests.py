from . import pages
from ._builtin import Bot


class PlayerBot(Bot):
    def play_round(self):
        if self.participant.vars["kickout"] != 1 and self.participant.vars["NoLevel2"] == 0:
            yield pages.Last, {
                "hierNeeds": 1,
                "hierEquality": 2,
                "hierRank": 3,
                "hierarchy_of_principlesArg": "no comment!"
            }
