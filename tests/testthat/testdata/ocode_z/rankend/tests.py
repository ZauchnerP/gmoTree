from . import pages
from ._builtin import Bot
import time

class PlayerBot(Bot):
    def play_round(self):
        time.sleep(0)
        if self.participant.vars["kickout"] != 1 and \
               self.session.config['dictator'] == 1:
            yield pages.Results, {"motivation": "My opinion is that soandso... Verstehst?"}

        if self.participant.vars["kickout"] != 1 and \
           self.session.config['dictator'] == 0:
            yield pages.ResultsAB
