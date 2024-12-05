from ._builtin import Page, WaitPage
from .models import Constants
import settings
import random
from otreeutils.pages import ExtendedPage


class Last(Page):
    def is_displayed(self):
        # <editor-fold desc ="Get kickout">
        if "kickout" not in self.participant.vars:
            self.participant.vars["kickout"] = 0
            print("(Pretest) Kickout not taken from participant vars.")
        # </editor-fold>
        # <editor-fold desc ="Get Part 2">
        if not "NoLevel2" in self.player.participant.vars:
            self.player.participant.vars["NoLevel2"] = 0
        # </editor-fold>
        return self.participant.vars["kickout"] != 1 and self.participant.vars["NoLevel2"] == 0

    form_model = "player"

    # Randomize questions
    def get_form_fields(self):
        mylist = ["hierNeeds", "hierEquality", "hierRank"]
        random.shuffle(mylist)  # Shuffles original list
        return ["hierarchy_of_principlesArg"] + mylist


class End(Page):
    """" End page with payoff """
    def is_displayed(self):
        return self.participant.vars["kickout"] != 1

    def vars_for_template(self):
        # <editor-fold desc ="Get Part 2">
        if not "NoLevel2" in self.participant.vars:
            self.player.participant.vars["NoLevel2"] = 0
        # </editor-fold>


        # <editor-fold desc ="Get payoff">
        try:
            self.player.part1Token = self.participant.vars["part1Token"]   # Are defined in rankend
            if self.participant.vars["NoLevel2"] == 0:
                self.player.part2Token = float(Constants.payoff_survey)
            elif self.participant.vars["NoLevel2"] == 1:
                self.player.part2Token = 0
            else:
                print("Error in payoff calculation!")
            self.player.payoffToken = float(Constants.showupToken) + self.player.part1Token + self.player.part2Token
            self.player.payoff = self.player.payoffToken / settings.ExchangeToken
        except:
            print("!!!! Caution: Only pretest values in endpage!!!!")
            self.player.part1Token = 24
            self.player.part2Token = 12
            self.player.payoffToken = float(Constants.showupToken) + self.player.part1Token + self.player.part2Token
            self.player.payoff = self.player.payoffToken/settings.ExchangeToken
        # </editor-fold>

        return {"Exchange": settings.ExchangeToken,
                "Showup": Constants.showupToken}


page_sequence = [
    Last,
    End]