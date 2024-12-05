from ._builtin import Page, WaitPage
from .models import Constants
from otreeutils.pages import ExtendedPage
import random


class YourRole(Page):
    def is_displayed(self):
        # <editor-fold desc ="Get kickout">
        if "kickout" not in self.participant.vars:
            self.participant.vars["kickout"] = 0
            print("(Pretest) Kickout not taken from participant vars.")
        # </editor-fold>
        return self.round_number == 1 and self.participant.vars["kickout"] != 1


class Task(ExtendedPage):
    custom_name_in_url = "decision"

    def is_displayed(self):
        return self.participant.vars["kickout"] != 1 and \
            self.player.role() == "dictator" and \
            self.round_number <= self.participant.vars["RRrounds"]

    def vars_for_template(self):
        if self.player.endowmentA > self.player.endowmentB:
            a_richer = 1
        else:
            a_richer = 0
        return {
            "a_richer": a_richer}

    form_model = "player"
    form_fields = ['acceptance']

    def before_next_page(self):
        # <editor-fold desc="Check trick questions">
        # <editor-fold desc="Trick1 = Yes must be klicked">
        if self.player.is_trick == 1:
            if self.player.acceptance == 0:  # If trick question is answered incorrectly
                self.player.acceptance = -1
                self.participant.vars["wrong_tricks"] = self.participant.vars["wrong_tricks"] + 1
                if self.participant.vars["wrong_tricks"] == Constants.maxtrickfalse:
                    self.participant.vars["kickout"] = 1
                    self.participant.vars["kickoutReason"] = "attentionCheck"
            else:
                self.player.acceptance = -2  # If trick question is answered correctly
        # </editor-fold>
        # <editor-fold desc="Trick2 = No must be klicked">
        if self.player.is_trick == 2:
            if self.player.acceptance == 1:  # If trick question is answered incorrectly
                self.player.acceptance = -1
                self.participant.vars["wrong_tricks"] = self.participant.vars["wrong_tricks"] + 1
                if self.participant.vars["wrong_tricks"] == Constants.maxtrickfalse:
                    self.participant.vars["kickout"] = 1
                    self.participant.vars["kickoutReason"] = "attentionCheck"
            else:
                self.player.acceptance = -2  # If trick question is answered correctly
        # </editor-fold>
        # </editor-fold>

        # <editor-fold desc="Done distribution">
        if self.round_number == self.participant.vars["RRrounds"] and self.participant.vars["kickout"] != 1:
            print("-- Rankaversion -- Save Participant vars.")
            done_distribution = random.randint(
                1, self.participant.vars["RRrounds"] - 4)  # 4 trickquestions
            self.participant.vars["done_distribution"] = done_distribution
            self.participant.vars["endowmentA_fix"] = self.player.in_round(
                done_distribution).endowmentA
            self.participant.vars["endowmentB_fix"] = self.player.in_round(
                done_distribution).endowmentB
            self.participant.vars["transfer_fix"] = self.player.in_round(
                done_distribution).transfer
            self.participant.vars["needs_threshold_fix"] = self.player.in_round(
                done_distribution).needs_threshold
            self.participant.vars["acceptance_fix"] = self.player.in_round(
                done_distribution).acceptance

            if self.participant.vars["acceptance_fix"] == 1:
                if self.player.in_round(done_distribution).a_richer == 1:
                    self.participant.vars["finalA"] = self.participant.vars["endowmentA_fix"] - \
                        self.participant.vars["transfer_fix"]
                    self.participant.vars["finalB"] = self.participant.vars["endowmentB_fix"] + \
                        self.participant.vars["transfer_fix"]
                else:
                    self.participant.vars["finalA"] = self.participant.vars["endowmentA_fix"] + \
                        self.participant.vars["transfer_fix"]
                    self.participant.vars["finalB"] = self.participant.vars["endowmentB_fix"] - \
                        self.participant.vars["transfer_fix"]
            else:
                self.participant.vars["finalA"] = self.participant.vars["endowmentA_fix"]
                self.participant.vars["finalB"] = self.participant.vars["endowmentB_fix"]
            if self.participant.vars["finalA"] >= self.participant.vars["needs_threshold_fix"]:
                self.participant.vars["needs_threshold_metA"] = 1
            else:
                self.participant.vars["needs_threshold_metA"] = 0
            if self.participant.vars["finalB"] >= self.participant.vars["needs_threshold_fix"]:
                self.participant.vars["needs_threshold_metB"] = 1
            else:
                self.participant.vars["needs_threshold_metB"] = 0
        # </editor-fold>


class TaskForPresentationReasons(Page):

    # Type 1
    def vars_for_template(self):
        return {
            "playerA": 4,
            "Needs": 1,
            "playerB": 1,
            "Transfer": 1,
        }

    # Type 2
    def vars_for_template(self):
        return {
            "playerA": 4,
            "Needs": 2,
            "playerB": 1,
            "Transfer": 1,
        }

    # Type 3
    def vars_for_template(self):
        return {
            "playerA": 4,
            "Needs": 1,
            "playerB": 1,
            "Transfer": 2,
        }

    # Type 4
    def vars_for_template(self):
        return {
            "playerA": 4,
            "Needs": 2,
            "playerB": 1,
            "Transfer": 2,
        }


class TaskWaitPage(ExtendedPage):
    custom_name_in_url = "nextdecision"
    """ Waitpage between two tasks. 
    Not really a Waitpage, because the participant doesn't need to wait for the others. """

    def is_displayed(self):
        return self.participant.vars["kickout"] != 1 and \
            self.player.role() == "dictator" and \
            self.round_number < self.participant.vars["RRrounds"]

    def vars_for_template(self):
        # <editor-fold desc="Check trick">
        if self.player.round_number > 1:
            if self.player.in_round(self.round_number).acceptance == -1:
                return {"Trickfalse": True}   # Answered incorrectly
            else:
                return {"Trickfalse": False}  # Answered correctly
        else:
            return {"Trickfalse": False}      # First round
        # </editor-fold>


page_sequence = [
    TaskForPresentationReasons
]
