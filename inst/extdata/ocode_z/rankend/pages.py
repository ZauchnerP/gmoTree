from ._builtin import Page
from .models import Constants
import random


# Results page only for dictators
class Results(Page):
    def is_displayed(self):
        # <editor-fold desc ="Get kickout">
        if "kickout" not in self.participant.vars:
            self.participant.vars["kickout"] = 0
            print("(Pretest) Kickout not taken from participant vars.")
        # </editor-fold>
        return self.participant.vars["kickout"] != 1 and \
            self.session.config['dictator'] == 1

    def vars_for_template(self):
        # For dictator
        try:
            self.player.assignedRole = "C"
            self.player.done_distribution = self.participant.vars["done_distribution"]
            self.player.endowmentA_fix = self.participant.vars["endowmentA_fix"]
            self.player.endowmentB_fix = self.participant.vars["endowmentB_fix"]
            self.player.transfer_fix = self.participant.vars["transfer_fix"]
            self.player.needs_threshold_fix = self.participant.vars["needs_threshold_fix"]
            self.player.acceptance_fix = self.participant.vars["acceptance_fix"]
            self.player.finalA = self.participant.vars["finalA"]
            self.player.finalB = self.participant.vars["finalB"]
            self.player.needs_threshold_metA = self.participant.vars["needs_threshold_metA"]
            self.player.needs_threshold_metB = self.participant.vars["needs_threshold_metB"]
        except:
            print("Rankend get distribution numbers: !-- CAUTION: PRETEST VALUES --!")
            self.player.assignedRole = "C"
            self.player.done_distribution = 1
            self.player.endowmentA_fix = 1
            self.player.endowmentB_fix = 2
            self.player.transfer_fix = 1
            self.player.needs_threshold_fix = 0.5
            self.player.acceptance_fix = 1
            self.player.finalA = 2
            self.player.finalB = 1
            self.player.needs_threshold_metA = 1
            self.player.needs_threshold_metB = 1

        # Part 2 handling
        self.participant.vars["NoLevel2"] = 0

        # Payment
        self.participant.vars["part1Token"] = float(Constants.payoffPart1_c)

        return {
            'distribution_number': self.player.done_distribution,
            "A": self.player.endowmentA_fix,
            "finalA": self.player.finalA,
            "finalB": self.player.finalB,
            "B": self.player.endowmentB_fix,
            "transfer": self.player.transfer_fix,
            "threshold": self.player.needs_threshold_fix,
            "acceptance": self.player.acceptance_fix,
        }

    form_model = "player"
    form_fields = ["motivation"]


# Results page only for A and B
class ResultsAB(Page):
    def is_displayed(self):
        # <editor-fold desc ="Get kickout">
        if "kickout" not in self.participant.vars:
            self.participant.vars["kickout"] = 0
            print("(Pretest) Kickout not taken from participant vars.")
        # </editor-fold>
        return self.participant.vars["kickout"] != 1 and \
            self.session.config['dictator'] == 0

    def vars_for_template(self):
        self.player.motivation = "Player is not dictator and must not answer this question."


        # <editor-fold desc="Select data and set it to a variable"> 
        # Pretest
        if "assignedRole" not in self.participant.vars:  # if not already assigned in the prior app (pretest)
            listofpart = [x for x in range(0, self.session.num_participants, 1)]
            random.shuffle(listofpart)
            print("Rankend (Pretest): Create new profits for Part 1")
            # <editor-fold desc="Player AC assigned values">
            self.player.assignedRole = (
                Constants.assignmentdf.iloc[listofpart[self.player.id_in_subsession-1]][
                    'role'])

            self.player.ab_profit_level1 = (
                Constants.assignmentdf.iloc[listofpart[self.player.id_in_subsession-1]][
                    'final'])

            self.player.ab_dictator = (
                Constants.assignmentdf.iloc[listofpart[self.player.id_in_subsession-1]][
                    'participant.code'])

            self.player.needs_threshold = (
                Constants.assignmentdf.iloc[listofpart[self.player.id_in_subsession-1]][
                    'player.needs_threshold_fix'])

            self.player.ab_needs_met = (
                Constants.assignmentdf.iloc[listofpart[self.player.id_in_subsession-1]][
                    'needsmet'])
            # </editor-fold>

        # Not pretest: Values were already assigned in previous stages.
        elif "assignedRole" in self.participant.vars:  # Dann sind die anderen auch da
            # <editor-fold desc="Player AC assigned values">
            self.player.assignedRole = self.participant.vars["assignedRole"]

            self.player.ab_profit_level1 = self.participant.vars["profit_level1"]

            self.player.ab_dictator = self.participant.vars["ab_dictator"]

            self.player.needs_threshold = self.participant.vars["needs_threshold"]

            self.player.ab_needs_met = self.participant.vars["ab_needs_met"]

        # </editor-fold>

        # <editor-fold desc="Level 2 handling">
        if self.player.ab_needs_met == 0:
            self.participant.vars["NoLevel2"] = 1
        else:
            self.participant.vars["NoLevel2"] = 0
        # </editor-fold>

        # Payment
        self.participant.vars["part1Token"] = self.player.ab_profit_level1


page_sequence = [
    Results,
    ResultsAB]
