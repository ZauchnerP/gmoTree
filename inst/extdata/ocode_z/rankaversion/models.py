from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
)
import random
import settings
import pandas as pd  # To import csv as dataframe

author = 'Patricia Zauchner'

doc = """
Redistribution Game. Third party dictator game varying needs and ranks.
This is a strongly modified part of the experiment for my dissertation
to test gmoTree on it.
"""


class Constants(BaseConstants):
    name_in_url = 'part_1'
    players_per_group = None
    MinutesForSurvey = settings.MinutesForSurvey  # Part 2
    timer123 = settings.timer123
    min_argumentsize = 25
    max_argumentsize = 300
    # <editor-fold desc="Payoff Structure">
    # Payoff Structure
    payoffPart1_c = settings.payoffPart1_c  # Token
    max_payoffPart1_ab = settings.max_payoffPart1_ab  # Token
    randomtest =  settings.payoffPart1_c  +  settings.max_payoffPart1_ab
    payoff_survey = settings.payoff_survey  # Token
    # Weiters
    ExchangeMainCurrency = 1
    ExchangeToken = settings.ExchangeToken
    ExchangeRate = ExchangeToken / ExchangeMainCurrency
    # </editor-fold>

    maxtrickfalse = 2
    # <editor-fold desc="Choices">
    choices = [
        # Order: Endowment Richer, Endowment Poorer, Transfer, Threshold,f Type, Result Richer, Result Poorer
        # RR = rank reversal, noRR = no rank reversal
        [4, 1, 2, 0.5, "RR_needSat", 2, 3], 
        [5, 1, 3, 0.5, "RR_needSat", 2, 4],
        [5, 2, 2, 1.5, "RR_needSat", 3, 4],
        [6, 2, 3, 1.5, "RR_needSat", 3, 5],
        [6, 3, 2, 2.5, "RR_needSat", 4, 5],
        [7, 1, 4, 0.5, "RR_needSat", 3, 5],
        [7, 3, 3, 2.5, "RR_needSat", 4, 6],
        [7, 4, 2, 3.5, "RR_needSat", 5, 6],
        [8, 1, 4, 0.5, "RR_needSat", 4, 5],
        [8, 2, 4, 1.5, "RR_needSat", 4, 6],
        [8, 4, 3, 3.5, "RR_needSat", 5, 7],
        [8, 5, 2, 4.5, "RR_needSat", 6, 7],
        [9, 2, 4, 1.5, "RR_needSat", 5, 6],
        [9, 5, 3, 4.5, "RR_needSat", 6, 8],
        [9, 6, 2, 5.5, "RR_needSat", 7, 8],
        [10, 1, 5, 0.5, "RR_needSat", 5, 6],
        [10, 3, 4, 2.5, "RR_needSat", 6, 7],
        [10, 4, 4, 3.5, "RR_needSat", 6, 8],
        [10, 6, 3, 5.5, "RR_needSat", 7, 9],
        [11, 2, 5, 1.5, "RR_needSat", 6, 7],
        # Type 2 no needs
        [4, 1, 1, 0.5, "noRR_needSat", 3, 2],
        [5, 1, 1, 0.5, "noRR_needSat", 4, 2],
        [5, 2, 1, 1.5, "noRR_needSat", 4, 3],
        [6, 2, 1, 1.5, "noRR_needSat", 5, 3],
        [6, 3, 1, 2.5, "noRR_needSat", 5, 4],
        [7, 3, 1, 2.5, "noRR_needSat", 6, 4],
        [7, 4, 1, 3.5, "noRR_needSat", 6, 5],
        [8, 1, 3, 0.5, "noRR_needSat", 5, 4],
        [8, 2, 2, 1.5, "noRR_needSat", 6, 4],
        [8, 4, 1, 3.5, "noRR_needSat", 7, 5],
        [8, 5, 1, 4.5, "noRR_needSat", 7, 6],
        [9, 2, 3, 1.5, "noRR_needSat", 6, 5],
        [9, 5, 1, 4.5, "noRR_needSat", 8, 6],
        [9, 6, 1, 5.5, "noRR_needSat", 8, 7],
        [10, 1, 4, 0.5, "noRR_needSat", 6, 5],
        [10, 3, 3, 2.5, "noRR_needSat", 7, 6],
        [10, 4, 2, 3.5, "noRR_needSat", 8, 6],
        [10, 6, 1, 5.5, "noRR_needSat", 9, 7],
        [11, 2, 4, 1.5, "noRR_needSat", 7, 6],
        [7, 1, 2, 0.5, "noRR_needSat", 5, 3],
        # Type 2 needs
        [4, 1, 1, 1.5, "noRR_needy", 3, 2],
        [5, 1, 1, 1.5, "noRR_needy", 4, 2],
        [5, 2, 1, 2.5, "noRR_needy", 4, 3],
        [6, 2, 1, 2.5, "noRR_needy", 5, 3],
        [6, 3, 1, 3.5, "noRR_needy", 5, 4],
        [7, 3, 1, 3.5, "noRR_needy", 6, 4],
        [7, 4, 1, 4.5, "noRR_needy", 6, 5],
        [8, 1, 3, 1.5, "noRR_needy", 5, 4],
        [8, 2, 2, 2.5, "noRR_needy", 6, 4],
        [8, 4, 1, 4.5, "noRR_needy", 7, 5],
        [8, 5, 1, 5.5, "noRR_needy", 7, 6],
        [9, 2, 3, 2.5, "noRR_needy", 6, 5],
        [9, 5, 1, 5.5, "noRR_needy", 8, 6],
        [9, 6, 1, 6.5, "noRR_needy", 8, 7],
        [10, 1, 4, 1.5, "noRR_needy", 6, 5],
        [10, 3, 3, 3.5, "noRR_needy", 7, 6],
        [10, 4, 2, 4.5, "noRR_needy", 8, 6],
        [10, 6, 1, 6.5, "noRR_needy", 9, 7],
        [11, 2, 4, 2.5, "noRR_needy", 7, 6],
        [7, 1, 2, 1.5, "noRR_needy", 5, 3],
        # Type 1 needs
        [4, 1, 2, 1.5, "RR_needy", 2, 3],
        [5, 1, 3, 1.5, "RR_needy", 2, 4],
        [5, 2, 2, 2.5, "RR_needy", 3, 4],
        [6, 2, 3, 2.5, "RR_needy", 3, 5],
        [6, 3, 2, 3.5, "RR_needy", 4, 5],
        [7, 1, 4, 1.5, "RR_needy", 3, 5],
        [7, 3, 3, 3.5, "RR_needy", 4, 6],
        [7, 4, 2, 4.5, "RR_needy", 5, 6],
        [8, 1, 4, 1.5, "RR_needy", 4, 5],
        [8, 2, 4, 2.5, "RR_needy", 4, 6],
        [8, 4, 3, 4.5, "RR_needy", 5, 7],
        [8, 5, 2, 5.5, "RR_needy", 6, 7],
        [9, 2, 4, 2.5, "RR_needy", 5, 6],
        [9, 5, 3, 5.5, "RR_needy", 6, 8],
        [9, 6, 2, 6.5, "RR_needy", 7, 8],
        [10, 1, 5, 1.5, "RR_needy", 5, 6],
        [10, 3, 4, 3.5, "RR_needy", 6, 7],
        [10, 4, 4, 4.5, "RR_needy", 6, 8],
        [10, 6, 3, 6.5, "RR_needy", 7, 9],
        [11, 2, 5, 2.5, "RR_needy", 6, 7],
        # Trick Questions
        # Order: Endowment Richer, Endowment Poorer, Transfer, Threshold, Type, Result Richer, Result Poorer
        [7, 4, 2, 4.5, "Trick1", None, None],
        [10, 4, 2, 4.5, "Trick2", None, None],
        [4, 1, 1, 1.5, "Trick1", None, None],
        [5, 2, 2, 1.5, "Trick2", None, None]
    ]
    num_choices = len(choices)  # All possible choices
    # </editor-fold>
    # print("choices per type 1: ", num_choicesperrr, " and choices per type 2: ",num_choicespernorr)

    num_choicetypes = 4  # Maximum value. Correct value is defined in subsession.
    num_choicespertype = 20  # Maximum value = 20. Correct value is defined in subsession.
    num_trickquestions = 4  # Number of attention checks
    num_choicetotal = num_choicetypes * num_choicespertype + num_trickquestions
    num_rounds = num_choicetotal  # Number of questions. Must be a multiple of 4 + trick questions

    assignmentdf = pd.read_csv("rankend/assignment.csv")


class Subsession(BaseSubsession):

    def creating_session(self):
        # Set language
        for p in self.get_players():
            p.participant.vars["planguage"] = "en"  # Alternatives: "en" and "de"

        # <editor-fold desc="Create roles, set round number, choicespertype">
        for p in self.get_players():
            p.role()
            p.participant.vars["RRrounds"] = \
                self.session.config["num_choicespertype"] * Constants.num_choicetypes + Constants.num_trickquestions
        # </editor-fold >

        # <editor-fold desc="For Player A and B: Assignment of the dictators' decisions">
        if self.session.config['dictator'] == 0:
            if self.round_number == 1:

                # Randomize participants for payout
                listofpart = [x for x in range(0, self.session.num_participants, 1)]
                random.shuffle(listofpart)

                # Select data and set it to a variable
                # TODO: Caution. If there is a warning message "single positional indexer is out-of-bounds",
                # TODO: then the number of participants is higher than the cases in the csv!
                for p in self.get_players():

                    # <editor-fold desc="Player AC assigned values">
                    p.participant.vars["assignedRole"] = (
                        Constants.assignmentdf.iloc[listofpart[p.id_in_subsession-1]][
                            'role'])

                    p.participant.vars["profit_level1"] = (
                        Constants.assignmentdf.iloc[listofpart[p.id_in_subsession-1]][
                            'final'])

                    p.participant.vars["ab_dictator"] = (
                        Constants.assignmentdf.iloc[listofpart[p.id_in_subsession-1]][
                            'participant.code'])

                    p.participant.vars["needs_threshold"] = (
                        Constants.assignmentdf.iloc[listofpart[p.id_in_subsession-1]][
                            'player.needs_threshold_fix'])

                    p.participant.vars["ab_needs_met"] = (
                        Constants.assignmentdf.iloc[listofpart[p.id_in_subsession-1]][
                            'needsmet'])
                    # </editor-fold>
        # </editor-fold >

        # <editor-fold desc="Dictator: Define order of all questions">
        for p in self.get_players():
            if self.session.config['dictator'] == 1:
                # <editor-fold desc="Define order of all questions">
                if self.round_number == 1:
                    # Important: Ony assign the orderlist once in round_number 1. Otherwise it is changed each round.
                    # Explanation: If not all decision situations are played (Pretest),
                    # take the first choices for each choice types
                    orderlist = random.sample(
                        list(range(0, self.session.config["num_choicespertype"])) +
                        list(range(20, 20 + self.session.config["num_choicespertype"])) +
                        list(range(20 + 20, 20 + 20 + self.session.config["num_choicespertype"])) +
                        list(range(20 + 20 + 20, 20 + 20 + 20 + self.session.config["num_choicespertype"])),
                        p.participant.vars["RRrounds"] - Constants.num_trickquestions)

                    # Insert trick questions to list
                    orderlist.insert(10, 80)
                    orderlist.insert(30, 81)
                    orderlist.insert(50, 82)
                    orderlist.insert(70, 83)
                    p.participant.vars["order_of_questions"] = orderlist
                    # print("Order of Questions for Player ", p.participant.id_in_session, "    ", orderlist)
                    # Order of Richest (Zuweisung ob A oder B reicher sind):
                    # 0 : A is the richest, 1: B is the richest
                    p.participant.vars["order_of_richest"] = \
                        random.sample(([0] * int(p.participant.vars["RRrounds"] / 2) +
                                       [1] * int(Constants.num_choices / 2)),
                                      p.participant.vars["RRrounds"])
                # </editor-fold>

                # <editor-fold desc="Define questions per round">
                if self.round_number <= p.participant.vars["RRrounds"]:
                    p.choicenumber = p.participant.vars["order_of_questions"][self.round_number - 1]
                    p.a_richer = p.participant.vars["order_of_richest"][self.round_number - 1]
                    # Endowments of A and B
                    if p.a_richer == 1:
                        p.endowmentA = Constants.choices[
                            p.participant.vars["order_of_questions"][self.round_number - 1]][0]
                        p.endowmentB = Constants.choices[
                            p.participant.vars["order_of_questions"][self.round_number - 1]][1]
                    else:
                        p.endowmentA = Constants.choices[
                            p.participant.vars["order_of_questions"][self.round_number - 1]][1]
                        p.endowmentB = Constants.choices[
                            p.participant.vars["order_of_questions"][self.round_number - 1]][0]
                    # Trick question
                    p.is_trick = 0
                    if Constants.choices[p.participant.vars["order_of_questions"][self.round_number - 1]][4] == "Trick1":
                        p.is_trick = 1
                    if Constants.choices[p.participant.vars["order_of_questions"][self.round_number - 1]][4] == "Trick2":
                        p.is_trick = 2
                    # Transfer, Needs and choicetype
                    p.transfer = Constants.choices[p.participant.vars["order_of_questions"][self.round_number - 1]][2]
                    p.needs_threshold = Constants.choices[p.participant.vars["order_of_questions"][self.round_number - 1]][3]
                    p.choicetype = Constants.choices[p.participant.vars["order_of_questions"][self.round_number - 1]][4]
                # </editor-fold>

                p.participant.vars["wrong_tricks"] = 0  # create trick question participant var

        # </editor-fold >


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    # User-Entries:
    acceptance = models.IntegerField(
        doc="Did the dictator accept the suggested transfer?",
        label="",
        choices=[
            [1, "Yes"],
            [0, "No"],
            [-1, "Wrong trick answer"],
            [-2, "Correct trick answer"],
        ],
        widget=widgets.RadioSelect,
    )

    # <editor-fold desc="Dictator: Decision situations">
    choicenumber = models.IntegerField()
    a_richer = models.IntegerField(
        doc="""Is A the richer person?""",

        choices=[
            [1, "A is the richer person"],
            [0, "B is the richer person"],
        ],
    )
    is_trick = models.IntegerField(
        doc="""Is this question a trick question?""",

        choices=[
            [0, "This is not a trick question"],
            [1, "Answer Yes trick"],
            [2, "Answer No trick"]]
    )

    endowmentA = models.FloatField(doc="The initial endowment of Person A")
    endowmentB = models.FloatField(doc="The initial endowment of the poorer person")
    transfer = models.FloatField(doc="Transfer from richer person to poorer person")
    needs_threshold = models.FloatField(doc="Needs threshold")
    choicetype = models.StringField(doc="Which choice type?")
    # </editor-fold>

    def role(self):
        if self.session.config['dictator'] == 1:
            return "dictator"
        elif self.session.config['dictator'] == 0:
            return "others"
        else:
            print("Error in role definition!")
