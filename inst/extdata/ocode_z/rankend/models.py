from otree.api import (
    models,
    widgets,
    BaseConstants,
    BaseSubsession,
    BaseGroup,
    BasePlayer,
)
import settings
import pandas as pd  # To import csv as dataframe

author = 'Patricia Zauchner'

doc = """
Redistribution Game (end). Show chosen distribution of rankaversion task.
"""


class Constants(BaseConstants):
    name_in_url = 'part1_'
    players_per_group = None
    num_rounds = 1
    min_argumentsize = 25
    max_argumentsize = 300

    # Payoff Structure
    payoffPart1_c = settings.payoffPart1_c  # Token
    # Weiters
    ExchangeMainCurrency = 1
    ExchangeToken = settings.ExchangeToken
    ExchangeRate = ExchangeToken / ExchangeMainCurrency
    # </editor-fold>

    assignmentdf = pd.read_csv("rankend/assignment.csv")


class Subsession(BaseSubsession):
    def creating_session(self):
        # Set language
        for p in self.get_players():
            p.participant.vars["planguage"] = "en"  # Alternatives: "en" and "de"

        # print("Show content of csv: ", Constants.assignmentdf)


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    motivation = models.TextField(
        doc="Motivation for acceptance/rejection as asked in results page",
        max_length=Constants.max_argumentsize
    )

    # <editor-fold desc="Played for A and B: Chosen distribution variables.">
    assignedRole = models.CharField(
        doc="Player A and B and C: The assigned roles in level 1. Player C are assigned automatically. "
            "The roles of Player A and B are received from the csv file.",

    )
    ab_profit_level1 = models.FloatField(
        doc="Player A and B: The assigned amount of token from Part 1.",

    )
    ab_dictator = models.CharField(
        doc="Player A and B: Who was the dictator of the assigned amount of token from Part 1.",

    )
    needs_threshold = models.FloatField(
        doc="Player A and B: Needs threshold",

    )
    ab_needs_met = models.IntegerField(
        doc="Player A and B: Did the person reach the needs threshold?",
        choices=[
            [0, "Needs threshold not met"],
            [1, "Needs threshold met"]
        ],

    )
    # </editor-fold>

    # <editor-fold desc="Played for player C: Chosen distribution variables.">
    done_distribution = models.IntegerField(

    )
    endowmentA_fix = models.FloatField(doc="Chosen distribution of player C: The initial endowment of player A.",
                               )
    endowmentB_fix = models.FloatField(doc="Chosen distribution of player C: The initial endowment of player B.",
                               )
    transfer_fix = models.FloatField(doc="Chosen distribution of player C: Transfer from player A to player B",
                             )
    finalA = models.FloatField(doc="Chosen distribution of player C: The final endowment of player A.",
                       )
    finalB = models.FloatField(doc="Chosen distribution of player C: The initial endowment of player B.",
                       )
    needs_threshold_fix = models.FloatField(doc="Chosen distribution of player C: Needs threshold.",
                                    )
    needs_threshold_metA = models.IntegerField(doc="Chosen distribution of player C: Did A meet the needs threshold?",
                                       )
    needs_threshold_metB = models.IntegerField(doc="Chosen distribution of player C: Did B meet the needs threshold?",
                                       )
    acceptance_fix = models.IntegerField(doc="Chosen distribution of player C: Did C accept the redistribution?",
                                 )
    # </editor-fold>
