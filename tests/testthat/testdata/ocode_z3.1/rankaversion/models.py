from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
)
import random
import settings
import pandas as pd  # To import csv as dataframe


doc = """
Example code just for testing gmoTree.
"""


class Constants(BaseConstants):
    Testnumber = settings.Testnumber
    StartToken = settings.StartToken
    StartToken2 = settings.StartToken * 2
    StartToken3 = StartToken * 10
    Something = settings.Something
    TestToken = StartToken * 5
    players_per_group = None
    Variable1 = "xyz"
    Variable2 = "aaa"
    Variable3 = "bbb"

    assignmentdf = pd.read_csv("rankend/assignment.csv")


class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Player(BasePlayer):
    # User-Entries:

    # Arguments Acceptance
    Acc_DistributionChange = models.IntegerField(
        choices=[
            [1, 'Ich hätte mich dennoch für Verfahren >' +
                Constants.Variable1 + '< entschieden.'],
            [2, 'Ich hätte mich für das alternative Verfahren >' +
                Constants.Variable2 + '<  entschieden.'],
            [3, 'Ich hätte mich für das alternative Verfahren >' +
                Constants.Variable3 + '< entschieden.']
        ],
        doc="Former variable name: AnerkennungPrivat",

        widget=widgets.RadioSelect)


    acceptance = models.IntegerField(
        doc="Did the dictator accept the suggested transfer, or not?",
        label="",
        choices=[
            [1, Constants.Variable1],
            [0, Constants.Variable2],
            [-1, "Wrong, trick answer"],
            [-2, "Correct trick answer"],
        ],
        widget=widgets.RadioSelect,
    )

    choicenumber = models.IntegerField()