from otree.api import (
    models,
    widgets,
    BaseConstants,
    BaseSubsession,
    BaseGroup,
    BasePlayer,
)
import settings
import string  # For completion code
import random

class Constants(BaseConstants):
    pass

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass



class Player(BasePlayer):

    P1_Acc_ArgAkz_4 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )

    money_in_month = models.IntegerField(
        label="Wie viel Geld steht Ihnen im Monat zur Verfügung (in Euro)?",
        choices=[
            [1, "0-399"],
            [2, "400-699"],
            [3, "700-999"],
            [4, "1000 oder mehr"],

        ]
    )