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
    Something = [
        [["List1.1", 1], ["List1.2"]],
        [["List2.1", 4], ["List2.2"]],
    ]


class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Player(BasePlayer):
    pass