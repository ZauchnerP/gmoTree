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
    Something = "[[information]"

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass


class Player(BasePlayer):
    gender = models.IntegerField(doc = "[[Gender]")

    variable = models.IntegerField(doc = "(A doc in brackets))")

    variable2 = models.IntegerField(doc = "A doc with one closing bracket)")