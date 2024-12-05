from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
)
import random
import settings
import pandas as pd  # To import csv as dataframe

author = 'Patricia Zauchner'

doc = """
Redistribution Game. Third party dictator game varying needs and ranks.
Major changes to test gmoTree package.
"""


class Constants(BaseConstants):
    choices = [
        [8, 2, 2, 2.5, "noRR_needy", settings.v2, 4],
        [80, 20, 2, 2.5, "noRR_needy", "settings.shouldnotwork", 4],
        ["8", 2, 2, 2.5, "noRR_needy", settings.v2, 4]
    ]
    
class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer): 
    pass