from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
)
import random
import settings
import pandas as pd  # To import csv as dataframe

class Constants(BaseConstants):
    pass
    
class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer): 
    gender = models.StringField(
        choices=[["Male1", "Male"], ["Female", "Femaleval"]],
        label='What is your gender?',
        widget=widgets.RadioSelect,
    )

    gender2 = models.StringField(
        choices=[['Male', 'Male'], ['Female', 'Femaleval']],
        label='What is your gender?',
        widget=widgets.RadioSelect,
    )