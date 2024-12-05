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
    gender = models.IntegerField(
        doc = "Gender documentation with double quotations",
        label="What is your gender?",
    )

    gender2 = models.IntegerField(
        doc = 'Gender documentation with single quotations',
        label='What is your gender?',
    )

    gender3 = models.IntegerField(
        doc = """Gender documentation with tripple double quotations""",
        label='What is your gender?',
    )

    gender4 = models.IntegerField(
        doc = '''Gender documentation with tripple single quotations''',
        label='What is your gender?',
    )


    crt_bat = models.IntegerField(
        label='''
        A bat and a ball cost 22 dollars in total.
        The bat costs 20 dollars more than the ball.
        How many dollars does the ball cost?'''
    )

    crt_bat2 = models.IntegerField(
        label="""
        A bat and a ball cost 22 dollars in total.
        The bat costs 20 dollars more than the ball.
        How many dollars does the ball cost?
        """
    )