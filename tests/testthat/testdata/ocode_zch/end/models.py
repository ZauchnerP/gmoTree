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

author = 'Patricia Zauchner'

doc = """
This is a strongly modified part of the experiment for my dissertation.
"""


class Constants(BaseConstants):
    name_in_url = 'end'
    players_per_group = None
    num_rounds = 1
    chinese = 格吕克
    # <editor-fold desc="Payoff Structure">
    showupToken = settings.showupToken
    showupTokenTest = showupToken
    # Payoff Part 1
    payoffPart1_c = settings.payoffPart1_c  # Token
    # Payoff Survey
    payoff_survey = settings.payoff_survey  # Token
    # Weiters
    ExchangeMainCurrency = settings.ExchangeMainCurrency  # 4 Token = 1 Dollar
    ExchangeToken = settings.ExchangeToken
    ExchangeRate = ExchangeToken / ExchangeMainCurrency
    ExchangeRateTest = settings.ExchangeToken / settings.ExchangeMainCurrency
    # </editor-fold>


class Subsession(BaseSubsession):
    def creating_session(self):
        # Set language
        for p in self.get_players():
            p.participant.vars["planguage"] = "en"  # Alternatives: "en" and "de"

        # Create Completion Code
        for p in self.get_players():
            p.completionCode = ''.join(random.choices(string.ascii_uppercase + string.digits, k=9))


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    payoffToken = models.FloatField(doc="Token received for the whole experiment")
    part1Token = models.FloatField(doc="Token received for the redistribution game")
    part2Token = models.FloatField(doc="Token received for level 2")
    completionCode = models.StringField(
        doc="Completion Code for MTurk. Has no role for the experiment and will not be validated unless there are "
            "legal problems with the participants."
    )

    hierarchy_of_principlesArg = models.TextField(
        doc=""" Argument for Rejection """,
        label="",
        blank=True
    )

    hierNeeds = models.IntegerField(
        doc=""" Rank of Needs """,
        label="Rank of principle 'needs':",
        min=1, max=3
    )
    
    thisisaveryveryveryverylongvariablename = models.IntegerField(
      doc = "Testtesttest"
    ) 

    hierEquality = models.IntegerField(
        doc=""" Rank of Equality """,
        label="Rank of principle 'equality':",
        min=1, max=3
    )

    hierRank = models.IntegerField(
        doc=""" Rank of Ranking """,
        label="Rank of principle 'stable income rankings':",
        min=1, max=3
    )
