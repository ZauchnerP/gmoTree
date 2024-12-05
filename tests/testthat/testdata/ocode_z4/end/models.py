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
This is a strongly modified part of the experiment for my dissertation and other experiments
to test gmoTree on it.
"""


class Constants(BaseConstants):
    name_in_url = 'end'
    name_in_url2 = "end"
    players_per_group = None
    num_rounds = 1
    without = "This is a without test"
    singlequoteinsidedouble = "This is a 'test'"
    singlequoteinsidedouble2 = "This is a 'test' inside 'another test'"
    doublequoteinsidesingle = 'This is another "test"'
    doublequoteinsidesingle2 = 'This is another "test" inside "another test"'
    escapedsinglequote = 'This ia a \'single quote test\''
    escapeddoublequote = "This is a \"double quote test\""
    TOKEN_ADD_1D_NAME = settings.TokenAdd_1dName
    TOKEN_ADD_2D_NAME = settings.TokenAdd_2dName
    TOKEN_ADD_3D_NAME = settings.TokenAdd_3dName

class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass



class Player(BasePlayer):
    ConsumptionFinal = models.FloatField(
        doc="Final consumption \=  with TokenAdd as voted for by group as alternative distribution")
    
    ConsumptionFinal2 = models.FloatField(
        doc="Final consumption with TokenAdd as voted for by group as alternative distribution")
    
    completionCode = models.StringField(
        doc="Completion Code for MTurk. Has no role for the experiment and will not be validated unless there are "
            "legal problems with the participants"
    )

    # With spaces at end
    completionCodespaces = models.StringField(
        doc="Completion Code for MTurk. Has no role for the experiment and will not be validated unless there are "   
            "legal problems with the participants"
    )

    Acc_DistributionChange = models.IntegerField(
        choices=[
            [1, 'Ich hätte mich dennoch für Verfahren >' +
                C.TOKEN_ADD_1D_NAME + '< entschieden.'],
            [2, 'Ich hätte mich für das alternative Verfahren >' +
                C.TOKEN_ADD_2D_NAME + '<  entschieden.'],
            [3, 'Ich hätte mich für das alternative Verfahren >' +
                C.TOKEN_ADD_2D_NAME + '< entschieden.']
        ],
        doc="Former variable name: AnerkennungPrivat",

        widget=widgets.RadioSelect)

    Acc_DistributionChange2 = models.IntegerField(
        choices=[
            [1, "Ich hätte mich dennoch für Verfahren >" +
                C.TOKEN_ADD_1D_NAME + "< entschieden."],
            [2, "Ich hätte mich für das alternative Verfahren >" +
                C.TOKEN_ADD_2D_NAME + "<  entschieden."],
            [3, "Ich hätte mich für das alternative Verfahren >" +
                C.TOKEN_ADD_2D_NAME + "< entschieden."]
        ],
        doc='Former variable name: AnerkennungPrivat',

        widget=widgets.RadioSelect)

    hierarchy_of_principlesArg = models.TextField(
        doc=""" Argument for Rejection """,
        label="",
        blank=True
    )

    hierarchy_of_principlesArg2 = models.TextField(
        doc=''' Argument for Rejection ''',
        label="",
        blank=True
    )