from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
)
import random
from scipy.stats import rankdata
import math


author = 'Patricia Zauchner'

doc = """
Migration Game. 
Run only on Chrome. Never on IE!
In IE, the dialogue windows won't work.
"""

class Constants(BaseConstants):
    name_in_url = 'MIG1'
    players_per_group = 5
    num_rounds = 1  # Wie oft das Experiment durchläuft
    ShowUpFeeEuro = 12
    ExchangeEuro = 1
    ExchangeToken = 1
    ExchangeRate = ExchangeToken / ExchangeEuro

    ShowUpFeeToken = ShowUpFeeEuro * ExchangeRate
    P1_Needs = 16  # Formerly known as "Bedarf".
    UsualConsumption = 0.9  # 90% of the income will be consumed
    Rounds = 5
    time_for_args = 0.2
    time_for_argsSek = time_for_args * 60
    # <editor-fold desc="Profit => StartToken, Taxes, ZusatzToken">
    P1_GesamtZusatzToken = 30
    P1_TokenAdd_Equal = [30 / 5, 30 / 5, 30 / 5, 30 / 5, 30 / 5]  # Zusatztoken bei Gleichverteilung
    # <editor-fold desc="For Treatment 0">
    P1_StartToken_0 = [0, 14, 19, 26, 41]  # StartToken --> Netto After Taxes
    P1_Taxes_0 = [0, 1, 5, 9, 15]
    P1_StartTokenPT_0 = []  # StartToken --> Brutto Pretaxes
    for i in range(0, 5):
        P1_StartTokenPT_0.append(P1_StartToken_0[i] + P1_Taxes_0[i])
    P1_TokenAddWeakerFirst_0 = [21, 7, 2, 0, 0]
    P1_TokenAddMixed_0 = [16, 2, 3, 4, 5]  # so besprochen am 31.3.

    # </editor-fold>
    # <editor-fold desc="For Treatment 9">
    P1_StartToken_9 = [9, 14, 19, 26, 41]
    P1_Taxes_9 = [0, 1, 5, 9, 15]
    P1_StartTokenPT_9 = []  # StartToken --> Brutto Pretaxes
    for i in range(0, 5):
        P1_StartTokenPT_9.append(P1_StartToken_9[i] + P1_Taxes_9[i])
    P1_TokenAddWeakerFirst_9 = [11, 6, 3, 0, 0]
    P1_TokenAddMixed_9 = [6.5, 1.5, 1, 4, 7]

    # </editor-fold>
    # </editor-fold>
    # <editor-fold desc="Argumente">
    ArgCharMax = 250
    P1_Rej_ArgBestToken = 2
    P1_BVote_ArgBestToken = 2
    P1_AVote_ArgBestToken = 2
    P1_Acc_ArgBestToken = 2
    # </editor-fold>
    # <editor-fold desc="Zeiten (in Minuten">
    ChatTime_Ack = 6  # Todo: Check if time is correct. Should be 6 minutes.
    ChatTime_Ack_Sec = ChatTime_Ack * 60
    ChatTime_Distribution = 5  # Formerly known as ChatTime_Verteilung #Todo: Check if time is correct. Should be 5 minutes.
    ChatTime_DistributionCorr = 1  # Formerly known as ChatTime_VerteilungKorr #Todo: Check if time is correct. Should be 1 minute.
    ChatTime_DistributionCorrSek = ChatTime_DistributionCorr * 60  # Formerly known as ChatTime_VerteilungKorrSek
    # </editor-fold>

    MinArgumentLength = 10
    MaxArgumentLength = 250

    # Grafiken
    TickInterval = 5
    LineWidth = 5
    ArgumentChatHeight = 600


class Session:
    pass


class Subsession(BaseSubsession):
    pass

class Group(BaseGroup):
    pass

class Player(BasePlayer):
    user_total = models.IntegerField(
        min = 1,
        max = 9999,
        doc="user's summation",
        widget=widgets.TextInput(attrs='autocomplete':'off'}))

    # <editor-fold desc="P1 Acc --> Bestes Argument für neue Verteilung berechnen - Gewinner und Token">
    P1_Acc_nArgBest = models.IntegerField(doc="Number of votes for best vote")
    P1_Acc_ArgBest = models.IntegerField(initial=-99, doc="Whether participant is one of the best-vote winners.")
    P1_Acc_ArgToken = models.IntegerField(initial=0, doc="Token für the argument.")
    P1_Acc_ArgBestSingle = models.IntegerField(initial=-99)

    # </editor-fold>#
    # </editor-fold>
    # </editor-fold>

    P1_SumRoundIncome = models.FloatField(initial=-99)
