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
    def creating_session(self):
        """"
            create ID, StartToken, Wealth, and Treatments;
        """
        # <editor-fold desc="Assign group membership">
        # Copy the group and player id structure of the first app
        if "id_matrix" in self.session.vars:
            self.set_group_matrix(self.session.vars['id_matrix'])
        else:
            self.group_randomly()
        # </editor-fold>
        # <editor-fold desc="Assign treatments: Economic and political participation">
        # Assign Treatments: Economic participation
        if "session_treatment_ec" in self.session.config:  # if treatment was already assigned in session config
            for g in self.get_groups():
                g.treatment_ec = self.session.config['session_treatment_ec']
        else:
            for g in self.get_groups():
                g.treatment_ec = 0  # Todo: Here you can enter the treatment manually.
        # Assign Treatments: Political participation
        if "session_treatment_pol" in self.session.config:  # if treatment was already assigned in session config
            for g in self.get_groups():
                g.treatment_pol = self.session.config['session_treatment_pol']
        else:
            for g in self.get_groups():
                g.treatment_pol = 1  # Todo: Here you can enter the treatment manually.
        # </editor-fold>
        # <editor-fold desc="Exclude player 1 from economic and political participation">
        for p in self.get_players():  # Exclusion of player 1 in effort task:
            if "Economically_Excluded" in p.participant.vars:
                p.Economically_Excluded = p.participant.vars['Economically_Excluded']
            else:
                if p.group.treatment_ec == 0 and p.id_in_group == 1:
                    p.participant.vars['Economically_Excluded'] = 1
                    p.Economically_Excluded = 1
                else:
                    p.participant.vars['Economically_Excluded'] = 0
                    p.Economically_Excluded = 0
        for p in self.get_players():  # Exclusion of player 1 in deliberation:
            if "Politically_Excluded" in p.participant.vars:
                p.Politically_Excluded = p.participant.vars['Politically_Excluded']
            else:
                if p.group.treatment_pol == 0 and p.id_in_group == 1:
                    p.participant.vars['Politically_Excluded'] = 1
                    p.Politically_Excluded = 1
                else:
                    p.participant.vars['Politically_Excluded'] = 0
                    p.Politically_Excluded = 0
        # </editor-fold>
        # Assign number of eligible voters:
        for g in self.get_groups():
            g.defineEligibleVoters()

    # <editor-fold desc="Variablen für Admin Report">
    def vars_for_admin_report(self):
        #Background-Info to Participants
        list_of_groupmembership = []
        for p in self.get_players():
            group = "Group: " + str(p.group.id_in_subsession)
            name = " - Participant: "+str(p.participant.label)
            effpoints = " - Effort Points: "+str(p.Effort_points)
            rank = " - Rank:"+ str(p.GroupRank)
            list_of_groupmembership.append(group+name+rank+effpoints)
        list_of_groupmembership = sorted(list_of_groupmembership)
        #Decisions and Treatments in Game
        listofgroupack = []
        polpart_treatmentlist = []
        ecpart_treatmentlist = []
        listP1_NeueVerteilungErreicht = []
        groupnumberlist = []
        for g in self.get_groups():
            groupnumberlist.append(g)
            polpart_treatmentlist.append(g.treatment_pol)
            ecpart_treatmentlist.append(g.treatment_ec)
            listofgroupack.append(g.P1_GroupAcknowledgement)
            listP1_NeueVerteilungErreicht.append(g.P1_NeueVerteilungErreicht)
        return {
            "participantsinlist": list_of_groupmembership,
            "Political_Participation_Treatment": polpart_treatmentlist,
            "Economic_Participation_Treatment": ecpart_treatmentlist,
            "groupnumberlist": groupnumberlist,
            "groupAcknowledgement": listofgroupack,
            "P1_NeueVerteilungErreicht": listP1_NeueVerteilungErreicht
        }

    # </editor-fold>


class Group(BaseGroup):
    # <editor-fold desc="Treatments">
    REDIS = models.IntegerField(initial=1)  # 0 = DIS, 1 = REDIS ---> From now on always redis
    Effort = models.IntegerField(
        initial=1)  # 0 = Lottery, 1 = Effort ---> From now on always effort for core group members

    # </editor-fold>

class Player(BasePlayer):
    P1_ConsumptionFinal = models.FloatField(initial=-99,
                                            doc="Final Consumption \=  with TokenAdd as voted for by group")
    # Final Payout
    ProfitEuroUnround = models.FloatField(initial=-99)
    # Suggested Distribution
    # <editor-fold desc="Wahl für Anerkennung der Verfahren">
    P1_AckVote = models.IntegerField(
        choices=[
            [1, 'Das Verfahren soll anerkannt werden'],
            [0, 'Das Verfahren soll nicht anerkannt werden'], ],
        doc="Former variable name: iAnerkennungVerfahren",
        widget=widgets.RadioSelect,
    )
    # </editor-fold>
    # <editor-fold desc="Token and wealth in suggested distributions">


    P1_ConsumptionNoDis = models.FloatField(
        initial=-99,
        doc="Initial Consumption \=  no Redistribution")
    P1_ConsumptionEqual = models.FloatField(
        initial=-99,
        doc="Initial Consumption \=  with TokenAdd as suggested by Experimenter")
    P1_ConsumptionWeakerFirst = models.FloatField(
        initial=-99,
        doc="Initial Consumption \=  with TokenAdd as suggested by Weaker First")
    P1_ConsumptionMixed = models.FloatField(
        initial=-99,
        doc="Initial Consumption \=  with TokenAdd as suggested by Mixed")
    P1_SumRoundIncomeEqual = models.FloatField(
        initial=-99,
        doc="Total Income Token minus Taxes but with TokenAdd as suggested by Experimenter")
    P1_SumRoundIncomeWeakerFirst = models.FloatField(
        initial=-99,
        doc="Total Income Token minus Taxes but with TokenAdd as suggested by Weaker First")
    P1_SumRoundIncomeMixed = models.FloatField(
        initial=-99,
        doc="Total Income Token minus Taxes but with TokenAdd as suggested by Mixed")

    P1_WealthNoDis_1R = models.FloatField(
        initial=-99,
        doc="Wealth for 1 Round - No Redistribution")
    P1_WealthNoDis_5R = models.FloatField(
        initial=-99,
        doc="Wealth for 5 Rounds -  No Redistribution")


    P1_WealthWeakerFirst_1R = models.FloatField(initial=-99, doc="Wealth for 1 Round - Suggested Distribution")
    P1_WealthWeakerFirst_5R = models.FloatField(initial=-99, doc="Wealth for 5 Rounds -  Suggested Distribution")

    P1_WealthMixed_1R = models.FloatField(initial=-99, doc="Wealth for 1 Round - Suggested Distribution")
    P1_WealthMixed_5R = models.FloatField(initial=-99, doc="Wealth for 5 Rounds -  Suggested Distribution")
    # </editor-fold>

    # New Distribution
    P1_TokenAdd = models.FloatField(initial=-99, doc="Chosen new Distribution - If suggested Distribution was rejected")
    # Wahl für neue Verteilungen bei Rejection
    # <editor-fold desc="P1_Wahl für Verteilung - Eingaben erster Versuch -P1_iTokenAddVoteFor_1">
    P1_iTokenAddVoteFor_1 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_2 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_3 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_4 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_5 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_6 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)

    # </editor-fold>
    # <editor-fold desc="P1_Wahl für Verteilung - Eingaben zweiter Versuch - P1_iTokenAddVoteFor_Corr_1">
    P1_iTokenAddVoteFor_Corr_1 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_Corr_2 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_Corr_3 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_Corr_4 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    P1_iTokenAddVoteFor_Corr_5 = models.FloatField(
        # initial=0,
        min=0,
        max=Constants.P1_GesamtZusatzToken)
    # </editor-fold>
    # <editor-fold desc="P1_Wahl für Verteilung - P1_CountSame und so">
    P1_CountSame = models.IntegerField(initial=-99)
    P1_CountSame_Corr = models.IntegerField(
        doc="CountSame2 in zTree",
        initial=-99)
    P1_CountGroup = models.IntegerField(initial=-99)

    # </editor-fold>
    # <editor-fold desc="P1_Wealth für Final Distribution as Voted">
    P1_WealthNewDis_1R = models.FloatField(initial=-99)
    P1_WealthNewDis_5R = models.FloatField(initial=-99)
    # </editor-fold> 1 R
    # <editor-fold desc="Argumente und Wahl des besten Arguments">
    # Before Voting
