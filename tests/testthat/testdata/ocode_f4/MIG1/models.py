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
    # <editor-fold desc="Treatments">
    REDIS = models.IntegerField(initial=1)  # 0 = DIS, 1 = REDIS ---> From now on always redis
    Effort = models.IntegerField(
        initial=1)  # 0 = Lottery, 1 = Effort ---> From now on always effort for core group members

    treatment_ec = models.IntegerField(
        doc="""Economic Participation Treatment""",
        choices=[
            [0, "0 \= tolle Anfangssausstattung"],  # Person not allowed to participate in effort task
            [1, "9 Anfangsausstattung"]  # Person allowed to participate in effort task
        ])
    treatment_pol = models.IntegerField(
        doc="""Political Participation Treatment""",
        choices=[
            [0, "No participation in the decision"],
            [1, "Participation in the decision"]
        ])

    # </editor-fold>
    # <editor-fold desc="GroupRank und Start Token und Channels zuweisen">
    def randomGroupRank(self):
        """
        If Effort = 0, the individuals in the group are assigned random IDs.
        This method is called by the Intro-WaitPage.
        I won't need this for the experiment, but it's good for pretesting.
        """
        randomnumber = random.sample(list(range(1, 6)), 5)
        for p in self.get_players():
            p.GroupRank = randomnumber[p.id_in_group - 1]

    def effortGroupRank(self):
        """ 
        If Effort = 1, the individuals in the group are assigned IDs according to their effort.
        This method is called by the intro-WaitPage.
        """
        if self.treatment_ec == 0:
            all_group_effort_new = []
            for p in self.get_players():
                all_group_effort_new.append(p.Effort_points - (p.Minus_points * 0.0001) - (p.id_in_group * 0.000001))
            rankgroupeffort = rankdata(all_group_effort_new)
        else:
            all_group_effort_new = []
            for p in self.get_players():
                if p.id_in_group != 1:
                    all_group_effort_new.append(
                        p.Effort_points - (p.Minus_points * 0.0001) - (p.id_in_group * 0.000001))
                if p.id_in_group == 1:
                    all_group_effort_new.append(0)
            rankgroupeffort = rankdata(all_group_effort_new)

        for p in self.get_players():
            p.GroupRank = int(rankgroupeffort[p.id_in_group - 1])

    def P1_Define_StartToken(self):
        if self.treatment_ec == 0:
            for p in self.get_players():
                # For specific positions
                p.P1_StartToken = Constants.P1_StartToken_0[p.GroupRank - 1]
                p.P1_StartTokenPT = Constants.P1_StartTokenPT_0[p.GroupRank - 1]
                p.P1_Taxes = Constants.P1_Taxes_0[p.GroupRank - 1]
                p.P1_TokenAddEqual = Constants.P1_TokenAdd_Equal[p.GroupRank - 1]
                p.P1_TokenAddWeakerFirst = Constants.P1_TokenAddWeakerFirst_0[p.GroupRank - 1]
                p.P1_TokenAddMixed = Constants.P1_TokenAddMixed_0[p.GroupRank - 1]
                # For all
                p.P1_SumRoundIncomeEqual = p.P1_StartToken + p.P1_TokenAddEqual
                p.P1_SumRoundIncomeWeakerFirst = p.P1_StartToken + p.P1_TokenAddWeakerFirst
                p.P1_SumRoundIncomeMixed = p.P1_StartToken + p.P1_TokenAddMixed
        if self.treatment_ec == 1:
            for p in self.get_players():
                # For specific positions
                p.P1_StartToken = Constants.P1_StartToken_9[p.GroupRank - 1]
                p.P1_StartTokenPT = Constants.P1_StartTokenPT_9[p.GroupRank - 1]
                p.P1_Taxes = Constants.P1_Taxes_9[p.GroupRank - 1]
                p.P1_TokenAddEqual = Constants.P1_TokenAdd_Equal[p.GroupRank - 1]
                p.P1_TokenAddWeakerFirst = Constants.P1_TokenAddWeakerFirst_9[p.GroupRank - 1]
                p.P1_TokenAddMixed = Constants.P1_TokenAddMixed_9[p.GroupRank - 1]
                # For all
                p.P1_SumRoundIncomeEqual = p.P1_StartToken + p.P1_TokenAddEqual
                p.P1_SumRoundIncomeWeakerFirst = p.P1_StartToken + p.P1_TokenAddWeakerFirst
                p.P1_SumRoundIncomeMixed = p.P1_StartToken + p.P1_TokenAddMixed



    def Nickname(self):
        self.Channel_Ack = str(self.id_in_subsession) + "_Ack"
        self.Channel_DisProcedure = str(self.id_in_subsession) + "_DisProc"
        self.Channel_DisProcedureKorr = str(self.id_in_subsession) + "_DisProcKorr"
        for p in self.get_players():
            p.Nickname = "Player " + str(p.GroupRank)

    # </editor-fold>
    # <editor-fold desc="Initial/Suggested Redistribution: (graphs und wealth)">
    def P1_calc_wealthD0(self):  # Keine Grafiken, nur Zahlen
        for p in self.get_players():
            # <editor-fold desc="For Start - No Redistribution">
            if (p.P1_StartTokenPT * 0.9) <= Constants.P1_Needs:
                p.P1_ConsumptionNoDis = Constants.P1_Needs
                p.P1_WealthNoDis_1R = p.P1_StartTokenPT - Constants.P1_Needs
            else:
                p.P1_ConsumptionNoDis = p.P1_StartTokenPT * 0.9
                p.P1_WealthNoDis_1R = p.P1_StartTokenPT * 0.1
            p.P1_WealthNoDis_5R = p.P1_WealthNoDis_1R * 5
            # </editor-fold>
            # <editor-fold desc="For Equality">
            if ((p.P1_StartToken + p.P1_TokenAddEqual) * 0.9) <= Constants.P1_Needs:
                p.P1_ConsumptionEqual = Constants.P1_Needs
                p.P1_WealthEqual_1R = p.P1_StartToken - Constants.P1_Needs + p.P1_TokenAddEqual
            else:
                p.P1_ConsumptionEqual = (p.P1_StartToken + p.P1_TokenAddEqual) * 0.9
                p.P1_WealthEqual_1R = (p.P1_StartToken + p.P1_TokenAddEqual) * 0.1
            p.P1_WealthEqual_5R = p.P1_WealthEqual_1R * 5
            # </editor-fold>
            # <editor-fold desc="For Weaker First">
            if ((p.P1_StartToken + p.P1_TokenAddWeakerFirst) * 0.9) <= Constants.P1_Needs:
                p.P1_ConsumptionWeakerFirst = Constants.P1_Needs
                p.P1_WealthWeakerFirst_1R = p.P1_StartToken - Constants.P1_Needs + p.P1_TokenAddWeakerFirst
            else:
                p.P1_ConsumptionWeakerFirst = (p.P1_StartToken + p.P1_TokenAddWeakerFirst) * 0.9
                p.P1_WealthWeakerFirst_1R = (p.P1_StartToken + p.P1_TokenAddWeakerFirst) * 0.1
            p.P1_WealthWeakerFirst_5R = p.P1_WealthWeakerFirst_1R * 5
            # </editor-fold>
            # <editor-fold desc="For Mixed">
            if ((p.P1_StartToken + p.P1_TokenAddMixed) * 0.9) <= Constants.P1_Needs:
                p.P1_ConsumptionMixed = Constants.P1_Needs
                p.P1_WealthMixed_1R = p.P1_StartToken - Constants.P1_Needs + p.P1_TokenAddMixed
            else:
                p.P1_ConsumptionMixed = (p.P1_StartToken + p.P1_TokenAddMixed) * 0.9
                p.P1_WealthMixed_1R = (p.P1_StartToken + p.P1_TokenAddMixed) * 0.1
            p.P1_WealthMixed_5R = p.P1_WealthMixed_1R * 5
            # </editor-fold>


    def P1_ResultsGraphEqual(self):
        """ Wird in Pages - class P1_Rej_9_Rounds(Page) aufgerufen"""
        series_Start_PT = []
        series_Start = []
        series_Taxes = []
        series_TokenAdd = []
        series_Income = []
        series_Cumulative = []
        series_highest_lowest = []
        for p in self.get_players():
            series_Start_PT.append(p.P1_StartTokenPT)
            series_Start.append(p.P1_StartToken)
            series_Taxes.append(p.P1_Taxes)
            series_TokenAdd.append(p.P1_TokenAddEqual)
            cumulativedata = [0,
                              p.P1_WealthEqual_1R * 1,
                              p.P1_WealthEqual_1R * 2,
                              p.P1_WealthEqual_1R * 3,
                              p.P1_WealthEqual_1R * 4,
                              p.P1_WealthEqual_1R * 5
                              ]
            series_Cumulative.append({
                "name": p.GroupRank,
                "data": cumulativedata})
            series_highest_lowest.append(p.P1_WealthEqual_1R * 5)
            series_Income.append(p.P1_StartToken + p.P1_TokenAddEqual)
        return {
            "series_StartToken_PT": series_Start_PT,
            "StartToken": series_Start,
            "Taxes": series_Taxes,
            "TokenAdd": series_TokenAdd,
            "Cumulative": series_Cumulative,
            "MinValue_B": self.MinMaxGraph(series_highest_lowest)[0],
            "MaxValue_B": self.MinMaxGraph(series_highest_lowest)[1],
            "MinValue_A": 0,
            "MaxValue_A": self.MinMaxGraph(series_Income)[1],
        }

    def P1_info3graph(self):
        series = []
        series_highest_lowest = []
        for p in self.get_players():
            playerdata = [0,
                          p.P1_WealthNoDis_1R * 1,
                          p.P1_WealthNoDis_1R * 2,
                          p.P1_WealthNoDis_1R * 3,
                          p.P1_WealthNoDis_1R * 4,
                          p.P1_WealthNoDis_1R * 5]
            series.append({
                "name": p.GroupRank,
                "data": playerdata})
            series_highest_lowest.append(p.P1_WealthNoDis_5R)
        return {
            'highcharts_series': series,
            "MinValue": self.MinMaxGraph(series_highest_lowest)[0],
            "MaxValue": self.MinMaxGraph(series_highest_lowest)[1]
        }

    def P1_ResultsGraphWeakerFirst(self):
        """ Wird aufgerufen bei Acceptance - Weaker First"""
        series_Start = []
        series_TokenAdd = []
        series_Income = []
        series_Cumulative = []
        series_highest_lowest = []
        for p in self.get_players():
            series_Start.append(p.P1_StartToken)
            series_TokenAdd.append(p.P1_TokenAddWeakerFirst)
            cumulativedata = [0,
                              p.P1_WealthWeakerFirst_1R * 1,
                              p.P1_WealthWeakerFirst_1R * 2,
                              p.P1_WealthWeakerFirst_1R * 3,
                              p.P1_WealthWeakerFirst_1R * 4,
                              p.P1_WealthWeakerFirst_1R * 5
                              ]
            series_Cumulative.append({
                "name": p.GroupRank,
                "data": cumulativedata})
            series_highest_lowest.append(p.P1_WealthWeakerFirst_1R * 5)
            series_Income.append(p.P1_StartToken + p.P1_TokenAddWeakerFirst)
        return {
            "StartToken": series_Start,
            "TokenAdd": series_TokenAdd,
            "Cumulative": series_Cumulative,
            "MinValue_B": self.MinMaxGraph(series_highest_lowest)[0],
            "MaxValue_B": self.MinMaxGraph(series_highest_lowest)[1],
            "MinValue_A": 0,
            "MaxValue_A": self.MinMaxGraph(series_Income)[1],
        }

    def P1_ResultsGraphMixed(self):
        """ Wird aufgerufen bei Acceptance - Weaker First"""
        series_Start = []
        series_TokenAdd = []
        series_Income = []
        series_Cumulative = []
        series_highest_lowest = []
        for p in self.get_players():
            series_Start.append(p.P1_StartToken)
            series_TokenAdd.append(p.P1_TokenAddMixed)
            cumulativedata = [0,
                              p.P1_WealthMixed_1R * 1,
                              p.P1_WealthMixed_1R * 2,
                              p.P1_WealthMixed_1R * 3,
                              p.P1_WealthMixed_1R * 4,
                              p.P1_WealthMixed_1R * 5
                              ]
            series_Cumulative.append({
                "name": p.GroupRank,
                "data": cumulativedata})
            series_highest_lowest.append(p.P1_WealthMixed_1R * 5)
            series_Income.append(p.P1_StartToken + p.P1_TokenAddMixed)
        return {
            "StartToken": series_Start,
            "TokenAdd": series_TokenAdd,
            "Cumulative": series_Cumulative,
            "MinValue_B": self.MinMaxGraph(series_highest_lowest)[0],
            "MaxValue_B": self.MinMaxGraph(series_highest_lowest)[1],
            "MinValue_A": 0,
            "MaxValue_A": self.MinMaxGraph(series_Income)[1],
        }

    # </editor-fold>

    # Wahlen
    P1_NumElegibleToVoteForAck = models.IntegerField(initial=-99)
    P1_NumElegibleToVoteNewDis = models.IntegerField(initial=-99)

    def defineEligibleVoters(self):
        if self.treatment_pol == 1:
            self.P1_NumElegibleToVoteForAck = Constants.players_per_group
            self.P1_NumElegibleToVoteNewDis = Constants.players_per_group
        elif self.treatment_pol == 0:
            self.P1_NumElegibleToVoteForAck = Constants.players_per_group - 1
            self.P1_NumElegibleToVoteNewDis = Constants.players_per_group - 1
        else:
            print("Problem. Check defineEligibleVoters()")

    P1_nGroupAnerkennung = models.IntegerField(initial=0)
    P1_GroupAcknowledgement = models.IntegerField(initial=0)

    def P1_AckResult(self):
        self.P1_nGroupAnerkennung = 0
        for p in self.get_players():
            if p.P1_AckVote == 1:
                self.P1_nGroupAnerkennung += 1
        if self.P1_nGroupAnerkennung == self.P1_NumElegibleToVoteForAck:
            self.P1_GroupAcknowledgement = 1
        else:
            self.P1_GroupAcknowledgement = 0

    # <editor-fold desc="P1_Wahl neue Verteilung - CountSame/CountGroup/Erreicht/TokenAdd_New/P1_SumRoundIncome/P1_WealthNewDis/P1_WealthNewDis_5R">
    P1_CountGroup = models.IntegerField(
        initial=0)
    P1_CountGroup_Corr = models.IntegerField(
        doc="in zTree: CountGroup2",
        initial=0)
    P1_NeueVerteilungErreicht = models.IntegerField(initial=0)
    P1_NeueVerteilungErreicht_Corr = models.IntegerField(initial=0)

    # <editor-fold desc="P1_Wahl für Verteilung WAHLERGEBNIS- TokenAdd_New_1/5">
    P1_TokenAdd_New_1 = models.FloatField(initial=-99)
    P1_TokenAdd_New_2 = models.FloatField(initial=-99)
    P1_TokenAdd_New_3 = models.FloatField(initial=-99)
    P1_TokenAdd_New_4 = models.FloatField(initial=-99)
    P1_TokenAdd_New_5 = models.FloatField(initial=-99)
    P1_TokenAdd_New_Corr_1 = models.FloatField(initial=-99)
    P1_TokenAdd_New_Corr_2 = models.FloatField(initial=-99)
    P1_TokenAdd_New_Corr_3 = models.FloatField(initial=-99)
    P1_TokenAdd_New_Corr_4 = models.FloatField(initial=-99)
    P1_TokenAdd_New_Corr_5 = models.FloatField(initial=-99)

    # </editor-fold>

    def abzahlen(self, Correction):
        """
        How many group members entered the same numbers?
        Old function name in ztree: Countsame.
        If this function is called after the Correction chat, enter "Corr" when calling the function.
        """
        corr = ""
        if Correction == "Corr":
            corr = "_" + "Corr"

        for p in self.get_players():
            setattr(p, "P1_CountSame" + corr, 0)
            for q in self.get_players():
                if (vars(p)["P1_iTokenAddVoteFor" + corr + "_" + str(1)] == vars(q)[
                    "P1_iTokenAddVoteFor" + corr + "_" + str(1)] and
                        vars(p)["P1_iTokenAddVoteFor" + corr + "_" + str(2)] == vars(q)[
                            "P1_iTokenAddVoteFor" + corr + "_" + str(2)] and
                        vars(p)["P1_iTokenAddVoteFor" + corr + "_" + str(3)] == vars(q)[
                            "P1_iTokenAddVoteFor" + corr + "_" + str(3)] and
                        vars(p)["P1_iTokenAddVoteFor" + corr + "_" + str(4)] == vars(q)[
                            "P1_iTokenAddVoteFor" + corr + "_" + str(4)] and
                        vars(p)["P1_iTokenAddVoteFor" + corr + "_" + str(5)] == vars(q)[
                            "P1_iTokenAddVoteFor" + corr + "_" + str(5)]): \
                        setattr(p, "P1_CountSame" + corr, getattr(p, "P1_CountSame" + corr) + 1)

    # </editor-fold>
    # <editor-fold desc="P1_Count Group ">
    def abzahlengruppe(self, Correction):
        """"Wie viele gleiche Zahleneingaben gibt es höchstens pro Gruppe? """
        corr = ""
        if Correction == "Corr":
            corr = "_" + "Corr"

        setattr(self, "P1_CountGroup" + corr, 0)
        for p in self.get_players():
            if (getattr(p, "P1_CountSame" + corr) > (p.group.P1_NumElegibleToVoteNewDis / 2)):
                setattr(self, "P1_CountGroup" + corr, getattr(self, "P1_CountGroup" + corr) + 1)

    # </editor-fold>
    # <editor-fold desc="P1_Wahlergebnis: P1_SumRoundIncome,P1_WealthNewDis_1R, P1_WealthNewDis_5R ">

    def NewDisVoteRes(self, Correction):
        """
        Prior name: P1_Wahlergebnis1 
        Wird in Pages --> P1_Rej_5_Chat_DistributionProcedure_2WP_After aufgerufen
        """
        corr = ""
        if Correction == "Corr":
            corr = "_" + "Corr"

        for p in self.get_players():
            if getattr(p, "P1_CountSame" + corr) > p.group.P1_NumElegibleToVoteNewDis / 2:
                for i in range(1, 6):
                    setattr(self, "P1_TokenAdd_New" + corr + "_" + str(i),
                            getattr(p, "P1_iTokenAddVoteFor" + corr + "_" + str(i)))
                break

        if getattr(self, "P1_CountGroup" + corr) > self.P1_NumElegibleToVoteNewDis / 2:
            setattr(self, "P1_NeueVerteilungErreicht" + corr, 1)
            # Calculate Token Add
            for p in self.get_players():
                for i in range(1, 6):
                    if p.GroupRank == i:
                        p.P1_TokenAdd = getattr(self, "P1_TokenAdd_New" + corr + "_" + str(i))

            # Calculate Roundincome after Tokenadd and Needs-Satisfaction
            for p in self.get_players():
                p.P1_SumRoundIncome = p.P1_StartToken + p.P1_TokenAdd
                if p.P1_SumRoundIncome < Constants.P1_Needs:
                    p.P1_ConsumptionFinal = Constants.P1_Needs
                    p.P1_WealthNewDis_1R = p.P1_SumRoundIncome - Constants.P1_Needs
                else:
                    p.P1_ConsumptionFinal = p.P1_SumRoundIncome * 0.9
                    p.P1_WealthNewDis_1R = p.P1_SumRoundIncome * 0.1
                p.P1_WealthNewDis_5R = p.P1_WealthNewDis_1R * 5
        else:
            setattr(self, "P1_NeueVerteilungErreicht" + corr, 0)

    # </editor-fold>
    # </editor-fold>

    # <editor-fold desc="Arguments">
    # <editor-fold desc="Arguments - Initialisation">
    # <editor-fold desc="P1 Priv --> Private Argumente am Start - Anzahl Akzeptanz von Argumenten und bestes Argument">

    P1_BVote_nArgAkz_1 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkz_2 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkz_3 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkz_4 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkz_5 = models.IntegerField(initial=-99)

    P1_BVote_nArgAkzNo_1 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkzNo_2 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkzNo_3 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkzNo_4 = models.IntegerField(initial=-99)
    P1_BVote_nArgAkzNo_5 = models.IntegerField(initial=-99)

    P1_BVote_nArgBest_1 = models.IntegerField(initial=-99)
    P1_BVote_nArgBest_2 = models.IntegerField(initial=-99)
    P1_BVote_nArgBest_3 = models.IntegerField(initial=-99)
    P1_BVote_nArgBest_4 = models.IntegerField(initial=-99)
    P1_BVote_nArgBest_5 = models.IntegerField(initial=-99)

    # </editor-fold>
    # <editor-fold desc="P1 Rej --> Anzahl Akzeptanz von Argumenten und bestes Argument">

    P1_Rej_nArgAkz_1 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkz_2 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkz_3 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkz_4 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkz_5 = models.IntegerField(initial=-99)

    P1_Rej_nArgAkzNo_1 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkzNo_2 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkzNo_3 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkzNo_4 = models.IntegerField(initial=-99)
    P1_Rej_nArgAkzNo_5 = models.IntegerField(initial=-99)

    P1_Rej_nArgBest_1 = models.IntegerField(initial=-99)
    P1_Rej_nArgBest_2 = models.IntegerField(initial=-99)
    P1_Rej_nArgBest_3 = models.IntegerField(initial=-99)
    P1_Rej_nArgBest_4 = models.IntegerField(initial=-99)
    P1_Rej_nArgBest_5 = models.IntegerField(initial=-99)

    # </editor-fold>
    # <editor-fold desc="P1_ AVote --> Akzeptanz der Argumente nach Chat - Anzahl Akzeptanz von Argumenten und bestes Argument">

    P1_AVote_nArgAkz_1 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkz_2 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkz_3 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkz_4 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkz_5 = models.IntegerField(initial=-99)

    P1_AVote_nArgAkzNo_1 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkzNo_2 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkzNo_3 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkzNo_4 = models.IntegerField(initial=-99)
    P1_AVote_nArgAkzNo_5 = models.IntegerField(initial=-99)

    P1_AVote_nArgBest_1 = models.IntegerField(initial=-99)
    P1_AVote_nArgBest_2 = models.IntegerField(initial=-99)
    P1_AVote_nArgBest_3 = models.IntegerField(initial=-99)
    P1_AVote_nArgBest_4 = models.IntegerField(initial=-99)
    P1_AVote_nArgBest_5 = models.IntegerField(initial=-99)

    # </editor-fold>
    # <editor-fold desc="P1_ Acc --> Akzeptanz der Argumente nach Chat - Anzahl Akzeptanz von Argumenten und bestes Argument">

    P1_Acc_nArgAkz_1 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkz_2 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkz_3 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkz_4 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkz_5 = models.IntegerField(initial=-99)

    P1_Acc_nArgAkzNo_1 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkzNo_2 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkzNo_3 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkzNo_4 = models.IntegerField(initial=-99)
    P1_Acc_nArgAkzNo_5 = models.IntegerField(initial=-99)

    P1_Acc_nArgBest_1 = models.IntegerField(initial=-99)
    P1_Acc_nArgBest_2 = models.IntegerField(initial=-99)
    P1_Acc_nArgBest_3 = models.IntegerField(initial=-99)
    P1_Acc_nArgBest_4 = models.IntegerField(initial=-99)
    P1_Acc_nArgBest_5 = models.IntegerField(initial=-99)

    # </editor-fold>
    # </editor-fold>
    def calcbestarg(self, argpart):
        # argpart are either BVote,

        # <editor-fold desc="Count acceptance and best argument">
        group_acc_base = "P1_" + argpart + "_nArgAkz_"
        group_accNo_base = "P1_" + argpart + "_nArgAkzNo_"
        group_best_base = "P1_" + argpart + "_nArgBest_"

        ind_acc_base = "P1_" + argpart + "_ArgAkz_"
        ind_accNo_base = "P1_" + argpart + "_ArgAkzNo_"
        ind_best_base = "P1_" + argpart + "_ArgBest_"
        for i in range(1, 6):
            group_acc_mod = group_acc_base + str(i)
            group_accNo_mod = group_accNo_base + str(i)
            group_best_mod = group_best_base + str(i)
            ind_acc_mod = ind_acc_base + str(i)
            ind_accNo_mod = ind_accNo_base + str(i)
            ind_best_mod = ind_best_base + str(i)

            # Calculations
            setattr(self, group_acc_mod,
                    sum((vars(p)[ind_acc_mod]) for p in self.get_players() if vars(p)[ind_acc_mod] >= 0))
            setattr(self, group_accNo_mod, 5 - vars(self)[group_acc_mod])
            setattr(self, group_best_mod, sum((vars(p)[ind_best_mod]) for p in self.get_players()))

        # </editor-fold>
        # <editor-fold desc="Assign count of acceptance and best argument to subjects.">
        listofbest = []  # Für übersicht
        for p in self.get_players():
            setattr(p, "P1_" + argpart + "_nArgBest", vars(self)[group_best_base + str(p.GroupRank)])
            listofbest.append(vars(p)["P1_" + argpart + "_nArgBest"])
        # Winners
        P1_Winners = [i for i, j in enumerate(listofbest) if j == max(listofbest)]
        # </editor-fold>
        # Falls Gewinner/in
        for p in self.get_players():
            if (p.GroupRank - 1) in P1_Winners and len(P1_Winners) <= 2:  # There are max 2 winners.
                setattr(p, "P1_" + argpart + "_ArgBest", 1)
                setattr(p, "P1_" + argpart + "_ArgToken", Constants.P1_BVote_ArgBestToken)
                if len(P1_Winners) == 1:
                    setattr(p, "P1_" + argpart + "_ArgBestSingle", 1)
                else:
                    setattr(p, "P1_" + argpart + "_ArgBestSingle", 2)
            # Falls Nicht-Gewinner/in:
            else:
                setattr(p, "P1_" + argpart + "_ArgBest", 0)
                setattr(p, "P1_" + argpart + "_ArgToken", 0)
                setattr(p, "P1_" + argpart + "_ArgBestSingle", 0)

        # </editor-fold>

    # </editor-fold>

    def P1_ResultsGraph(self):
        """ Wird in Pages - class P1_Rej_9_Rounds(Page) aufgerufen"""
        series_Start = []
        series_TokenAdd = []
        series_Income = []
        series_Cumulative = []
        series_highest_lowest = []
        for p in self.get_players():
            series_Start.append(p.P1_StartToken)
            series_TokenAdd.append(p.P1_TokenAdd)
            cumulativedata = [0,
                              p.P1_WealthNewDis_1R * 1,
                              p.P1_WealthNewDis_1R * 2,
                              p.P1_WealthNewDis_1R * 3,
                              p.P1_WealthNewDis_1R * 4,
                              p.P1_WealthNewDis_1R * 5
                              ]
            series_Cumulative.append({
                "name": p.GroupRank,
                "data": cumulativedata})
            series_highest_lowest.append(p.P1_WealthNewDis_1R * 5)
            series_Income.append(p.P1_StartToken + p.P1_TokenAdd)

        return {
            "StartToken": series_Start,
            "TokenAdd": series_TokenAdd,
            "Cumulative": series_Cumulative,
            "MinValue_B": self.MinMaxGraph(series_highest_lowest)[0],
            "MaxValue_B": self.MinMaxGraph(series_highest_lowest)[1],
            "MinValue_A": 0,
            "MaxValue_A": self.MinMaxGraph(series_Income)[1],
        }

    def MinMaxGraph(self, list=[]):
        if min(list) >= 0:
            minlist = -5
            maxlist = max(list) + 3
        else:
            minlist = min(list) - 5
            maxlist = max(list) + 3
        return minlist, maxlist


class Player(BasePlayer):
    Politically_Excluded = models.IntegerField(initial=-99, doc="Excluded from deliberation")
    Economically_Excluded = models.IntegerField(initial=-99, doc="Excluded from effort tak")
    GroupRank = models.IntegerField(initial=-99, doc="Rank in group")

    # <editor-fold desc="P1 BVote --> Player -->Bestes Argument für neue Verteilung berechnen- Gewinner und Token">
    P1_BVote_nArgBest = models.IntegerField(initial=-99, doc="Number of votes for best vote")
    P1_BVote_ArgBest = models.IntegerField(initial=-99, doc="Whether participant is one of the best-vote winners.")
    P1_BVote_ArgToken = models.IntegerField(initial=0, doc="Token für the argument.")
    P1_BVote_ArgBestSingle = models.IntegerField(initial=-99)
    # </editor-fold>

    Effort_points = models.FloatField(initial=-99)
    Minus_points = models.FloatField(initial=-99)

    # Start Token
    P1_StartToken = models.FloatField(initial=-99, doc="StartToken after deduction of Taxes")
    P1_StartTokenPT = models.FloatField(initial=-99, doc="StartToken pre Taxes; formally coded as StartTokenAlt")
    P1_Taxes = models.FloatField(initial=-99, doc="Taxes to be deducted from StartTokenPT")
    P1_ConsumptionFinal = models.FloatField(initial=-99,
                                            doc="Final Consumption \= with TokenAdd as voted for by group")
    P1_ConsumptionFinal2 = models.FloatField(initial=-99,
                                            doc="Final Consumption  with TokenAdd as voted for by group")
    # Final Payout
    ProfitEuroUnround = models.FloatField(initial=-99)
    ProfitEuroRound = models.FloatField(initial=-99)
    ProfitToken = models.FloatField(initial=-99)
    P1_FinalWealth_5R = models.FloatField(initial=-99)

    # Suggested Distribution
    # <editor-fold desc="Wahl für Anerkennung der Verfahren">
    P1_AckVote = models.IntegerField(
        choices=[
            [1, 'Das Verfahren x \= 1 soll anerkannt werden'],
            [0, 'Das Verfahren y \= 2 soll nicht anerkannt werden'], ],
        doc="Former variable name: iAnerkennungVerfahren",
        widget=widgets.RadioSelect,
    )
    # </editor-fold>
    # <editor-fold desc="Token and wealth in suggested distributions">

    P1_TokenAddEqual = models.FloatField(
        initial=-99,
        doc="TokenAdd as suggested by Experimenter")
    P1_TokenAddWeakerFirst = models.FloatField(
        initial=-99,
        doc="TokenAdd as suggested by WeakerFirst")
    P1_TokenAddMixed = models.FloatField(
        initial=-99,
        doc="TokenAdd as suggested by Mixed")
    P1_ConsumptionNoDis = models.FloatField(
        initial=-99,
        doc="Initial Consumption no Redistribution")
    P1_ConsumptionEqual = models.FloatField(
        initial=-99,
        doc="Initial Consumption  with TokenAdd as suggested by Experimenter")
    P1_ConsumptionWeakerFirst = models.FloatField(
        initial=-99,
        doc="Initial Consumption   with TokenAdd as suggested by Weaker First")
    P1_ConsumptionMixed = models.FloatField(
        initial=-99,
        doc="Initial Consumption with TokenAdd as suggested by Mixed")
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

    P1_WealthEqual_1R = models.FloatField(
        initial=-99,
        doc="Wealth for 1 Round - Suggested Distribution")
    P1_WealthEqual_5R = models.FloatField(
        initial=-99,
        doc="Wealth for 5 Rounds -  Suggested Distribution")

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
    # <editor-fold desc="Argumente Individual am Anfang">
    P1_BVote_YesNo = models.IntegerField(
        choices=[
            [1, 'Ich erkenne das Verteilungsverfahren der zusätzlichen Token an'],
            [0, 'Ich erkenne das Verteilungsverfahren der zusätzlichen Token nicht an'], ],
        doc="Former variable name: P1_AnerkennungPrivat",
        initial=-99,
        widget=widgets.RadioSelect
    )
    P1_BVote_YesNoString = models.TextField()

    P1_YesNoArg_BVote = models.TextField(
        label="",
        max_length=250,
        doc="Argument for Acceptance/Rejection of the suggested distribution. Former variable name: Argument"
        # blank=True  #das wird dann später im javascript aufgehoben
    )
    # </editor-fold>
    # <editor-fold desc="P1 BVote --> Argumente akzeptieren -Private Argumente am Anfang">
    P1_BVote_ArgAkz_1 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_BVote_ArgAkz_2 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_BVote_ArgAkz_3 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_BVote_ArgAkz_4 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_BVote_ArgAkz_5 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    # </editor-fold>
    # <editor-fold desc="P1 BVote --> Bestes Argument wählen  --> Private Argumente am Anfang">
    P1_BVote_ArgBest_1 = models.BooleanField(blank=True, initial=False)
    P1_BVote_ArgBest_2 = models.BooleanField(blank=True, initial=False)
    P1_BVote_ArgBest_3 = models.BooleanField(blank=True, initial=False)
    P1_BVote_ArgBest_4 = models.BooleanField(blank=True, initial=False)
    P1_BVote_ArgBest_5 = models.BooleanField(blank=True, initial=False)
    # </editor-fold>

    # After Voting
    P1_AVote_AckArg = models.TextField(
        label="",
        max_length=250,
    )
    # <editor-fold desc="P1 AVote --> Akezptieren -- Argumente nach Wahl">
    P1_AVote_ArgAkz_1 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_AVote_ArgAkz_2 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_AVote_ArgAkz_3 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_AVote_ArgAkz_4 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_AVote_ArgAkz_5 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    # </editor-fold>
    # <editor-fold desc="P1 AVote --> Bestes Argument wählen  -- Argumente nach Wahl">
    P1_AVote_ArgBest_1 = models.BooleanField(blank=True, initial=False)
    P1_AVote_ArgBest_2 = models.BooleanField(blank=True, initial=False)
    P1_AVote_ArgBest_3 = models.BooleanField(blank=True, initial=False)
    P1_AVote_ArgBest_4 = models.BooleanField(blank=True, initial=False)
    P1_AVote_ArgBest_5 = models.BooleanField(blank=True, initial=False)
    # </editor-fold>
    # <editor-fold desc="P1 AVote --> Bestes Argument für neue Verteilung berechnen - Gewinner und Token">
    P1_AVote_nArgBest = models.IntegerField(doc="Number of votes for best vote")
    P1_AVote_ArgBest = models.IntegerField(doc="Whether participant is one of the best-vote winners.")
    P1_AVote_ArgToken = models.IntegerField(initial=0, doc="Token für the argument.")
    P1_AVote_ArgBestSingle = models.IntegerField()

    # </editor-fold>#

    # Argumente rejection
    # <editor-fold desc="P1_Argumente eingeben für neue Vertielung. Nur bei Rejection                                                            ----- initials noch löschen">
    P1_iTokenAddSuggestion_1 = models.FloatField(
        # label="Player 1 (" + str(format(Constants.P1_StartToken_0[1-1], '.0f')) + "Token)",
        min=0, max=Constants.P1_GesamtZusatzToken,
    )
    P1_iTokenAddSuggestion_2 = models.FloatField(
        # label="Player 2 (" + str(format(Constants.P1_StartToken_0[2-1], '.0f')) + "Token)",
        min=0, max=Constants.P1_GesamtZusatzToken,
    )
    P1_iTokenAddSuggestion_3 = models.FloatField(
        # label="Player 3 (" + str(format(Constants.P1_StartToken_0[3-1], '.0f')) + "Token)",
        min=0, max=Constants.P1_GesamtZusatzToken,
    )
    P1_iTokenAddSuggestion_4 = models.FloatField(
        # label="Player 4 (" + str(format(Constants.P1_StartToken_0[4-1], '.0f')) + "Token)",
        min=0, max=Constants.P1_GesamtZusatzToken,
    )
    P1_iTokenAddSuggestion_5 = models.FloatField(
        # label="Player 5 (" + str(format(Constants.P1_StartToken_0[5-1], '.0f')) + "Token)",
        min=0, max=Constants.P1_GesamtZusatzToken,
    )
    P1_Rej_Arg = models.TextField(
        label="",
        max_length=250,
        # blank=True  #das wird dann später im javascript aufgehoben
    )
    # </editor-fold>
    # <editor-fold desc="P1 Argumente akzeptieren -Rejection">
    P1_Rej_ArgAkz_1 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Rej_ArgAkz_2 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Rej_ArgAkz_3 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Rej_ArgAkz_4 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Rej_ArgAkz_5 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    # </editor-fold>
    # <editor-fold desc="P1 Bestes Argument wählen  -Rejection">
    P1_Rej_ArgBest_1 = models.BooleanField(blank=True, initial=False)
    P1_Rej_ArgBest_2 = models.BooleanField(blank=True, initial=False)
    P1_Rej_ArgBest_3 = models.BooleanField(blank=True, initial=False)
    P1_Rej_ArgBest_4 = models.BooleanField(blank=True, initial=False)
    P1_Rej_ArgBest_5 = models.BooleanField(blank=True, initial=False)
    # </editor-fold>
    # <editor-fold desc="P1 Rej --> Bestes Argument für neue Verteilung berechnen - Gewinner und Token">
    P1_Rej_nArgBest = models.IntegerField(doc="Number of votes for best vote")
    P1_Rej_ArgBest = models.IntegerField(initial=-99, doc="Whether participant is one of the best-vote winners.")
    P1_Rej_ArgToken = models.IntegerField(initial=0, doc="Token für the argument.")
    P1_Rej_ArgBestSingle = models.IntegerField(initial=-99)

    # </editor-fold>#

    # <editor-fold desc="Acceptance">
    # Argumente Acceptance
    P1_Acc_DistributionChange = models.IntegerField(
        choices=[
            [1, 'Ich hätte mich dennoch für Verfahren >Gleichverteilung< entschieden.'],
            [2, 'Ich hätte mich für das alternative Verfahren >Angleichen von unten< entschieden.'],
            [3,
             "Ich hätte mich für das alternative Verfahren >Verteilung gemäß Ausgangseinkommen mit Berücksichtigung des Mindestkonsums< entschieden."]
        ],
        doc="Former variable name: P1_AnerkennungPrivat",
        initial=-99,
        widget=widgets.RadioSelect)
    P1_Acc_Arg = models.TextField(
        label="",
        max_length=250, )
    # <editor-fold desc="P1 Acc --> Akezptieren -- Argumente nach Wahl">
    P1_Acc_ArgAkz_1 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Acc_ArgAkz_2 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Acc_ArgAkz_3 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Acc_ArgAkz_4 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    P1_Acc_ArgAkz_5 = models.IntegerField(
        choices=[
            [1, "Ich akzeptiere das Argument"],
            [0, "Ich akzeptiere das Argument nicht"],
        ],
        widget=widgets.RadioSelect,
        initial=-99,
    )
    # </editor-fold>
    # <editor-fold desc="P1 Acc --> Bestes Argument wählen  -- Argumente nach Wahl">
    P1_Acc_ArgBest_1 = models.BooleanField(blank=True, initial=False)
    P1_Acc_ArgBest_2 = models.BooleanField(blank=True, initial=False)
    P1_Acc_ArgBest_3 = models.BooleanField(blank=True, initial=False)
    P1_Acc_ArgBest_4 = models.BooleanField(blank=True, initial=False)
    P1_Acc_ArgBest_5 = models.BooleanField(blank=True, initial=False)
    # </editor-fold>
    # <editor-fold desc="P1 Acc --> Bestes Argument für neue Verteilung berechnen - Gewinner und Token">
    P1_Acc_nArgBest = models.IntegerField(doc="Number of votes for best vote")
    P1_Acc_ArgBest = models.IntegerField(initial=-99, doc="Whether participant is one of the best-vote winners.")
    P1_Acc_ArgToken = models.IntegerField(initial=0, doc="Token für the argument.")
    P1_Acc_ArgBestSingle = models.IntegerField(initial=-99)

    # </editor-fold>#
    # </editor-fold>
    # </editor-fold>

    P1_SumRoundIncome = models.FloatField(initial=-99)

    # Hilfsvariablen
    P1_ChatDisProStart = models.BigIntegerField(initial=-99)
    P1_ChatDisProStartKorr = models.BigIntegerField(initial=-99)
    P1_AVote_ChatStart = models.BigIntegerField(initial=-99)
    P1_AVote_ChatStartKorr = models.BigIntegerField(initial=-99)
    Nickname = models.TextField(initial=-99)

    # <editor-fold desc="Old Variables that I don't need anymore">

    # </editor-fold>

    def roundupto50(self, number):
        if math.fmod(number, 0.5) != 0:
            return number + (0.5 - math.fmod(number, 0.5))
        else:
            return number


    work_hours = models.IntegerField(
        label="Wieviele Stunden arbeiten Sie neben des Studiums pro Woche?",
        choices=[
            [1, "0 Stunden "],
            [2, "1-10 Stunden"],
            [3, "11-20 Stunden"],
            [4, "21-30 Stunden"],
            [5, "31 Stunden oder mehr"],

        ]
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