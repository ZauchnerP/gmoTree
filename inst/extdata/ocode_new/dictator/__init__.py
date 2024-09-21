from otree.api import *
from otree import settings
# This code was adjusted to be used by gmotree to make it a bit more messy

class C(BaseConstants):
    NAME_IN_URL = 'dictator'
    PLAYERS_PER_GROUP = 2
    NUM_ROUNDS = 1

    print("1sadfasdfasdf",
          "2asdfadsfasdf",
         " 3asdfasdfasdf",
          "4asdasdfasdf")
    # Initial amount allocated to the dictator
    ENDOWMENT = cu(100)
    Variable = settings.variable


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    print("1sadfasdfasdf",
          "2asdfadsfasdf",
         " 3asdfasdfasdf",
          "4asdasdfasdf")
    kept = models.CurrencyField(
        doc="""Amount dictator decided to keep for himself""",
        min=0,
        max=C.ENDOWMENT,
        label="I will keep",
    )


class Player(BasePlayer):
    pass


# FUNCTIONS
def set_payoffs(group: Group):
    p1 = group.get_player_by_id(1)
    p2 = group.get_player_by_id(2)
    p1.payoff = group.kept
    p2.payoff = C.ENDOWMENT - group.kept


# PAGES
class Introduction(Page):
    pass


class Offer(Page):
    form_model = 'group'
    form_fields = ['kept']

    @staticmethod
    def is_displayed(player: Player):
        return player.id_in_group == 1



class ResultsWaitPage(WaitPage):
    after_all_players_arrive = set_payoffs


class Results(Page):
    @staticmethod
    def vars_for_template(player: Player):
        group = player.group

        return dict(offer=C.ENDOWMENT - group.kept)


page_sequence = [Introduction, Offer, ResultsWaitPage, Results]
doc = """
One player decides how to divide a certain amount between himself and the other
player.
See: Kahneman, Daniel, Jack L. Knetsch, and Richard H. Thaler. "Fairness
and the assumptions of economics." Journal of business (1986):
S285-S300.

This code was adjusted to be used by gmotree to make it a bit more messy """