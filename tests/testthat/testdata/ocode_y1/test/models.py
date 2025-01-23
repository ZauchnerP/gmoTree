class Constants(BaseConstants):
    min_payoffAB = (float(1.0)) + (float(1.1)) + (float(1.2))  # 3.3
    
class Subsession(BaseSubsession):
    pass

class Player(BasePlayer):
    min_payoffAB = models.FloatField(
        doc = "variable min_payoffAB",
        max = (float(1.0)) + (float(1.1)) + (float(1.2)) )

class Group(BaseGroup):
    min_payoffAB = models.FloatField(
        doc = "variable min_payoffAB",
        max = (float(1.0)) + (float(1.1)) + (float(1.2)) )

    variable2 = models.FloatField(
        doc = "variable variable2",
        max = ((1.0)) + ((1.1)) + ((1.2)) )
