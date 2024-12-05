# <standard imports>
from otree.api import *
import settings
import csv
import time
# </standard imports>

author = 'Jan Romann (jan.romann@uni-bremen.de), ' \
         'Patricia Zauchner (zauchner@uni-bremen.de)'

doc = """
Effort task: Add as many numbers as possible.
"""


# Models
class C(BaseConstants):
    NAME_IN_URL = 'effort_add'
    PLAYERS_PER_GROUP = 5  # Not really necessary in effort tasks. But so it looks better in the data
    TASK_TIMER = settings.task_timer
    NUM_ROUNDS = 1
    POINTS_PER_CORRECT_ANSWER = 1

    with open('effort_add/numbers.csv') as csvfile:
        reader = csv.reader(csvfile)
        INTS = [[int(x) for x in row] for row in reader]
    NUM_TASKS = len(INTS)


class Subsession(BaseSubsession):
    pass


def creating_session(subsession: Subsession):
    # <editor-fold desc="Assign group membership">
    # Copy the group and player id structure of the first app
    if "id_matrix" in subsession.session.vars:
        subsession.set_group_matrix(subsession.session.vars['id_matrix'])
    else:
        subsession.group_randomly()  # oTree function
        subsession.session.vars['id_matrix'] = subsession.get_group_matrix()
        print("ID Matrix created in app effort_add", subsession.session.vars['id_matrix'])
    # </editor-fold>

    # <editor-fold desc="Copy effort treatment">
    try:
        subsession.session.vars["DoEffortTask"] = subsession.session.config["DoEffortTask"]
    except KeyError:
        subsession.session.vars["DoEffortTask"] = getattr(settings, "DoEffortTask", True)
        # print("-------DoEffortTask: ", subsession.session.vars["DoEffortTask"])

    # Set session vars (For counting number of effort apps in effort_intro)
    subsession.session.vars["DoEffort_Add"] = True
    # </editor-fold>


class Group(BaseGroup):
    pass


# Group Functions
def get_numbers(index):
    """ Get numbers for effort task """

    numbers = C.INTS[index]

    return numbers[0], numbers[1]


def live_answer_question(player, data):
    player.question_index = index = player.question_index + 1

    number_1, number_2 = get_numbers(index - 1)  # Get current numbers
    new_number_1, new_number_2 = get_numbers(index)  # Get new numbers

    answer = int(data.get("answer", 0))
    solution = number_1 + number_2
    Answer.create(player=player, question_number=index - 1, number_1=number_1, number_2=number_2,
                  answer=answer)  # Save answer in database
    player.last_answer_correct = answer_correct = answer == solution  # Check if the answer was correct

    player.number_of_attempts = number_of_attempts = player.number_of_attempts + 1  # Increment number of attempts

    # Retrieve old number of right guesses from the database
    number_of_correct_answers = player.number_of_correct_answers

    # If correct, increment number of correct guesses and, at the same time, write new value back to the database
    if answer_correct:
        player.number_of_correct_answers = number_of_correct_answers = number_of_correct_answers + 1

    response = dict(number_1=new_number_1,
                    number_2=new_number_2,
                    answer_correct=answer_correct,
                    number_of_attempts=number_of_attempts,
                    number_of_correct_answers=number_of_correct_answers,
                    )

    return {player.id_in_group: response}


class Player(BasePlayer):
    question_index = models.IntegerField(
        doc="The number of the current task (for iterating through the list of values).",
        initial=0)

    number_of_correct_answers = models.IntegerField(
        doc="Number of correct answers in total.",
        initial=0)

    number_of_attempts = models.IntegerField(
        doc="Number of attempts in total.",
        initial=0)

    last_answer_correct = models.BooleanField(
        doc="Did the user answer the last question correctly?",
        initial=False) 

class Answer(ExtraModel):
    """ Creates an extra dataframe output. Is called with custom_export """

    player = models.Link(Player)
    question_number = models.IntegerField()
    number_1 = models.IntegerField()
    number_2 = models.IntegerField()
    answer = models.IntegerField()


def custom_export(players):
    """ Creates custom export """

    yield ['session_code', 'participant_code', 'question_number',
           "number_1", "number_2", "answer", "correct"]  # Header row
    for answer in Answer.filter():
        player = answer.player
        session = player.session
        participant = player.participant
        correct = 1 if (answer.number_1 + answer.number_2) == answer.answer else 0

        yield [session.code, participant.code, answer.question_number, answer.number_1, answer.number_2, answer.answer,
               correct]


# Pages
class EffortPage(Page):
    """ Handles if other pages are displayed """

    @staticmethod
    def is_displayed(player):
        return player.session.vars.get("DoEffortTask", True)


class Intro(EffortPage):
    """ Introduction to the addition task """

    @staticmethod
    def before_next_page(player, timeout_happened):
        # User has task_timer seconds to complete as many pages as possible
        player.participant.vars['expiry_timestamp'] = time.time() + C.TASK_TIMER


class Task(EffortPage):
    """ Addition task """

    live_method = 'live_answer_question'

    @staticmethod
    def get_timeout_seconds(player):
        return player.participant.vars['expiry_timestamp'] - time.time()
    timer_text = 'Verbleibende Zeit (in Sek.): '

    @staticmethod
    def vars_for_template(player):
        index = player.question_index
        int1, int2 = get_numbers(index)

        return {
            "int1": int1,
            "int2": int2,
            "debug": False
        }

    @staticmethod
    def before_next_page(player, timeout_happened):
        correct_answers = player.number_of_correct_answers

        player.participant.vars["addpoints"] = correct_answers
        player.participant.vars["addmistakes"] = player.number_of_attempts - correct_answers


class Results(EffortPage):
    """ Results of addition task """

    @staticmethod
    def is_displayed(player):
        return player.session.vars.get("DoEffortTask", True)


page_sequence = [
    Intro,
    Task,
    Results
]
