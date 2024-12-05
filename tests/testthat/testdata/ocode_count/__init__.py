from otree.api import *
import settings
import time


author = 'Patricia Zauchner (zauchner@uni-bremen.de)'

doc = """
Effort task: Count letters in a sentence.
"""


# Models
def get_unique_chars(string_to_guess: str) -> list:
    """
    Create a sorted list of the unique characters in the string whose letters the participants have to count.
    The use of the set function in the creation process ensures that only one instance of each letter is returned
    after filtering out all non-alphabetic characters.
    :param string_to_guess: The string whose characters shall be counted
    :return: A sorted list of the unique characters in the input string
    """
    return sorted(set(filter(str.isalpha, list(string_to_guess.lower()))))


def get_char_counts(string_to_guess: str) -> list:
    """
    Maps the alphabetic characters within a given string to their number of occurrences in that string.
    The return format -- forming a list of lists -- is the following:

        [["X_1/x_1", c_1], ["X_2/x_2", c_2], ... ,  ["X_n/x_n", c_n]]

    where X_i and x_i are the uppercase and lowercase variants of the i-th alphabetic character, while c_i is the
    respective count within the given string. As not all characters are necessarily contained within an input string
    there might be characters of the alphabet missing from the list. Note also that special latin characters like
    "ä" or "ö" only appear after all ASCII characters (i. e. a, b, c, ..., z).
    :param string_to_guess: The string whose characters shall be counted
    :return: A list of lists containing the unique characters of the string and their count.
    """
    unique_chars = get_unique_chars(string_to_guess)

    return [[f'{y.upper()}/{y}', sum(map(lambda x: 1 if y in x else 0, string_to_guess.lower()))] for y in unique_chars]


class C(BaseConstants):
    NAME_IN_URL = 'effort_counting'
    # Not really necessary in effort tasks. But so it looks better in the data
    PLAYERS_PER_GROUP = 5

    STRING_TO_GUESS = "Die ältesten bekannten kieferlosen Fischartigen (z. B. die Pteraspidomorphi) stammen aus dem " \
                      "frühen Ordovizium vor rund 450–470 Millionen Jahren."

    SOLUTION = get_char_counts(STRING_TO_GUESS)

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    pass
