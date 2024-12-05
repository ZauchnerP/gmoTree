from os import environ
SECRET_KEY = '=3bxca&b&17=bbn4g^ihborndnezuh7d&m&lqn+58zzeg3i05s'

# Use Cloudresearch workerId as participant_label:
EXTENSION_APPS = ['cloudcapturer']  # To redirect cloudresearch WorkerId to participant_label

# Disable cookies (see app appset)
MIDDLEWARE = ['appset.middle.DisableCSRFMiddleware']

DEMO_PAGE_INTRO_HTML = """Experiment (30.1.2021)."""

# <editor-fold desc="Central Variables / Payoff / Time">
ExchangeToken = 4  # x Token are 1 Dollar
# Time
MinutesForSurvey = 18
timer123 = 500
# Payoff in Token
showup = "1.40"  # In dollar
showupToken = "{:.2f}".format(float(showup)*ExchangeToken)
payoffPart1_c = 6*ExchangeToken  # Token
min_payoffPart1_ab = "1.00"  # Token
max_payoffPart1_ab = "11.00"  # Token
MainCurrencyForSurvey = 3  # Euro or Dollar for participating in the survey
payoff_survey = "{:.2f}".format(MainCurrencyForSurvey*ExchangeToken)  # Token
TestVector2 = [99,66,33]
TestVectorC2 = ["d", "e", 4]
error2 = "Error2insettings"

# </editor-fold>

LANGUAGE_CODE = 'en'  # ISO-639 code # for example: de, fr, ja, ko, zh-hans
USE_POINTS = False

ADMIN_USERNAME = 'admin'
# ADMIN_PASSWORD = "admin"
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')  


SESSION_CONFIG_DEFAULTS = {
    'real_world_currency_per_point': 1.00,
    'participation_fee': 0.00,
    'doc': "",
    # if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
    # in SESSION_CONFIGS, except those that explicitly override it.
    # the session config can be accessed from methods in your apps as self.session.config,
    # e.g. self.session.config['participation_fee']
    "mturk_hit_settings": dict(
        keywords='bonus, study',
        title='Title for your experiment',
        description='Description for your experiment',
        frame_height=500,
        template='global/mturk_template.html',
        minutes_allotted_per_assignment=60,
        expiration_hours=7 * 24,
        qualification_requirements=[]
        # grant_qualification_id='YOUR_QUALIFICATION_ID_HERE', # to prevent retakes
    )
}
ROOMS = [
    {
        "name": "room1",
        "display_name": "room1",
    },
    {
        "name": "room2",
        "display_name": "room2",

    }
]
SESSION_CONFIGS = [
    {
        "name": "rankendothers",
        "display_name": "rankend others",
        "num_demo_participants": 9,
        'dictator': 0,
        "app_sequence": [
            "rankend",
            "dismiss"
        ]
    }
    # </editor-fold>
]

# if an app is included in SESSION_CONFIGS, you don't need to list it here
INSTALLED_APPS = ['otree'
                  ]

POINTS_DECIMAL_PLACES = 2
REAL_WORLD_CURRENCY_DECIMAL_PLACES = 2
