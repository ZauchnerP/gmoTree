from os import environ

# Use Cloudresearch workerId as participant_label:
EXTENSION_APPS = ["cloudcapturer"]  # To redirect cloudresearch WorkerId to participant_label

# <editor-fold desc="Central Variables / Payoff / Time">
Something = [
    [["List1.1", 1], ["List1.2"]],
    [["List2.1", 4], ["List2.2"]],
]

# </editor-fold>

# LANGUAGE_CODE = 'en'  # ISO-639 code # for example: de, fr, ja, ko, zh-hans
# USE_POINTS = False

# ADMIN_USERNAME = 'admin'
# ADMIN_PASSWORD = "admin"
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')


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

# POINTS_DECIMAL_PLACES = 2
# REAL_WORLD_CURRENCY_DECIMAL_PLACES = 2
