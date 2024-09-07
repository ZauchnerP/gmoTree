from os import environ

# Use Cloudresearch workerId as participant_label:
EXTENSION_APPS = ["cloudcapturer"]  # To redirect cloudresearch WorkerId to participant_label

# <editor-fold desc="Central Variables / Payoff / Time">
Testnumber = 1
Testnumber2 = Testnumber
StartToken = [9, 14, 17, 23, 37 + Testnumber]
OtherTest1 = StartToken
OtherTest2  = StartToken * 5
Something = [1, 3, "test", 3, 1]
# </editor-fold>

# LANGUAGE_CODE = 'en'  # ISO-639 code # for example: de, fr, ja, ko, zh-hans
# USE_POINTS = False

# ADMIN_USERNAME = 'admin'
# ADMIN_PASSWORD = "admin"
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')  # Todo: Change Password


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