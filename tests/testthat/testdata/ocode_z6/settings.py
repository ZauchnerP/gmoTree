from os import environ

v2 = "replacingsettingsworked"

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
