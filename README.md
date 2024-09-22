# gmoTree – Get and Modify oTree Data

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gmoTree)](https://cran.r-project.org/package=gmoTree)
[![R-CMD-check](https://github.com/ZauchnerP/gmoTree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZauchnerP/gmoTree/actions/workflows/R-CMD-check.yaml)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/gmoTree?color=blue)](https://r-pkg.org/pkg/gmoTree)

<!-- badges: end -->

gmoTree is an R package developed for importing, merging, and
efficiently managing data obtained from running
<a href="https://www.otree.org/" target="_blank">oTree</a> experiments.
It’s particularly valuable when dealing with complex experimental
designs that span multiple sessions and generate a large number of files
that need to be integrated.[^1]

[^1]: gmoTree is not an official package of the oTree team but is built to complement the oTree open-source platform.

# Installation

To install the CRAN version of this package, use the following command:

`install.packages("gmoTree")`

To install the development version:

`devtools::install_github("ZauchnerP/gmoTree")`

# List of all functions

See the page
<a href="https://zauchnerp.github.io/gmoTree/articles/intro_to_gmoTree.html" target="_blank">Introduction
to gmoTree</a> for a more detailed overview of the functions. 
For further details on the package as a whole, visit the 
<a href="https://zauchnerp.github.io/gmoTree/" target="_blank">
gmoTree website</a>.

## Importing data

- `import_otree()`: Imports your oTree data and combines them in a list of data frames.

## Cleaning up data

- `messy_chat()`: Checks for a messy `Chats` data frame and combines variables that refer to
the same concept.

- `messy_chat()`: Checks for a messy `Time` data frame and combines variables that refer to
the same concept.

- `delete_duplicate()`: Deletes duplicate rows from all data frames in the oTree list.

## Dealing with dropouts and deleting cases

- `show_dropouts()`: Shows participant codes of people who did not finish at (a) certain
app(s) or page(s).

- `delete_dropouts()`: Deletes the data of participants who did not finish at (a) certain
app(s) or page(s). This function deletes the participants’ data from all
data frames in the oTree list. Caution: It does not delete the cases
from the original CSV and Excel files!

- `delete_cases()`: Deletes the data of specified participants from all data frames in the
oTree list. Caution: This function does not delete the data from the
original CSV and Excel files!

- `delete_sessions()`: Deletes the data of specified sessions from all data frames in the oTree
list. Caution: This function does not delete the data from the original
CSV and Excel files!

## Deleting sensitive information

- `delete_plabels()`: Deletes the variable `participant.label` from every app because it might
contain identifiable information on the participants, such as their
MTurk ID. Caution: This function does not delete the variable from the
original CSV and Excel files!

## Making IDs

- `make_ids()`: Makes participant, group, and session IDs that are the same across all
apps.

## Measuring time

- `apptime()`: Calculates the time spent on a specific app.

- `extime()`: Calculates the time spent on the experiment.

- `pagesec()`: Calculates the time spent on each page.

# Transferring variables between the apps

- `assignv()`: Copies a variable from the all_apps_wide data frame to the data frames
of all other apps.

- `assignv_to_aaw()`: Copies a variable from one of your data frames to the `all_apps_wide` data
frame.

# Before running the experiment

- `show_constant()`: Shows constant variables.
- `codebook()`: Creates a codebook based on the oTree code.
