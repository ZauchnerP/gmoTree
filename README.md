# gmoTree – Get and Modify oTree Data
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/ZauchnerP/gmoTree/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZauchnerP/gmoTree/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

gmoTree is an R package developed for importing, merging, and efficiently managing data obtained from running oTree (https://www.otree.org/) experiments. It's particularly valuable when dealing with complex experimental designs that span multiple sessions and generate a large number of files that need to be integrated.

gmoTree is not an official package of the oTree team, but it was created to complement the open-source platform.

## About this version
This is the beta version of the gmoTree package!

This package has been thoroughly tested, but since it is still in beta,
it's recommended to double-check the results when importing data. For example:
Verify if the number of cases matches the numbers reported by the lab
and ensure that the time results are reasonable. Please let me know if you
encounter any errors or have suggestions for improvement.

The final package might differ from this package, so please
keep track of the changes.

# Installation

To install this package, use the following command:

`devtools::install_github("ZauchnerP/gmoTree")`

# List of all functions
See the page <a href="https://zauchnerp.github.io/gmoTree/articles/intro_to_gmoTree.html" target="_blank">Introduction to gmoTree</a> for a
more detailed overview of the functions. Or see the <a href="https://zauchnerp.github.io/gmoTree" target="_blank">website of gmoTree</a> for more information on the package in general.

## Importing data

### import_otree()

Imports your oTree data and combines them in a list of data frames.

## Cleaning up data

### messy_chat()

Checks for a messy Chats data frame and combines variables that refer to
the same concept.

### messy_chat()

Checks for a messy Time data frame and combines variables that refer to
the same concept.

### delete_duplicate()

Deletes duplicate rows from all data frames in
the oTree list.

## Dealing with dropouts and deleting cases

### show_dropouts()

Shows participant codes of people who did not finish at (a) certain
app(s) or page(s).

### delete_dropouts()

Deletes the data of participants who did not finish at (a) certain app(s) or
page(s). This function deletes the participants' data from all data frames in
the oTree list. Caution: It does not delete the cases from the original
CSV and Excel files!

### delete_cases()

Deletes the data of specified participants from all data frames in the 
oTree list. Caution: This function does not delete the data from the original
CSV and Excel files!

### delete_sessions()
Deletes the data of specified sessions from all data frames in the oTree list. 
Caution: This function does not delete the data from the original CSV and Excel
files!

## Deleting sensitive information

### delete_plabels()

Deletes the variable `participant.label` from every app because it might
contain identifiable information on the participants, such as their
MTurk ID. Caution: This function does not delete the variable from the
original CSV and Excel files!

## Making IDs

### make_ids()

Makes participant, group, and session IDs that are the same across all
apps.

## Calculating the time

### apptime()

Calculates the time spent on a specific app.

### extime()

Calculates the time spent on the experiment.

### pagesec()

Calculates the time spent on each page.

# Transferring variables between the apps

### assignv()

Copies a variable from the all_apps_wide data frame to the data frames
of all other apps.

### assignv_to_aaw()

Copies a variable from one of your data frames to the all_apps_wide data
frame.

# Before running the experiment

### show_constant()

Shows constant variables.
