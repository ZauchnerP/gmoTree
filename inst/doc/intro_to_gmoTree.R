## ----collapse=TRUE, include=FALSE---------------------------------------------
library(gmoTree)

## ----  collapse=TRUE----------------------------------------------------------
# Get path to the package data
path <- system.file("extdata/exp_data_5.4.0", package = "gmoTree")

# Import without specifications
# Import all oTree files in this folder and its subfolders
otree <- gmoTree::import_otree(path = path)

# Check the structure of the oTree list of data frames
str(otree, 1)

# The initial info list
otree$info

## ----  collapse=TRUE----------------------------------------------------------
# Initial check before deletion
length(otree$all_apps_wide$participant.code)
length(unique(otree$all_apps_wide$participant.code))
otree$info$initial_n

## ---- collapse=TRUE-----------------------------------------------------------
# Delete duplicate cases
otree <- delete_duplicate(otree)

## ---- collapse=TRUE-----------------------------------------------------------
# Check participant codes and initial_n after deletion
length(otree$all_apps_wide$participant.code)
length(unique(otree$all_apps_wide$participant.code))
otree$info$initial_n

## ---- collapse=TRUE-----------------------------------------------------------
# Import data from different oTree versions
otree_all <- gmoTree::import_otree(
  path = system.file("extdata", package = "gmoTree"))

# Check names of Time data frame
names(otree_all$Time)

# Check names of Chats data frame
names(otree_all$Chats)

## ---- collapse=TRUE-----------------------------------------------------------
otree_all <- messy_time(otree_all,
                        combine = TRUE,
                        info = TRUE)

## ---- collapse=TRUE-----------------------------------------------------------
otree_all <- messy_chat(otree_all,
                        combine = TRUE,
                        info = TRUE)

## ---- collapse=TRUE-----------------------------------------------------------
# Check names of Time data frame again
names(otree_all$Time)

# Check names of Chats data frame again
names(otree_all$Chats)

## ----show dropouts, collapse=TRUE---------------------------------------------
# Show everyone that has not finished with the app "survey"
dropout_list <- show_dropouts(otree, "survey")

head(dropout_list$full)

## ---- collapse=TRUE-----------------------------------------------------------
dropout_list$unique

## ---- collapse=TRUE-----------------------------------------------------------
dropout_list$all_end

## ----  collapse=TRUE----------------------------------------------------------
# First, check some row numbers
nrow(otree$all_apps_wide)
nrow(otree$survey)
nrow(otree$Time)
nrow(otree$Chats)

## ----delete dropouts,  collapse=TRUE------------------------------------------
# Delete all cases that didn't end the experiment on the page "Demographics"
# within the app "survey"
otree2 <- delete_dropouts(otree,
                         final_apps = c("survey"),
                         final_pages = c("Demographics"),
                         info = TRUE)

# Check row numbers again
nrow(otree2$all_apps_wide)
nrow(otree2$survey)
nrow(otree2$Time)
nrow(otree2$Chats)


## ---- collapse=TRUE-----------------------------------------------------------
head(otree2$info$deleted_cases$full)


otree2$info$deleted_cases$unique


otree2$info$deleted_cases$all_end

## ----  collapse=TRUE----------------------------------------------------------
# First, check some row numbers
nrow(otree2$all_apps_wide)
nrow(otree2$survey)
nrow(otree2$Time)
nrow(otree2$Chats)

## ----delete cases,  collapse=TRUE---------------------------------------------
# Delete one participant
person <- otree2$all_apps_wide$participant.code[1]
otree2 <- delete_cases(otree2,
                       pcodes = person,
                       reason = "requested",
                       saved_vars = "participant._index_in_pages",
                       info = TRUE)


## ----  collapse=TRUE----------------------------------------------------------
# Check row numbers again
nrow(otree2$all_apps_wide)
nrow(otree2$survey)
nrow(otree2$Time)
nrow(otree2$Chats)

## ---- collapse=TRUE-----------------------------------------------------------
# Check for all deleted cases (also dropouts):
tail(otree2$info$deleted_cases$full)

## ----  collapse=TRUE----------------------------------------------------------
# First, check some row numbers
nrow(otree2$all_apps_wide)
nrow(otree2$survey)
nrow(otree2$Time)
nrow(otree2$Chats)

## ----delete sessions,  collapse=TRUE------------------------------------------
# Delete one session
otree2 <- delete_sessions(otree,
  scodes = "jk9ekpl0",
  reason = "Only tests",
  info = TRUE)

## ----  collapse=TRUE----------------------------------------------------------
# Check row numbers again
nrow(otree2$all_apps_wide)
nrow(otree2$survey)
nrow(otree2$Time)
nrow(otree2$Chats)

## ----delete plables,  collapse=TRUE-------------------------------------------
# Check variables
head(otree2$all_apps_wide$participant.label)
head(otree2$all_apps_wide$participant.mturk_worker_id)
head(otree2$survey$participant.label)

# Delete all participant labels
otree2 <- delete_plabels(otree2, del_mturk = TRUE)

# Check variables
head(otree2$all_apps_wide$participant.label)
head(otree2$all_apps_wide$participant.mturk_worker_id)
head(otree2$survey$participant.label)

## ---- collapse=TRUE-----------------------------------------------------------
# Check variables first
otree2$all_apps_wide$participant.code
otree2$all_apps_wide$session.code
otree2$all_apps_wide$dictator.1.group.id_in_subsession

## ---- collapse=TRUE-----------------------------------------------------------
# Make session IDs only
otree2 <- make_ids(otree2)

## ---- collapse=TRUE-----------------------------------------------------------
# Check variables
otree2$all_apps_wide$participant_id
otree2$all_apps_wide$session_id

## ---- collapse=TRUE-----------------------------------------------------------
# Get IDs from "from_variable" in the data frame "all_apps_wide"
otree2 <- make_ids(otree2,
                   # gmake = TRUE,  # Not necessary if from_var is not NULL
                   from_var = "dictator.1.group.id_in_subsession")

## ---- collapse=TRUE-----------------------------------------------------------
# Check variables
otree2$all_apps_wide$participant_id
otree2$all_apps_wide$group_id
otree2$all_apps_wide$session_id

## ----apptime, collapse=TRUE---------------------------------------------------
# Calculate the time all participants spent on app "survey"
apptime(otree2, apps = "survey", digits = 3)

## ----apptime with participant, collapse=TRUE----------------------------------
# Calculate the time one participant spent on app "dictator"
apptime(otree2, pcode = "c9inx5wl", digits = 3)

## ----extime, collapse=TRUE----------------------------------------------------
# Calculate the time that all participants spent on the experiment
extime(otree2, digits = 3)

## ----extime with pcode, collapse=TRUE-----------------------------------------
# Calculate the time one participant spent on the experiment
extime(otree2, pcode = "c9inx5wl", digits = 3)

## ----pagesec, collapse=TRUE---------------------------------------------------
# Create two new columns: seconds_on_page2 and minutes_on_page
otree2 <- pagesec(otree2, rounded = TRUE, minutes = TRUE)
tail(otree2$Time)

## ----assignv, collapse=TRUE---------------------------------------------------
# Assign variable "survey.1.player.gender" and name it "gender"
otree2 <- assignv(oTree = otree2,
                 variable = "survey.1.player.gender",
                 newvar = "gender")
# Control
otree2$dictator$gender
otree2$chatapp$gender
# In app "survey", the variable is now twice because it is taken from here
otree2$survey$gender
otree2$survey$player.gender
# In app "all_apps_wide," the variable is also there twice
# (This can be avoided by calling the new variable the same
# as the old variable)
otree2$all_apps_wide$gender
otree2$all_apps_wide$survey.1.player.gender

## ----assignv_to_aaw,  collapse=TRUE-------------------------------------------
# Create a new variable
otree2$survey$younger30 <- ifelse(otree2$survey$player.age < 30, 0, 1)

# Get variable younger30 from survey to all_apps_wide
# and put the new variable right behind the old age variable
otree2 <- assignv_to_aaw(otree2,
                        app = "survey",
                        variable = "younger30",
                        newvar = "younger30",
                        resafter = "survey.1.player.age")

# Control
otree2$all_apps_wide$survey.1.player.age

# Check the position of the old age variable and the new variable
match("survey.1.player.age", colnames(otree2$all_apps_wide))
match("younger30", colnames(otree2$all_apps_wide))

## ----show constant, collapse=TRUE---------------------------------------------
# Make a constant column (this variable is usually created in oTree)
otree2$dictator$constant <- 3

# Show all columns that contain columns containing only one specified value
show_constant(oTree = otree2)

