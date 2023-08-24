#' Check if the Time data frame is messy
#' @description
#' Check if the Time data frame includes both participant-related variables
#' and time stamp variables that appear multiple times. This may occur when
#' data from different oTree versions, which use different variable names,
#' are combined.
#'
#' If desired, the function can merge these variables,
#' storing the data using the newer oTree version's variable names
#' and removing the outdated variables.
#'
#' @keywords oTree
#' @param oTree A list of data frames that were created by import_otree().
#' @param combine Logical. TRUE if all variables referring to epoch time should
#' be merged and/or all variables referring to participant code should be merged
#' in case data of several versions of oTree are used.
#' @param epoch_time Logical. TRUE if all variables referring to the time stamp
#' should be checked and merged. Only works if combine = TRUE.
#' @param participant Logical. TRUE if all variables referring to the
#' participant code should be checked and merged. Only works if combine = TRUE.
#' @param info Logical. TRUE if a brief information on the process should
#' be printed.
#' @returns
#' This function returns an oTree list of data frames that is
#' an exact copy of the original oTree list of data frames but - if the user
#' wishes to do so - combines the
#' time tamps and participant codes in the Time data frame if several variables
#' are referring to those because of the
#' combination of different oTree versions. The final variables are called
#' epoch_time_completed and participant_code.
#'
#' If combine = FALSE, the function only checks for the existence of several
#' variables referring to the time stamp or the participant code and throws an
#' error if yes.
#' @examplesIf rlang::is_installed("withr")
#' # Set data folder first
#' withr::with_dir(system.file("extdata", package = "gmoTree"), {
#'
#' # Import all oTree files in this folder and its subfolders
#' oTree <- import_otree()
#' })
#'
#' # Show all Time column names
#' print(colnames(oTree$Time))
#'
#' # Run function
#' oTree <- messy_time(oTree, combine = TRUE)
#'
#' # Show all Time column names again
#' print(colnames(oTree$Time))

#' @export
messy_time <- function(oTree,
                       combine = FALSE,
                       epoch_time = TRUE,
                       participant = TRUE,
                       info = FALSE) {

  # Error messages
  stop_messages <- c()
  warning_messages <- c()

  participant_stop <- FALSE
  participant_stopmessage <-
    paste0("More than one variable referred to the participant ",
            "code in your Time data frame. ",
            "This could be because you mixed data of different ",
            "versions of oTree in your data frame. ",
            "Please combine them into one ",
            "variable called either \"participant_code,\" or ",
            "\"participant__code.\" ",
            "You can do this by using the combine-argument of this ",
            "function.")

  time_stop <- FALSE
  time_stopmessage <-
    paste0("More than one variable referred to the time stamp ",
           "in your Time data frame. This could be because you ",
           "mixed data of different versions of oTree in your data frame. ",
           "Please combine them into one variable called either ",
           "\"epoch time,\" \"epoch_time_completed,\" or ",
           "\"time stamp.\" You can do this by using the ",
           "combine-argument of this function.")


  # Set epoch times first with error messages  ####
  if (epoch_time == TRUE) {
    time_names <- c("epoch_time", "epoch_time_completed", "time_stamp")
    time_names_in_otree <-
      time_names[time_names %in% colnames(oTree$Time)]
    length_epoch_times <- length(time_names_in_otree)
    other_time_names <- time_names_in_otree[
      time_names_in_otree != "epoch_time_completed"]

    if (length_epoch_times > 1) {
      if (combine == FALSE) {
        time_stop <- TRUE
        stop_messages <- c(stop_messages, time_stopmessage)

      } else if (combine == TRUE) {

        # Merge epoch time codes
        if ("epoch_time" %in% colnames(oTree$Time)) {
          # Merge variables
          oTree$Time$epoch_time_completed[
            is.na(oTree$Time$epoch_time_completed)] <-
            oTree$Time$epoch_time[is.na(oTree$Time$epoch_time_completed)]

          # Delete old variable
          oTree$Time$epoch_time <- NULL
        }

        if ("time_stamp" %in% colnames(oTree$Time)) {
          # Merge variables
          oTree$Time$epoch_time_completed[
            is.na(oTree$Time$epoch_time_completed)] <-
            oTree$Time$time_stamp[is.na(oTree$Time$epoch_time_completed)]

          # Delete old variable
          oTree$Time$time_stamp <- NULL
        }

        warning_messages <- c(warning_messages, paste0(
                "More than one variable referred to the time ",
                "stamp. You asked for combining them with the ",
                "argument combine = TRUE. ",
                "Variable(s) \"",
                paste(other_time_names, collapse = ",\" and \""),
                "\" was/were integrated to variable ",
                "\"epoch_time_completed\" and deleted afterward."))
      }
    }
  }

  # Set participant code variable first with error messages  ####
  if (participant == TRUE) {
    length_part_codes <- sum(
      c("participant_code",
        "participant__code") %in% colnames(oTree$Time))

    if (length_part_codes > 1) {
      if (combine == FALSE) {
        participant_stop <- TRUE
        stop_messages <- c(stop_messages, participant_stopmessage)
      } else if (combine == TRUE) {
        # Combine participant codes
        oTree$Time$participant_code[is.na(oTree$Time$participant_code)] <-
          oTree$Time$participant__code[is.na(oTree$Time$participant_code)]
        oTree$Time$participant__code <- NULL
        warning_messages <- c(warning_messages, paste0(
                "More than one variable referred to the participant code. ",
                "You asked for combining them with the ",
                "argument combine = TRUE. ",
                "Variable \"participant__code\" was integrated into ",
                "variable \"participant_code\" ",
                "The variable \"participant__code\" was deleted afterward."))
      }
    }
  }

  # Return all error messages and warning messages  ####
  if (participant_stop == TRUE || time_stop == TRUE) {
    stop(paste(stop_messages, collapse  = "\n"))
  }

  if (info == TRUE && !is.null(warning_messages)) {
    # This is printed as a warning for other functions to catch the message
    warning(paste(warning_messages, collapse = "\n"))
  }

  # Return  ####
  return(oTree)
}
