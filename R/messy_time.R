#' Check if the Time data frame is messy
#' @description
#' Checks if the Time data frame includes both participant-related variables
#' and time stamp variables that appear multiple times. This may occur when
#' data from different oTree versions, which use different variable names,
#' are combined.
#'
#' If desired, the function can merge these variables,
#' storing the data using the newer oTree version's variable names
#' and removing the outdated variables.
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @param combine Logical. \code{TRUE} if all variables referring to epoch time
#' should be merged and/or all variables referring to participant code
#' should be merged in case data of several versions of oTree are used.
#' @param epoch_time Logical. \code{TRUE} if all variables referring to the time
#' stamp should be checked and merged. Only works if \code{combine = TRUE}.
#' @param participant Logical. \code{TRUE} if all variables referring to the
#' participant code should be checked and merged.
#' Only works if \code{combine = TRUE}.
#' @param info Logical. \code{TRUE} if a brief information on the process should
#' be printed.
#' @returns
#' This function searches for multiple variables related to the time stamp
#' or the participant code in the \code{$Time} data frame,
#' which can occur when data from both old and new oTree versions are used.
#'
#' If \code{combine = FALSE}, the function will throw an error
#' if such variables are found.
#'
#' If \code{combine = TRUE}, the function will not throw an error
#' if such variables are found.
#' Instead, it automatically combines the variables into new variables
#' and adds them to the original \code{$Time} data frame.
#' This function then returns a duplicate of the original oTree list but
#' with the \code{$Time} data frame modified.
#'
#' The new variables are
#' called \code{epoch_time_completed} and \code{participant_code}.
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
  stop_messages <- character(0L)
  warning_messages <- character(0L)

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

  time_stopmessage <-
    paste0("More than one variable referred to the time stamp ",
           "in your Time data frame. This could be because you ",
           "mixed data of different versions of oTree in your data frame. ",
           "Please combine them into one variable called either ",
           "\"epoch time,\" \"epoch_time_completed,\" or ",
           "\"time stamp.\" You can do this by using the ",
           "combine-argument of this function.")

  # Set epoch times first with error messages  ####
  if (epoch_time) {
    time_names <- c("epoch_time", "epoch_time_completed", "time_stamp")
    time_names_in_otree <-
      time_names[time_names %in% colnames(oTree$Time)]
    length_epoch_times <- length(time_names_in_otree)
    other_time_names <- time_names_in_otree[
      time_names_in_otree != "epoch_time_completed"]

    if (length_epoch_times > 1L) {
      if (!combine) {
        stop_messages <- c(stop_messages, time_stopmessage)

      } else if (combine) {

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
                "stamp. You asked to combine them with the ",
                "argument combine = TRUE. ",
                "Variable(s) \"",
                paste(other_time_names, collapse = ",\" and \""),
                "\" was/were integrated to variable ",
                "\"epoch_time_completed\" and deleted afterward."))
      }
    }
  }

  # Set participant code variable first with error messages  ####
  if (participant) {
    length_part_codes <- sum(
      c("participant_code",
        "participant__code") %in% colnames(oTree$Time))

    if (length_part_codes > 1L) {
      if (!combine) {
        stop_messages <- c(stop_messages, participant_stopmessage)

      } else if (combine) {
        # Combine participant codes
        oTree$Time$participant_code[is.na(oTree$Time$participant_code)] <-
          oTree$Time$participant__code[is.na(oTree$Time$participant_code)]
        oTree$Time$participant__code <- NULL
        warning_messages <- c(warning_messages, paste0(
                "More than one variable referred to the participant code. ",
                "You asked to combine them with the ",
                "argument combine = TRUE. ",
                "Variable \"participant__code\" was integrated into ",
                "variable \"participant_code\" ",
                "The variable \"participant__code\" was deleted afterward."))
      }
    }
  }

  # Return all error messages and warning messages  ####
  if (length(stop_messages) > 0L) {
    stop(paste(stop_messages, collapse  = "\n"))
  }

  if (info && length(warning_messages) > 0L) {
    # This is printed as a warning for other functions to catch the message
    warning(paste(warning_messages, collapse = "\n"))
  }

  # Return  ####
  return(oTree)
}
