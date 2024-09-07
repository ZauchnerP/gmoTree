#' Calculate the seconds spent on each page
#' @description
#' Create a new variable in the \code{$Time} data frame that contains the time
#' spent on each page.
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @param minutes Logical. \code{TRUE} if the output should be
#' minutes instead of seconds.
#' @param rounded Logical. \code{TRUE} if the output should be rounded.
#' @param digits Integer. The number of digits to which the
#' output should be rounded.
#' This parameter has no effect unless \code{rounded = TRUE}.
#' @param combine Logical. \code{TRUE} if all variables referring to epoch time
#' should be merged, and all variables referring to participant code should be
#' merged in case data of several versions of oTree are used.
#' @returns This function returns a duplicate of the original oTree list of
#' data frames that also contains a column in the \code{$Time} data frame
#'  named \code{seconds_on_page2} or \code{minutes_on_page}.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Create two new columns: seconds_on_page2 and minutes_on_page
#' oTree <- pagesec(oTree, rounded = TRUE, minutes = TRUE)
#'
#' # Show the Time data frame
#' head(oTree$Time, n = 30)

#' @export
pagesec <- function(
    oTree,
    rounded = TRUE,
    digits = 2,
    minutes = FALSE,
    combine = FALSE) {

  # Check if time data frame is there  ####
  if (is.null(oTree$Time)) {
    stop("No time data frame found!")
  }

  # Check if there are too many epoch times and participant code variables
  withCallingHandlers({
    oTree <- messy_time(oTree, combine, info = TRUE)
  }, error = function(e) {
    stop(e)
  }, warning = function(w) {
    warning(w)
    invokeRestart("muffleWarning")
  })

  # Set time variable
  if ("epoch_time" %in% colnames(oTree$Time)) {
    timestamp_var_name <- "epoch_time"
  } else if ("epoch_time_completed" %in% colnames(oTree$Time)) {
    timestamp_var_name <- "epoch_time_completed"
  } else if ("time_stamp" %in% colnames(oTree$Time))  {
    timestamp_var_name <- "time_stamp"
  } else {
    stop("There is no variable referring to the time stamp in your Time ",
         "data frame. This should be a variable called either ",
         "\"epoch time,\" \"epoch_time_completed,\" or \"time stamp.\"")
  }

  # Set participant code variable
  if ("participant_code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant_code"
  } else if ("participant__code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant__code"
  } else {
    stop("There is no variable referring to the participant ",
         "code in your Time data frame. ",
         "This should be a variable called either \"participant_code,\" or",
         "\"participant__code.\"")
  }

  # Make list of Participants  ####
  list_of_participants <-
    unique(stats::na.omit(oTree$Time[[participant_code_name]]))

  # Make new variable  ####
  for (participant in list_of_participants) {

    # Make list of indices   ####
    allindices <-
      unique(stats::na.omit(oTree$Time$page_index[
        oTree$Time[[participant_code_name]] == participant]))

    # Calculate  ####
    for (index in allindices) {

      versionminindex <- ifelse(min(allindices) == 0L, 0L, 1L)

      # If the page index exists and is bigger than 0 or 1 (depending on whether
      # there is a 0 index)
      if (!is.na(index) && index > versionminindex) {

        # Time = index - next lower index
        oTree$Time$seconds_on_page2[
          oTree$Time$page_index == index &
            !is.na(oTree$Time[[participant_code_name]]) &
            oTree$Time[[participant_code_name]] == participant] <-

          oTree$Time[[timestamp_var_name]][
            oTree$Time$page_index == index &
              !is.na(oTree$Time[[participant_code_name]]) &
              oTree$Time[[participant_code_name]] == participant] -

          oTree$Time[[timestamp_var_name]][
            oTree$Time$page_index == max(allindices[allindices < index]) &
              !is.na(oTree$Time[[participant_code_name]]) &
              oTree$Time[[participant_code_name]] == participant]
      }
    }
  }

  # Translate to minutes
  if (minutes) {

    oTree$Time$minutes_on_page <- oTree$Time$seconds_on_page2 / 60

    if (rounded) {
      oTree$Time$minutes_on_page <- round(oTree$Time$minutes_on_page,
                                          digits = digits)
    }
    oTree$Time$seconds_on_page2 <- NULL
  }

  # Return  ####
  return(oTree)
}
