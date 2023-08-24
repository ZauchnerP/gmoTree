#' Calculate the time that was spent on the whole experiment
#' @description
#' Calculate the time spent on the experiment.
#' If not stated otherwise, the calculation only starts at the end of
#' the first page!
#' @param oTree A list of data frames that were created by import_otree().
#' @param pcode Character. The value of the participant.code variable if the
#' time should only be calculated for one specified participant.
#' @param plabel Character. The value of the participant.label variable if the
#' time should only be calculated for one specified participant.
#' @param group_id Integer. The value of the group_id variable if the
#' time should only be calculated for one specified group. The group_id
#' variable can be created with make_ids().
#' @param seconds Logical.
#' TRUE if the output should be in seconds instead of minutes.
#' @param rounded Logical. TRUE if the output should be rounded.
#' @param digits Integer. The number of digits to which the output
#' should be rounded.
#' This parameter has no effect unless rounded = TRUE.
#' @param startat Integer or character string "real".
#' Whether the start of the experiment should be taken from the time at
#' a certain index of each person's vector of page_indexes in the
#' Time data frame or from the "time_started"
#' variable in all_apps_wide ("real"). Important: If integer,
#' it represents the position within the page index sequence,
#' not the numeric value of the index.
#' @param tz Character. Time zone.
#' @param sinfo Character.
#' "session_id" to use session ID for additional information in the data frame
#' of single durations, "session_code" to use session codes, or NULL if no
#' session column should be shown.
#' @param combine Logical. TRUE if all variables referring to epoch time should
#' be merged, and all variables referring to participant code should
#' be merged in case data of several versions of oTree are used. If FALSE,
#' the function returns an error if several oTree versions' data are present.
#' @returns
#' This function returns either a single value if only the data of one person
#' is calculated or a list of information on the time several participants
#' spent on the experiment.
#'
#' In this list, you can find the following information:
#'
#' $mean_duration = The experiment's average duration.
#'
#' $min_duration = The experiment's minimum duration.
#'
#' $max_duration = The experiment's maximum duration.
#'
#' $single_durations = A data frame of all durations that
#' are used for calculating the min, max, and mean duration.
#'
#' $messages = All important notes to the calculations.
#'
#' $only_one_page = A vector of all individuals who only have one time stamp.
#' @keywords oTree
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Make a data frame of durations
#' extime(oTree)
#'
#' # Show time for one participant
#' extime(oTree, pcode = "wk247s9w")

#' @export
extime <- function(
    oTree,
    pcode = NULL,
    plabel = NULL,
    group_id = NULL,
    seconds = FALSE,
    rounded = TRUE,
    digits = 2,
    startat = 1,
    tz = "UTC",
    sinfo = "session_code",
    combine = TRUE) {

  # Info: epoch_time is called epoch_time_completed in the new oTree version.
  # The code works nevertheless.
  # Old Version of oTree (time_stamp & participant__code)
  # New Version of oTree (epoch_time & participant_code)

  firststageproblemparticipants <- c()
  messages <- c()

  # Define error and warning messages  ####
  errormax1min1 <- paste0(
    "Warning: For at least one participant, the experiment only has one page. ",
    "I.e., the indices for the first and the last page are the same. ",
    "See $only_one_page for information on this participant",
    "/these participants! Participants in this list are not included in the ",
    "output."
  )

  errormax1min1_specific <- paste0(
    "Warning: For this participant, the experiment only has one page. ",
    "I.e., the indices for the first and the last page are the same. "
  )

  indextoohigh <- FALSE
  indextoohigh_message <-
    paste0("The chosen starting value startat is higher than the ",
    "total number of all indices (for at least one case if more participants ",
    "are chosen). Please select a valid starting value.")

  indextoolow <- FALSE
  indextoolow_message <-
    paste0("The chosen starting value startat is lower than ",
    "the lowest number of all indices (for at least one case if ",
    "more participants are chosen). Please select a valid starting value.")


  # Error handling  ####
  if (is.null(oTree$Time)) {
    stop("There is no \"Time\" data frame.")
  }

  if (nrow(oTree$Time) == 0) {
    stop("Your \"Time\" data frame is empty")
  }

  if (startat == "real" && is.null(oTree$all_apps_wide)) {
    stop("The argument \"startat = real\" only works if there is a ",
         "\"all apps wide\" data frame in your oTree list of data frames!")
  }

  if (startat != "real" && startat < 1) {
    stop("Please choose a valid \"startat\"!")
  }

  # Check if there are too many epoch times and participant code variables
  withCallingHandlers({
    oTree <- messy_time(oTree, combine = combine)
  }, error = function(e) {
    stop("It seems as if you are using data from different oTree versions. ",
         "Please combine the participant and time stamp variables manually ",
         "or use combine=TRUE.")
  }, warning = function(w) {
    # Don't show any warnings, because the oTree list of data frames
    # does not change due to this function!
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
    stop("There is no variable referring to the epoch time tamp ",
         "in your Time data frame. ",
         "This should be a variable called either \"epoch time,\" ",
         "\"epoch_time_completed,\" or ",
         "\"time stamp.\"")
  }

  # Set participant code variable
  if ("participant_code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant_code"
  } else if ("participant__code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant__code"
  } else {
    stop("There is no variable referring to the ",
         "participant code in your Time data frame. ",
         "This should be a variable called either \"participant_code,\" or",
         "\"participant__code.\"")
  }

  # Other errors  ####
  if (length(pcode) > 1) {
    stop("Please enter only one participant!")
  }

  if (!is.null(sinfo) &&
      !(sinfo %in% c("session_code", "session_id"))) {
    stop("Please specify a valid sinfo! Possibilities are ",
         "\"session_code\" or \"session_id\"")
  }

  if (!is.null(pcode) && !is.null(group_id)) {
    stop("Please specify either the pcode or the group_id, ",
         "but not both together.")
  }

  if (!is.null(pcode) &&
     !(pcode %in% oTree$Time[[participant_code_name]])) {
    stop("The participant is not in the \"Time\" data frame.")

  }

  if (!is.null(plabel) && !is.null(group_id)) {
    stop("Please enter only plabel or group_id")
  }

  if (!is.null(pcode) && !is.null(plabel)) {
    stop("Please enter only pcode or plabel")
  }

  if (length(plabel) > 1) {
    stop("Please enter only one participant label!")
  }

  if (is.null(oTree$all_apps_wide) && !is.null(plabel)) {
    stop(paste0("You can only use the argument plabel ",
                "if there is an all_apps_wide-data frame in your oTree list"))
  }

  if (!is.null(group_id) &&
      !is.null(oTree$Time) &&
      is.null(oTree$Time$group_id)) {
    stop(paste0("There is no variable called group_id in your ",
                "\"Time\" data frame. \n",
                "Please run make_ids first before using this function. ",
                "(Use argument gmake = TRUE.)"))
  }

  if (!is.null(group_id) &&
      !is.null(oTree$Time$group_id) &&
      !(group_id %in% oTree$Time$group_id)) {
    stop(paste0("group_id ",
                group_id,
                "is not in your \"Time\" data frame."))
  }

  if (!is.null(sinfo) &&
    sinfo == "session_id" &&
    is.null(oTree$Time$session_id)) {
    stop("There is no session_id in the \"Time\" data frame")
  }

  if (!is.null(sinfo) &&
      sinfo == "session_code" &&
      is.null(oTree$Time$session_code) &&
      is.null(oTree$Time$session__code)) {
    stop(
      paste0("There is no variable called session_code or session__code ",
             "in the \"Time\" ",
             "data frame.\n Try sinfo=\"session_id,\" ",
             "or use ",
             "argument sinfo = NULL if you do not need session information."
         ))
  }

  # Check for several session_code infos in Time data frame
  if (!is.null(sinfo)) {

    # Check if there are old and new session_code variables
    length_session_code_variables <- sum(
      c("session_code",
        "session__code",
        "participant__session__code") %in% colnames(oTree$Time))

    if (length_session_code_variables > 1) {
       # Are there old oTree versions where this could be relevant?
       # I did not test for multiple session codes in messy_time()
       stop("Multiple variables referring to the session code were ",
            "detected in your \"Time\" data frame. ",
            "This issue may arise if data from different versions of ",
            "oTree are combined within the same data frame. ",
            "Before using this function, please combine these variables ",
            "into a single one.")
    }
  }

  # Seconds  ####
  if (seconds == TRUE) {
    divsec <- 1
  } else {
    divsec <- 60 # Divide seconds by 60 to get minutes
  }

  # Transform plabel to pcode identifier  ####
  if (!is.null(plabel)) {
    if (length(unique(oTree$all_apps_wide$participant.label)) ==
        length(oTree$all_apps_wide$participant.label)) {

      pcode <- oTree$all_apps_wide$participant.code[
        oTree$all_apps_wide$participant.label == plabel]
    } else {
      stop("You do not have unique participant labels in your ",
           "all_apps_wide data frame! The argument plabel is ",
           "not working in such a case!")
    }
  }

  # Make sub functions 1 - indices and time stamps and durations  ####
  calc_max_index <- function(allindices) {
    max_index <- if (length(allindices)) {
      max(allindices)
    } else {
      NA
    }
    return(max_index)  # Returns to higher level function
  }

  min_max_stamps_dur_spec <- function(allindices,
                                who,
                                max_index = max_index) {

    # First time stamp for specific individuals
    if (startat == "real") {
      mintimestamp <- as.numeric(
        as.POSIXct(oTree$all_apps_wide$participant.time_started[
          oTree$all_apps_wide$participant.code == who
        ], tz = tz))

    } else {
      # Check if startat is valid
      if (startat > length(allindices)) {
        indextoohigh <<- TRUE
        stop(indextoohigh_message)

      } else if (startat < min(allindices)) {
        indextoolow <<- TRUE
        stop(indextoolow_message)
      }

      # Assign minimum time stamp
      mintimestamp <- oTree$Time[[timestamp_var_name]][
          !is.na(oTree$Time[[participant_code_name]]) &
          oTree$Time[[participant_code_name]] == who &
          oTree$Time$page_index == allindices[[startat]]]
    }

    # Last time stamp for specific individuals
    maxtimestamp <- oTree$Time[[timestamp_var_name]][
      !is.na(oTree$Time[[participant_code_name]]) &
        oTree$Time[[participant_code_name]] == who &
        oTree$Time$page_index == max_index]

    # Duration of the whole experiment
    duration <- (maxtimestamp - mintimestamp) / divsec

    return(duration)
  }

  # Minimum time stamp
  mintimestamp_all <- function(allindices) {
    # First stamp for all individuals

    # Check if startat is valid
    if (startat == "real") {
      mintimestamp <- as.numeric(
        as.POSIXct(oTree$all_apps_wide$participant.time_started[
          oTree$all_apps_wide$participant.code == i
        ], tz = tz))
    } else {
      if (startat > length(allindices)) {
        indextoohigh <<- TRUE
        stop()
      } else if (startat < min(allindices)) {
        indextoolow <<- TRUE
        stop()
      }

      # Assign minimum time stamp
      mintimestamp <- oTree$Time[[timestamp_var_name]][
          !is.na(oTree$Time[[participant_code_name]]) &
          oTree$Time[[participant_code_name]] == i &
          oTree$Time$page_index == allindices[[startat]]]

    }
    return(mintimestamp)  # Returns to next higher level
  }

  # Make sub functions 2 - duration calculation  ####
  duration_specific <- function() {
    # Duration for one person
    # Info: Existence of this person in the Time data frame was already checked
    # at the beginning of the extime function!

    # Get indices - specific
    allindices <-
      oTree$Time$page_index[
        oTree$Time[[participant_code_name]] == pcode]
    allindices <- allindices[!is.na(allindices)]
    max_index <- calc_max_index(allindices)

    # Calculate time - specific
    if ((max_index == 1 && min(allindices) == 1) ||
        max_index == 0)  {

      # Warning: If there is only one page in the experiment
      firststageproblemparticipants <<- c(firststageproblemparticipants, pcode)
      messages <<- c(messages, errormax1min1_specific)
      duration <- NA

    } else {
      # Get time stamps and duration - specific
      duration <- min_max_stamps_dur_spec(allindices = allindices,
                                           who = pcode,
                                           max_index = max_index)
    }

    # Return output - specific to next higher level
    return(duration)
  }

  duration_all <- function() {
    # Duration for several individuals
    # Info: List of participants is from Time data frame itself
    # so there is no need to control for empty indices vectors

    allindices <-
      oTree$Time$page_index[oTree$Time[[participant_code_name]] == i]
    allindices <- allindices[!is.na(allindices)]
    max_index <- calc_max_index(allindices)

    if (max_index == 0 ||
      (max_index == 1  && min(allindices) == 1))  {
      # Warning: If there is only one page in the experiment
      firststageproblemparticipants <<- c(firststageproblemparticipants, i)
      messages <<- c(messages, errormax1min1)
      duration <- NA

    } else {
      # Get time stamps and duration
      mintimestamp <- mintimestamp_all(allindices)
      maxtimestamp <- oTree$Time[[timestamp_var_name]][
        !is.na(oTree$Time[[participant_code_name]]) &
          oTree$Time[[participant_code_name]] == i &
          oTree$Time$page_index == max_index]
      duration <- (maxtimestamp - mintimestamp) / divsec
    }

    # Returns duration_all to next higher level
    return(duration)
  }

  # Get session information for singledurations table
  get_session <- function(who) {
    if (is.null(sinfo)) {
      session <- NA
    } else  {
      if (sinfo == "session_id") {
        session <- unique(oTree$Time$session_id[
          !is.na(oTree$Time[[participant_code_name]]) &
          oTree$Time[[participant_code_name]] == who])

      } else if (sinfo == "session_code") {
        if (!is.null(oTree$Time$session_code)) {
          session <- unique(oTree$Time$session_code[
            !is.na(oTree$Time[[participant_code_name]]) &
            oTree$Time[[participant_code_name]] == who])

        } else if (!is.null(oTree$Time$session__code)) {
          # Is that even happening? I don't have such data yet.
          session <- unique(oTree$Time$session__code[
            !is.na(oTree$Time[[participant_code_name]]) &
            oTree$Time[[participant_code_name]] == who])
        }
      }
    }
    return(session)
  }

  # Make output for several/all individuals
  output_all <- function() {
    output <- list()

    output[["mean_duration"]] <- ifelse(
      rounded == TRUE,
      round(mean(singledurations[, "duration"]),
            digits = digits),
      mean(singledurations[, "duration"])
    )

    output[["min_duration"]] <- ifelse(
      rounded == TRUE,
      round(min(singledurations[, "duration"]),
           digits = digits),
      min(singledurations[, "duration"])
    )

    output[["max_duration"]] <- ifelse(
      rounded == TRUE,
      round(max(singledurations[, "duration"]),
            digits = digits),
      max(singledurations[, "duration"])
    )

    output[["single_durations"]] <-
      singledurations[order(singledurations$duration), ]

    if (rounded == TRUE) {
      output[["single_durations"]][["duration"]] <-
        round(output[["single_durations"]][["duration"]], digits = digits)
    }

    if (length(unique(messages) > 0)) {
      output[["messages"]] <- unique(messages)
    }

    output[["only_one_page"]] <- firststageproblemparticipants

    if (length(warningparticipants) > 0) {
      output[["warnings"]] <- unique(warningparticipants)

      output[["messages"]] <-
        c(output[["messages"]],
          paste0("For some participants, no duration could be ",
                 "calculated. See list in $warnings. Did they ",
                 "make it to the app(s) or are there data ",
                 "there twice?"))
    }

    # Directly to final return
    return(output)
  }

  # Calculate time for specified individuals
  time_for_specific <- function() {

    # Get time
    withCallingHandlers({
      duration <- duration_specific()   # Info: Messages are set here
    }, error = function(e) {
      stop(e)
    }, warning = function(w) {
      warning(w)
      invokeRestart("muffleWarning")
    })

    # Make output
    if (rounded == TRUE) {
      duration <- round(duration, digits = digits)
    }
    duration <- duration
    if (!is.null(messages)) {
      warning(unique(messages))
    }
    return(duration)
  }

  # Choose between time specific or time for more individuals  ####
  if (!is.null(pcode)) {
    withCallingHandlers({
      return(time_for_specific())
    }, error = function(e) {
      stop(e)}, warning = function(w) {
        warning(w)
        invokeRestart("muffleWarning")
      }
    )
  } else {    # Time for all participants  ####
    singledurations <- data.frame()
    warningparticipants <- c()

    # Make list of all participants for all groups ####
    if (is.null(group_id)) {
      listallparticipants <- c(unique(oTree$Time[[participant_code_name]]))
    } else {
      listallparticipants <- unique(oTree$Time[[participant_code_name]][
        oTree$Time$group_id == group_id])
    }

    # Calculate time for all participants  ####
    for (i in listallparticipants) {
      tryCatch(
        {
          duration <- duration_all()

          if (length(duration) > 1) stop("One participant is there twice")

          session <- get_session(who = i)

          # Make data frame  ####
          if (!is.na(duration)) {
            singledurations <- plyr::rbind.fill(
              singledurations,
              data.frame(
                participant = i,
                session = ifelse(!is.null(sinfo),
                                 session,
                                 NA),
                duration = duration
              )
            )

            if (is.null(sinfo)) {
              singledurations <- singledurations[, c("participant", "duration")]
            }
          }
        }, error = function(e) {
          warningparticipants <<- c(warningparticipants, i)

        }, warning = function(w) {
          warning(paste("Warning: ", w))
        }
      )
      if (indextoohigh == TRUE) stop(indextoohigh_message)
      if (indextoolow == TRUE) stop(indextoolow_message)
    }

    # Make output  ####
    return(output_all())
  }
}
