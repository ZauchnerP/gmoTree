#' Calculate the time that was spent on an app
#' @description
#' Calculate the time spent on one app or several apps.
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}
#' @param apps Character. Name(s) of the app(s) for which the time
#' should be calculated.
#' @param pcode Character. The value of the \code{participant.code}
#' variable if the time should only be calculated for one specified participant.
#' @param plabel Character. The value of the \code{participant.label}
#' variable if the time should only be calculated for one specified participant.
#' @param group_id Integer. The value of the \code{group_id} variable if the
#' time should only be calculated for one specified group. The \code{group_id}
#' variable can be created with \code{\link{make_ids}}.
#' @param seconds Logical. \code{TRUE} if the output should be
#' in seconds instead of minutes.
#' @param rounded Logical. \code{TRUE} if the output should be rounded.
#' @param digits Integer.
#' The number of digits to which the output should be rounded.
#' This parameter has no effect unless \code{rounded = TRUE}.
#' @param sinfo Character. \code{"session_id"} to use session ID
#' for additional information in the data frame
#' of single durations, \code{"session_code"} to use session codes,
#' or \code{NULL} if no
#' session column should be shown.
#' @param combine Logical. \code{TRUE} if all variables relating to epoch time
#' should be merged, and
#' all variables relating to participant code should be merged
#' when data from multiple versions of oTree are used.
#' @returns This function returns a list for each app containing
#' information on the mean, the minimum, and maximum time the participants
#' spent on the app, a data frame with information on the time
#' each participant spent on the app, and eventually,
#' vectors of relevant background information on these numbers.
#'
#' If the experiment's duration is only calculated for one participant,
#' the output returns an \code{NA} (per app) if the person did not make
#' it to the app(s).
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Show how much time all participants spent on app "survey"
#' apptime(oTree, apps = "survey")
#'
#' # Show how much time the participant "a7dppel1" spent on
#' # the app "survey"
#' apptime(oTree, pcode = "a7dppel1", apps = "survey")
#'
#' # Show how much time the participants in group 4 spent on
#' # the app "survey"
#' oTree <- make_ids(oTree,
#'     gmake = TRUE,
#'     from_var = "dictator.1.group.id_in_subsession"
#' )
#' apptime(oTree, group_id = 4, apps = "survey")
#'
#' # Show how much time all participants spent on all apps
#' apptime(oTree)

#' @export
apptime <- function(oTree,
                    apps = NULL,
                    pcode = NULL,
                    plabel = NULL,
                    group_id = NULL,
                    seconds = FALSE,
                    rounded = TRUE,
                    digits = 2,
                    sinfo = "session_code",
                    combine = FALSE) {

  output <- list()
  participant_code_name <- NULL
  message_vector <- c()
  duplicate_participants <- c()
  firststageproblemparticipants <- c()
  warningparticipants <- c()
  onepersonnoapp <- c()
  othertime <- FALSE  # Whether seconds_on_page2 should be used

  # Create list of apps if argument apps is empty  ####
  if (is.null(apps)) {
    apps <- names(oTree)
    apps <- apps[apps != "info"]
    apps <- apps[apps != "all_apps_wide"]
    apps <- apps[apps != "Time"]
    apps <- apps[apps != "Chats"]
  } else {
    # If apps are defined, check if they are there
    if (length(apps[apps %in% names(oTree)]) != length(apps)) {
      if (length(apps[apps %in% names(oTree)]) > 0) {
        warning(
          "The following app(s) is/are not in ",
          "the list of oTree data frames: ",
          paste(
            collapse = ", ",
            apps[!(apps %in% names(oTree))]
          )
        )
      } else {
        stop(
            "The apps specified in the argument apps are not in the ",
            "oTree list of data frames!"
        )
      }
      apps <- apps[apps %in% names(oTree)]
    }
  }

  # Seconds or minutes  ####
  if (seconds == TRUE) {
    divsec <- 1L
  } else {
    divsec <- 60L # Divide seconds by 60 to get minutes
  }

  # Error messages  ####
  if (!("Time" %in% names(oTree))) {
    stop("There is no \"Time\" data frame.")
  }

  if (nrow(oTree$Time) == 0) {
    stop("Your \"Time\" data frame is empty.")
  }

  if (!is.null(pcode) && !is.null(group_id)) {
    stop("Please enter only pcode or group_id")
  }

  if (!is.null(plabel) && !is.null(group_id)) {
    stop("Please enter only plabel or group_id")
  }

  if (!is.null(pcode) && !is.null(plabel)) {
    stop("Please enter only pcode or plabel")
  }

  if (length(pcode) > 1L) {
    stop("Please enter only one participant code!")
  }

  if (length(plabel) > 1L) {
    stop("Please enter only one participant label!")
  }

  if (!is.null(group_id) && is.null(oTree$Time$group_id)) {
    stop(
      "Variable group_id is not in \"Time\" data frame!\n",
      "Run make_ids first!")
  }

  if (!is.null(group_id) &&
      length(oTree$Time$group_id[oTree$Time[["group_id"]] == group_id]) == 0L) {
    stop("group_id is not in \"Time\" data frame!")
  }

  if (is.null(oTree$all_apps_wide) && !is.null(plabel)) {
    stop(
      "You can only use the argument plabel ",
      "if there is an all_apps_wide-data frame in your oTree list"
    )
  }

  # Check if there are too many epoch times and participant code variables
  withCallingHandlers(
    {
      # Call messy_time()
      oTree <- messy_time(oTree, combine, info = TRUE)
    },
    error = function(e) {
      # Stop if there is an error
      stop(e)
    },
    warning = function(w) {
      # Catch warning, but continue with messy_time()
      warning(w)
      invokeRestart("muffleWarning")
    }
  )

  # Set time variable
  if ("epoch_time" %in% colnames(oTree$Time)) {
    timestamp_var_name <- "epoch_time"
  } else if ("epoch_time_completed" %in% colnames(oTree$Time)) {
    timestamp_var_name <- "epoch_time_completed"
  } else if ("time_stamp" %in% colnames(oTree$Time)) {
    timestamp_var_name <- "time_stamp"
  } else if ("seconds_on_page2" %in% colnames(oTree$Time)) {
    othertime <- TRUE
  } else if ("seconds_on_page" %in% colnames(oTree$Time)) {
    othertime <- TRUE
  } else {
    stop(
      "There is no variable referring to the time stamps ",
      "in your Time data frame. ",
      "This should be a variable called either \"epoch time,\" ",
      "\"epoch_time_completed,\" ",
      "\"time stamp,\" \"seconds_on_page,\" or \"seconds_on_page2.\"")
  }

  # Set participant code variable
  if ("participant_code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant_code"
  } else if ("participant__code" %in% colnames(oTree$Time)) {
    participant_code_name <- "participant__code"
  } else {
    stop(
      "No variable referring to the participant code ",
      "in your Time data frame. ",
      "This should be a variable called either \"participant_code,\" or",
      "\"participant__code.\""
    )
  }

  if (!is.null(sinfo) &&
      !(sinfo %in% c("session_code", "session_id"))) {
    stop(
      "Please specify a valid sinfo! Possibilities are ",
      "\"session_code\" or \"session_id\""
    )
  }

  if (!is.null(sinfo) &&
      sinfo == "session_id" &&
      is.null(oTree$Time$session_id)) {
    stop("There is no session_id in the Time data frame")
  }

  if (!is.null(sinfo) &&
      sinfo == "session_code" &&
      is.null(oTree$Time$session_code) &&
      is.null(oTree$Time$session__code)) {
    # Does this possibility even exist?
    stop(
      "There is no session_code or session__code in the Time data frame.\n",
      "This might be because you are using an ",
      "old oTree version that does not ",
      "contain this information. Choose sinfo = NULL to avoid this ",
      "error and omit session information."
    )
  }

  # Check for several session_code information in Time data frame
  if (!is.null(sinfo)) {
    # Check if there are old and new session_code variables
    length_session_code_variables <- sum(
      c(
        "session_code",
        "session__code",
        "participant__session__code"
      ) %in% colnames(oTree$Time)
    )

    if (length_session_code_variables > 1L) {
      # Does this possibility even exist?
      # Are there old oTree versions where this could be relevant?
      stop(
        "More than one variable referred to the session code ",
        "in your Time data frame. This could be because ",
        "you mixed data of different ",
        "versions of oTree in your data frame. ",
        "Before using this function, please integrate ",
        "both variables and ensure ",
        "you only have one of them."
      )
    }
  }

  # First app warning
  errormax1min1 <- paste0(
    "If the first app only has one page, ",
    "the indices for the first and the last page are the same ",
    "- Duration = NA!!",
    " This applies to all participants listed in $first_app_one_page."
  )

  duplicatewarning <- paste0(
    "Some participants have duplicate data and are not ",
    "used in the analyses. ",
    "See $dulicate_participants!"
  )

  # Transform plabel to pcode identifier  ####
  if (!is.null(plabel)) {
    if (length(unique(oTree$all_apps_wide$participant.label)) ==
        length(oTree$all_apps_wide$participant.label)) {
      pcode <- oTree$all_apps_wide$participant.code[
        oTree$all_apps_wide$participant.label == plabel
      ]
    } else {
      stop(
        "You do not have unique participant labels in your ",
        "all_apps_wide data frame! The argument plabel is ",
        "not working in such a case!"
      )
    }
  }

  # Sub functions 1 - indices  ####
  # Make a vector of all indices within an app
  calc_pages_per_app_indices <- function(participant_code_name,
                                         who,
                                         appname) {
    app_indices <- oTree$Time$page_index[
      !is.na(oTree$Time[[participant_code_name]]) &
        oTree$Time[[participant_code_name]] == who &
        !is.na(oTree$Time$app_name) &
        oTree$Time$app_name == appname
    ]
    return(app_indices)
  }

  # Make a vector of all app indices for a person
  calc_all_indices <- function(participant_code_name, who) {
    all_indices <-
      oTree$Time$page_index[oTree$Time[[participant_code_name]] == who]
    all_indices <- all_indices[!is.na(all_indices)]
    return(all_indices)
  }

  # Get the minimum page index of an app (step 1)
  calc_minpageindex1 <- function(app_indices, who) {
    # Minimum index in the App (Measures time at the end of the first page)

    if (!anyNA(app_indices) && length(app_indices)) {
      minpageindex <- min(app_indices)
    } else {
      minpageindex <- NA
    }

    # if (!is.na(minpageindex) && minpageindex == 1) {
    #   Is done in calc_minpageindex2()
    # }
    return(minpageindex)
  }

  # Get the minimum page index of an app (step 2)
  calc_minpageindex2 <- function(all_indices, app_indices,
                                 minpageindex, who) {
    # Adjust min and max page index
    # min page index should jump to the next lower page_index
    # or stay at 1 if it was 1 and used in the old oTree version

    if (minpageindex != 1L) {
      minpageindex <- max(all_indices[all_indices < minpageindex])
    } else if (minpageindex == 1L && min(all_indices) == 0L) {
      minpageindex <- 0L
    }

    maxpageindex <- if (!anyNA(app_indices) && length(app_indices)) {
      max(app_indices)
    } else {
      NA
    }

    # Warning: If there is only one page in the first app
    if (maxpageindex == 1L && minpageindex == 1L) {
      firststageproblemparticipants <<- c(firststageproblemparticipants, who)
      message_vector <<- c(message_vector, errormax1min1)
    }
    output <- list(
      min = minpageindex,
      max = maxpageindex
    )

    return(output)
  }

  # Get time stamp for the minimum page index of an app
  min_max_stamps_dur <- function(participant_code_name, who,
                                 minpageindex, maxpageindex) {
    # Get time stamps and duration
    mintimestamp <- oTree[["Time"]][[timestamp_var_name]][
      !is.na(oTree[["Time"]][[participant_code_name]]) &
        oTree[["Time"]][[participant_code_name]] == who &
        oTree[["Time"]][["page_index"]] == minpageindex
    ]

    maxtimestamp <- oTree[["Time"]][[timestamp_var_name]][
      !is.na(oTree[["Time"]][[participant_code_name]]) &
        oTree[["Time"]][[participant_code_name]] == who &
        oTree[["Time"]][["page_index"]] == maxpageindex
    ]

    duration <- (maxtimestamp - mintimestamp) / divsec

    if (length(mintimestamp) > 1L || length(maxtimestamp) > 1L) {
      duplicate_participants <<- c(
        duplicate_participants,
        who
      )
      message_vector <<- c(duplicatewarning, message_vector)
    }

    return(duration)
  }

  # Calculate duration for a specific individual
  # (is called by specified_time and all_time)
  specified_duration <- function(participant_code_name,
                                 who,
                                 appname,
                                 several_participants = FALSE) {

    app_indices <- calc_pages_per_app_indices(
      participant_code_name = participant_code_name,
      who = who,
      appname = appname)

    all_indices <- calc_all_indices(
      participant_code_name = participant_code_name,
      who = who)

    # Check for duplicate pages   ####
    if (several_participants) {
      # Return NA for this person and continue to the next
      # for one participant of all (all_time)
      if (any(duplicated(all_indices))) {
        duplicate_participants <<- c(duplicate_participants, who)
        message_vector <<- c(duplicatewarning, message_vector)
        # Do not throw a warning here, because the handling of
        # duplicate cases is handled a level above.
        return(NA)
      }
    } # Duplicates for only one person are checked at another point!

    if (!othertime) {
      # Calculate indices with time stamp

      # Minimum index in the App ####
      # (Measures time at the end of the page)
      minpageindex <- calc_minpageindex1(
        app_indices = app_indices,
        who = who)

      # If indices exist calculate time  ####
      if (!is.na(minpageindex)) {
        # Adjust page indices
        newminmax <- calc_minpageindex2(
          all_indices = all_indices,
          app_indices = app_indices,
          minpageindex = minpageindex,
          who = who
        ) # Info: Here, firststageproblemparticipants are calculated

        minpageindex <- newminmax$min
        maxpageindex <- newminmax$max

        # Get time stamps and duration
        if (minpageindex != maxpageindex) {
          duration <- min_max_stamps_dur(
            participant_code_name = participant_code_name,
            who = who,
            minpageindex = minpageindex,
            maxpageindex = maxpageindex
          )
        } else {
          duration <- NA
        }
      } else {
        if (!several_participants) {
          onepersonnoapp <<- c(onepersonnoapp, appname)

        } else if (several_participants) {
          message_vector <<- unique(message_vector)
          message_vector <<-
            c(message_vector,
              paste0(
                "For some participants, no duration could be ",
                "calculated. See list in $warnings."))

          warningparticipants <<- c(warningparticipants, who)
        }
        duration <- NA
      }
    } else if (othertime) {

      if ("seconds_on_page" %in% names(oTree$Time)) {
        secondsonetwo <- "seconds_on_page"
      } else {
        secondsonetwo <- "seconds_on_page2"
      }

      # Calculate indices with seconds_on_page2
      duration <- sum(oTree$Time[[secondsonetwo]][
        !is.na(oTree$Time[[participant_code_name]]) &
          oTree$Time[[participant_code_name]] == who &
          oTree$Time$app_name == appname
      ], na.rm = TRUE)

      duration <- duration / divsec

      if (duration == 0) {
        duration <- NA
      }

      if (is.na(duration)) {
        if (several_participants) {
          message_vector <<- unique(message_vector)
          message_vector <<-
            c(
              message_vector,
              paste0(
                "For some participants, no duration could be ",
                "calculated. See list in $warnings. Did they ",
                "make it to the app(s)?"
              )
            )
          warningparticipants <<- c(warningparticipants, who)
        } else {
          message_vector <<- unique(message_vector)
          message_vector <<- c(message_vector, paste0(
            "Duration could not be calculated for person ",
            who, "in app ", appname,
            "."))
        }
      }
    }

    # Round duration
    if (rounded) {
      duration <- round(duration, digits = digits)
    }

    return(duration)
  }

  # Make sub functions 2 - time  ####

  # Calculate time for a specified individual
  specified_time <- function() {
    # Calls time calculation for (a) specific individual(s)

    # Duration  ####
    if (pcode %in% unique(oTree$Time$participant__code) ||
        pcode %in% unique(oTree$Time$participant_code)) {

      duration <- specified_duration(
        participant_code_name = participant_code_name,
        who = pcode,
        appname = appname
      )
    } else if (!(pcode %in% unique(oTree$Time$participant__code)) &&
               !(pcode %in% unique(oTree$Time$participant_code))
    ) {
      # Participant not there
      # This is not a stop, because if you calculate the time in a loop,
      # this would stop the loop
      duration <- NA
    }

    if (length(duration) > 1L) {
      stop("This person has duplicate data in their Time data frame.")
    }

    # Make output for specified individuals  ####
    if (length(apps) == 1L) {
      output <- duration
    } else {
      output[[appname]] <- duration
      # Do not return yet because the other apps must be added too!
    }

    return(output)
  }

  # Function for all individuals for specified app names
  # appname is set before this function is called
  all_time <- function() {

    # Create variables for all participants (all_time)
    singledurations <- data.frame()
    # Reset the vectors because they are shown for each app individually
    firststageproblemparticipants <<- c() # Calculated in specified_duration()
    warningparticipants <<- c()
    message_vector <<- c()

    # Make list of all participants for all participants (all_time)
    if (is.null(group_id)) {
      # For all groups
      listallparticipants <- unique(oTree$Time[[participant_code_name]])
    } else {
      # Only for special groups
      listallparticipants <- unique(oTree$Time[[participant_code_name]][
        oTree$Time$group_id == group_id
      ])
    }

    # Calculate time for all participants (all_time)  ####
    for (i in listallparticipants) {
      # Calculate duration
      duration <- specified_duration(participant_code_name,
                                     who = i,
                                     appname = appname,
                                     several_participants = TRUE) # Whether specified_duration is separately run for several people

      # Add to data frame  ####

      if (!is.null(duration) && !is.na(duration)) {
        session <- get_session(who = i)

        singledurations <- plyr::rbind.fill(
          singledurations,
          data.frame(
            participant = i,
            session = ifelse(!is.null(sinfo), session, NA),
            duration = duration))

        if (is.null(sinfo)) {
          singledurations <- singledurations[, c(
            "participant",
            "duration")]
        }
      }
    }

    # Single durations data frame is empty - dealing with the reasons  ####
    if (nrow(singledurations) == 0L) {

      if (!is.null(duplicate_participants) &&
          length(duplicate_participants) > 1) {

        # Duplicate data

        output[[appname]]$messages <-
          paste0(
            "Durations not calculated. ",
            "There are duplicate data in your ",
            "Time data frame.")

        return(output)

      } else {

        output[[appname]]$first_app_one_page <- firststageproblemparticipants
        message_vector <<- unique(message_vector)

        if (length(listallparticipants) ==
            length(firststageproblemparticipants)) {

          # If all are first stage participants

          output[[appname]]$message <- paste0(
            "Durations not calculated. ", message_vector)
        } else {

          # Several reasons

          output[[appname]]$message <- paste0(
            "Durations not calculated. ",
            "Check your data before rerunning ",
            "the function. Plus: ", message_vector)
        }

        # Return from all_time()
        return(output)
      }
    }

    # Make output for all participants  ####
    return(call_output_all_participants(
      singledurations,
      message_vector,
      firststageproblemparticipants,
      warningparticipants
    ))
  }


  # Make sub functions 3 - output  ####

  # Make output for a specific app if there is only 1 app in the final output
  # Get min, max, mean, and single durations
  output_oneapp <- function(singledurations,
                            message_vector,
                            firststageproblemparticipants,
                            warningparticipants) {
    output[["mean_duration"]] <-
      ifelse(rounded,
             round(
               mean(singledurations$duration,
                    na.rm = TRUE
               ),
               digits = digits
             ),
             mean(singledurations$duration,
                  na.rm = TRUE
             )
      )

    output[["min_duration"]] <-
      ifelse(rounded,
             round(
               min(singledurations$duration,
                   na.rm = TRUE
               ),
               digits = digits
             ),
             min(singledurations$duration,
                 na.rm = TRUE
             )
      )

    output[["max_duration"]] <-
      ifelse(rounded,
             round(
               max(singledurations$duration,
                   na.rm = TRUE
               ),
               digits = digits
             ),
             max(singledurations$duration,
                 na.rm = TRUE
             )
      )

    output[["single_durations"]] <- singledurations

    if (rounded) {
      output[["single_durations"]]$duration <-
        round(output[["single_durations"]]$duration, digits = digits)
    }

    output[["messages"]] <- unique(message_vector)
    output[["first_app_one_page"]] <- firststageproblemparticipants

    if (length(warningparticipants > 0L)) {
      output[["warnings"]] <- unique(warningparticipants)
    }

    if (length(duplicate_participants) > 0L) {
      output[["duplicate_participants"]] <- unique(duplicate_participants)
    }

    return(output)
  }

  # Make output for a specific app if there are more apps in the final output
  # Get min, max, mean, and single durations
  output_moreapps <- function(singledurations,
                              message_vector,
                              firststageproblemparticipants,
                              warningparticipants) {
    if (nrow(singledurations) > 0L) {
      output[[appname]][["mean_duration"]] <-
        ifelse(rounded,
               round(
                 mean(singledurations$duration,
                      na.rm = TRUE), digits = digits),
               mean(singledurations$duration,
                    na.rm = TRUE))

      output[[appname]][["min_duration"]] <-
        ifelse(rounded,
               round(
                 min(singledurations$duration,
                     na.rm = TRUE
                 ),
                 digits = digits
               ),
               min(singledurations$duration,
                   na.rm = TRUE
               )
        )

      output[[appname]][["max_duration"]] <-
        ifelse(rounded,
               round(
                 max(singledurations$duration,
                     na.rm = TRUE),
                 digits = digits),
               max(singledurations$duration,
                   na.rm = TRUE
               ))

      output[[appname]]$single_durations <-
        singledurations[order(singledurations$duration), ]

      if (rounded) {
        output[[appname]]$single_durations$duration <-
          round(output[[appname]]$single_durations$duration, digits = digits)
      }

      output[[appname]]$messages <- unique(message_vector)

      output[[appname]]$first_app_one_page <- firststageproblemparticipants

      if (length(warningparticipants) > 0L) {
        output[[appname]]$warnings <- unique(warningparticipants)
      }
    } # Else:
    # If single durations are not there
    # This was already dealt with at another level

    return(output)
  }

  # Call output_oneapp or output_moreapps
  call_output_all_participants <- function(singledurations, message_vector,
                                           firststageproblemparticipants,
                                           warningparticipants) {
    # Output for all participants or several
    if (length(apps) == 1L) {
      return(output_oneapp(
        singledurations,
        message_vector,
        firststageproblemparticipants,
        warningparticipants
      ))
    } else {
      message_vector <<- unique(message_vector)

      return(output_moreapps(
        singledurations,
        message_vector,
        firststageproblemparticipants,
        warningparticipants
      ))
    }
  }

  get_session <- function(who) {
    if (is.null(sinfo)) {
      session <- NA
    } else {
      if (sinfo == "session_id") {
        session <-
          unique(oTree$Time$session_id[
            oTree$Time[[participant_code_name]] == who
          ])
      } else if (sinfo == "session_code") {
        if (!is.null(oTree$Time$session_code)) {
          session <-
            unique(oTree$Time$session_code[
              oTree$Time[[participant_code_name]] == who
            ])
        } else if (!is.null(oTree$Time$session__code)) {
          # Does this even exist?
          # I don't have session__code in my current data
          session <-
            unique(oTree$Time$session__code[
              oTree$Time[[participant_code_name]] == who
            ])
        }
      }
    }
    return(session)
  }

  # Call functions  ####
  for (appname in apps) {
    if (!is.null(pcode)) {
      # Time for app for specified individuals  ####
      output <- specified_time()
    } else {
      # Time for app for all participants  ####
      output <- all_time()

      if (length(output) == 1 &&
          grepl("Durations not calculated", output)) {
        next
      }
    }
  }


  if (!is.null(pcode)) {
    if (length(onepersonnoapp) > 0) {
      warning("Duration could not be calculated for the person in app(s): ",
              paste(onepersonnoapp, collapse = ", "), ".")
    }
  }


  # Return  ####
  return(output)
}
