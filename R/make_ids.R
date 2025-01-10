#' Make IDs
#' @description
#' Make session IDs and, optionally, group IDs and participant IDs
#' that span across all data frames created by \code{\link{import_otree}}.
#' Information for these IDs is taken from \code{$all_apps_wide}
#' but can be defined otherwise.
#'
#' Note: Older versions of oTree may already contain a
#' variable called session_id in their \code{$Time} data frames.
#' This variable is overwritten by this function!
#'
#' Important: Combine duplicate data before running this function!
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @param gmake Logical. \code{TRUE} if a variable
#' called group_id should be made.
#' If from_var is not \code{NULL}, gmake is automatically set to \code{TRUE}.
#' @param pmake Logical. \code{TRUE} if a variable called \code{participant_id}
#' should be made.
#' @param from_app Character. Name of the data frame from which the session,
#' group, and participant information should be taken.
#' All normal app data frames and \code{$all_apps_wide} are allowed.
#' @param from_var Character. Name of the variable from which the group
#' information should be taken. This argument is only relevant
#' when \code{$all_apps_wide} is used as from_app and has group information
#' that contradicts each other.
#' @param sstart Integer.
#' The number that serves as a starting point for session IDs.
#' @param gstart Integer.
#' The number that serves as a starting point for group IDs.
#' @param pstart Integer.
#' The number that serves as a starting point for participant IDs.
#' @param emptyrows Character. \code{"no"} if the function should stop if
#' there are empty rows in from_app. \code{"yes"} if the function should
#' continue to make IDs.
#' @param icw Logical. \code{TRUE} if the warning message should be
#' ignored that states that IDs cannot be made because of an oTree bug.
#' @returns ID variables are made in \code{$all_apps_wide}, all app data frames,
#' the \code{$Time} data frame, and the \code{$Chats} data frame.
#' See list of the additional ID variables in \code{$info$additional_variables}.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Make session IDs only
#' oTree2 <- make_ids(oTree)
#'
#' # Show new variables
#' oTree2$all_apps_wide$session_id
#'
#' # Make session IDs and group IDs
#' # Not working with this data set because group ID is not the same in all apps
#' \dontrun{
#'       oTree2 <- make_ids(oTree, gmake = TRUE)
#'
#'       # Show new variables
#'       oTree2$all_apps_wide$session_id
#'       oTree2$all_apps_wide$group_id
#' }
#'
#' # Get IDs from variable "dictator.1.group.id_in_subsession"
#' # in the data frame "all_apps_wide"
#' oTree2 <- make_ids(oTree,
#'                    gmake = TRUE,
#'                    from_var = "dictator.1.group.id_in_subsession")
#'
#' # Show new variables
#' oTree2$all_apps_wide$session_id
#' oTree2$all_apps_wide$group_id
#'
#' # Get IDs from another app than all_apps_wide
#' oTree2 <- make_ids(oTree, gmake = TRUE, from_app = "dictator")
#'
#' # Show new variables
#' oTree2$all_apps_wide$session_id
#' oTree2$all_apps_wide$group_id

#' @export
make_ids <- function(oTree,
                     gmake = FALSE,
                     pmake = TRUE,
                     from_app = "all_apps_wide",
                     from_var = NULL,
                     sstart = 1L,
                     gstart = 1L,
                     pstart = 1L,
                     emptyrows = NULL,
                     icw = FALSE) {

  env <- new.env(parent = emptyenv())
  env$time_messed <- FALSE
  env$chat_messed <- FALSE
  env$messed_message <- character(0L)
  env$my_warnings <- character(0L)

  # Before start: Error messages  ####
  if (from_app %in% c("info", "Chats", "Time") && 
      !startsWith(prefix = "custexp_", x = from_app)) {
    stop(
      "You can not use \"Chats,\" or \"Time,\" or \"info\" or ",
      "custom exports as ",
      "\"from_app\"!")
  }

  if (is.null(oTree[[from_app]])) {
    stop("from_app \"",
                from_app,
                "\" not found. Please select another from_app.")
  }

  if (!(is.data.frame(oTree[[from_app]]))) {
    stop("Your from_app is not a data frame!")
  }

  if (!(any(grepl("subsession.round_number", colnames(oTree[[from_app]]))))) {
    stop("Your from_app is not a normal oTree all_apps_wide or apps data ",
    "frame. Choose another from_app!")
  }

  if (!is.null(from_var)) {

    if (from_app != "all_apps_wide") {
      stop("Please only use from_app (all except all_apps_wide) ",
      "or from_var!")

    } else if (from_app == "all_apps_wide" && is.null(oTree[[from_app]][[from_var]])) {
        stop("from_var \"",
              from_var,
              "\" not found. ",
              "Please select another one.")
      
    }

    # gmake should be automatically TRUE if from_var is set
    gmake <- TRUE
  }

  if (!inherits(oTree[[from_app]], "data.frame")) {
    stop("from_app \"", from_app, "\" is not a data frame.")
  }

  if (inherits(oTree[[from_app]], "data.frame") &&
      nrow(oTree[[from_app]]) == 0L) {
    stop("from_app \"", from_app, "\" data frame has no entries.")
  }

  # Check mixed Time data
  tryCatch({
    messy_time(oTree, combine = FALSE)
  }, error = function(e) {
    env$time_messed <- TRUE
    env$messed_message <- paste0("Please run messy_time() with the argument ",
                              "combine=TRUE before running this function.")
  })

  # Check mixed Chat data
  tryCatch({
    messy_chat(oTree, combine = FALSE)
  }, error = function(e) {
    env$chat_messed <- TRUE

    if (env$time_messed) {

      # Combine messy chat message with messy time message
      env$messed_message <<-
        paste0(env$messed_message,
               " AND: Run messy_chat() with the argument ",
               "combine=TRUE before running this function!")
    } else {

      # Make messy chat message
      env$messed_message <<-
        paste0("Run messy_chat() with the argument ",
               "combine=TRUE before running this function!")
    }
  })

  # Stop if messy time and/or chat variables should not be merged
  if (env$time_messed || env$chat_messed) {
    stop("You combined data from old and new oTree versions. ",
         env$messed_message)
  }

  # Check for NAs in the relevant variables
  if (anyNA(oTree[[from_app]]$participant.code)) {
    stop("There are NAs in your participant.code variable in your ",
                "from_app! Clean your data or assign ",
                "participant.code values by hand ",
                "before running this function!")
  }

  if (anyNA(oTree$Chats$participant_code)) {
    stop("There are NAs in your participant_code variable in ",
         "the oTree$Chats data frame.")
  }

  if (anyNA(oTree$Chats$participant__code)) {
    stop("There are NAs in your participant__code variable in ",
         "the oTree$Chats data frame.")
  }

  if (from_app == "all_apps_wide") {

    if (length(oTree[[from_app]]$participant.code) !=
       length(unique(oTree[[from_app]]$participant.code))) {

      stop(
        from_app,
        ": The length of participant codes is not equal the length of ",
        "unique participant codes. Please check your data for ",
        "duplicates or empty rows! ",
        "(Advice: You may use delete_duplicate() to ",
        "remove duplicate rows of all oTree data frames.")
    }
  } else {
    if (length(unique(oTree[[from_app]]$participant.code)) !=
       length(oTree[[from_app]]$participant.code) /
       max(oTree[[from_app]]$subsession.round_number)) {

      stop(
        from_app,
        ": The length of participant codes is not equal the length of ",
        "unique participant codes. Please check your data for ",
        "duplicates or empty rows! ",
        "(Advice: You may use delete_duplicate() to ",
        "remove duplicate rows of all oTree data frames.")
    }
  }

  # Caution. Participant-code in chat in oTree version 5 is faulty (all 1) ####
  messymessage <- paste0(
    "Caution. All participant_codes in at least one ",
    "session of the chat are the same. ",
    "This might be because the participant code in the chat is faulty ",
    "because of an oTree bug; however, there could also be another ",
    "problem with your data. Maybe really only one person ",
    "within a session submitted text to the chat. ",
    "In any way: Check your Chat data and calculate IDs by hand or ",
    "run the function with icw = TRUE! ",
    "(For example, if really only one person submitted text to the chat.)")

  for (code in c(unique(oTree$Chats$session_code),
                 unique(oTree$Chats$session__code),
                 unique(oTree$Chats$participant__session__code)
                 )) {

    if (length(unique(oTree$Chats$participant_code[
      oTree$Chats$session_code == code])) == 1L) {

        if (!icw) {
          stop(messymessage)
        }

    } else if (length(unique(
      oTree$Chats$participant__code[
        oTree$Chats$session__code == code])) == 1L) {

      if (!icw) {
        stop(messymessage)  # Can this even happen?
      }

    } else if (length(unique(
      oTree$Chats$participant__code[
        oTree$Chats$participant__session__code == code])) == 1L) {

      if (!icw) {
        stop(messymessage)  # Can this even happen?
      }
    }
  }

  # Check if group numbers are the same in all variables
  # if app and round is not specified
  if (gmake &&
      from_app == "all_apps_wide" &&
      is.null(from_var)) {

    checkdata <- oTree[[from_app]][, endsWith(names(oTree[[from_app]]),
                                              "group.id_in_subsession")]

    if (ncol(checkdata) == 0L) {
      stop("No variable that ends with \"group.id_in_subsession\"")
    }

    if (inherits(checkdata, "data.frame") &&
        !(all(checkdata == checkdata[, 1L]))) {
        # Not all the same
        stop(
          "group_id can not be calculated. ",
          "You don't have the same group.id_in_subsession in every app. ",
          "Therefore, there are several group IDs in all_apps_wide. ",
          "You must either enter the exact variable that defines the ",
          "group numbers in all apps wide (from_var) or the app from ",
          "which the group numbers should be taken (from_app)."
        )

    }
  }

  # Record original from_app order ####
  oTree[[from_app]]$initial_order <- seq_len(nrow(oTree[[from_app]]))

  # Sub functions  ####
  ids_fromvariable <- function(oTree) {

    # Get group IDs from a certain variable

    if (!is.null(oTree[[from_app]][[from_var]])) {

        oTree[[from_app]]$GroupSessionID <- paste(
          as.character(oTree[[from_app]]$session_id),
          as.character(oTree[[from_app]][[from_var]]))

        oTree[[from_app]] <- dplyr::arrange(
          oTree[[from_app]],
          oTree[[from_app]]$session_id,
          oTree[[from_app]]$GroupSessionID)

        oTree[[from_app]]$group_id <-
          data.table::rleidv(oTree[[from_app]]$GroupSessionID)

        oTree[[from_app]]$group_id <-
          oTree[[from_app]]$group_id + (gstart - 1L)

        if (length(unique(oTree[[from_app]][[from_var]])) == 1L) {
          env$my_warnings <<- c(env$my_warnings, paste0(
            "The group variable values are constant. ",
            "Group IDs now correspond to session IDs."))
        }

        return(oTree)
    } # Else statement is already checked for at the start of the function
  }

  ids_notfromvariable <- function(oTree) {

    # 1) Assign group number in subsession

    # Check if there are more than one columns with a group_id in it
    # Info: It was already tested before if those variables exist
    all_group_ids <- oTree[[from_app]][, grep("group.id_in_subsession",
                                              colnames(oTree[[from_app]]))]

    if (inherits(all_group_ids, "data.frame") &&
        ncol(all_group_ids) > 0L) {

      # Make a helping variable GroupSessionID
      oTree <- group_session_id_df(oTree, env = env)

    } else {
      # Make a helping variable GroupSessionID
      oTree <- group_session_id_vector(oTree, env = env)
    }

    # 2) Arrange group numbers, too (I took the first occurrence)
    oTree[[from_app]] <-
      dplyr::arrange(oTree[[from_app]],
                     oTree[[from_app]]$session_id,
                     oTree[[from_app]]$GroupSessionID)

    # 3) Assign session wide group number
    oTree[[from_app]]$group_id <-
      data.table::rleidv(oTree[[from_app]]$GroupSessionID) + (gstart - 1L)

    return(oTree)
  }

  group_session_id_df <- function(oTree, env) {
    # Here several variables are called group.id_in_subsession.
    # Take the first one.

    # Add session ID so there are no group IDs twice
    oTree[[from_app]]$GroupSessionID <-
      paste(
       oTree[[from_app]]$session_id,
       oTree[[from_app]][, grep("group.id_in_subsession",
                                colnames(oTree[[from_app]]))][, 1L])

    if (length(
      unique(oTree[[from_app]][,
        grep("group.id_in_subsession",
             colnames(oTree[[from_app]]))][, 1L])) == 1L) {

      env$my_warnings <<- c(env$my_warnings, paste0(
        "The group variable values (of the first group variable) ",
        "are constant. ",
        "Group IDs now correspond to session IDs."))
    }
    return(oTree)
  }

  group_session_id_vector <- function(oTree, env) {
    # Here only one variable is called group.id_in_subsession

    # Add session ID so there are no group IDs twice
    oTree[[from_app]]$GroupSessionID <- paste(
      oTree[[from_app]]$session_id,
      oTree[[from_app]][,
                        grep("group.id_in_subsession",
                             colnames(oTree[[from_app]]))])

    if (length(
      unique(oTree[[from_app]][, grep("group.id_in_subsession",
                                      colnames(oTree[[from_app]]))])) == 1L) {

      env$my_warnings <<- c(env$my_warnings, paste0(
        "The group variable values are constant. ",
        "Group IDs now correspond to session IDs."))
    }
    return(oTree)
  }

  # Assign session, group, and participant IDs to a specific data frame
  ids_in_all_normal_apps <- function(oTree, df_group_in_date, i) {
    for (participant in unique(df_group_in_date$participant.code)) {

      # Session ID
      oTree[[i]]$session_id[oTree[[i]]$participant.code == participant] <-
        df_group_in_date$session_id[
          df_group_in_date$participant.code == participant][1L]

      # Group ID
      if (group_size_info) {
        oTree[[i]]$group_id[oTree[[i]]$participant.code == participant] <-
          df_group_in_date$group_id[
            df_group_in_date$participant.code == participant][1L]
      }

      # Participant ID
      if (pmake) {
        oTree[[i]]$participant_id[oTree[[i]]$participant.code == participant] <-
          df_group_in_date$participant_id[
            df_group_in_date$participant.code == participant][1L]
      }
    }
    return(oTree)
  }

  # Assign participant, group and session ID values to the Time/Chats data frame
  # For variables with only one underscore
  ids_in_new_time_apps <- function(oTree, df_group_in_date, i) {
    for (participant in unique(df_group_in_date$participant.code)) {

      # Session ID
      oTree[[i]]$session_id[oTree[[i]]$participant_code == participant] <-
        df_group_in_date$session_id[
          df_group_in_date$participant.code == participant][1L]

      # Group ID
      if (group_size_info) {
        oTree[[i]]$group_id[oTree[[i]]$participant_code == participant] <-
          df_group_in_date$group_id[
            df_group_in_date$participant.code == participant][1L]
      }

      # Participant ID
      if (pmake) {
        oTree[[i]]$participant_id[oTree[[i]]$participant_code == participant] <-
          df_group_in_date$participant_id[
            df_group_in_date$participant.code == participant][1L]
      }
    }
    return(oTree)
  }

  # Assign participant, group and session ID values to the Time/Chats data frame
  # For variables with two underscores
  ids_in_old_time_apps <- function(oTree, df_group_in_date, i) {
    for (participant in unique(df_group_in_date$participant.code)) {

      # Session ID
      oTree[[i]]$session_id[oTree[[i]]$participant__code == participant] <-
        df_group_in_date$session_id[
          df_group_in_date$participant.code == participant][1L]

      # Group ID
      if (group_size_info) {
        oTree[[i]]$group_id[oTree[[i]]$participant__code == participant] <-
          df_group_in_date$group_id[
            df_group_in_date$participant.code == participant][1L]
      }

      # Participant ID
      if (pmake) {
        oTree[[i]]$participant_id[
          oTree[[i]]$participant__code == participant] <-
          df_group_in_date$participant_id[
            df_group_in_date$participant.code == participant][1L]
      }
    }
    return(oTree)
  }

  emptyrows_dealing <- function() {
    # Ask user whether they want to stop function
    while (is.null(emptyrows) ||
           (emptyrows != "yes" && emptyrows != "no")) {
      cat(
        "Your from_app contains empty rows. This might lead to faulty ",
        "IDs. It is strongly advised to import data ",
        "using the del_empty = TRUE ",
        "argument of import_oTree().",
        "Do you still want to continue with the creation of IDs?\n",
        "Enter \"no\" to stop the function.\n",
        "If you want to continue, please enter \"yes\".\n",
        "Input: \n")

      emptyrows <- readLines(
                             con = getOption("mypkg.connection"),
                             n = 1L)
    }
    return(emptyrows)
  }

  # First in all_apps_wide  ####

  # Step 1: Make session number  ####

  # It's a bit more complicated
  # because sometimes sessions start at the same time.

  # Get the entrance of the first participant to the session
  startedlist <- stats::aggregate(
    x = oTree[[from_app]]$participant.time_started,
    by = list(session.code = oTree[[from_app]]$session.code),
    FUN = min
  ) # Info: NAs are not in the list

  # Deal with empty rows
  if (any("" %in% oTree[[from_app]]$participant._current_app_name)) {

    emptyrows <- emptyrows_dealing()
    if (emptyrows == "no") {
      stop(
        "Your from_app contains empty rows. This might lead to faulty ",
        "IDs. You chose to stop this function. You can either import data ",
        "using the del_empty = TRUE or rerun this function with the argument ",
        "emptyrows=\"yes\" (not advised).")
    }
  }

  # Add the entrance of first participant in session as a variable
  for (i in unique(oTree[[from_app]]$session.code)) {

    if (i %in% startedlist$session.code) {

      oTree[[from_app]]$participant.time_started_min[
        oTree[[from_app]]$session.code == i] <- startedlist[
          startedlist$session.code == i, ]$x
    } else {
      # This part is usually called if session.code is NA
      # This does not happen with cleaned data
      if (anyNA(oTree[[from_app]]$session.code)) {
        env$my_warnings <-
          c(env$my_warnings,
            (paste0("At least one of your session.codes in your from_app is ",
                "NA. All session codes that are Na are ",
                "handled as being the same ",
                "session. This might also result in faulty group_ids!! ",
                "If this is not your intention, please manually assign ",
                "session codes to avoid this issue.")))
      }
      oTree[[from_app]]$participant.time_started_min[
        oTree[[from_app]]$session.code == i] <- NA
    }
  }

  # Sort data frame by session starting time
  oTree[[from_app]] <- oTree[[from_app]][
    base::order(oTree[[from_app]]$participant.time_started_min,
                decreasing = FALSE), ]

  # Make session_id
  oTree[[from_app]]$session_id <-
    data.table::rleid(oTree[[from_app]]$session.code) + (sstart - 1L)

  # Delete variable again
  oTree[[from_app]]$participant.time_started_min <- NULL

  # Step 2: Make group_id  ####

  # Calculate
  if (gmake) {

    if (from_app == "all_apps_wide" && !is.null(from_var)) {

      group_size_info <- TRUE

      oTree <- ids_fromvariable(oTree)

    } else if (is.null(from_var)) {

      group_size_info <- TRUE

      oTree <- ids_notfromvariable(oTree)

    }
  } else {
    group_size_info <- FALSE
  }

  # Step 3: Make participant ID  ####
  # Up to now the data frame should be arranged according to
  # session_id and GroupSessionID, hence
  if (pmake) {

    if ("participant.code" %in% names(oTree[[from_app]])) {

      # Sort data frame
      if ("group_id" %in% colnames(oTree[[from_app]])) {
        oTree[[from_app]] <- oTree[[from_app]][order(
          oTree[[from_app]]$session_id,
          oTree[[from_app]]$group_id,
          oTree[[from_app]]$participant.code), ]
      } else {
        oTree[[from_app]] <- oTree[[from_app]][order(
          oTree[[from_app]]$session_id,
          oTree[[from_app]]$participant.code), ]
      }

      # Make participant_id
      oTree[[from_app]]$participant_id <-
        data.table::rleidv(oTree[[from_app]]$participant.code) +
        (pstart - 1L)

    } else {
      stop("There is no participant.code in ",
           from_app,
            "!") # This should never happen!
    }
  }

  # Step 4: Make info lists  ####
  listincluded <- c("session_id", "session.code", "participant.code")
  oTree[["info"]][["additional_variables"]] <- "session_id"

  if (group_size_info) {

    listincluded <- append(listincluded, "group_id")

    oTree[["info"]][["additional_variables"]] <-
      append(oTree[["info"]][["additional_variables"]],
             "group_id")
  }

  if (pmake) {

    listincluded <- append(listincluded, "participant_id")

    oTree[["info"]][["additional_variables"]] <-
      append(oTree[["info"]][["additional_variables"]],
             "participant_id")
  }

  # Step 5: Make list of groups  ####
  df_group_in_date <- oTree[[from_app]][, listincluded]
  df_group_in_date <- df_group_in_date[!duplicated(df_group_in_date), ]

  # Step 6: Apply info list of groups to all apps ####
  for (i in seq_along(oTree)) {

    # Get the name of the app/Time/Chats data frame
    name_of_app <- rlist::list.names(oTree[i])

    # Assign values
    if (name_of_app != "info" && 
        !startsWith(prefix = "custexp_", x = name_of_app)) {

      if ("participant.code" %in% names(oTree[[i]])) {  # For all normal DFs

        # Get a warning, if the participant is not in the from_app
        participants_more <- unique(oTree[[i]]$participant.code[
          !(oTree[[i]]$participant.code %in%
              oTree[[from_app]]$participant.code)])

        if (length(participants_more) > 0L) {
          env$my_warnings <- c(env$my_warnings,
                           paste0("Data frame \"",
                                  names(oTree)[[i]],
                                  "\" has more participants than ",
                                  from_app, ": ",
                                  paste(participants_more,
                                        collapse = ", ")))
        }

        # Assign participant, group and session ID values to the data frame
        oTree <- ids_in_all_normal_apps(oTree, df_group_in_date, i)

      } else if ("participant_code" %in% names(oTree[[i]])) { # For Chats/Time

          # Get a warning, if the participant is not in the from_app
          participants_more <- unique(oTree[[i]]$participant_code[
            !(oTree[[i]]$participant_code %in%
                oTree[[from_app]]$participant.code)])

          if (length(participants_more) > 0L) {
            env$my_warnings <- c(env$my_warnings,
                             paste0("Data frame \"",
                                    names(oTree)[[i]],
                                    "\" has more participants than ",
                                    from_app, ": ",
                                    paste(participants_more,
                                          collapse = ", ")))
          }

          # Assign participant, group and session ID values to the data frame
          oTree <- ids_in_new_time_apps(oTree, df_group_in_date, i)
      } else if ("participant__code" %in% names(oTree[[i]])) {

          # Get a warning, if the participant is not in the from_app
          participants_more <- oTree[[i]]$participant__code[
            !(oTree[[i]]$participant__code %in%
                oTree[[from_app]]$participant.code)]

          participants_more <- unique(participants_more)

          if (length(participants_more) > 0L) {
            env$my_warnings <- c(env$my_warnings,
                             paste0("Data frame \"", names(oTree)[[i]],
                                    "\" has more participants than ",
                                    from_app, ": ",
                                    paste(participants_more, collapse = ", ")))
          }

          # Assign participant, group and session ID values to the data frame
          oTree <- ids_in_old_time_apps(oTree, df_group_in_date, i)

      } else {
        env$my_warnings <-
          c(env$my_warnings,
            paste0("Participant code variable couldn't be found in \"",
                   name_of_app,
                   "\"! No IDs are calculated for this data frame."))
      }

      # Reorder columns  ####
      j <- 0L
      if (pmake) {
        j <- j + 1L
      }
      if (group_size_info) {
        j <- j + 1L
      }
      oTree[[i]] <-
        oTree[[i]][, c(c((ncol(oTree[[i]]) - j):ncol(oTree[[i]])),
                        c(1L:(ncol(oTree[[i]]) - j - 1L)))
      ] # Again -1 because otherwise a number is there twice

      oTree[[i]]$GroupSessionID <- NULL
    }
  }

  # Restore initial order of from_app  ####
  oTree[[from_app]] <-
    oTree[[from_app]][order(oTree[[from_app]]$initial_order), ]

  oTree[[from_app]]$initial_order <- NULL

  # Print warnings  ####
  if (length(env$my_warnings) > 0L) {
    warning(paste(env$my_warnings, collapse = "\n"))
  }

  return(oTree)
}
