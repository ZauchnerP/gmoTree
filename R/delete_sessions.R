#' Delete all cases of one session
#' @description
#' Delete cases from specific sessions in all data frames within the
#' list of data frames.
#'
#' Caution 1: This function does not delete cases from the
#' original CSV and Excel files!
#'
#' Caution 2: This function does not delete cases from custom exports if the
#' custom exports do not have a variable named \code{participant.code}
#' and a variable named \code{session.code}!
#' @keywords oTree
#' @inheritParams apptime
#' @param scodes Character string or character vector.
#' The session.code(s) of the
#' session(s) whose data should be removed.
#' @param saved_vars Character string or character vector.
#' The name(s) of variable(s) that need(s) to be
#' stored in the list of information on deleted cases
#' in \code{$info$deleted_cases}.
#' @param reason Character string. The reason for deletion that should be stored
#' in the list of information on deleted cases in \code{$info$deleted_cases}.
#' @param info Logical. \code{TRUE} if a brief information on the session
#' deletion process should be printed.
#' @returns This function returns a duplicate of the original oTree list of
#' data frames that do not include the deleted sessions.
#'
#' It adds information on the deleted cases to \code{$info$deleted_cases}.
#' (This list is also filled by other functions.)
#'
#' In this list, you can find the following information:
#'
#' - \code{$full} and \code{$unique} = The data frames \code{$full}
#' and \code{$unique} contain
#' information on all participants
#' whose data were deleted. The entries to the \code{$full}
#' and the \code{$unique} data
#' frames in this list are the same. Columns \code{end_app}
#' and \code{end_page} are left
#' empty intentionally because they are only filled by
#' the \code{\link[=delete_dropouts]{delete_dropouts()}}
#' function. Columns \code{participant.code} and \code{reason} are filled.
#'
#' - \code{$codes} = A vector containing the participant codes of
#' all deleted participants.
#'
#' - \code{$count} = The number of all deleted participants.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Delete one session
#' oTree2 <- delete_sessions(oTree,
#'   scodes = "7bfqtokx",
#'   reason = "Only tests")
#'
#' # Show changes in row numbers
#' print(paste("Row numbers before deletion: ",
#'   nrow(oTree$all_apps_wide), nrow(oTree$survey),
#'   nrow(oTree$Time), nrow(oTree$Chats)))
#'
#' print(paste("Row numbers after deletion: ",
#'   nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#'   nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Delete two sessions and show deletion message
#' oTree2 <- delete_sessions(oTree,
#'   scodes = c("7bfqtokx", "vd1h01iv"),
#'   reason = "Only tests",
#'   info = TRUE)
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#'   nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Delete session and save variable to the info list
#' oTree2 <- delete_sessions(oTree,
#'   scodes = c("7bfqtokx", "vd1h01iv"),
#'   reason = "Server Crash",
#'   saved_vars = "dictator.1.group.id_in_subsession")
#'
#' # Check the "full" deletion information
#' oTree2$info$deleted_cases$full
#'
#' # See codes of deleted variables
#' oTree2$info$deleted_cases$codes
#'
#' # See number of deleted variables
#' oTree2$info$deleted_cases$count
#'
#' # Delete a single case and then delete a session
#' oTree2 <- delete_cases(oTree,
#'   pcodes = "4zhzdmzo",
#'   reason = "requested")
#' oTree2 <- delete_sessions(oTree2,
#'   scodes = c("vd1h01iv"),
#'   reason = "Server Crash",
#'   saved_vars = "dictator.1.group.id_in_subsession")
#'
#' # Check the "full" deletion information
#' oTree2$info$deleted_cases$full
#'
#' # See codes of deleted variables
#' oTree2$info$deleted_cases$codes
#'
#' # See number of deleted variables
#' oTree2$info$deleted_cases$count

#' @export
delete_sessions <- function(oTree,
                            scodes,
                            saved_vars = NULL,
                            reason,
                            info = FALSE) {

  messages <- character(0L)
  deleted_participants <- character(0L)
  deletion_frame <- data.frame()


  env <- new.env(parent = emptyenv())
  env$messed_message <- character(0L)
  env$time_messed <- FALSE
  env$chat_messed <- FALSE


  # Create list of apps  ####
  appnames <- names(oTree)
  appnames <- appnames[appnames != "info"]

  # Check if oTree is a list of data frames
  if (!is.list(oTree) ||
      !(length(oTree) > 1L)) {
    stop("Your oTree is not a list of oTree data frames.")
  }

  for (app in appnames) {
    if (!(is.data.frame(oTree[[app]]))) {
      stop("Your oTree is not a list of oTree data frames.")
    }
  }

  # Check mixed Time data
  tryCatch({
    messy_time(oTree, combine = FALSE)
  }, error = function(e) {
    env$time_messed <- TRUE
    env$messed_message <- paste0("Please run messy_time() with the argument ",
                      "combine=TRUE before running this function.")
  })

  # Check mixed Chats data
  tryCatch({
    messy_chat(oTree, combine = FALSE)
  }, error = function(e) {
    env$chat_messed <- TRUE

    if (env$time_messed) {
      env$messed_message <-
        paste0(env$messed_message,
               " AND: Please run messy_chat() with the argument ",
               "combine=TRUE before running this function.")
    } else {
      env$messed_message <-
        paste0("Please run messy_chat() with the argument ",
               "combine=TRUE before running this function.")
    }
  })

  if (env$time_messed || env$chat_messed) {
    stop("You combined data from old and new oTree versions. ",
         env$messed_message)
  }

  # Set background function: chat function   ####
  # The Time data frame is special because old sessions
  # are not deleted from the data frame

  delete_chats_sessions <- function() {
    # Old / new differently
    if (!is.null(oTree[["Chats"]]$session_code)) {
      oTree[["Chats"]] <-
        oTree[["Chats"]][
          !(oTree[["Chats"]]$session_code %in% scodes), ]

    } else if (!is.null(oTree[["Chats"]]$session__code)) {
      # Not sure if that's needed
      oTree[["Chats"]] <-
        oTree[["Chats"]][
          !(oTree[["Chats"]]$session__code %in% scodes), ]

    } else if (!is.null(oTree[["Chats"]]$participant__session__code)) {
      oTree[["Chats"]] <-
        oTree[["Chats"]][
          !(oTree[["Chats"]]$participant__session__code %in% scodes), ]

    } else {

      if (!is.null(oTree[["Chats"]]$participant_code)) {
        # Not sure if that's needed because session_code is there anyway
        oTree[["Chats"]] <-
          oTree[["Chats"]][
            !oTree[["Chats"]]$participant_code %in% deleted_participants, ]

      } else if (!is.null(oTree[["Chats"]]$participant__code)) {

        oTree[["Chats"]] <-
          oTree[["Chats"]][
            !oTree[["Chats"]]$participant__code %in% deleted_participants, ]
      }
    }
    return(oTree)
  }

  # Get list of all participants to be deleted that really exist in the DF  ####
  for (app in appnames) {

    if (app != "Time" && app != "info" && app != "Chats") {

      if (all(c("participant.code", "session.code") %in%
          names(oTree[[app]]))) { # Exclude custom exports

        deleted_participants <-
          c(deleted_participants,
            oTree[[app]]$participant.code[
              (oTree[[app]]$session.code %in% scodes)])
      }

    } else if (app == "Time") {

      # Old / new differently
      if (!is.null(oTree[["Time"]]$session__code)) {
        # Not sure if needed
        deleted_participants <-
          c(deleted_participants,
            oTree[["Time"]]$participant__code[
              oTree[["Time"]]$session__code %in% scodes])

      } else if (!is.null(oTree[["Time"]]$session_code)) {
        deleted_participants <-
          c(deleted_participants,
            oTree[["Time"]]$participant_code[
              (oTree[["Time"]]$session_code %in% scodes)])
      } else {
        warning("No variable called \"session code\" in ",
                       "the \"Time\" data frame. ",
                       "Session information is taken from the ",
                       "data frames of the ",
                       "other apps and/or \"all_apps_wide\".")
      }

    } else if (app == "Chats") {
      if (!is.null(oTree[[app]]$session_code)) {
        deleted_participants <-
          c(deleted_participants,
            oTree[[app]]$participant_code[
              oTree[[app]]$session_code %in% scodes])

      } else if (!is.null(oTree[[app]]$session__code)) {
        # Not sure if needed
        deleted_participants <-
          c(deleted_participants,
            oTree[[app]]$participant__code[
              (oTree[[app]]$session__code %in% scodes)])

      } else if (!is.null(oTree[[app]]$participant__session__code)) {
        deleted_participants <-
          c(deleted_participants,
            oTree[[app]]$participant__code[
              oTree[["Chats"]]$participant__session__code %in%
                  scodes])

      } else {
        warning(
          "No variable called \"session code\" ",
                 "in the Chats data frame. ",
                 "Session information is taken from the ",
                 "data frames of the ",
                 "other apps and/or \"all_apps_wide\".")
      }
    }
  }

  if (length(deleted_participants) == 0L) {
    warning("The session can not be found in any of the data frames.")
    return(oTree)
  }

  # Create data frame of deletions  ####
  if ("all_apps_wide" %in% names(oTree)) {
    deletion_frame <- as.data.frame(oTree$all_apps_wide[
      oTree$all_apps_wide$participant.code %in% deleted_participants,
      c("participant.code", "session.code", saved_vars)])

    colnames(deletion_frame) <- c("participant.code",
                                  "session.code",
                                  saved_vars)

    deletion_frame <- cbind(deletion_frame,
                            end_app = "",
                            end_page = "",
                            reason = reason)

    oTree[["info"]][["deleted_cases"]][["full"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["full"]],
      deletion_frame)

    oTree[["info"]][["deleted_cases"]][["unique"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["unique"]],
      deletion_frame)  # Deletion frame is already unique!

  } else {
    # Retrieve information on session.code in the next apps that contain the
    # session code and the participant code

    for (app_name in appnames) {
      if (!(app_name %in% c("Chats", "Time", "info")) &&
          all(c("participant.code", "session.code") %in%
            colnames(oTree[[app_name]]))) {  # Exclude custom exports

          deletion_frame <- plyr::rbind.fill(
            deletion_frame,
            as.data.frame(oTree[[app_name]][
              oTree[[app_name]]$participant.code %in% deleted_participants,
              c("participant.code", "session.code")]))
        }
    }

    # Add reason
    deletion_frame <- unique(deletion_frame)
    deletion_frame <- cbind(deletion_frame,
                            end_app = NA,
                            end_page = NA,
                            reason = reason)

    # Add data frames to the data frames of deleted cases
    oTree[["info"]][["deleted_cases"]][["full"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["full"]],
      deletion_frame)
    rownames(oTree[["info"]][["deleted_cases"]][["full"]]) <- NULL

    oTree[["info"]][["deleted_cases"]][["unique"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["unique"]],
      deletion_frame)
    rownames(oTree[["info"]][["deleted_cases"]][["unique"]]) <- NULL
  }

  # Delete participant in all apps  ####
  for (app in appnames) {
    if (app != "Time" && app != "Chats") {
      if (all(c("participant.code", "session.code") %in%
          colnames(oTree[[app]]))) {  # Exclude custom exports

        oTree[[app]] <- oTree[[app]][which(!(oTree[[app]]$session.code %in%
                                               scodes)), ]
      }
    } else if (app == "Time") {
      # Old / new differently
      if (!is.null(oTree[["Time"]]$session_code)) {
        oTree[["Time"]] <- oTree[["Time"]][!(oTree[["Time"]]$session_code %in%
                                               scodes), ]
      } else {
        oTree[["Time"]] <- oTree[["Time"]][!(oTree[["Time"]]$session__code %in%
                                               scodes), ]
      }
    } else if (app == "Chats")  {
      oTree <- delete_chats_sessions()
    }
  }

  # Message on deleted cases  ####
  messages <- c(paste(length(unique(deleted_participants)), "case(s) deleted"),
                messages)

  # Number of deleted cases  ####
  oTree[["info"]][["deleted_cases"]][["codes"]] <- unique(
    c(unique(deleted_participants),
      oTree[["info"]][["deleted_cases"]][["unique"]]$participant.code))
  oTree[["info"]][["deleted_cases"]][["count"]] <- length(unique(
    oTree[["info"]][["deleted_cases"]][["codes"]]))

  # Return and print messages  ####
  if (info) {
    message(messages)
  }

  return(oTree)
}
