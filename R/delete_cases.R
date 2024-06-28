#' Delete specific cases
#' @description
#' Delete specific cases from all data frames in the oTree list.
#'
#' Caution 1: This function does not delete cases from the original
#' CSV and Excel files!
#'
#' Caution 2: This function does not delete cases from custom exports
#' and custom data frames if these data frames do not have a variable
#' named participant.code!
#'
#' Caution 3: This function does not delete any data from 
#' the \code{$Chats} data frame!
#' (As the interpretation of chat data depends on how participants
#' engage with each other, the data must be deleted
#' with more care than deleting data in other apps.
#' Hence, this function does not delete data in this data frame.
#' Please do this manually if necessary!)
#' @keywords oTree
#' @param oTree A list of data frames that were created 
#' by \code{\link{import_otree}}.
#' @param pcodes Character. The value(s) of the participant.code variable of
#' the participants whose data should be removed.
#' @param plabels Character. The value(s) of the participant.label variable of
#' the participants whose data should be removed.
#' @param saved_vars Character. The name(s) of variable(s) that need(s) to be
#' stored in the list of information on deleted cases 
#' in \code{$info$deleted_cases}.
#' @param reason Character. The reason for deletion that should be stored in
#' the list of information on deleted cases in \code{$info$deleted_cases}.
#' @param omit Logical. \code{TRUE} if the deleted cases should not be added to
#' the information on deleted cases in \code{$info$deleted_cases}.
#' @param info Logical. \code{TRUE} if a brief information on the case deletion
#' process should be printed.
#' @returns This function returns a duplicate of the original oTree list
#' of data frames that do not include the deleted cases.
#'
#' It adds information on the deleted cases to \code{$info$deleted_cases}. (This
#' list is also filled by other functions.)
#'
#' In this list, you can find the following information:
#'
#' - \code{$codes} = A vector with the participant codes of all deleted cases.
#'
#' - \code{$count} = The number of participants in \code{$codes}.
#'
#' - \code{$full} and \code{$unique} = The data frames \code{$full} 
#' and \code{$unique} contain information
#' on each deleted participant and the reason why they were
#' deleted. The entries to the \code{$full} and the \code{$unique} 
#' data frames are the same.
#' Columns \code{"end_app"} and \code{"end_page"} are left empty intentionally
#' because they are only filled by the \code{\link{delete_dropouts}} function.
#'
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # First, show some row numbers
#' print(paste(nrow(oTree$all_apps_wide), nrow(oTree$survey),
#' nrow(oTree$Time), nrow(oTree$Chats)))
#'
#' # Delete only one case
#' oTree2 <- delete_cases(oTree,
#'                        pcodes = "xmxl46rm",
#'                        reason = "requested")
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Delete several cases
#' deletionlist <- c("4zhzdmzo", "xmxl46rm")
#' oTree2 <- delete_cases(oTree,
#'                        pcodes = deletionlist,
#'                        reason = "requested")
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Show information on all deleted cases (also dropouts):
#' oTree2$info$deleted_cases$full
#'
#' # Save one variable
#' oTree2 <- delete_cases(oTree,
#'   pcodes = deletionlist,
#'   reason = "requested",
#'   saved_vars = "participant._index_in_pages")
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Save some variables
#' oTree2 <- delete_cases(oTree,
#'   pcodes = deletionlist,
#'   reason = "requested",
#'   saved_vars = c(
#'     "participant._index_in_pages",
#'     "participant._max_page_index"))
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Get a list of all deleted cases
#' # (If there is already a list, the new list is added to it)
#' oTree2$info$deleted_cases$codes
#'
#' # Show number of all deleted cases
#' length(oTree2$info$deleted_cases$codes)
#' oTree2$info$deleted_cases$count

#' @export
delete_cases <- function(oTree,
                         pcodes = NULL,
                         plabels = NULL,
                         saved_vars = NULL,
                         reason,
                         omit = FALSE,
                         info = FALSE) {

  all_deleted <- character(0L)
  deletion_frame <- data.frame()
  time_messed <- FALSE
  chat_messed <- FALSE
  messed_message <- character(0L)

  # Create list of apps  ####
  appnames <- names(oTree)
  appnames <- appnames[appnames != "info" & appnames != "Chats"]

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
    time_messed <<- TRUE
    messed_message <<- paste0("Please run messy_time() with the argument ",
                              "combine=TRUE before running this function.")
  })

  # Check mixed Chat data
  tryCatch({
    messy_chat(oTree, combine = FALSE)
  }, error = function(e) {
    chat_messed <<- TRUE

    if (time_messed) {

      # Combine messy chat message with messy time message
      messed_message <<-
        paste0(messed_message,
               " AND: Please run messy_chat() with the argument ",
               "combine=TRUE before running this function.")
    } else {

      # Make messy chat message
      messed_message <<-
        paste0("Please run messy_chat() with the argument ",
               "combine=TRUE before running this function.")
    }
  })

  # Stop if messy time and/or chat variables should not be merged

  if (time_messed || chat_messed) {
    stop("You combined data from old and new oTree versions. ",
         messed_message)
  }

  # Warnings  ####
  my_messages <- character(0L)

  # Stopping rules  ####
  if (is.null(plabels) && is.null(pcodes)) {
    stop("Please specify pcodes or plabels!")
  }

  if (!is.null(plabels) && !is.null(pcodes)) {
    stop("Please only specify either pcodes or plabels!")
  }

  if (!("all_apps_wide" %in% names(oTree)) && !is.null(saved_vars)) {
    stop("The argument \"saved_vars\" only works when ",
         "you have \"all_apps_wide\" in your ",
         "oTree list of data frames.")
  }

  if (!(is.null(saved_vars)) &&
      any(!(saved_vars %in% colnames(oTree$all_apps_wide)))) {
    stop("saved_vars not in \"all_apps_wide\" data frame!")
  }

  # Translate labels to codes  ####

    # If pcodes was chosen  ####
    if (!is.null(pcodes)) {

      # Error messages
      if (anyNA(pcodes)) {
        stop("At least one element in pcodes is NA")
      }

      del_participant_code_aaw <-
        oTree$all_apps_wide$participant.code[
          oTree$all_apps_wide$participant.code %in% pcodes]
      delete <- pcodes
    }

    # If plabels was chosen  ####
    if (!is.null(plabels)) {

      if (anyNA(plabels)) {
        stop("At least one element in plabel is NA")
      }

      if (is.null(oTree$all_apps_wide$participant.code)) {
        stop("Even though you chose option \"plabels\", ",
             "this function needs the variable ",
             "$all_apps_wide$participant.code to work. Did you delete it?"
        )
      }
      del_participant_code_aaw <-
        oTree$all_apps_wide$participant.code[
          oTree$all_apps_wide$participant.label %in% plabels]

      delete <- del_participant_code_aaw
    }

  # Get list of all deletion participants that really ####
  # exist in the data frames   ####
  for (app in appnames) {
    if (app != "Time" && app != "info" && app != "Chats") {

      # Exclude custom exports
      if ("participant.code" %in% colnames(oTree[[app]])) {

        # Make vector
        all_deleted <- c(all_deleted,
                         oTree[[app]]$participant.code[
                           which(oTree[[app]]$participant.code %in% delete)])
      }

    } else if (app == "Time" || app == "Chats") {
      # Old / new differently
      if (!is.null(oTree[[app]]$participant__code)) {
        all_deleted <-
          c(all_deleted,
            oTree[[app]]$participant__code[
              oTree[[app]]$participant__code %in% delete])

      } else if (!is.null(oTree[[app]]$participant_code)) {
        all_deleted <-
          c(all_deleted,
            oTree[[app]]$participant_code[
              oTree[[app]]$participant_code %in% delete])
      }
    }
  }

  if (length(all_deleted) == 0L) {
    stop("Participant(s) not in data frames.")
  }

  # Create data frame of deletions  ####
  if (!omit && 
      length(del_participant_code_aaw) > 0L) {

    deletion_frame <- as.data.frame(oTree$all_apps_wide[
      oTree$all_apps_wide$participant.code %in% del_participant_code_aaw,
      c("participant.code", "session.code", saved_vars)])

    colnames(deletion_frame) <- c("participant.code", "session", saved_vars)
    deletion_frame <- cbind(deletion_frame,
                            end_app = "",
                            end_page = "",
                            reason = reason)

    # Rearrange
    deletion_frame <- deletion_frame[, c(c("participant.code", "session",
                                           "end_app", "end_page",
                                           "reason"), saved_vars)]

    oTree[["info"]][["deleted_cases"]][["full"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["full"]],
      deletion_frame
    )

    # "unique" is the same as "full" because it is based on all_apps_wide!
    oTree[["info"]][["deleted_cases"]][["unique"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["unique"]],
      deletion_frame
    )
  } else if (!omit && length(del_participant_code_aaw) == 0L) {

    # Retrieve information on session.code in the next apps that contain the
    # session code and the participant code
    for (app_name in appnames) {

      if (any(pcodes %in% oTree[[app_name]]$participant.code) &&
          "session.code" %in% colnames(oTree[[app_name]])) {

        deletion_frame <-
          plyr::rbind.fill(
            deletion_frame,
            as.data.frame(oTree[[app_name]][
              oTree[[app_name]]$participant.code %in% delete,
              c("participant.code", "session.code")
        ]))
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

    oTree[["info"]][["deleted_cases"]][["unique"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["unique"]],
      deletion_frame)
  }

  # Check if chats are there
  # This is the same in delete_dropouts
  if ("Chats" %in% names(oTree)) {
    my_messages <- c(my_messages,
      paste0("Cases are deleted from all data frames. ",
             "Except: ",
             "The list of oTree data frames includes a chat. ",
             "As the interpretation of chat data depends on ",
             "how participants engage ",
             "with each other, the data must be deleted with more care than ",
             "deleting data in other apps. ",
             "Hence, this function does not delete ",
             "data in this data frame. Please do this manually if necessary!"))
  }

  # Delete participant in all apps  ####
  for (app in appnames) {
    if (app != "Time") {
      # Exclude custom exports
      if ("participant.code" %in% colnames(oTree[[app]])) {

        # Delete
        oTree[[app]] <-
          oTree[[app]][which(!(oTree[[app]]$participant.code %in% delete)), ]
      }

    } else {
      # Old / new differently
      if (!is.null(oTree[["Time"]]$participant_code)) {
        oTree[["Time"]] <-
          oTree[["Time"]][
            !(oTree[["Time"]]$participant_code %in% delete), ]
      } else {
        oTree[["Time"]] <-
          oTree[["Time"]][
            !(oTree[["Time"]]$participant__code %in% delete), ]
      }
    }
  }

  # Message on deleted cases  ####
  my_messages <- c(paste(length(unique(all_deleted)),
                         "case(s) deleted. "),
                   my_messages)

  # Number of deleted cases
  oTree[["info"]][["deleted_cases"]][["codes"]] <-
    unique(c(unique(all_deleted),
             oTree[["info"]][["deleted_cases"]][["unique"]]$participant.code))

  oTree[["info"]][["deleted_cases"]][["count"]] <-
    length(unique(oTree[["info"]][["deleted_cases"]][["codes"]]))

  # Return and warnings  ####
  if (info) {
    message(my_messages)
  }

  return(oTree)
}
