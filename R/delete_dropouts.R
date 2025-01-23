#' Delete dropouts
#' @description
#' Delete the data of all participants who did not end the experiment at (a)
#' certain page(s) and/or app(s).
#'
#' Caution 1: This function does not delete cases from the original CSV and
#' Excel files!
#'
#' Caution 2: This function does not delete cases from custom export
#' data frames if they do not have a variable named \code{participant.code} and
#' a variable named \code{session.code}!
#'
#' Caution 3: This function does not delete any data from
#' the \code{$Chats} data frame!
#' (As the interpretation of chat data depends on how participants engage with
#' each other, the data must be deleted with more care than deleting data in
#' other apps. Hence, this function does not delete data in this data frame.
#' Please do this manually if necessary!)
#' @keywords oTree
#' @inheritParams apptime
#' @param final_apps Character string or character vector.
#' The name(s) of the app(s) at which the participants have to finish the
#' experiment.
#' @param final_pages Character string or character vector.
#' The name(s) of the page(s) at which the participants have to finish the
#' experiment.
#' @param saved_vars Character string or character vector.
#' The name(s) of variable(s) that need(s) to be
#' stored in the list of information on deleted cases
#' in \code{$info$deleted_cases}.
#' @param reason Character string.
#' The reason for deletion that should be stored in
#' the list of information on deleted cases in \code{$info$deleted_cases}.
#' @param inconsistent Character string.
#' Should the function continue or be stopped if
#' at least one participant has inconsistent end_pages, inconsistent end_apps,
#' or both? To continue, type \code{"yes"},
#' to stop the function, type \code{"no"}.
#' @param info Logical. \code{TRUE} if a brief information on the dropout
#' deletion process should be printed.
#' @returns
#' This function returns a duplicate of the original list of data frames
#' but without the deleted cases.
#'
#' It adds information on the deleted cases to \code{$info$deleted_cases}. (This
#' list is also filled by other functions.)
#'
#' In this list, you can find the following information:
#'
#' - \code{$full} = A data frame that contains information
#' on all participants who did not finish the study;
#' it shows their participant codes, the names of the apps in which they
#' left the experiment,
#' the names of the pages in which they left the experiment,
#' the names of the app data frames in which this information was found, and
#' the dropout reason (\code{"ENC"}, experiment not completed, combined
#' with the name of the data frame in which the dropout was observed).
#' Because participants usually appear in multiple app data frames,
#' the \code{$info$deleted_cases$full} data frame may contain several entries
#' for each person.
#'
#' - \code{$unique} = A data frame that contains similar information as
#' the \code{$full} data frame but with only one row per participant and
#' no information on the data frame in which the dropout was observed.
#'
#' - \code{$all_end} = A table that provides information on the app and page
#' combinations where participants ended the experiment.
#' This table also includes information for participants who did not drop out
#' of the experiment.
#' The \code{$all_end} table is only shown if an \code{$all_apps_wide} data
#' frame exists.
#'
#' - \code{$codes} = A vector containing the participant codes of
#' all deleted participants.
#'
#' - \code{$count} = The number of all deleted participants.
#'
#' It is important to note that if only the argument \code{final_pages} is set,
#' this function does not distinguish between page names that reoccur in
#' different apps.
#'
#' If the columns \code{end_app} and \code{end_page} in the output are empty,
#' these variables were not saved by oTree for the specific participants.
#' This could be because empty rows were not deleted. This can be done
#' by using the argument \code{del_empty = TRUE} when
#' using \code{\link[=import_otree]{import_otree()}}.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # First, show some row numbers
#' print(paste(nrow(oTree$all_apps_wide), nrow(oTree$survey),
#' nrow(oTree$Time), nrow(oTree$Chats)))
#'
#' # Delete all cases that didn't end the experiment on the page "Demographics"
#' # within the app "survey"
#' oTree2 <- delete_dropouts(oTree,
#'                          final_apps = c("survey"),
#'                          final_pages = c("Demographics"))
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Delete all cases that didn't end the experiment on the page "Demographics"
#' # This page can be in any app
#' oTree2 <- delete_dropouts(oTree, final_pages = "Demographics")
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Delete all cases that didn't end the experiment on
#' # any page in the app "survey"
#' oTree <- delete_dropouts(oTree, final_apps = "survey")
#'
#' # Show row numbers again
#' print(paste(nrow(oTree2$all_apps_wide), nrow(oTree2$survey),
#' nrow(oTree2$Time), nrow(oTree2$Chats)))
#'
#' # Get list of information on all deleted cases
#' # (If there is already a list, the new list is added to it!)
#' oTree2$info$deleted_cases

#' @export
delete_dropouts <- function(oTree,
                            final_apps = NULL,
                            final_pages = NULL,
                            saved_vars = NULL,
                            inconsistent = NULL,
                            reason = "ENC",
                            info = FALSE) {

  keep_these_participants <- character(0L)
  delete_these_participants <- character(0L)
  dropout_data <- data.frame()
  messages <- character(0L)

  # Making sure the arguments are not empty   ####
  if (is.null(final_apps) && is.null(final_pages)) {
    stop("Please specify final_apps or final_pages!")
  }

  # Elements in oTree that are not apps  ####
  nonappelements <- c("Chats", "Time", "info", "deleted_cases")

  # Check if all apps wide and saved_vars work ####
  if (!("all_apps_wide" %in% names(oTree)) && !is.null(saved_vars)) {
    stop("The argument \"saved_vars\" only works when you ",
         "have \"all_apps_wide\" in your ",
         "list of data frames.")
  } else if ("all_apps_wide" %in% names(oTree) &&
             !is.null(saved_vars) &&
             !(all(saved_vars %in% colnames(oTree$all_apps_wide)))) {
    # If saved_vars is not in all_apps_wide
    stop("\"saved_vars\" not in all_apps_wide.")

  }

  # Inconsistency action  ####
  incons_function <- function(inconsistent = inconsistent) {
    # Ask user whether they want to stop function
    while (is.null(inconsistent) ||
           (inconsistent != "yes" && inconsistent != "no")) {
      cat(
        "At least one participant in the dropout list ",
        "has inconsistent end pages,\n",
        "inconsistent end apps, or both.\n",
        "Do you still want to continue with the deletion of the cases?\n",
        "Enter \"no\" to stop the function.\n",
        "If you want to delete all cases that are marked as ",
        "dropouts in at least one app, please enter \"yes\".\n",
        "Input:\n")

      inconsistent <- readLines(
        con = getOption("mypkg.connection"),
        n = 1L)
    }

    # Stop if user requests it
    if (inconsistent == "no") {
      stop(
        "At least one participant in the dropout list has ",
        "inconsistent end pages,\n",
        "inconsistent end apps, or both.\n",
        "The user requested termination, hence no cases were deleted."
      )
    } else if (inconsistent == "yes") {
      messages <- c(paste0(
        "At least one participant in the dropout list has ",
        "inconsistent end pages,\n",
        "inconsistent end apps, or both.\n",
        "The user requested no termination of the function. "
      ), messages)
      return(messages)
    }
  }

  # Create list of included and excluded participants  ####
  for (i in seq_along(oTree)) {

    # Every app except ...
    if (!(rlist::list.names(oTree[i]) %in% nonappelements)) {

      # Except user defined data frames
      if ("participant._current_app_name" %in% colnames(oTree[[i]])) {

        #  Check current apps
        if (!is.null(final_apps)) {
          appif <- oTree[[i]]$participant._current_app_name %in% final_apps
        } else {
          appif <- TRUE
        }

        # Check current pages
        if (!is.null(final_pages)) {
          pageif <- oTree[[i]]$participant._current_page_name %in% final_pages
        } else {
          pageif <- TRUE
        }

        # Add to the list of "keep" and "delete" people
        keep_these_participants <-
          c(oTree[[i]]$participant.code[appif & pageif],
            keep_these_participants)

        delete_these_participants <-
          c(oTree[[i]]$participant.code[!(appif) | !(pageif)],
            delete_these_participants)

        # Make data frame of people who were excluded
        dropout_data_app <- data.frame(
          participant.code = oTree[[i]]$participant.code[
            !(appif) | !(pageif)],
          session.code = oTree[[i]]$session.code[
            !(appif) | !(pageif)],
          end_app = oTree[[i]]$participant._current_app_name[
            !(appif) | !(pageif)],
          end_page = oTree[[i]]$participant._current_page_name[
            !(appif) | !(pageif)]
        )

        # Add reason
        if (nrow(dropout_data_app) >= 1L) {
          for (j in seq_len(nrow(dropout_data_app))) {
            dropout_data_app$reason[j] <-
              paste0("ENC. ",
                     "Noticed at: ", rlist::list.names(oTree[i]))
          }
          dropout_data <- plyr::rbind.fill(dropout_data_app, dropout_data)
        }
      } else {
        # User-made data frames are not tackled!
      }
    }
  }

  keep_these_participants <- unique(keep_these_participants)
  delete_these_participants <- unique(delete_these_participants)

  # Test if no one in "keep" is in "delete"  ####
  newlist <- character(0L)
  for (element in keep_these_participants) {
    if (element %in% delete_these_participants) {
      newlist <- append(element, newlist)
    }
  }

  if (length(newlist) > 0L) {
    messages <- incons_function(inconsistent)
  }

  # Make output data frames of people deleted ####
  if (nrow(dropout_data) >= 1L) {
    # Save variables for people who were excluded
    if ("all_apps_wide" %in% names(oTree)) {
      saved_vars_frame <- oTree$all_apps_wide[
        oTree$all_apps_wide$participant.code %in% delete_these_participants,
        c(
          "participant.code",
          saved_vars
        )
      ]
      saved_vars_frame <- as.data.frame(saved_vars_frame)
      colnames(saved_vars_frame) <- c("participant.code", saved_vars)

      # Info: "full" could have been defined in delete_cases, too
      dropout_data <- unique(dropout_data)

      # Add saved variables
      if (ncol(saved_vars_frame) > 0L) {
        dropout_data <- merge(
          x = dropout_data,
          y = saved_vars_frame,
          by.x = "participant.code",
          by.y = "participant.code",
          all.x = TRUE,
          all.y = TRUE
        )
      }
    }

    # Add to the existing full list (because of other deletions)
    oTree[["info"]][["deleted_cases"]][["full"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["full"]],
      dropout_data
    )

    # Unique
    uniquelist <- c(c("participant.code",
                      "session.code",
                      "end_app",
                      "end_page"),
                    saved_vars)

    dropout_data2 <- cbind(unique(dropout_data[uniquelist]),
                           reason = reason
    )

    oTree[["info"]][["deleted_cases"]][["unique"]] <- plyr::rbind.fill(
      oTree[["info"]][["deleted_cases"]][["unique"]],
      dropout_data2
    )
  }

  # Make table of all ending apps  ####
  if ("all_apps_wide" %in% names(oTree)) {
    # This is the only assignment of this table!
    oTree[["info"]][["deleted_cases"]][["all_end"]] <- table(
      oTree$all_apps_wide$participant._current_app_name,
      oTree$all_apps_wide$participant._current_page_name
    )
  } else {
    warning("No \"all_apps_wide\" to make the table of end pages.")
  }

  # Delete excluded participants from data frames  ####
  for (i in seq_along(oTree)) {

    # Every app except user defined data frames
    if (!(rlist::list.names(oTree[i]) %in% nonappelements) &&
        "participant._current_app_name" %in% colnames(oTree[[i]])) {

        # Delete participants
        oTree[[i]] <-
          oTree[[i]][
            !(oTree[[i]]$participant.code %in% delete_these_participants), ]

    }
  }

  # For Time  ####
  # Not "keep these people" because of inconsistent end pages/apps
  if (!is.null(oTree$Time$participant_code) &&
      !is.null(oTree$Time$participant__code)) {
    oTree[["Time"]] <-
      oTree[["Time"]][!(
        (oTree$Time$participant_code %in% c(delete_these_participants)) |
          (oTree$Time$participant__code %in% c(delete_these_participants))), ]

  } else if (!is.null(oTree$Time$participant_code) &&
             is.null(oTree$Time$participant__code)) {
    oTree[["Time"]] <-
      oTree[["Time"]][
        !(oTree$Time$participant_code %in% c(delete_these_participants)), ]

  } else if (is.null(oTree$Time$participant_code) &&
             !is.null(oTree$Time$participant__code)) {
    oTree[["Time"]] <-
      oTree[["Time"]][
        !(oTree$Time$participant__code %in% c(delete_these_participants)), ]
  }

  # Message on deleted cases  ####
  messages <- c(paste(length(unique(delete_these_participants)),
                       "case(s) deleted\n"),
                messages)

  # Check if chats are there
  # This is the same in delete_cases
  if ("Chats" %in% names(oTree)) {
    messages <-
      c(messages,
        (paste0("Dropouts are deleted from all data frames. Except: ",
                "The list of oTree data frames includes a chat. ",
                "As the interpretation of chat data depends ",
                "on how participants engage ",
                "with each other, the data must be deleted ",
                "with more care than ",
                "deleting data in other apps. ",
                "Hence, this function does not delete ",
                "data in this data frame. Please do this ",
                "manually if necessary!")))
  }

  # Codes and number of deleted cases  ####
  oTree[["info"]][["deleted_cases"]][["codes"]] <-
    unique(c(oTree[["info"]][["deleted_cases"]][["codes"]],
             oTree[["info"]][["deleted_cases"]][["unique"]]$participant.code))

  oTree[["info"]][["deleted_cases"]][["count"]] <-
    nrow(oTree[["info"]][["deleted_cases"]][["unique"]])

  # Return and messages  ####
  if (info) {
    message(messages)
  }

  return(oTree)
}
