#' Show participants who did not finish the experiment
#' @description
#' Show information on the people who did not finish the experiment at (a)
#' certain page(s) and/or app(s).
#' @keywords oTree
#' @param oTree A list of data frames that were created 
#' by \code{\link{import_otree}}.
#' @param final_apps Character.
#' The name(s) of the app(s) at which the participants have to finish the
#' experiment.
#' @param final_pages Character.
#' The name(s) of the page(s) at which the participants have to finish the
#' experiment.
#' @param saved_vars The name(s) of variable(s) that need(s) to be
#' shown in the list of information on dropout cases.
#' @returns
#' This function returns a list of information on participants who did not
#' finish the experiment.
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
#' the \code{$full} data frame may contain several entries for
#' each person.
#'
#' - \code{$unique} = A data frame that contains similar information as 
#' the \code{$full} data frame but with only one row per participant and 
#' no information on the data frame in which the dropout was observed.
#'
#' - \code{$all_end} = A table that provides information 
#' on the app and page combinations
#' where participants ended the experiment. This table also includes
#' information on participants who did not drop out of the experiment.
#' The \code{$all_end} table is only shown if an \code{$all_apps_wide} 
#' data frame exists.
#'
#' - \code{$codes} = A vector containing the participant codes of
#' all participants who did not finish the experiment.
#'
#' - \code{$count} = The number of all participants who did not 
#' finish the experiment.
#'
#' It is important to note that if only the argument \code{final_pages} is set,
#' this function does not distinguish between page names that reoccur in
#' different apps.
#'
#' If the columns \code{end_app} and \code{end_page} in the output are empty,
#' these variables were not saved by oTree for the specific participants.
#' This could be because empty rows were not deleted. This can be done
#' by using the argument \code{del_empty = TRUE}" when 
#' using \code{\link{import_otree}}.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Show everyone who did not finish with the app "survey"
#' show_dropouts(oTree, final_apps = "survey")
#'
#' # Show everyone who did not finish with the page "Demographics"
#' show_dropouts(oTree, final_pages = "Demographics")
#'
#' # Show everyone who finished with the following apps: "survey," "dictator"
#' final_apps <- unique(oTree$all_apps_wide$participant._current_app_name)
#' final_apps <- final_apps[final_apps != "survey"]
#' final_apps <- final_apps[final_apps != "dictator"]
#' show_dropouts(oTree, final_apps = final_apps)

#' @export
show_dropouts <- function(oTree,
                          final_apps = NULL,
                          final_pages = NULL,
                          saved_vars = NULL) {

  keep_these_participants <- c() # Is just here for the inconsistency test
  delete_these_participants <- c()  # Not deleted here. Inconsistency check!
  dropout_data <- data.frame()
  output <- list()
  my_warnings <- list()

  # Elements in oTree that are not apps  ####
  nonappelements <- c("Chats", "Time", "info", "deleted_cases")

  # Error checks  ####
  # Making sure the arguments are not empty
  if (is.null(final_apps) && is.null(final_pages)) {
    stop("Please specify final_apps or final_pages or both!")
  }

  # Check if saved_vars is in all_apps_wide  ####
  if (!is.null(saved_vars)) {
    if ("all_apps_wide" %in% names(oTree)) {
        if (!(all(saved_vars %in% colnames(oTree$all_apps_wide)))) {
            stop("saved_vars not in all_apps_wide.")
        } # Else: everything is okay

    } else {
      saved_vars <- NULL
      my_warnings <-
        paste0("Caution! saved_vars are taken from \"all_apps_wide\". ",
               "Since \"all_apps_wide\" is not in your oTree, this argument ",
               "is ignored.")
    }
  }

  # Create list of participants who did not finish the experiment ####
  for (i in seq_along(oTree)) {

    keep_these_participants <- unique(keep_these_participants)
    delete_these_participants <- unique(delete_these_participants)

    # Every app except ...
    if (!(rlist::list.names(oTree[i]) %in% nonappelements)) {

      # Make if-statements
      if (!is.null(final_apps)) {
        appif <- oTree[[i]]$participant._current_app_name %in% final_apps
      } else {
        appif <- TRUE
      }

      if (!is.null(final_pages)) {
        pageif <- oTree[[i]]$participant._current_page_name %in% final_pages
      } else {
        pageif <- TRUE
      }

      # Make list of people whose data is to be kept or "deleted"
      keep_these_participants <-
        c(oTree[[i]]$participant.code[appif & pageif],
          keep_these_participants)

      delete_these_participants <-
        c(oTree[[i]]$participant.code[!(appif) | !(pageif)],
          delete_these_participants)

      # Make data frame of people who did not finish the experiment
      dropout_app_data <- data.frame(
        participant.code = oTree[[i]]$participant.code[!appif | !pageif],
        session.code = oTree[[i]]$session.code[!appif | !pageif],
        end_app = oTree[[i]]$participant._current_app_name[!appif | !pageif],
        end_page = oTree[[i]]$participant._current_page_name[!appif | !pageif])

      # Add reason
      if (nrow(dropout_app_data) >= 1L) {
        for (j in seq_len(nrow(dropout_app_data))) {
          dropout_app_data$reason[j] <- paste0(
            "Experiment not completed. Noticed at: ",
            rlist::list.names(oTree[i]))
        }
        dropout_data <- plyr::rbind.fill(dropout_data, dropout_app_data)
      }
    }
  }

  delete_these_participants <- unique(delete_these_participants)

  # Test if no one in "keep" is in "delete"  ####
  newlist <- c()
  for (element in keep_these_participants) {
    if (element %in% delete_these_participants) {
      newlist <- append(element, newlist)
    }
  }

  if (length(newlist) > 0L) {
    my_warnings <- c(my_warnings, paste0(
      "At least one participant in the dropout list has inconsistent end ",
      "pages, inconsistent end apps, or both."))
  }

  # Make output data frames of people who did not finish the experiment  ####
  if (nrow(dropout_data) >= 1L) {
    dropout_data <- unique(dropout_data)

    # Save variables for people who did not finish the experiment
    if ("all_apps_wide" %in% names(oTree)) {
      saved_vars_frame <- oTree$all_apps_wide[
        !appif | !pageif,
        c("participant.code", saved_vars)]

      saved_vars_frame <- as.data.frame(saved_vars_frame)
      colnames(saved_vars_frame) <- c("participant.code", saved_vars)

      # Add saved variables
      if (ncol(saved_vars_frame) > 0L) {
        dropout_data <- merge(
          x = dropout_data,
          y = saved_vars_frame,
          by.x = "participant.code",
          by.y = "participant.code",
          all.x = TRUE,
          all.y = TRUE)
      }
    }

    output[["full"]] <- dropout_data

    # Unique
    dropout_data2 <- unique(dropout_data[c(c("participant.code",
                                             "session.code",
                                             "end_app",
                                             "end_page"),
                                           saved_vars)])

    output[["unique"]] <- cbind(dropout_data2,
                                reason = "Experiment not completed")
  }

  # Make table of all ending apps and pages  ####
  if ("all_apps_wide" %in% names(oTree)) {
    output[["all_end"]] <- table(
      oTree$all_apps_wide$participant._current_app_name,
      oTree$all_apps_wide$participant._current_page_name)
  } else {
    my_warnings <- c(my_warnings,
                     ("No \"all_apps_wide\" to make the table of end pages."))
  }

  # Number of dropouts
  output[["codes"]] <- output[["unique"]]$participant.code

  output[["count"]] <- nrow(output[["unique"]])

  # Warnings
  if (length(my_warnings) > 0L) {
    warning(paste(my_warnings, collapse = "\n\n"))
  }

  # Return  ####
  return(output)
}
