#' Assign a variable to all_apps_wide
#' @description
#' Assign a variable from one of the app data frames to \code{$all_apps_wide}.
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @param app Character. The data frame from which the variable is taken.
#' @param variable Character.
#' The name of the variable that should be assigned to \code{$all_apps_wide}.
#' @param newvar Character.
#' The name of the newly created variable in the \code{$all_apps_wide} data
#' frame.
#' @param resafter Character.
#' The name of the variable that precedes the new variable.
#' If \code{NULL}, the new variable will be placed at the end of the data frame.
#' @returns This function returns a duplicate of the original oTree list of
#' data frames but with an additional column in the \code{$all_apps_wide} data
#' frame that contains the variable in question.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Check out the old variable 
#' oTree$survey$player.age
#' 
#' # Create a new variable
#' oTree$survey$younger30 <- ifelse(oTree$survey$player.age < 30, 1, 0)
#'
#' # Assign the variable younger30 to all_apps_wide
#' oTree2 <- assignv_to_aaw(
#'   oTree = oTree,
#'   app = "survey",
#'   variable = "younger30",
#'   newvar = "younger30")
#'
#'# Show the new variable in the all_apps_wide data frame
#'oTree2$all_apps_wide$younger30
#'
#'# Check the position of the new variable
#'match("younger30",names(oTree2$all_apps_wide))
#'
#'# Place the new variable immediately after the "survey.1.player.age" variable
#'oTree2 <- assignv_to_aaw(oTree,
#'                         app = "survey",
#'                         variable = "younger30",
#'                         newvar = "younger30",
#'                         resafter = "survey.1.player.age")
#'
#'# Show the new variable in the all_apps_wide data frame
#'oTree2$all_apps_wide$younger30
#'
#'# Show the position of the new variable
#'match("younger30", names(oTree2$all_apps_wide))

#' @export
assignv_to_aaw <- function(oTree,
                           app,
                           variable,
                           newvar,
                           resafter = NULL) {

  # Error messages  ####
  if (app == "Chats" ||
      app == "Time" ||
      app == "info" ||
      !("participant.code" %in% colnames(oTree[[app]]))) {
    stop("This function does not work with ",
                app, "!")
  }

  # Check if all_apps_wide is there
  if ("all_apps_wide" %in% names(oTree)) {
    if (nrow(oTree[[app]]) != nrow(oTree$all_apps_wide)) {
      warning("New variable is created. However, there is an unequal ",
               "number of participants in \"all_apps_wide\" (",
               nrow(oTree$all_apps_wide),
               ") and app \"", app,
               "\" (",
               nrow(oTree[[app]]),
               "). Did you forget to delete dropouts and empty cases ",
               "or did you forget to import app data? Sometimes, this can ",
               "happen if you import data from within a session or room ",
               "where you can only import \"all_apps_wide\" but not the ",
               "separate app data, time data or chat data.")
    }
  } else {
    stop("There is no \"all_apps_wide\" in your oTree list of ",
                "data frames!")
  }

  # Check if there is only one variable/new variable
  if (length(variable) > 1L) {
    stop("Plase enter only one variable name!")
  }

  if (length(newvar) > 1L) {
    stop("Plase enter only one new variable name!")
  }

  # Check if variable is there
  if (is.null(oTree[[app]][[variable]])) {
    stop("The variable does not exist in the app.")
  }

  # Assign variable  ####
  for (i in oTree$all_apps_wide$participant.code) {
    if (i %in% oTree[[app]]$participant.code) {
      oTree$all_apps_wide[[newvar]][
        oTree$all_apps_wide$participant.code == i] <-
          unique(oTree[[app]][[variable]][oTree[[app]]$participant.code == i])
    }
  }

  # Rearrange  ####
  if (!is.null(resafter)) {

    # Make indices
    indices <- c(
      1L:which(names(oTree$all_apps_wide) == resafter),
      ncol(oTree$all_apps_wide),  # Put the new variable here
      (which(names(oTree$all_apps_wide) == resafter) + 1L):
        (ncol(oTree$all_apps_wide) - 1L)
    )

    # Apply indices
    oTree$all_apps_wide <- oTree$all_apps_wide[, indices]
  }

  # Return  ####
  return(oTree)
}
