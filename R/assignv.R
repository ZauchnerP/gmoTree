#' Assign a variable from all_apps_wide
#' @description
#' Assign a variable from \code{$all_apps_wide} to the other app data frames.
#' @param oTree A list of data frames that were 
#' created by \code{\link{import_otree}}
#' @param variable Character. The variable in the \code{$all_apps_wide} 
#' data frame that should be assigned to all other apps.
#' @param newvar Character. The name of the newly created variable.
#' @returns This function returns a duplicate of the
#' original oTree list of data frames
#' but with an additional column in all data frames. The additional column
#' contains data from the specified variable found in \code{$all_apps_wide}.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Assign variable "survey.1.player.gender" and name it "gender"
#' oTree <- assignv(oTree = oTree,
#'                  variable = "survey.1.player.gender",
#'                  newvar = "gender")
#'
#' # Show the new variable in some of the other app data frames
#' oTree$dictator$gender
#' oTree$chatapp$gender
#'
#' # The variable is now duplicated in app "survey" because it is obtained from
#' # there (This can be avoided by naming the new variable the same as the old
#' # variable)
#' oTree$survey$gender
#' oTree$survey$player.gender
#'
#' # In app "all_apps_wide," the variable is also there twice (This can be
#' # avoided by naming the new variable the same as the old variable)
#' oTree$all_apps_wide$gender
#' oTree$all_apps_wide$survey.1.player.gender

#' @export
assignv <- function(oTree,
                    variable,
                    newvar) {

  # Check  ####
  if (!("all_apps_wide" %in% names(oTree))) {
    stop("There is no \"all_apps_wide\" in your oTree list of ",
         "data frames!")
  }

  if (length(variable) > 1L) {
    stop("Plase enter only one variable name!")
  }

  if (length(newvar) > 1L) {
    stop("Plase enter only one new variable name!")
  }

  if (!(variable %in% colnames(oTree[["all_apps_wide"]]))) {
    stop("The variable does not exist in  \"all_apps_wide\"!")
  }

  # Create list of apps  ####
  appnames <- names(oTree)
  appnames <- appnames[appnames != "info"]

  # Assign variable  ####
  for (i in unique(oTree$all_apps_wide$participant.code)) {

    for (app in appnames) {

      if (app != "Time" && app != "Chats") {
        # Exclude custom exports
        if ("participant.code" %in% colnames(oTree[[app]])) {
          # Assign variable
          oTree[[app]][[newvar]][oTree[[app]]$participant.code == i] <-
            oTree[["all_apps_wide"]][[variable]][
              oTree[["all_apps_wide"]]$participant.code == i]
        }
      } else {
        # Old / new differently
        if (!is.null(oTree[[app]]$participant__code)) {
          oTree[[app]][[newvar]][oTree[[app]]$participant__code == i] <-
            oTree[["all_apps_wide"]][[variable]][
              oTree[["all_apps_wide"]]$participant.code == i]

        } else {
          oTree[[app]][[newvar]][oTree[[app]]$participant_code == i] <-
            oTree[["all_apps_wide"]][[variable]][
              oTree[["all_apps_wide"]]$participant.code == i]
        }
      }
    }
  }
  return(oTree)
}
