#' Delete participant labels in all apps
#' @description
#' If you work with MTurk, the MTurk IDs will be stored in the
#' participant labels variable.
#' This function deletes this variable in all_apps_wide and every app data frame
#' in the list of data frames that was created by import_otree() and/or all
#' variables referring to MTurk, such as participant.mturk_worker_id.
#' @param oTree A list of data frames that were created by import_otree().
#' @param del_plabel Logical.
#' TRUE if all participant labels should be deleted.
#' @param del_mturk Logical.
#' TRUE if all MTurk variables should be deleted.
#' @returns This function returns a duplicate of the original oTree list of
#' data frames that do not include the participant labels and/or the MTurk
#' variables.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#'# Show participant labels
#' oTree$all_apps_wide$participant.label
#' oTree$survey$participant.label
#'
#' # Delete all participant labels
#' oTree2 <- delete_plabels(oTree)
#'
#' # Show participant labels again
#' oTree2$all_apps_wide$participant.label
#' oTree2$survey$participant.label

#' @export
delete_plabels <- function(oTree,
                           del_plabel = TRUE,
                           del_mturk = TRUE) {

  # Create list of apps  ####
  appnames <- names(oTree)
  appnames <- appnames[appnames != "info"]

  # Delete variable  ####
  if (del_plabel == TRUE) {
      for (app in appnames) {
        oTree[[app]][["participant.label"]] <- NULL
      }
  }

  if (del_mturk == TRUE) {
    for (app in appnames) {
      oTree[[app]][["participant.mturk_worker_id"]] <- NULL
      oTree[[app]][["participant.mturk_assignment_id"]] <- NULL
      oTree[[app]][["mturk_HITGroupId"]] <- NULL
      oTree[[app]][["mturk_HITId"]] <- NULL
      oTree[[app]][["session.mturk_HITId"]] <- NULL
      oTree[[app]][["session.mturk_HITGroupId"]] <- NULL
    }
  }

  return(oTree)
}
