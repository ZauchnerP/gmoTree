#' Delete duplicate data
#' @description
#' Delete duplicate rows from all oTree app data frames
#' and \code{$all_apps_wide}.
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @returns This function returns a duplicate of the original oTree
#' list of data frames but without duplicate rows in all app data
#' frames and \code{$all_apps_wide}. This function has no effect on the data
#' frames \code{$Time} and \code{$Chats}.
#'
#' This function does NOT add information to \code{$info$deleted_cases},
#' because it does not delete any important information but only
#' cleans up a messy data import.
#'
#' However, the function adjusts \code{$info$initial_n}, if
#' an \code{$all_apps_wide} data frame exists.
#' @examplesIf rlang::is_installed("withr")
#' # Set data folder first
#' withr::with_dir(system.file("extdata", package = "gmoTree"), {
#'
#' # Import all oTree files in this folder and its subfolders
#' oTree <- import_otree()
#' })
#'
#' # First, show some row numbers
#' print(paste(nrow(oTree$all_apps_wide), nrow(oTree$survey),
#' nrow(oTree$Time), nrow(oTree$Chats)))
#'
#' # Delete duplicate rows
#' oTree <- delete_duplicate(oTree)
#'
#' # Show row numbers again
#' print(paste(nrow(oTree$all_apps_wide), nrow(oTree$survey),
#' nrow(oTree$Time), nrow(oTree$Chats)))

#' @export
delete_duplicate <- function(oTree) {

  appnames <- names(oTree)
  appnames <- appnames[appnames != "info"]
  appnames <- appnames[appnames != "Time"]
  appnames <- appnames[appnames != "Chats"]
  appnames <- appnames[!startsWith(prefix = "custexp_", x = appnames)]

  for (app in appnames) {
    columns <- names(oTree[[app]])
    columns <- columns[columns != "participant.label"]

    oTree[[app]] <-
      dplyr::distinct(oTree[[app]], .keep_all = TRUE, !!! rlang::syms(columns))
  }

  if ("all_apps_wide" %in% names(oTree)) {
    oTree$info$initial_n <- nrow(oTree$all_apps_wide)
    # message(paste0("Any duplicate rows are deleted. ",
    #         "Information on deleted rows is not added to ",
    #         "the list of deleted ",
    #         "cases because this list might be used for analyses, and ",
    #         "this function only accounts for a messy data import. ",
    #         "info$initial_n is adjusted."))
  }
  # else {
    # message(paste0(
    #         "Any duplicate rows are deleted. ",
    #         "Information on deleted rows is not added to
    #         "the list of deleted ",
    #         "cases because this list might be used for analyses, and ",
    #         "this function only accounts for a messy data import."))
  # }

  return(oTree)
}
