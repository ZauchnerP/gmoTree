#' Show constant columns
#' @description
#' Show all columns with no variation in their values for each data frame
#' in the oTree list of data frames (except the ones in the info list).
#' This function is helpful before running an experiment to check if there
#' are unnecessary variables.
#' You can check for columns that have any unchanging possible value
#' or for columns containing only a specific value.
#' @keywords oTree
#' @param oTree A list of data frames that were created
#' by \code{\link{import_otree}}.
#' @param value The value that is controlled to be the same within a column.
#' If the value is set to \code{"any"}, the function checks for
#' columns where any possible values are identical.
#' @returns This function returns a list of vectors, one for each app,
#' \code{$all_apps_wide}, the \code{$Time} and/or the \code{$Chats} data frame.
#' Each vector contains the names of all variables with constant values.
#' If there are no variables with constant values, the vector is empty.
#' @examples
#' # Use package-internal list of oTree data frames
#' oTree <- gmoTree::oTree
#'
#' # Show all columns that contain only NAs
#' show_constant(oTree = oTree)
#' show_constant(oTree = oTree, value = NA)
#'
#' # Show all columns that contain only -99
#' show_constant(oTree = oTree, value = -99)

#' @export
show_constant <- function(oTree,
                          value = "any") {

  # Error messages  ####
  if (length(value) > 1L) {
    stop("Please only enter only one value!")
  }

  if (is.null(value)) {
    stop("Please enter a valid value!")
  }

  # Make output list
  output <- list()

  # Check for specific values  ####
  if (is.na(value) ||
      value != "any") {

    # Go through all oTree elements
    for (i in seq_along(oTree)) {

      # Only use the data frames (i.e. Apps and Time and Chats)
      if (inherits(oTree[[i]], "data.frame")) {

        # Get name of the app/Time/Chats
        df_name <- rlist::list.names(oTree[i])

        # Make data frame output (call function constant_col())
        output[[df_name]] <- constant_col(
          df = oTree[[i]],
          value = value
        )
      }
    }

  } else {
    # Check for any constant columns background function  ####
    get_constant_cols <- function(df) {
      constant_cols <- sapply(df, function(col) length(unique(col)) == 1L)
      names(df)[constant_cols]
    }

    # Check all apps for empty columns  ####
    for (i in seq_along(oTree)) {
      if (inherits(oTree[[i]], "data.frame")) {

        # Get name of the app/Time/Chats
        df_name <- rlist::list.names(oTree[i])

        # Make data frame output (call function get_constant_cols())
        output[[df_name]] <- get_constant_cols(oTree[[i]])
      }
    }
  }

  # Return  ####
  return(output)
}
