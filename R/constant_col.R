#' Show columns that have no variation
#' @description
#' This function is called by \code{\link[=show_constant]{show_constant()}}.
#' The app in question
#' is already specified there. I must admit that I found the idea to this
#' code somewhere on Stack Overflow but cannot remember where.
#' @param df Data frame.
#' @param value The value that should be constant within a column.
#' @returns This function returns a vector of names of variables
#' that are constant.
#' @noRd

constant_col <- function(
    df,
    value = NULL) {

  # Check all columns and create a named logical vector
  if (!is.na(value)) {

    # Check for NA values
    logi_vec <- vapply(df,
                       function(df) all(!is.na(df) & df == value),
                       FUN.VALUE = logical(1L))

  } else if (is.na(value)) {

    # Check for NA
    logi_vec <- vapply(df,
                       function(df) all(is.na(df)),
                       FUN.VALUE = logical(1L))
  }

  # Get the names of all constant columns
  const_vec <- names(df[!is.na(logi_vec) & logi_vec])

  # Return
  return(const_vec)
}
