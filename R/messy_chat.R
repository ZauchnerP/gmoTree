#' Check if the Chats data frame is messy
#' @description
#' Check if the \code{$Chats} data frame includes both
#' session-related variables and participant-related variables that
#' appear multiple times. This may occur when data from different
#' oTree versions, which use different variable names, are combined.
#'
#' If desired, the function can merge these variables,
#' storing the data using the newer oTree version's variable names
#' and removing the outdated variables.
#'
#' @keywords oTree
#' @inheritParams apptime
#' @param combine Logical. \code{TRUE} if all variables referring to
#' the session code should be merged and/or all variables referring
#' to participant code should be merged in case data of several versions
#' of oTree are used.
#' @param session Logical. \code{TRUE} if all variables referring to the session
#' code should be checked and merged. Merging only works
#' if \code{combine = TRUE}.
#' @param participant Logical. \code{TRUE} if all variables referring to the
#' participant code should be checked and merged. Merging only works
#' if \code{combine = TRUE}.
#' @param info Logical. \code{TRUE} if a brief information on the process should
#' be printed.
#' @returns
#' This function searches for multiple variables related to the session code
#' or the participant code in the \code{$Chats} data frame.
#' which can occur when data from both old and new oTree versions are used.
#'
#' If \code{combine = FALSE}, the function will throw an error
#' if such variables are found.
#'
#' If \code{combine = TRUE}, the function will not throw an error
#' if such variables are found.
#' Instead, it automatically combines the variables into new variables
#' and adds them to the original \code{$Chats} data frame.
#' This function then returns a duplicate of the original oTree list but
#' with the \code{$Chats} data frame modified.
#'
#' The new variables are called
#' \code{participant_code} and \code{session_code}.
#' @examplesIf rlang::is_installed("withr")
#' # Set data folder first
#' withr::with_dir(system.file("extdata", package = "gmoTree"), {
#'
#' # Import all oTree files in this folder and its subfolders
#' oTree <- import_otree()
#' })
#'
#' # Show all Chats column names
#' print(colnames(oTree$Chats))
#'
#' # Run function
#' oTree <- messy_chat(oTree, combine = TRUE)
#'
#' # Show all Chats column names again
#' print(colnames(oTree$Chats))

#' @export
messy_chat <- function(oTree,
                       combine = FALSE,
                       session = TRUE,
                       participant = TRUE,
                       info = FALSE) {

  stop_messages <- character(0L)
  warning_messages <- character(0L)

  # Background calculations plus error and warning messages
  if (session) {
    length_chat_vars <- sum(
      c("session_code",
        "participant__session__code") %in% colnames(oTree$Chats))

    if (length_chat_vars > 1L && !combine) {
        stop_messages <-
          c(stop_messages,
             paste0(
             "More than one variable referred to the session code ",
             "in your Chats data frame. This could be because ",
             "you mixed data of different ",
             "versions of oTree in your data frame. Please combine ",
             "them into one variable called either \"session_code,\" or ",
             "\"participant__session__code\". ",
             "You can do this by using the combine-argument in this ",
             "function."))

    } else if (length_chat_vars > 1L &&
               combine) {
      oTree$Chats$session_code[is.na(oTree$Chats$session_code)] <-
        oTree$Chats$participant__session__code[is.na(oTree$Chats$session_code)]

      oTree$Chats$participant__session__code <- NULL

      warning_messages <-
        c(warning_messages,
          paste0("More than one variable referred to ",
                 "the session code in ",
                 "your Chats data frame. You asked to combine them with ",
                 "the argument combine = TRUE. ",
                 "Therefore, variable \"participant__session__code\" was ",
                 "integrated into variable ",
                 "\"session_code.\"",
                 " The variable \"participant__session__code\" was ",
                 "deleted afterward."))
    }
  }

  if (participant) {
    length_participant_vars <- sum(
      c("participant_code",
        "participant__code") %in% colnames(oTree$Chats))

    if (length_participant_vars > 1L && !combine) {
        stop_messages <-
          c(stop_messages,
            paste0("More than one variable referred to ",
                    "the participant code ",
                    "in your Chats data frame. This could be because you ",
                    "mixed data of different ",
                    "versions of oTree in your data frame. ",
                    "Please combine them into one ",
                    "variable called either \"participant_code,\" or ",
                    "\"participant__code\". You can do this by using the ",
                    "combine-argument in this ",
                    "function."))
    } else if (length_participant_vars > 1L && combine) {
      oTree$Chats$participant_code[is.na(oTree$Chats$participant_code)] <-
        oTree$Chats$participant__code[is.na(oTree$Chats$participant_code)]
      oTree$Chats$participant__code <- NULL
      warning_messages <-
        c(warning_messages,
        paste0("More than one variable referred to ",
               "the participant code in ",
               "your Chats data frame. You asked to combine them ",
               "with the argument combine = TRUE. ",
               "Therefore, variable \"participant__code\" was integrated ",
               "into variable \"participant_code\"",
               " The variable \"participant__code\" was deleted afterward."))
    }
  }

  if (length(stop_messages) > 0L) {
    stop(paste(stop_messages, collapse  = "\n"))
  }

  if (info && length(warning_messages) > 0L) {
    # This is printed as a warning because other functions catch the message
    warning(paste(warning_messages, collapse = "\n"))
  }

  return(oTree)
}
