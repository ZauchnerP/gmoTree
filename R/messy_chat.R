#' Check if the Chats data frame is messy
#' @description
#' Check if the Chats data frame includes both
#' session-related variables and participant-related variables that
#' appear multiple times. This may occur when data from different
#' oTree versions, which use different variable names, are combined.
#'
#' If desired, the function can merge these variables,
#' storing the data using the newer oTree version's variable names
#' and removing the outdated variables.
#'
#' @keywords oTree
#' @param oTree A list of data frames that were created by import_otree().
#' @param combine Logical. TRUE if all variables referring to
#' the session code should be merged and/or all variables referring
#' to participant code should be merged in case data of several versions
#' of oTree are used.
#' @param session Logical. TRUE if all variables referring to the session code
#' should be checked and merged. Merging only works if combine = TRUE.
#' @param participant Logical. TRUE if all variables referring to the
#' participant code should be checked and merged. Merging only works if
#' combine = TRUE.
#' @param info Logical. TRUE if a brief information on the process should
#' be printed.
#' @returns
#' This function returns an oTree list of data frames that is
#' an exact copy of the original oTree list of data frames but - if the user
#' wishes to do so - combines the participant code and session code
#' variables in the Chats data frame if several variables are referring to
#' those because of the
#' combination of different oTree versions. The final variables are called
#' participant_code and session_code.
#'
#' If combine = FALSE, the function only checks for the existence of several
#' variables referring to the participant code and session code and throws an
#' error if yes.
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

  stop_messages <- c()
  warning_messages <- c()

  # Background calculations plus error and warning messages
  if (session == TRUE) {
    length_chat_vars <- sum(
      c("session_code",
        "participant__session__code") %in% colnames(oTree$Chats))

    if (length_chat_vars > 1 && combine == FALSE) {
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

    } else if (length_chat_vars > 1 && combine == TRUE) {
      oTree$Chats$session_code[is.na(oTree$Chats$session_code)] <-
        oTree$Chats$participant__session__code[is.na(oTree$Chats$session_code)]

      oTree$Chats$participant__session__code <- NULL

      warning_messages <-
        c(warning_messages,
          paste0("More than one variable referred to ",
                 "the session code in ",
                 "your Chats data frame. You asked for combining them with ",
                 "the argument combine = TRUE. ",
                 "Therefore, variable \"participant__session__code\" was ",
                 "integrated into variable ",
                 "\"session_code.\"",
                 " The variable \"participant__session__code\" was ",
                 "deleted afterward."))
    }
  }

  if (participant == TRUE) {
    length_participant_vars <- sum(
      c("participant_code",
        "participant__code") %in% colnames(oTree$Chats))

    if (length_participant_vars > 1 && combine == FALSE) {
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
    } else if (length_participant_vars > 1 && combine == TRUE) {
      oTree$Chats$participant_code[is.na(oTree$Chats$participant_code)] <-
        oTree$Chats$participant__code[is.na(oTree$Chats$participant_code)]
      oTree$Chats$participant__code <- NULL
      warning_messages <-
        c(warning_messages,
        paste0("More than one variable referred to ",
               "the session code in ",
               "your Chats data frame. You asked for combining them ",
               "with the argument combine = TRUE. ",
               "Therefore, variable \"participant__code\" was integrated ",
               "into variable \"participant_code\"",
               " The variable \"participant__code\" was deleted afterward."))
    }
  }

  if (!is.null(stop_messages)) {
    stop(paste(stop_messages, collapse  = "\n"))
  }

  if (info == TRUE && !is.null(warning_messages)) {
    # This is printed as a warning because other functions catch the message
    warning(paste(warning_messages, collapse = "\n"))
  }

  return(oTree)
}
