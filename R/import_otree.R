#' Import oTree data
#' @description
#' Import data files that were created by oTree.
#' All files containing the pattern YYYY-MM-DD at the end
#' of their file names are considered oTree files.
#' Bot outputs are saved by oTree without the date included. Hence, to
#' import bot data, you must either rename the original bot files
#' using the YYYY-MM-DD format or use the argument \code{onlybots = TRUE}.
#' By using the second option, only data of bot files are imported.
#'
#' Caution! Data can be downloaded from within the
#' session and globally at the same time. If both files are downloaded,
#' this can lead to the \code{$all_apps_wide} data being there twice! You can 
#' remove duplicate data by using \code{\link{delete_duplicate}}.
#'
#' Caution! When importing Excel files, this function does not check
#' for erroneous data structures
#' and will combine all data frames with the same file name patterns.
#' Before using the \code{CSV = FALSE} argument, 
#' clean up your data appropriately.
#' @keywords oTree
#' @param final_apps Character.
#' The name(s) of the app(s) at which the participants have to finish the
#' experiment. If the argument final_apps is left empty, you can still call
#' for deleting the participants who did not finish the experiment
#' with \code{\link{delete_dropouts}}.
#' @param final_pages Character.
#' The name(s) of the page(s) at which the participants have to finish the
#' experiment. If the argument final_pages is left empty, you can still
#' call for deleting the participants who did not finish the experiment
#' with \code{\link{delete_dropouts}}.
#' @param encoding Character. Encoding of the CSV files that are imported.
#' Default is \code{"UTF-8"}.
#' @param path Character. The path to the files (default is the
#' working directory).
#' @param recursive Logical. \code{TRUE} if the files in the path's
#' subfolders should also be imported.
#' @param file_names Character. The name(s) of the file(s) to be imported.
#' If not specified, all files in the path and subfolders are imported.
#' @param csv Logical. \code{TRUE} if only CSV files should be 
#' imported. \code{FALSE} if only Excel files should be imported.
#' @param onlybots Logical. \code{TRUE} if only bot-created files 
#' should be imported.
#' @param del_empty Logical. \code{TRUE} if all empty cases should be deleted 
#' from the \code{$all_apps_wide} or normal app data frames (not Time or Chats).
#' @param info Logical. \code{TRUE} if a brief information on the data 
#' import should be printed.
#' @returns
#' Returns a list of data frames (one data frame for each app 
#' and \code{$all_apps_wide}) and a list of information on this list 
#' of data frames in \code{$info}.
#'
#' See detailed information on the imported files 
#' in \code{$info$imported_files}.
#'
#' If \code{$all_apps_wide} is imported, see the number of imported cases
#' in \code{$info$initial_n}. In this number, empty rows are
#' already considered. So, if empty rows are deleted 
#' with \code{del_empty=TRUE}, \code{initial_n} 
#' counts all rows that are not empty.
#' Cases that are deleted because the participants did not make it to the
#' last page and/or app are not subtracted from this number.
#'
#' Information: Empty rows are rows without 
#' the \code{participant._current_app_name}
#' variable set. Empty rows are deleted from all app data frames
#' and \code{$all_apps_wide} when using \code{del_empty = TRUE}. Empty rows in 
#' the \code{$Chats} and \code{$Time} data frames are not deleted.
#'
#' If old and new oTree versions are combined, the \code{$Time} data frame 
#' contains variables called \code{participant_code} 
#' and \code{participant__code} (the difference is in the underscores).
#' Caution! If there is an unusual amount of \code{NA}s,
#' check if everything got imported correctly.
#' Sometimes, the CSV or Excel file may be corrupted, and all information is
#' only found in one column.
#' @examplesIf rlang::is_installed("withr")
#' # Set data folder first
#' withr::with_dir(system.file("extdata", package = "gmoTree"), {
#'
#' # Import all oTree files in this folder and its subfolders
#' oTree <- import_otree()
#'
#' # Show the structure of the import
#' str(oTree, max.level = 1)
#'
#' # Show the names of all imported files
#' oTree$info$imported_files
#'
#' # Delete empty cases and delete every case of a person
#' # who didn't end the experiment in the app "survey"
#' oTree <- import_otree(
#'   del_empty = TRUE,
#'   final_apps = "survey",
#'   info = TRUE)
#'
#' # Show the structure of the import
#' str(oTree, max.level = 1)
#'
#' # Import bot files
#' import_otree(
#'   path = "./bot_data",
#'   onlybots = TRUE,
#'   csv = TRUE,
#'   info = TRUE)
#'
#' # Show the structure of the import
#' str(oTree, max.level = 1)
#'
#' # Import with file names (path separately)
#' oTree2 <- import_otree(
#'      del_empty = TRUE,
#'      path = "./exp_data",
#'      file_names = c("all_apps_wide-2023-03-27.csv",
#'                    "ChatMessages-2023-03-27.csv",
#'                    "PageTimes-2023-03-27.csv"),
#'      onlybots = FALSE,
#'      csv = TRUE,
#'      info = TRUE)
#'
#' # Show the structure of the import
#' str(oTree, max.level = 1)
#'
#' # Import with file names (without path separately)
#' oTree2 <- import_otree(
#'      del_empty = TRUE,
#'      file_names = c("exp_data/all_apps_wide-2023-03-27.csv",
#'                    "exp_data/ChatMessages-2023-03-27.csv",
#'                    "exp_data/PageTimes-2023-03-27.csv"),
#'      onlybots = FALSE,
#'      csv = TRUE,
#'      info = TRUE)
#'
#' # Show the structure of the import
#' str(oTree, max.level = 1)
#' })

# Info: New oTree versions (at least 5.10.3) don't save
# their data with Excel files anymore.
# It does not matter whether you download files with
# the "Excel" or "CSV" version,
# you get CSV files in each case. It does not seem as if
# there are not any differences between the downloaded csv-files.

#' @export
import_otree <- function(
    path = ".",
    file_names = NULL,
    final_apps = NULL,
    final_pages = NULL,
    recursive = TRUE,
    csv = TRUE,
    onlybots = FALSE,
    del_empty = TRUE,
    info = FALSE,
    encoding = "UTF-8"
    ) {


  # Make oTree list
  oTree <- list()

  # Specify type of files
  if (onlybots) {
    csv <- TRUE
  }

  # Make messages
  errorfiles <- data.frame(file = character(0L), 
                           content = character(0L))
  warningfiles <- data.frame(file = character(0L), 
                             content = character(0L))
  time_message <- character(0L)
  chat_message <- character(0L)
  other_messages <- character(0L)

  # Define path
  if (!is.null(path)) {
    # Change Windows paths to paths that can be read by Ubuntu
    path2 <- gsub("\\\\", "/", path)
  } else {
    stop("Path must not be NULL!")
  }

  # If path in file names:
  # Change Windows paths to paths that can be read by Ubuntu
  if (!is.null(file_names)) {
    file_names <- gsub("\\\\", "/", file_names)
  }

  # Check if path(s) exist(s)
  for (i in path2) {
    if (!dir.exists(i)) {
      stop("This path does not exist: ", i)
    }
  }

  # oTree pattern handling
  if (onlybots) {
    pattern_definer <- ""  # For regex search later
  } else {
    if (csv) {
      # For regex search later
      # The second part refers to Chat and Time and is always csv
      pattern_definer <-
        "[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv|[0-9]{4}-[0-9]{2}-[0-9]{2}\\)\\.csv"
    } else {
      pattern_definer <-
        "[0-9]{4}-[0-9]{2}-[0-9]{2}\\.xlsx|[0-9]{4}-[0-9]{2}-[0-9]{2}\\)\\.csv"
    }
  }

  # List all file names if none are specified  ####
  if (is.null(file_names)) {

    # Get all file names
    all_file_names <- list.files(
      path = path2,
      pattern = pattern_definer,
      full.names = TRUE,
      recursive = recursive
    )
  } else if (!is.null(file_names)) {
    # List all file names if they are specified  ####
    all_file_names <- paste0(path2, "/", file_names)
  }

  # Stop if there are no files  ####
  if (length(all_file_names) == 0L || 
      is.null(all_file_names)) {
    stop("No files to import! ",
                "Did you specify the CSV argument correctly? ",
                "Is the directory correctly specified? ?\n",
                "The directory is: ", path2
    )
  }

  # Make app-names to file names (= all file names without path and time)  ####
  app_filedf <- data.frame(app = all_file_names,
                           file = all_file_names)

  # Take path away
  app_filedf$app <- gsub(".*/", "", app_filedf$app)
  app_filedf$app <- gsub(".*\\\\", "", app_filedf$app)

  # Delete dates from file names to get app names
  if (pattern_definer != "") {
    app_filedf$app <- stringr::str_remove(app_filedf$app,
                                          pattern_definer)
  }

  # Special all apps wide definer
  app_filedf$app <- stringr::str_remove(app_filedf$app,
                                        "-[0-9]{4}-[0-9]{2}-[0-9]{2}")

  # Remove file name extension
  app_filedf$app <- stringr::str_remove(app_filedf$app,
                                        ".xlsx")
  app_filedf$app <- stringr::str_remove(app_filedf$app,
                                        ".csv")

  # Remove access information for Chats and Time
  app_filedf$app <-
    stringr::str_remove(app_filedf$app,
                        ".\\(accessed.[0-9]{4}-[0-9]{2}-[0-9]{2}\\)")
  # Info: The dot is only there to ensure portable file names for the examples!
  # Was \\s* before

  # Remove final underscore
  app_filedf$app <- stringr::str_remove(app_filedf$app, "_$")

  app_filedf$app <- stringr::str_remove(app_filedf$app, "-$")

  # Special handling of Time and Chats
  app_filedf$app <- gsub("ChatMessages.*", "Chats", app_filedf$app)
  app_filedf$app <- gsub("PageTimes.*", "Time", app_filedf$app)
  app_filedf$app <- gsub("Chat.messages.*", "Chats", app_filedf$app)
  # Info: The dot is only there to ensure portable file names for the examples!
  app_filedf$app <- gsub("TimeSpent.*", "Time", app_filedf$app)

  # Sort app-names
  appnames <- unique(app_filedf$app)
  appnames <- appnames[order(appnames)]

  ##########################################################

  # Import all data except time and chat  ####

  # Import normal apps and all apps wide  ####
  app_list <- unique(app_filedf$app)

  for (App in app_list) {

    # Import files  ####
    if (App != "Chats" &&
        App != "Time") {

      # Get file names  ####
      allAppsFilesWP <- app_filedf$file[app_filedf$app == App]

      # Import files  ####
      if (!csv) {

        # Import all Excel files for the App  ####

        # Caution: Error management does not work that well.
        # If an excel file is empty or faulty,
        # it is still added to the data frame. Reason: The read.xlsx
        # function works that way. + rbind.fill adds NA to all empty cells.

        for (i in seq_along(allAppsFilesWP)) {
          new <- NULL  # Future file data frame

          tryCatch({
            withCallingHandlers({

                # Read data
                new <- openxlsx::read.xlsx(file.path(allAppsFilesWP[i]),
                                           sheet = 1L)

                # If data is there: Add data to data frame + info about it

                if (!is.null(new) & nrow(new) > 0L) {

                  oTree[[App]] <- plyr::rbind.fill(new, oTree[[App]])

                  oTree[["info"]][["imported_files"]] <- c(
                    toString(allAppsFilesWP[i]),
                    oTree[["info"]][["imported_files"]]
                  )
                }
              }, warning = function(w) {

                # Append error message
                warningfiles <<- rbind(
                  warningfiles,
                  data.frame(file = allAppsFilesWP[i],
                             content = as.character(w)))

                invokeRestart("muffleWarning")

              })}, error = function(e) {
                # If the data frame is empty, there is no
                # error shown!

                # Append error message
                errorfiles <<-
                  rbind(errorfiles,
                    data.frame(file = allAppsFilesWP[i],
                               content = as.character(e)))
              }
            )
          }
          # Info: That's so complicated, because tryCatch does not
          # continue after warnings and withCallingHandlers throws errors

        } else if (csv) {

        # Import all CSV files for the App  ####
        for (i in seq_along(allAppsFilesWP)) {
          new <- NULL  # Temporary file data frame

          tryCatch({
            withCallingHandlers(
              {
                # Read data
                new <- utils::read.csv(
                  allAppsFilesWP[i],
                  encoding = encoding,
                  header = TRUE)

                # If data is there: Add data to data frame + info about it

                if (!is.null(new) && nrow(new) > 0L) {

                  oTree[[App]] <- plyr::rbind.fill(new, oTree[[App]])

                  oTree[["info"]][["imported_files"]] <- c(
                    toString(allAppsFilesWP[i]),
                    oTree[["info"]][["imported_files"]]
                  )
                }
              }, warning = function(w) {

                # Append warning message
                warningfiles <<- rbind(warningfiles,
                                       data.frame(file = allAppsFilesWP[i],
                                                  content = as.character(w)))
                invokeRestart("muffleWarning")
              }
           )}, error = function(e) {

             # Append error message
             errorfiles <<- rbind(errorfiles,
                                  data.frame(file = allAppsFilesWP[i],
                                             content = as.character(e)))
           })
        }
      }

      # Delete empty  ####
      if (del_empty) {
          oTree[[App]] <- oTree[[App]][
            !(is.na(oTree[[App]]$participant._current_app_name) |
                oTree[[App]]$participant._current_app_name == "<NA>" |
                oTree[[App]]$participant._current_app_name == ""), ]
      }
    }
  }

  # All apps wide handling   ####
  # Get warning message if AAW is saved room-specific and globally
  if (("All apps - wide" %in% names(oTree) &&
       "all_apps_wide" %in% names(oTree)) ||
      (any(grepl("all_apps_wide-", oTree[["info"]][["imported_files"]])) &&
       any(grepl("all_apps_wide_", oTree[["info"]][["imported_files"]]))
      )) {

    warning(
      "You have stored all_apps_wide ",
      "globally but also room-specific. ",
      "This function will import both of them. ",
      "(Globally, the files are saved as \"all_apps_wide_.\" ",
      "Room-specific, the files are saved as \"All apps - wide-\" or ",
      "\"all_apps_wide-.\") ",
      "After importing the data, ",
      "make sure nothing is there twice! ",
      "(Advice: You may use delete_duplicate() to ",
      "remove duplicate rows of all oTree data frames."
    )
  }

  # Combine possible AAWs (will be NULL if none are there)
  oTree[["all_apps_wide"]] <- plyr::rbind.fill(
    oTree[["all_apps_wide"]],
    oTree[["All apps - wide"]]
  )
  oTree[["All apps - wide"]] <- NULL

  # Import time  ####

  # List all time files
  time_files <- app_filedf$file[app_filedf$app == "Time"]

  # Sort it
  time_files <- sort(time_files)

  # Import time data
  if (length(time_files) != 0L) {

    for (i in seq_along(time_files)) {
      new <- NULL  # Future file data frame

      tryCatch({
        withCallingHandlers(
        {
          # Read data
          new <- utils::read.csv(time_files[i],
                                 sep = ",",
                                 header = TRUE,
                                 encoding = encoding)

          # If data is there: Add data to data frame + info about it
          if (!is.null(new) && nrow(new) > 0L) {
            oTree[["Time"]] <- plyr::rbind.fill(new, oTree[["Time"]])

            oTree[["info"]][["imported_files"]] <- c(
              toString(time_files[i]),
              oTree[["info"]][["imported_files"]])
          }
        }, warning = function(w) {
          # Append warning message
          warningfiles <<- rbind(warningfiles,
                                 data.frame(file = time_files[i],
                                            content = as.character(w)))
          invokeRestart("muffleWarning")
        })
      }, error = function(e) {

        # Append error message
        errorfiles <<- rbind(errorfiles,
                             data.frame(file = time_files[i],
                                        content = as.character(e)))
      })
    }
  }

  # Import chat data ####

    # When importing data from Excel, be aware that if a participant uses
    # special characters, their data in one row might be split and also shown
    # on the next line. This can cause the information on the next line to be
    # overwritten by new information, which can lead to issues like missing
    # time stamps for certain entries.

    chat_files <- app_filedf$file[app_filedf$app == "Chats"]
    chat_files <- sort(chat_files)

    # Import/reading data: "Chats"
    if (length(chat_files) != 0L) {

      for (i in seq_along(chat_files)) {
        new <- NULL  # Future file data frame

        tryCatch({
          withCallingHandlers(
          {
            # Read data
            new <- utils::read.csv(chat_files[i],
                                   sep = ",",
                                   header = TRUE,
                                   encoding = encoding)

            # If data is there: Add data to data frame + info about it
            if (!is.null(new) & nrow(new) > 0L) {
              oTree[["Chats"]] <- plyr::rbind.fill(new, oTree[["Chats"]])

              oTree[["info"]][["imported_files"]] <- c(
                toString(chat_files[i]),
                oTree[["info"]][["imported_files"]])
            }

          }, warning = function(w) {
            warningfiles <<- rbind(warningfiles,
                               data.frame(file = chat_files[i],
                                          content = as.character(w)))
            invokeRestart("muffleWarning")
          })

        }, error = function(e) {
          errorfiles <<- rbind(errorfiles,
                               data.frame(file = chat_files[i],
                                          content = as.character(e)))
        })
        # Info: That's so complicated, because tryCatch does not
        # continue after warnings and withCallingHandlers throws errors
      }
    }

  # Initial N   ####
  if ("all_apps_wide" %in% names(oTree)) {
    oTree$info$initial_n <- nrow(oTree$all_apps_wide)

  } else {
    my_messages <- paste0("Import successful, but all_apps_wide is not there. ",
                   "This might affect other functions in this package.")
    # e.g. make_ids
  }

  # Delete dropouts  ####
  if (!(is.null(final_apps) && is.null(final_pages))) {

    # Run delete_dropouts with withCallingHandlers to catch the message
    oTree <- withCallingHandlers({
      delete_dropouts(oTree,
                      final_apps = final_apps,
                      final_pages = final_pages,
                      info = TRUE)},
      message = function(c) {
        other_messages <<- c$message
        invokeRestart("muffleMessage")
      }
    )
  }

  # Messages  ####
  if (info) {

      numapps <- length(oTree)
      numapps <- ifelse("Time" %in% names(oTree), numapps - 1L, numapps)
      numapps <- ifelse("Chats" %in% names(oTree), numapps - 1L, numapps)
      numapps <- ifelse("info" %in% names(oTree), numapps - 1L, numapps)

      my_messages <- c(paste0("Imported: ",
                              numapps,
                              " app(s) and/or all_apps_wide"))

      if (!is.null(errorfiles) && 
          !nrow(errorfiles) == 0L) {

        # Make error messages
        errorfiles$pasteresult <- paste0("File: ",
                                         errorfiles$file,
                                         ": ",
                                         errorfiles$content)

        errormessages <- paste0("Errors when importing these files:\n",
                                paste(collapse = "\n", errorfiles$pasteresult))

        # Throw an error if there is nothing else in the oTree list
        if (length(oTree) == 0L) {
          stop(errormessages)
        }

        # Combine with other messages
        other_messages <- c(other_messages, errormessages)
      }

      # First check if the file is not already in errorfiles
      warningfiles <- warningfiles[!(warningfiles$file %in% errorfiles$file), ]

      if (!is.null(warningfiles) && !(nrow(warningfiles) == 0L)) {
        # Make warning message
        warningfiles$pasteresult <- paste0("File: ", warningfiles$file,
                                           ": ", warningfiles$content)

        # Combine with other messages
        other_messages <- c(
            paste0(
              "Warnings when importing these files (and other warnings):\n",
              paste(collapse = "\n", warningfiles$pasteresult),
              other_messages))
      }

      if ("Time" %in% names(oTree)) {
        time_message <- paste0("Imported: Time file(s)")
      } else {
        time_message <- "No Time files available"
      }

      if ("Chats" %in% names(oTree)) {
        chat_message <- paste0("Imported: Chat file(s)")
      } else {
        chat_message <- "No chat files available"
      }

      my_messages <- paste0(my_messages, "\n",
                           time_message, "\n",
                           chat_message, "\n",
                           paste(other_messages, collapse = "\n"))
      message(my_messages)
  }

  # Return  #####
  return(oTree)
}
