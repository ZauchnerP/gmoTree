if (rlang::is_installed(c("withr", "testthat"))) {

      # Imports that will be used later  ####
      # Must be outside of testthat, so that they can be referred to later
      suppressWarnings({
        otree_2_2_4 <- import_otree(
          del_empty = TRUE,
          testthat::test_path("testdata", "exp_data_2.2.4"),
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        otree_5_4_0 <- import_otree(
          del_empty = TRUE,
          path = "./testdata/exp_data_5.4.0",
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)
        # Alternative: # file.path(".", "exp_data_5.4.0"),

        otree_5_4_0_non_unique <- otree_5_4_0
        otree_5_4_0 <- delete_duplicate(otree_5_4_0)

        otree_old_one <- import_otree(
          del_empty = TRUE,
          path = ".\\testdata\\old_one",
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)
        # Alternative: # file.path(".", "old_one"),

        otree_all <- import_otree(
          del_empty = TRUE,
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        otree_new_empty <- import_otree(
          del_empty = FALSE,
          path = ".\\testdata\\exp_data",
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)
        # Alternative:  # file.path(".", "exp_data"),

      })

      testthat::test_that("Import - Excel", {

        file_names <- list.files(
          path = testthat::test_path("testdata", "exp_data_2.1.0"),
          pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}.xlsx")

        # Run function
        testthat::expect_message(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata", "exp_data_2.1.0"),
            onlybots = FALSE,
            csv = FALSE,
            info = TRUE), "Imported:.*Errors when importing these files")

        # Test
        testthat::expect_output(str(otree2), "List of 8")
        testthat::expect_vector(otree2$info$imported_files)
        # (Plus two because of Chat and Time!)
        test1 <- length(otree2$info$imported_files)  == 7L
        # Don't do that because there are also
        # faulty files: length(file_names) + 2
        testthat::expect_true(test1)
      })

      testthat::test_that("Import - subapp name test1", {
        # Run function
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata", "exp_data_5.4.0subapp"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE),
          "globally but also room-specific")

        # Test
        test1 <- "2chatapp" %in% names(otree2)
        test2 <- "chatapp" %in% names(otree2)
        test3 <- "dictator" %in% names(otree2)
        test4 <- "dictator2" %in% names(otree2)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Import - delete dropouts", {
        # Run functions

        # Delete dropouts in two steps
        testthat::expect_warning(
          otree1 <- import_otree(
            del_empty = TRUE,
            path = ".\\testdata\\exp_data_5.4.0",
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE), "globally")

        testthat::expect_message(
          otree2 <- delete_dropouts(otree1,
                                    final_apps = "survey",
                                    info = TRUE),
          "Dropouts are deleted"
        )

        # Delete dropouts in one step
        suppressWarnings({
          testthat::expect_message(
            otree3 <- import_otree(
              del_empty = TRUE,
              path = ".\\testdata\\exp_data_5.4.0",
              onlybots = FALSE,
              csv = TRUE,
              final_apps = "survey",
              info = TRUE),
            "Dropouts are deleted from all data frames.")
        })

        #  No message should be shown if info = FALSE
        suppressWarnings({
          testthat::expect_message(
            otree3 <- import_otree(
              del_empty = TRUE,
              path = ".\\testdata\\exp_data_5.4.0",
              onlybots = FALSE,
              csv = TRUE,
              final_apps = "survey",
              info = FALSE), NA)
        })

        # Test if dropouts were deleted
        test1 <- nrow(otree1$all_apps_wide) > nrow(otree2$all_apps_wide)
        test2 <- nrow(otree1$dictator) > nrow(otree2$dictator)
        test3 <- nrow(otree1$survey) > nrow(otree2$survey)
        test4 <- nrow(otree1$Chats) == nrow(otree2$Chats) # Must be equal!
        test5 <- nrow(otree1$Time) > nrow(otree2$Time)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test if after-dropout numbers are the same
        test1 <- nrow(otree3$all_apps_wide) == nrow(otree2$all_apps_wide)
        test2 <- nrow(otree3$dictator) == nrow(otree2$dictator)
        test3 <- nrow(otree3$survey) == nrow(otree2$survey)
        test4 <- nrow(otree3$Chats) == nrow(otree2$Chats)
        test5 <- nrow(otree3$Time) == nrow(otree2$Time)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))
      })

      testthat::test_that("Import - bot files", {
        # Run function
        otree_bot <- import_otree(
          del_empty = TRUE,
          path = file.path("testdata", "bot_data"),
          onlybots = TRUE,
          csv = TRUE,
          info = FALSE)

        # Test
        testthat::expect_identical(
          unique(otree_bot$all_apps_wide$participant._is_bot), 1L)
      })

      testthat::test_that("Import - all info FALSE", {
        # Run function
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = TRUE,
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE),
          "globally but also room-specific")

        # Test
        testthat::expect_output(str(otree2), "List of 10")
        # [1] "all_apps_wide" "info"          "chatapp"       "dictator"
        # [5] "start"         "survey"        "Time"          "Chats"
      })

      # Import data tests  ####
      testthat::test_that("Import - dropouts", {
        # Run function
        testthat::expect_message(
          testthat::expect_warning(
            otree2 <- import_otree(
              del_empty = TRUE,
              onlybots = FALSE,
              csv = TRUE,
              info = TRUE), "globally but also room-specific"),
          "Imported:.*Errors when importing these files")

        # Test
        testthat::expect_output(str(otree2), "List of 10")
      })

      testthat::test_that("Import - all", {

        # Del_empty = TRUE
        testthat::expect_message(
          testthat::expect_warning(
            otree1 <- import_otree(
              del_empty = TRUE,
              onlybots = FALSE,
              csv = TRUE,
              info = TRUE
            ), "globally but also room-specific"),
          "Imported:.*Errors when importing")

        # Del_empty = FALSE
        testthat::expect_message(
          testthat::expect_warning(
            otree2 <- import_otree(
              del_empty = FALSE,
              onlybots = FALSE,
              csv = TRUE,
              info = TRUE
            ), "globally but also room-specific"),
          "Imported:.*Errors when importing")

        # Test
        testthat::expect_output(str(otree2),
                                "List of 10")
        # "start", "dictator", "chatapp", "survey",
        # "all_apps_wide", "Time", "Chats"

        diff <- nrow(otree2$all_apps_wide[
          otree2$all_apps_wide$participant._current_app_name == "", ])
        test1 <- (otree2$info$initial_n - otree1$info$initial_n) == diff

        testthat::expect_true(test1)
      })

      testthat::test_that("Import - subapp name test2 ", {
        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = testthat::test_path("testdata", "exp_data_5.4.0subapp"),
          file_names = c("2chatapp_2023-05-16.csv",
                         "chatapp_2023-05-16.csv",
                         "dictator2_2023-05-16.csv",
                         "dictator_2023-05-16.csv"),
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        # Test
        test1 <- "2chatapp" %in% names(otree2)
        test2 <- "chatapp" %in% names(otree2)
        test3 <- "dictator" %in% names(otree2)
        test4 <- "dictator2" %in% names(otree2)
        testthat::expect_true(all(c(
          test1, test2, test3, test4)))
      })

      testthat::test_that("Import - empty rows check", {
        # Run function
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = FALSE,
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE), "globally but also room-specific")

        testthat::expect_warning(
          otree3 <- import_otree(
            del_empty = TRUE,
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE), "globally but also room-specific")

        # Test
        testthat::expect_gt(nrow(otree2$all_apps_wide),
                            nrow(otree3$all_apps_wide))
        testthat::expect_gt(nrow(otree2$dictator),
                            nrow(otree3$dictator))
        testthat::expect_gt(nrow(otree2$survey),
                            nrow(otree3$survey))
        testthat::expect_identical(nrow(otree2$Time),
                                   nrow(otree3$Time))
        testthat::expect_identical(nrow(otree2$Chats),
                                   nrow(otree3$Chats))
      })

      testthat::test_that("Import - with 2 paths", {
        # Run function
        otree1 <- import_otree(
          del_empty = TRUE,
          path = c(testthat::test_path("testdata", "exp_data_2.2.4")),
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)  # No warning here

        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata", "exp_data_5.4.0"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE),
          " stored all_apps_wide globally but also room-specific")

        testthat::expect_warning(
          otree3 <- import_otree(
            del_empty = TRUE,
            path = c(testthat::test_path("testdata", "exp_data_2.2.4"),
                     testthat::test_path("testdata", "exp_data_5.4.0")),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE
          ), " stored all_apps_wide globally but also room-specific")

        # Test
        test1 <- nrow(otree1$all_apps_wide) +
          nrow(otree2$all_apps_wide) ==
          nrow(otree3$all_apps_wide)
        testthat::expect_true(test1)
      })

      testthat::test_that("Import - with file_names(path included)", {
        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          file_names = c("testdata\\exp_data_5.4.0\\all_apps_wide-2023-05-16.csv",
                         "testdata\\exp_data_5.4.0\\ChatMessages-2023-05-16.csv",
                         "testdata\\exp_data_5.4.0\\PageTimes-2023-05-16.csv"),
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        test1 <- ("initial_n" %in% names(otree2$info))
        testthat::expect_true(test1)
        test2 <- length(otree2$info$imported_files) == 3L
        testthat::expect_true(test2)
      })

      testthat::test_that("Import - with file_names- one warning", {
        # Test if there is no warning if "all apps wide" is saved with -
        # and not with _ before the date. (as it should be)

        # Run function
        expect_warning(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = "testdata",
            file_names = c("exp_data\\all_apps_wide-2023-03-27.csv",
                           "exp_data\\ChatMessages-2023-03-27.csv",
                           "exp_data\\PageTimes-2023-03-27.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE), NA)

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        test1 <- ("initial_n" %in% names(otree2$info))
        testthat::expect_true(test1)
        test2 <- length(otree2$info$imported_files) == 3L
        testthat::expect_true(test2)
      })

      testthat::test_that("Import - only Chats", {
        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = "testdata",
          file_names = "exp_data_5.4.0/ChatMessages-2023-05-16.csv",
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        # Test
        test <- "Chats" %in% names(otree2)
        testthat::expect_true(test)
      })

      testthat::test_that("Import - only time", {
        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = testthat::test_path("testdata", "exp_data_5.4.0"),
          file_names = "PageTimes-2023-05-16.csv",
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        # Test
        test <- "Time" %in% names(otree2)
        testthat::expect_true(test)
      })

      testthat::test_that("Import - only Chats not faulty", {
        # Run function
        testthat::expect_message(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata"),
            file_names = c("exp_data_5.4.0/all_apps_wide-2900-05-16.csv",
                           "exp_data_5.4.0/ChatMessages-2023-05-16.csv",
                           "exp_data_5.4.0/PageTimes-2900-05-16.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE),
          "Imported: 0 app.*No Time fil.*Imported: Chat file.*Errors when importing")

        # Test
        test0 <- !is.null(otree2$info)
        test1 <- !("initial_n" %in% names(otree2$info))  # Not available without AAW
        test2 <- length(otree2$info$imported_files) == 1L
        test3 <- ("Chats" %in% names(otree2))
        testthat::expect_true(all(c(test0, test1, test2, test3)))
      })

      testthat::test_that("Import - only TIME not faulty", {
        # Run function
        testthat::expect_message(
          otree2 <- import_otree(
            del_empty = TRUE,
            testthat::test_path("testdata"),
            file_names = c("exp_data_5.4.0/all_apps_wide-2900-05-16.csv",
                           "exp_data_5.4.0/ChatMessages-2090-05-16.csv",
                           "exp_data_5.4.0/PageTimes-2023-05-16.csv",
                           "exp_data_5.4.0/PageTimes-2900-05-16.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE),
          "Imported: 0 app.*Imported: Time fil.*No chat files available.*Errors when")

        # Test
        test1 <- !("initial_n" %in% names(otree2$info))  # Not available without AAW
        test2 <- length(otree2$info$imported_files) == 1L
        test3 <- ("Time" %in% names(otree2))
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Import - with some faulty file_names", {
        # Run function
        message <- capture_messages(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata"),
            file_names = c(
              "exp_data_5.4.0/all_apps_wide-2023-05-16.csv",
              "exp_data_5.4.0/all_apps_wide-2023-05-96.csv",  # -
              "exp_data_5.4.0/dictator_2023-05-16.csv",
              "exp_data_5.4.0/ChatMessages-2900-05-16.csv",  # -
              "exp_data_5.4.0/ChatMessages-2900-05-26.csv",  # -
              "exp_data_5.4.0/PageTimes-2900-05-16.csv",   # -
              "exp_data_5.4.0/PageTimes-2901-05-16.csv",   # -
              "exp_data_5.4.0/PageTimes-2023-05-16.csv",
              "exp_data_5.4.0/ChatMessages-2023-05-16.csv"
            ),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE))

        testthat::expect_true(
          grepl(pattern = "Imported:.*Errors when importing.*cannot open the connection",
                x = message)
        )

        # Test
        test1 <- ("initial_n" %in% names(otree2$info))
        test2 <- length(otree2$info$imported_files) == 4L
        test3 <- "dictator" %in% names(otree2)
        test4 <- "Chats" %in% names(otree2)
        test5 <- "Time" %in% names(otree2)
        test6 <- "all_apps_wide" %in% names(otree2)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6)))
      })

      testthat::test_that("Import - with file_names(path included)", {
        # Run function
        message <- capture_messages(
          otree2 <- import_otree(
            del_empty = TRUE,
            file_names = c("testdata/exp_data_5.4.0/all_apps_wide-2023-05-16.csv",
                           "testdata/exp_data_5.4.0/ChatMessages-2023-05-16.csv",
                           "testdata/exp_data_5.4.0/PageTimes-2023-05-16.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE))

        testthat::expect_true(
          grepl(pattern = "Imported",
            x = message))

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        test1 <- ("initial_n" %in% names(otree2$info))
        test2 <- length(otree2$info$imported_files) == 3L
        testthat::expect_true(test1)
        testthat::expect_true(test2)
      })

      testthat::test_that("Import - specific csv without all apps wide", {
        file_names <- c(
          "dictator_2023-03-27.csv",
          "ChatMessages-2023-03-27.csv",
          "PageTimes-2023-03-27.csv")

        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = testthat::test_path("testdata", "exp_data"),
          file_names = file_names,
          onlybots = FALSE,
          csv = TRUE,
          recursive = TRUE,
          info = FALSE)

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        # Test if there is no initial_n (should be omitted if there is no aaw!)
        test1 <- !("initial_n" %in% names(otree2$info))
        testthat::expect_true(test1)
        test2 <- length(otree2$info$imported_files) == length(file_names)
        testthat::expect_true(test2)
      })

      testthat::test_that("Import - without all apps wide xlsx", {
        file_names <- c(
          "dictator_2023-05-16.xlsx",
          "Chat-messages-(accessed-2023-05-16).csv",
          "TimeSpent-(accessed-2023-05-16).csv")

        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = testthat::test_path("testdata", "exp_data_2.1.0"),
          file_names = file_names,
          onlybots = FALSE,
          csv = FALSE,
          recursive = TRUE,
          info = FALSE)

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        # Test if there is no initial_n (should be omitted if there is no aaw!)
        test1 <- !("initial_n" %in% names(otree2$info))
        test2 <- length(otree2$info$imported_files) == length(file_names)
        test3 <- "dictator" %in% names(otree2)
        test4 <- "Chats" %in% names(otree2)
        test5 <- "Time" %in% names(otree2)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))
      })

      testthat::test_that("Import - with file names", {
        file_names <- c("all_apps_wide-2023-03-27.csv",
                        "ChatMessages-2023-03-27.csv",
                        "PageTimes-2023-03-27.csv")

        # Run function
        otree2 <- import_otree(
          del_empty = TRUE,
          path = testthat::test_path("testdata", "exp_data"),
          file_names = file_names,
          onlybots = FALSE,
          csv = TRUE,
          info = FALSE)

        # Test
        testthat::expect_output(str(otree2), "List of 4")
        test1 <- "initial_n" %in% names(otree2$info)
        test2 <- length(otree2$info$imported_files) == length(file_names)
        test3 <- "all_apps_wide" %in% names(otree2)
        test4 <- "Chats" %in% names(otree2)
        test5 <- "Time" %in% names(otree2)
        testthat::expect_true(all(c(test1, test3, test4, test5)))
      })

      testthat::test_that("Import (e) - no files", {
        # Run function and test
        testthat::expect_error(import_otree(
          del_empty = TRUE,
          path =  testthat::test_path("testdata", "empty"),
        ), "No files to import")
      })

      testthat::test_that("Import (e) - file_names(empty path)", {
        # Run function
        testthat::expect_error(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = NULL,
            file_names = c("exp_data_5.4.0\\all_apps_wide-2023-05-16.csv",
                           "exp_data_5.4.0\\ChatMessages-2023-05-16.csv",
                           "exp_data_5.4.0\\PageTimes-2023-05-16.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE), "Path must not be NULL!")
      })

      testthat::test_that("Import (e) - csv - all faulty file_names", {
        # Run function and test
        # This error message is only shown because all of them are faulty

        testthat::expect_error(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata"),
            file_names = c("exp_data_5.4.0/all_apps_wide-2900-05-16.csv",
                           "exp_data_5.4.0/ChatMessages-2900-05-16.csv",
                           "exp_data_5.4.0/PageTimes-2900-05-16.csv"),
            onlybots = FALSE,
            csv = TRUE,
            info = TRUE), "Errors when importing these files")
      })

      testthat::test_that("Import (e) - xlsx - all faulty file_names", {
        # Run function and test
        # This error message is only shown because all of them are faulty

        testthat::expect_error(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = testthat::test_path("testdata"),
            file_names = c("exp_data_5.4.0/all_apps_wide-2900-05-16.xlsx",
                           "exp_data_5.4.0/dictator-2900-05-16.xlsx",
                           "exp_data_5.4.0/ChatMessages-2900-05-16.csv",
                           "exp_data_5.4.0/PageTimes-2900-05-16.csv"),
            onlybots = FALSE,
            csv = FALSE,
            info = TRUE), "Errors when importing these files")
      })



      testthat::test_that("Import (e) - xlsx - some faulty file_names", {
        # Run function
        testthat::expect_message(
          otree2 <- import_otree(
            del_empty = TRUE,
            path = "testdata",
            file_names = c("./exp_data_2.1.0/all_apps_wide-2900-05-16.xlsx",
                           "./exp_data_2.1.0/all_apps_wide_2023-05-16.xlsx",
                           "./exp_data_2.1.0/dictator_2023-05-16.xlsx",
                           "./exp_data_2.1.0/dictator-2900-05-16.xlsx",
                           "./exp_data_2.1.0/start-2900-05-16.xlsx",
                           "./exp_data_2.1.0/ChatMessages-2900-05-16.csv",
                           "./exp_data_2.1.0/Chat-messages-(accessed-2023-05-16).csv",
                           "./exp_data_2.1.0/TimeSpent-(accessed-2023-05-16).csv",
                           "./exp_data_2.1.0/PageTimes-2900-05-16.csv"),
            onlybots = FALSE,
            csv = FALSE,
            info = TRUE), "Errors when importing these files")

        # Test
        test1 <- !("start" %in% names(otree2))
        test2 <- "dictator" %in% names(otree2)
        test3 <- "Time" %in% names(otree2)
        test4 <- "Chats" %in% names(otree2)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Import (w) - erroneous files", {
        # Dictator file is faulty

        # Run function
        testthat::expect_warning(
          testthat::expect_message(
            otree2 <- import_otree(
              info = TRUE,
              path =  testthat::test_path("testdata", "exp_wrong_data")),
            "Errors when importing these files"),
          "stored all_apps_wide globally but also room-specific")

        # Test
        test2 <- "survey" %in% names(otree2)
        test3 <- !("dictator" %in% names(otree2))
        test4 <- "Chats" %in% names(otree2)
        test5 <- "Time" %in% names(otree2)
        test6 <- "all_apps_wide" %in% names(otree2)
        test7 <- !(any(grepl("dictator", otree2$info$imported_files)))
        testthat::expect_true(all(c(test2, test3, test4, test5, test6, test7)))
      })

      testthat::test_that("Import (e) - path", {
        # Run function and test
        testthat::expect_error(import_otree(
          del_empty = TRUE,
          path = "xyz",
          onlybots = FALSE,
          csv = TRUE,
          info = TRUE), "This path does not exist")
      })

      print("---- delete_duplicate -----")

      # Delete duplicate  ####
      testthat::test_that("Delete duplicate", {
        # Prepare data
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = TRUE,
            path =  testthat::test_path("testdata", "exp_data_5.4.0"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE))
        # Alternative: # file.path(".", "exp_data_5.4.0"),

        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        before_n <- otree2$info$initial_n

        # Run function
        otree2 <- delete_duplicate(otree2)

        # Test
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        after_n <- otree2$info$initial_n
        testthat::expect_gt(n_before_aaw, n_after_aaw)
        testthat::expect_gt(n_before_dictator, n_after_dictator)
        testthat::expect_gt(before_n, after_n)
      })

      testthat::test_that("Delete duplicate - time and chat", {
        # Prepare data
        otree2 <- otree_all

        otree2$all_apps_wide <- rbind(otree2$all_apps_wide,
                                      otree2$all_apps_wide)
        otree2$dictator <- rbind(otree2$dictator,
                                 otree2$dictator)
        otree2$survey <- rbind(otree2$survey,
                               otree2$survey)
        otree2$Time <- rbind(otree2$Time,
                             otree2$Time)
        otree2$Chats <- rbind(otree2$Chats,
                              otree2$Chats)

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)
        before_n <- otree2$info$initial_n

        # Run function
        otree2 <- delete_duplicate(otree2)

        # Test
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        after_n <- otree2$info$initial_n
        testthat::expect_gt(n_before_aaw, n_after_aaw)
        testthat::expect_gt(n_before_dictator, n_after_dictator)
        testthat::expect_gt(before_n, after_n)
      })

      print("---- messy_time -----")

      # Messy time ####
      testthat::test_that("Messy time", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test (combine only time stamps)
        testthat::expect_warning(
          otree2 <- messy_time(otree2,
                               combine = TRUE,
                               epoch_time = TRUE,
                               info = TRUE),
          "referred to the time stamp.*referred to the participant code")

        test1 <- !("epoch_time" %in% names(otree2$Time))
        test2 <- !("time_stamp" %in% names(otree2$Time))
        test3 <- !("participant__code" %in% names(otree2$Time))
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      # The other messy tests are below inside the other functions tests
      testthat::test_that("Messy time", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          messy_time(otree2),
          "referred to the time stamp.*referred to the participant code")
      })

      testthat::test_that("Messy time", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(messy_time(otree2),
                               "referred to the time stamp")
      })

      testthat::test_that("Messy time - combine false, epoch time false", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test (combine only time stamps
        testthat::expect_error(messy_time(otree2,
                                          combine = FALSE,
                                          epoch_time = FALSE),
                               "referred to the participant code")
      })

      print("---- messy_chat -----")

      # Messy chat  ####
      testthat::test_that("Messy chat", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(messy_chat(otree2),
                               " referred to the session code")
      })

      testthat::test_that("Messy chat - combine", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_warning(
          otree2 <- messy_chat(otree2,
                               combine = TRUE,
                               info = TRUE),
          "referred to")
      })

      testthat::test_that("Messy chat (e) - more participant code variables", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(messy_chat(otree2,
                                          session = FALSE),
                               "referred to the participant code")
      })

      print("---- delete_cases -----")

      # Delete cases  ####
      testthat::test_that("Delete cases - new oTree", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function and test
        testthat::expect_message(
          delete_cases(otree2,
                       person,
                       reason = "Upon request",
                       info = TRUE),
          "Cases are deleted")
        otree2 <- delete_cases(otree2, person, reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test0 <- length(otree2$info[["deleted_cases"]][["codes"]]) == 1L
        test1 <- person %in% otree2$info[["deleted_cases"]][["codes"]]
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))
        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$all_apps_wide$participant.code))
        testthat::expect_true(test0)
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count

        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - new oTree, one random data frame", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)
        before_random <- nrow(otree2$random_dataframe)

        # Run function and test
        testthat::expect_message(
          delete_cases(otree2,
                       person,
                       reason = "Upon request",
                       info = TRUE),
          "Cases are deleted")
        otree2 <- delete_cases(otree2, person, reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)
        n_after_random <- nrow(otree2$random_dataframe)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat
        diff_random <- before_random - n_after_random

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)
        testthat::expect_identical(diff_random, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)
        testthat::expect_gt(n_after_random, 0L)

        # Test if deleted people are really deleted
        test0 <- length(otree2$info[["deleted_cases"]][["codes"]]) == 1L
        test1 <- person %in% otree2$info[["deleted_cases"]][["codes"]]
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))
        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$all_apps_wide$participant.code))
        testthat::expect_true(test0)
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - old and new otree", {
        # Prepare data (delete person from old and new data frame)
        otree2 <- otree_all
        person1 <- otree2$Time$participant_code[
          !is.na(otree2$Time$participant_code)][1L]
        person2 <- otree2$Time$participant__code[
          !is.na(otree2$Time$participant__code)][1L]
        person <- c(person1, person2)

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function and test
        otree2 <- messy_time(otree2, combine = TRUE)
        otree2 <- messy_chat(otree2, combine = TRUE)
        testthat::expect_message(
          otree2 <- delete_cases(otree2,
                                 person,
                                 reason = "Upon request",
                                 info = TRUE),
          "2 case")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test1 <- all(person %in% otree2$info[["deleted_cases"]][["codes"]])
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code[
                          !is.na(otree2$Time$participant_code)]))

        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$all_apps_wide$participant.code))
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        test1 <- length(otree2$info[["deleted_cases"]][["codes"]]) == 2L
        testthat::expect_true(test1)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)

        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - more people ", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[c(1L, 2L, 3L, 4L)]

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_cases(otree2,
                               person,
                               reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test: Nothing should be deleted
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test1 <- all(
          person %in% otree2$info[["deleted_cases"]][["codes"]])
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))

        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$all_apps_wide$participant.code))
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - more people - one not there", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- c(otree2$all_apps_wide$participant.code[c(1L, 2L, 3L, 4L)],
                    "notthere")

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_cases(otree2,
                               person,
                               reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test1 <- all(
          person[person != "notthere"] %in%
            otree2$info[["deleted_cases"]][["codes"]])

        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))

        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$all_apps_wide$participant.code))
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - person not in aaw", {
        # Prepare data
        otree1 <- otree_5_4_0
        person <- otree1$all_apps_wide$participant.code[1L]
        otree1$all_apps_wide <- tail(otree1$all_apps_wide, -1L)

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_cases(otree1, person,
                               reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_identical(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - without aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]
        otree2$all_apps_wide <- NULL

        # Before
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_cases(otree2,
                               pcodes = person,
                               reason = "Upon request")

        # After
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test1 <- person %in% otree2$info[["deleted_cases"]][["codes"]]
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))

        test3 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$dictator$participant.code))
        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test if participant is really deleted - version 2
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - plabels", {
        # Prepare data
        otree1 <- otree_5_4_0
        person <- c("Person4", "Person1")
        person_codes <- otree1$all_apps_wide$participant.code[
          otree1$all_apps_wide$participant.label %in% person]

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_cases(otree1,
                               plabels = person,
                               reason = "Only tests")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test0 <- all(person_codes %in%
                       otree2$info[["deleted_cases"]][["codes"]])
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test0, test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        test1 <- length(otree2$info[["deleted_cases"]][["codes"]]) ==
          length(person_codes)
        testthat::expect_true(test1)

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - old", {
        # Prepare data
        otree1 <- otree_2_2_4
        labels <- c("Person4", "Person1")
        person <- otree1$all_apps_wide$participant.code[
          otree1$all_apps_wide$participant.label %in% labels]

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_cases(otree1,
                               plabels = labels,
                               reason = "Only tests")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test0a <- all(
          person %in% otree2$info[["deleted_cases"]][["codes"]])

        test0b <- length(otree2$info[["deleted_cases"]][["codes"]]) ==
          length(person)

        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test0a, test0b, test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - saved vars", {
        # Prepare data
        otree1 <- otree_5_4_0
        person <- otree1$all_apps_wide$participant.code[1L]

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_cases(otree1,
                               pcodes = person,
                               reason = "Upon request",
                               saved_vars = "dictator.1.group.id_in_subsession")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test0 <- person %in% otree2$info[["deleted_cases"]][["codes"]]

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test0, test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        test1 <- length(otree2$info[["deleted_cases"]][["codes"]]) == 1L
        testthat::expect_true(test1)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- !(person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases - old", {
        # Prepare data
        otree1 <- otree_2_2_4
        person <- otree1$all_apps_wide$participant.code[1L]

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        check_time <- person %in% otree1$Time$participant__code
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_cases(otree1, person, reason = "Upon request")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        if (check_time) {
          testthat::expect_gt(diff_time, 0L)
        } else {
          testthat::expect_identical(diff_time, 0L)
        }
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant__code)
        test5a <- otree1$Chats$participant__code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant__code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        test1 <- length(otree2$info[["deleted_cases"]][["codes"]]) == 1L
        testthat::expect_true(test1)

        # Test if participant is really deleted - version 2
        test1 <- !(person %in% otree2$all_apps_wide$participant.code)
        test2 <- !(person %in% otree2$dictator$participant.code)
        test3 <- !(person %in% otree2$start$participant.code)
        test4 <- !(person %in% otree2$survey$participant.code)
        test5 <- !(person %in% otree2$Time$participant__code)
        test6 <- (person %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete cases (e) - old label not there", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            plabels = "xyz",
                                            reason = "Only tests"))
      })

      testthat::test_that("Delete cases (e)- person not there", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            "notthere",
                                            reason = "Upon request"),
                               "not in data frames")
      })

      testthat::test_that("Delete cases (e) - saved vars", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(delete_cases(otree2, "46kxib6w",
                                            reason = "Upon request",
                                            saved_vars = "wrongvars"), "not in")
      })

      testthat::test_that("Delete cases (e) - only participant code or label", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            pcodes = "46kxib6w",
                                            plabels = "Person1",
                                            reason = "Upon request",
                                            saved_vars = "wrongvars"),
                               "Please only specify either")
      })

      testthat::test_that("Delete cases (e) - participant not there", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            pcodes = "falsecode",
                                            reason = "Upon request"),
                               "not in data frames")
      })

      testthat::test_that("Delete cases (e) - no variable", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$participant.code <- NULL

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            plabels = "Person1",
                                            reason = "Upon request"),
                               "this function needs the variable")
      })

      testthat::test_that("Delete cases (e) - saved vars", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(delete_cases(otree2, "46kxib6w",
                                            reason = "Upon request",
                                            saved_vars = "wrongvars"),
                               "only works")
      })

      testthat::test_that("Delete cases (e) - no participants", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            reason = "Upon request",
                                            saved_vars = "wrongvars"),
                               "Please specify pcodes or plabels")
      })

      testthat::test_that("Delete cases (e) - participant code NA", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- c("46kxib6w", NA)

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            pcodes = person,
                                            reason = "Only tests"),
                               "At least one element in pcodes is NA")
      })

      testthat::test_that("Delete cases (e) - empty participant code", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- c()

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            pcodes = person,
                                            reason = "Only tests"),
                               "Please specify pcodes or plabels")
      })

      testthat::test_that("Delete cases (e) - participant label NA", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- c("Person4", NA)

        # Run function and test
        testthat::expect_error(delete_cases(otree2,
                                            plabels = person,
                                            reason = "Only tests"),
                               "plabel is NA")
      })

      testthat::test_that("Delete cases (e) - not oTree", {
        x <- c(1L, 2L, 3L)
        testthat::expect_error(delete_cases(x,
                                            reason = "Only tests"),
                               "not a list of oTree")
      })

      testthat::test_that("Delete cases (e) - not otree2", {
        x <- list(a = 1L, b = 2L, c = 3L)
        testthat::expect_error(delete_cases(x,
                                            reason = "Only tests"),
                               "not a list of oTree")
      })

      testthat::test_that("Delete cases (e) - old and new otree", {
        # Prepare data (delete person from old and new data frame)
        otree2 <- otree_all
        person1 <- otree2$Time$participant_code[
          !is.na(otree2$Time$participant_code)][1L]
        person2 <- otree2$Time$participant__code[
          !is.na(otree2$Time$participant__code)][1L]

        person <- c(person1, person2)

        # Run function
        error <- tryCatch(
          delete_cases(otree2,
                       person,
                       reason = "Upon request",
                       info = TRUE),
          error = function(e) e)

        # Test
        testthat::expect_true(grepl(
          "You combined data from old and new oTree versions", error))
        testthat::expect_true(grepl(
          "messy_chat", error))
        testthat::expect_true(grepl(
          "messy_time", error))
      })

      testthat::test_that("Delete cases (e) - all but messed time done", {
        # Prepare data (delete person from old and new data frame)
        otree2 <- otree_all
        person1 <- otree2$Time$participant_code[
          !is.na(otree2$Time$participant_code)][1L]
        person2 <- otree2$Time$participant__code[
          !is.na(otree2$Time$participant__code)][1L]

        person <- c(person1, person2)

        otree2 <- messy_time(otree2, combine = TRUE)
        # Run function
        error <- tryCatch(
          delete_cases(otree2,
                       person,
                       reason = "Upon request",
                       info = TRUE),
          error = function(e) e)

        # Test
        testthat::expect_true(grepl(
          "You combined data from old and new oTree versions", error))
        testthat::expect_true(grepl(
          "messy_chat", error))
        testthat::expect_false(grepl(
          "messy_time", error))
      })

      testthat::test_that("Delete cases (e) - all but messed chat done", {
        # Prepare data (delete person from old and new data frame)
        otree2 <- otree_all
        person1 <- otree2$Time$participant_code[
          !is.na(otree2$Time$participant_code)][1L]
        person2 <- otree2$Time$participant__code[
          !is.na(otree2$Time$participant__code)][1L]
        person <- c(person1, person2)
        otree2 <- messy_chat(otree2, combine = TRUE)

        # Run function
        error <- tryCatch(
          delete_cases(otree2,
                       person,
                       reason = "Upon request",
                       info = TRUE),
          error = function(e) e)

        # Test
        testthat::expect_true(grepl(
          "You combined data from old and new oTree versions", error))
        testthat::expect_false(grepl(
          "messy_chat", error))
        testthat::expect_true(grepl(
          "messy_time", error))
      })

      print("---- show_dropouts -----")

      # Delete sessions ####

      testthat::test_that("Delete sessions - old ", {
        # Prepare data (get a session with at least 4 cases)
        otree2 <- otree_2_2_4
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        part <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$session.code == session]

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        testthat::expect_warning(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests"),
          "Session information is taken from the data frames")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_chat, 0L)

        # Test if sessions are really deleted
        test1 <- !(session %in% otree2$all_apps_wide$session.code)
        test2 <- !(session %in% otree2$dictator$session.code)
        test3 <- !(session %in% otree2$start$session.code)
        test4 <- !(session %in% otree2$survey$session.code)
        test5 <- !(session %in% otree2$Chats$participant__session__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test if participant is really deleted
        test1 <- !(part %in% otree2$all_apps_wide$participant.code)
        test2 <- !(part %in% otree2$dictator$participant.code)
        test3 <- !(part %in% otree2$start$participant.code)
        test4 <- !(part %in% otree2$survey$participant.code)
        test5 <- !(part %in% otree2$Time$participant__code)
        test6 <- !(part %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions - new", {
        # Prepare data (get a session with at least 4 cases)
        otree2 <- otree_5_4_0
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        part <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$session.code == session]

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_sessions(otree2,
                                  scodes = session,
                                  reason = "Only tests")
        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_chat, 0L)

        # Test if participant is really deleted - version 1
        test1 <- !(part %in% otree2$all_apps_wide$participant.code)
        test2 <- !(part %in% otree2$dictator$participant.code)
        test3 <- !(part %in% otree2$start$participant.code)
        test4 <- !(part %in% otree2$survey$participant.code)
        test5 <- !(part %in% otree2$Time$participant__code)
        test6 <- !(part %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test if deleted people are really deleted - version 2
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5 <- !(otree2$Chats$participant_code %in%
                     otree2$info$deleted_cases$codes)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_true(all(test5))

        # Test if deleted people are really deleted - version 3
        test1 <- !(session %in% otree2$all_apps_wide$session.code)
        test2 <- !(session %in% otree2$dictator$session.code)
        test3 <- !(session %in% otree2$start$session.code)
        test4 <- !(session %in% otree2$survey$session.code)
        test5 <- !(session %in% otree2$Time$session_code)
        test6 <- !(session %in% otree2$Chats$session_code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5,
                                    test6)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions - new, one random data frame", {
        # Prepare data (get a session with at least 4 cases)
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        part <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$session.code == session]

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)
        before_random <- nrow(otree2$random_dataframe)

        # Run function
        otree2 <- delete_sessions(otree2,
                                  scodes = session,
                                  reason = "Only tests")
        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)
        n_after_random <- nrow(otree2$random_dataframe)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat
        diff_random <- before_random - n_after_random

        # Test
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_chat, 0L)
        testthat::expect_identical(diff_random, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5 <- !(otree2$Chats$participant_code %in%
                     otree2$info$deleted_cases$codes)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_true(all(test5))

        # Test if participant is really deleted - version 2
        test1 <- !(part %in% otree2$all_apps_wide$participant.code)
        test2 <- !(part %in% otree2$dictator$participant.code)
        test3 <- !(part %in% otree2$start$participant.code)
        test4 <- !(part %in% otree2$survey$participant.code)
        test5 <- !(part %in% otree2$Time$participant__code)
        test6 <- !(part %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test if participant is really deleted - version 3
        test1 <- !(session %in% otree2$all_apps_wide$session.code)
        test2 <- !(session %in% otree2$dictator$session.code)
        test3 <- !(session %in% otree2$start$session.code)
        test4 <- !(session %in% otree2$survey$session.code)
        test5 <- !(session %in% otree2$Time$session_code)
        test6 <- !(session %in% otree2$Chats$session_code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5,
                                    test6)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions - new info = TRUE", {
        # Prepare data (get a session with at least 4 cases)
        otree2 <- otree_5_4_0
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]

        # Run function
        testthat::expect_message(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests",
                                    info = TRUE),
          "deleted")
      })

      testthat::test_that("Delete sessions - no cases", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_time <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        testthat::expect_warning(
          otree2 <- delete_sessions(otree2,
                                    scodes = "xyz",
                                    reason = "Only tests"),
          "The session can not be found in any of the data frames")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_time - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test
        testthat::expect_identical(diff_aaw, 0L)
        testthat::expect_identical(diff_dictator, 0L)
        testthat::expect_identical(diff_survey, 0L)
        testthat::expect_identical(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test for consistency of the info output (everything is NULL!)
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions - without aaw", {
        # Prepare data
        # (take a session with more people so there is a chat in it)
        otree2 <- otree_5_4_0
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        part <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$session.code == session]
        otree2$all_apps_wide <- NULL

        # Before
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_sessions(otree2,
                                  scodes = session,
                                  reason = "Only tests")

        # After
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_chat, 0L)  # here greater 0 because all people
        # in one group/chat are deleted

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5 <- !any(otree2$Chats$participant_code %in%
                        otree2$info$deleted_cases$codes)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_true(test5)

        # Test if participant is really deleted - version 2
        test1 <- !(part %in% otree2$all_apps_wide$participant.code)
        test2 <- !(part %in% otree2$dictator$participant.code)
        test3 <- !(part %in% otree2$start$participant.code)
        test4 <- !(part %in% otree2$survey$participant.code)
        test5 <- !(part %in% otree2$Time$participant__code)
        test6 <- !(part %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions - old - warning Chat", {
        # Prepare data
        otree2 <- otree_2_2_4
        session <- unique(otree2$Chats$participant__session__code)[1L]
        otree2$Chats$participant__session__code <- NULL
        part <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$session.code == session]

        # Before
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_time <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        warning <- capture_warnings(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests"))

        testthat::expect_true(
          any(grepl(x = warning,
                    pattern = "No variable called .session")))

        # After
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_time - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_chat, 0L)  # here greater 0 because all people
        # in one group/chat are deleted

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant__code)
        test5 <- !any(otree2$Chats$participant__code %in%
                         otree2$info$deleted_cases$codes)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_true(test5)

        # Test if participant is really deleted - version 2
        test1 <- !(part %in% otree2$all_apps_wide$participant.code)
        test2 <- !(part %in% otree2$dictator$participant.code)
        test3 <- !(part %in% otree2$start$participant.code)
        test4 <- !(part %in% otree2$survey$participant.code)
        test5 <- !(part %in% otree2$Time$participant__code)
        test6 <- !(part %in% otree2$Chats$participant__code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete sessions (e) - not oTree", {
        x <- c(1L, 2L, 3L)
        testthat::expect_error(delete_sessions(x,
                                               scodes = "t0rog7nz",
                                               reason = "Only tests"),
                               "not a list of oTree")
      })

      testthat::test_that("Delete sessions (e) - app Test", {
        x <- list(a = 1L, b = 2L, c = 3L)
        testthat::expect_error(delete_sessions(x,
                                               scodes = "t0rog7nz",
                                               reason = "Only tests"),
                               "not a list of oTree")
      })

      testthat::test_that("Delete sessions (e) - all messy time and chat", {
        # Prepare data
        otree2 <- otree_all
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]

        # Run function and test
        testthat::expect_error(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests"),
          "You combined data from old and new oTree versions")
      })

      testthat::test_that("Delete sessions (e) - all messy chat", {
        # Prepare data
        otree2 <- otree_all
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        testthat::expect_warning(
          otree2 <- messy_time(otree2, combine = TRUE, info = TRUE))

        # Run function and test
        testthat::expect_error(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests"),
          "You combined data from old and new oTree versions")
      })

      testthat::test_that("Delete sessions (e) - all messy time", {
        # Prepare data
        otree2 <- otree_all
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 4L, 1L]
        otree2 <- messy_chat(otree2, combine = TRUE)

        # Run function and test
        testthat::expect_error(
          otree2 <- delete_sessions(otree2,
                                    scodes = session,
                                    reason = "Only tests"),
          "You combined data from old and new oTree versions")
      })

      # Show dropouts  ####
      testthat::test_that("Show dropouts - new oTree, final_apps", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- show_dropouts(otree2, final_apps = "survey")

        # Test

        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)
        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - new oTree,
                          final_apps, one random df", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Run function
        output <- show_dropouts(otree2, final_apps = "survey")

        # Test

        # Test if random df is not in list
        test1 <- !(any(grepl("random_dataframe", output$full$reason)))
        testthat::expect_true(test1)

        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)

        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - 2", {
        # Prepare data (example with inconsistent end pages)
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant._current_app_name == "survey"][1L]
        otree2$all_apps_wide$participant._current_app_name[
          otree2$all_apps_wide$participant._current_app_name == "survey"][1L] <-
          "othername"

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_time <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        testthat::expect_warning(
          output <- show_dropouts(otree2,
                                  final_apps = "survey"),
          "At least one participant in the dropout")

        # Test
        test1 <- person %in% output$unique$participant.code
        test2 <- person %in% output$full$participant.code
        test3 <- person %in% output$codes
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)
        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - 2.2.4", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        output <- show_dropouts(otree2, final_apps = "survey")

        # Test
        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)
        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - pagenames", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- show_dropouts(otree2, final_pages = "Demographics")

        # Test
        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)
        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - final_apps and pagenames", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- show_dropouts(otree2,
                                final_apps = "survey",
                                final_pages = "Demographics")

        # Test
        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)

        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Show dropouts - saved vars", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <-
          show_dropouts(otree2, "survey",
                        saved_vars = "dictator.1.group.id_in_subsession")

        # Test
        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        test3 <- "all_end" %in% names(output)
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)

        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if saved variable is there
        test1 <- "dictator.1.group.id_in_subsession" %in% names(output$unique)
        test2 <- "dictator.1.group.id_in_subsession" %in% names(output$full)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Show dropouts - saved vars not there", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(show_dropouts(otree2, "survey",
                                             saved_vars = "invalidvar"),
                               "saved_vars not in all_apps_wide")
      })

      testthat::test_that("Show dropouts (e) - specify", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        expect_error(show_dropouts(otree2),
                     "Please specify final_apps or final_pages or both")
      })

      testthat::test_that("Show dropouts (w) - saved vars", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_warning(
          output <-
            show_dropouts(otree2,
                          final_apps = "survey",
                          saved_vars = "dictator.1.group.id_in_subsession"),
          "is ignored.")

        # Test
        # Test if all data frames and other vectors/values are there
        test1 <- "full" %in% names(output)
        test2 <- "unique" %in% names(output)
        # all_end is only shown if AAW exist:
        test3 <- !("all_end" %in% names(output))
        test4 <- "codes" %in% names(output)
        test5 <- "count" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5)))

        # Test for consistency of the info output
        test1 <- setequal(output$unique$participant.code,
                          output$full$participant.code)

        test2 <- setequal(output$unique$participant.code, output$codes)
        test3 <- length(output$codes) == output$count
        test4 <- length(output$codes) == nrow(output$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      print("---- delete_dropouts -----")

      # Delete dropouts  ####
      testthat::test_that("Delete dropouts - final_apps new oTree", {
        # Prepare data
        otree1 <- otree_5_4_0

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_apps = "survey")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        #  if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))

        # Chat differently
        test5a <- (otree2$info$deleted_cases$codes %in%
                     otree1$Chats$participant_code)  # Before
        test5b <- (otree2$info$deleted_cases$codes %in%
                     otree2$Chats$participant_code) # After

        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts - final_apps new oTree, random df", {
        # Prepare data
        otree1 <- otree_5_4_0
        otree1$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)
        before_random <- nrow(otree1$random_dataframe)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_apps = "survey")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)
        n_after_random <- nrow(otree2$random_dataframe)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat
        diff_random <- before_random - n_after_random

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!
        testthat::expect_identical(diff_random, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)
        testthat::expect_gt(n_after_random, 0L)

        # Test if random df is not in list
        test1 <- !(any(grepl("random_dataframe",
                             otree2$info$deleted_cases$full$reason)))
        testthat::expect_true(test1)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))

        # Chat differently
        test5a <- (otree2$info$deleted_cases$codes %in%
                     otree1$Chats$participant_code)  # Before
        test5b <- (otree2$info$deleted_cases$codes %in%
                     otree2$Chats$participant_code) # After

        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts - final_apps old oTree", {
        # Prepare data
        otree1 <- otree_2_2_4

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_apps = "survey")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))

        # Chat differently
        test5a <- (otree2$info$deleted_cases$codes %in%
                     otree1$Chats$participant_code)  # Before
        test5b <- (otree2$info$deleted_cases$codes %in%
                     otree2$Chats$participant_code) # After

        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts - different otree versions", {
        # Prepare data
        otree1 <- otree_all

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_apps = "survey")
        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3)))

        # Time differently
        test1 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant_code))
        test2 <- !any(otree2$info[["deleted_cases"]][["codes"]] %in%
                        unique(otree2$Time$participant__code))
        testthat::expect_true(test1)
        testthat::expect_true(test2)

        # Chat differently
        test5a <- otree2$info$deleted_cases$codes %in%
          otree1$Chats$participant_code  # Before
        test5b <- otree2$info$deleted_cases$codes %in%
          otree2$Chats$participant_code # After
        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts - final_apps & pagenames", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Before
        n_before_aaw <- nrow(otree2$all_apps_wide)
        n_before_dictator <- nrow(otree2$dictator)
        n_before_survey <- nrow(otree2$survey)
        n_before_survey <- nrow(otree2$Time)
        n_before_chat <- nrow(otree2$Chat)

        # Run function
        otree2 <- delete_dropouts(otree2,
                                  final_apps = "survey",
                                  final_pages = "Demographics")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted (sample)
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))

        # Chat differently
        test5a <- otree2$info$deleted_cases$codes %in%
          otree2$Chats$participant_code  # Before
        test5b <- otree2$info$deleted_cases$codes %in%
          otree2$Chats$participant_code  # After

        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts - only pagenames", {
        # Prepare data
        otree1 <- otree_5_4_0

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_pages = "Demographics")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        # Chat differently
        test5a <- otree2$info$deleted_cases$codes %in%
          otree1$Chats$participant_code  # Before
        test5b <- otree2$info$deleted_cases$codes %in%
          otree2$Chats$participant_code # After

        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts 2 - inconsistent yes", {
        # Prepare data (example with inconsistent end pages)
        otree1 <- otree_5_4_0

        person <- otree1$all_apps_wide$participant.code[
          otree1$all_apps_wide$participant._current_app_name == "survey"][1L]

        otree1$all_apps_wide$participant._current_app_name[
          otree1$all_apps_wide$participant._current_app_name == "survey"][1L] <-
          "othername"

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        before_start <- nrow(otree1$start)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chats)

        # Run function
        otree2 <- delete_dropouts(otree1,
                                  final_apps = "survey",
                                  inconsistent = "yes")

        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_identical(diff_chat, 0L)

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        # Test - check if person gets deleted anyways (version 2)
        test3 <- person %in% otree2$info[["deleted_cases"]][["codes"]]
        test4 <- !(person %in% otree2$all_apps_wide$participant.code)
        test5 <- !(person %in% otree2$dictator$participant.code)
        test6 <- !(person %in% otree2$Time$participant.code)
        test7 <- !(person %in% otree2$start$participant.code)
        test8 <- !(person %in% otree2$survey$participant.code)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6,
                                    test7, test8)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts 2 - inconsistent no", {
        # Prepare data (example with inconsistent end pages)
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$participant._current_app_name[
          otree2$all_apps_wide$participant._current_app_name == "survey"][1L] <-
          "othername"
        before <- nrow(otree2$all_apps_wide)

        # Run function and test
        testthat::expect_error(delete_dropouts(otree2,
                                               final_apps = "survey",
                                               inconsistent = "no"),
                               "The user requested termination")
      })

      testthat::test_that("Delete dropouts 2 - inconsistent check", {
        # Prepare data (example with inconsistent end pages)
        otree1 <- otree_5_4_0

        otree1$all_apps_wide$participant._current_app_name[
          otree1$all_apps_wide$participant._current_app_name == "survey"][1L] <-
          "othername"

        # Before
        n_before_aaw <- nrow(otree1$all_apps_wide)
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function and capture user-interaction
        ans <- "yes"
        my_file <- textConnection(ans)

        withr::with_options(
          list(mypkg.connection = my_file),
          {
            output <- capture.output(
              otree2 <- delete_dropouts(otree1,
                                        final_apps = "survey"))

            # Test if the output from the cat() function is shown
            cat_output_displayed <-
              any(grepl("At least one participant in the dropout list",
                        output))

            testthat::expect_true(cat_output_displayed)
        })

        close(my_file)

        # Test
        # After
        n_after_aaw <- nrow(otree2$all_apps_wide)
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_aaw <- n_before_aaw - n_after_aaw
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_aaw, 0L)
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_aaw, 0L)
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)
        test5a <- otree1$Chats$participant_code %in%
          otree2$info$deleted_cases$codes  # Chat differently: Before
        test5b <- otree2$Chats$participant_code %in%
          otree2$info$deleted_cases$codes # Chat differently: After

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        testthat::expect_identical(test5a, test5b)

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("Delete dropouts (e) - specify final_apps or pages", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        expect_error(delete_dropouts(otree2),
                     "Please specify final_apps or final_pages")
      })

      testthat::test_that("Delete dropouts (e) - saved vars no aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(
          delete_dropouts(otree2,
                          final_apps = "survey",
                          saved_vars = "dictator.1.group.id_in_subsession"),
          "The argument \"saved_vars\" only works when")
      })

      testthat::test_that("Delete dropouts (e) - saved vars 2 ", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(delete_dropouts(otree2,
                                               final_apps = "survey",
                                               saved_vars = "notthere"),
                               "not in all_apps_wide")
      })

      testthat::test_that("Delete dropouts (e) - no all apps wide", {
        # Prepare data
        otree1 <- otree_5_4_0
        otree1$all_apps_wide <- NULL

        # Before
        n_before_dictator <- nrow(otree1$dictator)
        n_before_survey <- nrow(otree1$survey)
        n_before_survey <- nrow(otree1$Time)
        n_before_chat <- nrow(otree1$Chat)

        # Run function
        testthat::expect_warning(
          otree2 <- delete_dropouts(otree1,
                                    final_pages = "Demographics"), "No")

        # Test
        # After
        n_after_dictator <- nrow(otree2$dictator)
        n_after_survey <- nrow(otree2$survey)
        n_after_time <- nrow(otree2$Time)
        n_after_chat <- nrow(otree2$Chat)

        # Diff
        diff_dictator <- n_before_dictator - n_after_dictator
        diff_survey <- n_before_survey - n_after_survey
        diff_time <- n_before_survey - n_after_time
        diff_chat <- n_before_chat - n_after_chat

        # Test difference
        testthat::expect_gt(diff_dictator, 0L)
        testthat::expect_gt(diff_survey, 0L)
        testthat::expect_gt(diff_time, 0L)
        testthat::expect_identical(diff_chat, 0L) # Equal zero!!!

        # Test if not all cases were deleted!
        testthat::expect_gt(n_after_dictator, 0L)
        testthat::expect_gt(n_after_survey, 0L)
        testthat::expect_gt(n_after_time, 0L)
        testthat::expect_gt(n_after_chat, 0L)

        # Test if deleted people are really deleted
        test1 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$all_apps_wide$participant.code)
        test2 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$dictator$participant.code)
        test3 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$survey$participant.code)
        test4 <-
          !(otree2$info$deleted_cases$codes %in%
              otree2$Time$participant_code)

        testthat::expect_true(all(c(test1, test2)))
        testthat::expect_true(all(c(test3, test4)))
        # Chat differently
        test5a <- otree2$info$deleted_cases$codes %in%
          otree1$Chats$participant_code  # Before
        test5b <- otree2$info$deleted_cases$codes %in%
          otree2$Chats$participant_code # After

        testthat::expect_identical(test5a, test5b)
      })

      print("---- delete_sessions -----")

      # Combined deletion test  ####
      testthat::test_that("several deletions  - version 1", {
        otree2 <- otree_5_4_0

        # Delete cases
        person <- otree2$all_apps_wide$participant.code[c(1L, 2L, 3L, 4L)]
        otree2 <- delete_cases(otree2, person,
                               reason = "Upon request")

        # Delete dropouts
        otree2 <- delete_dropouts(otree2,
                                  final_apps = "survey")

        # Session delete
        session <- as.data.frame(table(otree2$all_apps_wide$session.code))
        session <- session[session$Freq >= 2L, 1L]
        otree2 <- delete_sessions(otree2,
                                  scodes = session,
                                  reason = "Only tests")

        # Test
        test0 <- !("" %in% otree2$info$deleted_cases$unique$reason)
        test1 <- "ENC" %in% otree2$info$deleted_cases$unique$reason
        test2 <- "Only tests" %in% otree2$info$deleted_cases$unique$reason
        test3 <- "Upon request" %in% otree2$info$deleted_cases$unique$reason
        testthat::expect_true(all(c(test0, test1, test2, test3)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("several deletions - version 2", {
        # Prepare data
        otree2 <- otree_5_4_0
        unique(otree2$all_apps_wide$participant.code)

        # Delete dropouts
        otree2 <- delete_dropouts(otree2,
                                  final_apps = "survey")

        # Delete cases
        person <- otree2$all_apps_wide$participant.code[1L]
        otree2 <- delete_cases(otree2, person,
                               reason = "Upon request")

        # Session delete
        otree2$all_apps_wide$session.code[1L]
        otree2 <-
          delete_sessions(otree2,
                          scodes = otree2$all_apps_wide$session.code[1L],
                          reason = "Only tests")

        # Test
        test0 <- !("" %in% otree2$info$deleted_cases$unique$reason)
        test1 <- "ENC" %in% otree2$info$deleted_cases$unique$reason
        test2 <- "Only tests" %in% otree2$info$deleted_cases$unique$reason
        test3 <- "Upon request" %in% otree2$info$deleted_cases$unique$reason
        testthat::expect_true(all(c(test0, test1, test2, test3)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)
        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("several deletions - version 3", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Session delete
        otree2 <-
          delete_sessions(otree2,
                          scodes = otree2$all_apps_wide$session.code[1L],
                          reason = "Only tests")

        # Delete dropouts
        otree2 <- delete_dropouts(otree2,
                                  final_apps = "survey")

        # Delete cases
        person <- otree2$all_apps_wide$participant.code[c(1L, 2L)]
        otree2 <- delete_cases(otree2, person,
                               reason = "Upon request")
        # Test
        test0 <- !("" %in% otree2$info$deleted_cases$unique$reason)
        test1 <- "ENC" %in% otree2$info$deleted_cases$unique$reason
        test2 <- "Only tests" %in% otree2$info$deleted_cases$unique$reason
        test3 <- "Upon request" %in% otree2$info$deleted_cases$unique$reason
        testthat::expect_true(all(c(test0, test1, test2, test3)))

        # Test for consistency of the info output
        test1 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$full$participant.code)

        test2 <- setequal(otree2$info$deleted_cases$unique$participant.code,
                          otree2$info$deleted_cases$codes)
        test3 <- length(otree2$info$deleted_cases$codes) ==
          otree2$info$deleted_cases$count
        test4 <- length(otree2$info$deleted_cases$codes) ==
          nrow(otree2$info$deleted_cases$unique)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      # Make IDs  ####
      testthat::test_that("Make IDs - from_var, gmake = TRUE", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2,
                           pmake = TRUE,
                           gmake = TRUE,
                           from_var = "dictator.1.group.id_in_subsession",
                           emptyrows = "yes") # ignore empty rows stop
        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !(anyNA(otree2$dictator$group_id))
        test3 <- !is.null(otree2$dictator$session_id)
        test4 <- !(anyNA(otree2$dictator$session_id))
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_var, gmake = TRUE", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             pmake = TRUE,
                             gmake = TRUE,
                             from_var = "dictator.1.group.id_in_subsession",
                             emptyrows = "yes"), # ignore empty rows stop
          "Participant code variable couldn't be found in")

        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !anyNA(otree2$dictator$group_id)
        test3 <- !is.null(otree2$dictator$session_id)
        test4 <- !anyNA(otree2$dictator$session_id)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if all IDs are there
        test1 <- all(
          seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
            otree2$all_apps_wide$participant_id)
        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - two sessions na", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$session.code[c(1L, 2L, 3L)] <- NA
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             pmake = TRUE,
                             gmake = TRUE,
                             from_var = "dictator.1.group.id_in_subsession",
                             emptyrows = "yes"),
          "At least one of your session.codes in your from_app is NA")

        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !anyNA(otree2$dictator$group_id)
        test3 <- !is.null(otree2$dictator$session_id)
        test4 <- !anyNA(otree2$dictator$session_id)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]

        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)

        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_var", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2,
                           gmake = TRUE,
                           pmake = TRUE,
                           from_var = "dictator.1.group.id_in_subsession")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !anyNA(otree2$dictator$group_id)
        test3 <- !is.null(otree2$dictator$session_id)
        test4 <- !anyNA(otree2$dictator$session_id)
        test5 <- !is.null(otree2$dictator$participant_id)
        test6 <- !anyNA(otree2$dictator$participant_id)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing Ids
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]

        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_var, other starts", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2,
                           gmake = TRUE,
                           pmake = TRUE,
                           sstart = 3L,
                           gstart = 4L,
                           pstart = 8L,
                           from_var = "dictator.1.group.id_in_subsession")
        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$all_apps_wide$group_id)
        test2 <- !is.null(otree2$all_apps_wide$session_id)
        test3 <- !is.null(otree2$all_apps_wide$participant_id)
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for minimum values
        test1 <- min(otree2$all_apps_wide$group_id) == 4L
        test2 <- min(otree2$all_apps_wide$session_id) == 3L
        test3 <- min(otree2$all_apps_wide$participant_id) == 8L
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_var,
                          dictator.1.group.id_in_subsession", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_before <- otree2$dictator$player.payoff

        # Run function
        testthat::expect_no_warning(
          otree2 <- make_ids(otree2,
                             gmake = FALSE,  # will be overwritten by code!
                             pmake = FALSE,
                             from_var = "dictator.1.group.id_in_subsession"))

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_after <- otree2$dictator$player.payoff

        test1 <- order_aaw_before == order_aaw_after
        test2 <- order_dictator_before == order_dictator_after
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$all_apps_wide$group_id)
        test2 <- !is.null(otree2$all_apps_wide$session_id)
        test3 <- is.null(otree2$all_apps_wide$participant_id)
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test if all IDs are there
        # No participants!
        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code))  %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)
        testthat::expect_true(all(c(test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)
        testthat::expect_true(test1)
      })

      testthat::test_that("Make IDs - all the same", {
        # Prepare data (make group IDs all the same)
        otree2 <- otree_5_4_0

        otree2$all_apps_wide$start.1.group.id_in_subsession <-
          otree2$all_apps_wide$dictator.1.group.id_in_subsession

        otree2$all_apps_wide$survey.1.group.id_in_subsession <-
          otree2$all_apps_wide$dictator.1.group.id_in_subsession

        otree2$all_apps_wide$chatapp.1.group.id_in_subsession <-
          otree2$all_apps_wide$dictator.1.group.id_in_subsession

        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2, gmake = TRUE)

        # Test
        max_part <- length(otree2$all_apps_wide$participant.code)
        max_group <- length(
          unique(paste(otree2$all_apps_wide$session.code,
                       otree2$all_apps_wide$dictator.1.group.id_in_subsession)))

        max_session <- length(unique(otree2$all_apps_wide$session.code))

        testthat::expect_equal(
          max(otree2$all_apps_wide$participant_id, na.rm = TRUE), max_part)
        testthat::expect_equal(
          max(otree2$all_apps_wide$group_id, na.rm = TRUE), max_group)
        testthat::expect_equal(
          max(otree2$all_apps_wide$session_id, na.rm = TRUE), max_session)

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if all IDs are there
        test1 <- all(1L:max_part %in%
                       otree2$all_apps_wide$participant_id)
        test2 <- all(1L:max_session %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:max_group %in%
            otree2$dictator$group_id)
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs - no time start", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$participant.time_started[
          seq_len(nrow(otree2$all_apps_wide))] <- NA
        order_aaw_before <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_before <- otree2$dictator$player.payoff

        # Run function
        otree2 <- make_ids(otree2)

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_after <- otree2$dictator$player.payoff

        test1 <- order_aaw_before == order_aaw_after
        test2 <- order_dictator_before == order_dictator_after
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$all_apps_wide$participant_id)
        test2 <- is.null(otree2$dictator$group_id)
        test3 <- !is.null(otree2$survey$participant_id)
        test4 <- is.null(otree2$survey$group_id)
        test5 <- !is.null(otree2$survey$session_id)

        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test - check if no two session codes have the same session id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$session_id,
          otree2$all_apps_wide$session.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        test2 <- !anyDuplicated(strings)

        testthat::expect_true(test1)
        testthat::expect_true(test2)

        # Test - check if no two participant codes have the same participant id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$participant_id,
          otree2$all_apps_wide$participant.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        test2 <- !anyDuplicated(strings)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables exist / don't exist
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <- is.null(otree2$dictator$group_id)

        test4 <- is.null(otree2$all_apps_wide$group_id)

        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test for increasing IDs
        # Test if participant_ids are increasing with session_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$session_id,
                                otree2$dictator$participant_id), ]

        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_var - first constant", {
        # Prepare data (make all group variables to 1)
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        otree2$all_apps_wide$dictator.1.group.id_in_subsession <-
          otree2$all_apps_wide$survey.1.group.id_in_subsession

        otree2$all_apps_wide$dictator.2.group.id_in_subsession <-
          otree2$all_apps_wide$survey.1.group.id_in_subsession

        otree2$all_apps_wide$dictator.3.group.id_in_subsession <-
          otree2$all_apps_wide$survey.1.group.id_in_subsession

        otree2$all_apps_wide$chatapp.1.group.id_in_subsession <-
          otree2$all_apps_wide$survey.1.group.id_in_subsession

        # Run function
        testthat::expect_warning(

          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             icw = TRUE),
          "are constant. Group IDs now correspond to session IDs")

        # Test
        test1 <- all(otree2$all_apps_wide$group_id ==
                       otree2$all_apps_wide$session_id)
        testthat::expect_true(test1)

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test - check if no two session codes have the same session id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$session_id,
          otree2$all_apps_wide$session.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        test2 <- !anyDuplicated(strings)
        testthat::expect_true(all(c(test1, test2)))

        # Test - check if no two participant codes have the same participant id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$participant_id,
          otree2$all_apps_wide$participant.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        test2 <- !anyDuplicated(strings)
        testthat::expect_true(all(c(test1, test2)))

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <-
          otree2$all_apps_wide$session_id == otree2$all_apps_wide$group_id

        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs - from_app", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test
        test_participant <-
          unique(otree2$all_apps_wide$participant.code[
            order(otree2$all_apps_wide$participant.code)])

        testthat::expect_length(otree2$all_apps_wide$participant_id,
                                   length(test_participant))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !is.null(otree2$dictator$session_id)
        testthat::expect_true(test1)
        testthat::expect_true(test2)

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs - from_app, random_df", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Run function and test
        testthat::expect_error(
          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             from_app = "random_dataframe"),
          "Your from_app is not a normal oTree")
      })

      testthat::test_that("Make IDs - one participant not in from_app", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- delete_duplicate(otree2)
        delsomedictators <- unique(otree2$dictator$participant.code)[1L]
        otree2$dictator <-  otree2$dictator[
          !(otree2$dictator$participant.code %in% delsomedictators), ]
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator"),
          "all_apps_wide.*chatapp.*start.*survey.*Time.*has more participants than")

        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test
        test_participant <-
          unique(otree2$all_apps_wide$participant.code[
            order(otree2$all_apps_wide$participant.code)])

        testthat::expect_length(otree2$all_apps_wide$participant_id,
                                   length(test_participant))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !is.null(otree2$dictator$session_id)
        testthat::expect_true(test1)
        testthat::expect_true(test2)

        # Test if all IDs are there
        test1 <-
          all(
            1L:(length(unique(otree2$all_apps_wide$participant.code)) - 1L) %in%
              otree2$all_apps_wide$participant_id)
        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs - from_var constant", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             from_app = "survey"),
          "values are constant")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test
        testthat::expect_identical(otree2$group_id, otree2$session_id)

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$all_apps_wide$participant_id)
        test2 <- !is.null(otree2$dictator$group_id)
        test3 <- !is.null(otree2$survey$participant_id)
        test4 <- !is.null(otree2$survey$group_id)
        test5 <- !is.null(otree2$survey$session_id)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test if all IDs are there
        test1 <- all(
          seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
            otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          otree2$all_apps_wide$session_id == otree2$all_apps_wide$group_id)
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - others more cases than from-app", {
        # Info: Cases are then shown as NA
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$dictator <- otree2$dictator[7L:nrow(otree2$dictator), ]
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             from_app = "dictator"),
          "has more participants than")

        # Test
        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test
        test_participant <-
          unique(otree2$all_apps_wide$participant.code[
            order(otree2$all_apps_wide$participant.code)])

        testthat::expect_identical(
          length(otree2$all_apps_wide$participant_id),
          length(test_participant))

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !is.null(otree2$dictator$session_id)
        test3 <- !is.null(otree2$dictator$participant_id)

        # Test if NAs are there
        test4 <- NA %in% otree2$all_apps_wide$participant_id
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if all IDs are there
        # Bit more complicated here, because some IDs are NA
        test1 <-
          all(1L:(length(otree2$all_apps_wide$participant.code) -
                    sum(is.na(otree2$all_apps_wide$participant_id))) %in%
                otree2$all_apps_wide$participant_id)

        test2 <-
          all(1L:(length(unique(otree2$all_apps_wide$session.code)) - 1L) %in%
                otree2$all_apps_wide$session_id)

        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for NAs
        test1 <- NA %in% otree2$all_apps_wide$participant_id
        test2 <- NA %in% otree2$all_apps_wide$group_id
        test3 <- NA %in% otree2$all_apps_wide$session_id
        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs - from_app more cases than all apps wide", {
        # Prepare data
        otree2 <- otree_5_4_0
        delperson <- otree2$dictator$participant.code[1L]

        otree2$all_apps_wide <-
          otree2$all_apps_wide[
            otree2$all_apps_wide$participant.code != delperson, ]
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2,
                           gmake = TRUE,
                           from_app = "dictator")

        # Test
        test_participant <-
          unique(otree2$all_apps_wide$participant.code)

        testthat::expect_length(otree2$all_apps_wide$participant_id,
                                length(test_participant))

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !anyNA(otree2$dictator$group_id)
        test3 <- !is.null(otree2$dictator$session_id)
        test4 <- !anyNA(otree2$dictator$session_id)
        test5 <- !is.null(otree2$dictator$participant_id)
        test6 <- !anyNA(otree2$dictator$participant_id)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5, test6)))

        # Test if values are not there twice
        test <- !anyDuplicated(otree2$all_apps_wide$participant_id)
        testthat::expect_true(test)

        # Test if all IDs are there
        test1 <- all(2L:(length(otree2$all_apps_wide$participant.code)) %in%
                       otree2$all_apps_wide$participant_id)
        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs (w) - from_var constant", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_before <- otree2$dictator$player.payoff

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             from_var = "survey.1.group.id_in_subsession"),
          "The group variable values are constant")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_after <- otree2$dictator$player.payoff

        test1 <- order_aaw_before == order_aaw_after
        test2 <- order_dictator_before == order_dictator_after
        testthat::expect_true(all(c(test1, test2)))

        # Test
        testthat::expect_identical(otree2$all_apps_wide$group_id,
                                   otree2$all_apps_wide$session_id)

        # Test if variables are there and not NA
        test1 <- !is.null(otree2$dictator$group_id)
        test2 <- !anyNA(otree2$dictator$group_id)
        test3 <- !is.null(otree2$dictator$participant_id)
        test4 <- !anyNA(otree2$dictator$participant_id)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if all IDs are there
        test1 <-
          all(seq_along(unique(otree2$all_apps_wide$participant.code)) %in%
                otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        # Because group is constant:
        test3 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs (w) - constant group variable", {
        # Prepare data
        otree2 <- otree_5_4_0
        order_aaw_before <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_before <- otree2$dictator$player.payoff

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             from_app = "survey"),
          "group variable values are constant")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_after <- otree2$dictator$player.payoff

        test1 <- order_aaw_before == order_aaw_after
        test2 <- order_dictator_before == order_dictator_after
        testthat::expect_true(all(c(test1, test2)))

        # Test
        test1 <- !is.null(otree2$all_apps_wide$participant_id)
        test2 <- !is.null(otree2$dictator$group_id)
        test3 <- !is.null(otree2$survey$participant_id)
        test4 <- !is.null(otree2$survey$group_id)
        test5 <- !is.null(otree2$survey$session_id)
        test6 <- otree2$survey$group_id == otree2$survey$session_id
        test7 <-
          otree2$all_apps_wide$group_id == otree2$all_apps_wide$session_id

        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5, test6,
                                    test7)))

        # Test if all IDs are there
        test1 <- all(seq_along(
          unique(otree2$all_apps_wide$participant.code)) %in%
            otree2$all_apps_wide$participant_id)

        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)

        testthat::expect_true(test1)
        testthat::expect_true(test2)

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs (w) - from_var -
                          no participant code variable", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- delete_duplicate(otree2)
        otree2$survey$participant.code <- NULL
        order_aaw_before <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_before <- otree2$dictator$player.payoff

        # Run function
        testthat::expect_warning(
          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             pmake = TRUE,
                             from_app = "dictator"),
          "Participant code variable couldn't be found")

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$dictator.1.player.payoff
        order_dictator_after <- otree2$dictator$player.payoff

        test1 <- order_aaw_before == order_aaw_after
        test2 <- order_dictator_before == order_dictator_after
        testthat::expect_true(all(c(test1, test2)))

        # Test
        test1 <- !is.null(otree2$all_apps_wide$participant_id)
        test2 <- !is.null(otree2$dictator$group_id)
        test3 <- is.null(otree2$survey$participant_id)
        test4 <- is.null(otree2$survey$group_id)
        test5 <- is.null(otree2$survey$session_id)

        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test if all IDs are there
        test1 <- all(seq_along(unique(otree2$all_apps_wide$participant.code))
                     %in% otree2$all_apps_wide$participant_id)
        test2 <- all(seq_along(unique(otree2$all_apps_wide$session.code)) %in%
                       otree2$all_apps_wide$session_id)
        test3 <- all(
          1L:(length(otree2$dictator$group_id) / 3L / 2L) %in%
            otree2$dictator$group_id)

        testthat::expect_true(all(c(test1, test2, test3)))

        # Test for increasing IDs
        # Test if group_ids are increasing with session_id
        otree2$dictator <- otree2$dictator[order(otree2$dictator$session_id,
                                                 otree2$dictator$group_id), ]
        test1 <- all(diff(otree2$dictator$group_id) >= 0L)

        # Test if participant_ids are increasing with group_ids
        otree2$dictator <-
          otree2$dictator[order(otree2$dictator$group_id,
                                otree2$dictator$participant_id), ]
        test2 <- all(diff(otree2$dictator$participant_id) >= 0L)
        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("Make IDs (e) - emtpy rows - from app aaw", {
        # If there are empty rows, an error occurs
        # Prepare data
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = FALSE,
            path = testthat::test_path("testdata", "exp_data_5.4.0"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE), "globally but also room-specific")

        testthat::expect_error(
          otree2 <- make_ids(otree2,
                             pmake = TRUE,
                             gmake = TRUE,
                             from_var = "dictator.1.group.id_in_subsession",
                             emptyrows = "yes"),
          "length of participant codes is not equal the length of unique participant")

      })

      testthat::test_that("Make IDs (e) - emtpy rows - from app dictator", {
        # If there are empty rows, an error occurs
        # Prepare data
        testthat::expect_warning(
          otree2 <- import_otree(
            del_empty = FALSE,
            path =  testthat::test_path("testdata", "exp_data_5.4.0"),
            onlybots = FALSE,
            csv = TRUE,
            info = FALSE),
          "globally but also room-specific")

        # Run function and test
        testthat::expect_error(
          otree2 <- make_ids(otree2,
                             pmake = TRUE,
                             gmake = TRUE,
                             from_app = "dictator",
                             emptyrows = "yes"),
          "The length of participant codes is not equal the length of unique part")
      })

      testthat::test_that("Make IDs (e) - empty rows yes", {
        # Prepare data
        otree2 <- otree_new_empty

        ans <- "yes"
        my_file <- textConnection(ans)

        withr::with_options(
          list(mypkg.connection = my_file),
          {
            # Capture the output of make_ids
            output <- capture.output(
              make_ids(otree2,
                       pmake = TRUE,
                       gmake = TRUE,
                       from_var = "dictator.1.group.id_in_subsession"))

            # Test if the output from the cat() function is shown
            cat_output_displayed <-
              any(grepl("Your from_app contains empty rows", output))

            testthat::expect_true(cat_output_displayed)
        })

        close(my_file)
      })

      testthat::test_that("Make IDs (e) - empty rows no", {
        # Prepare data
        otree2 <- otree_new_empty

        ans <- paste("no")
        my_file <- textConnection(ans)

        withr::with_options(
          list(mypkg.connection = my_file),
          {
            # Capture the output of make_ids
            testthat::expect_error(
              output <- capture.output(
                make_ids(otree2,
                         pmake = TRUE,
                         gmake = TRUE,
                         from_var = "dictator.1.group.id_in_subsession")),
              "You chose to stop this function")
          }
        )

        close(my_file)
      })

      testthat::test_that("Make IDs (e) - duplicate data", {
        # Prepare data
        otree2 <- otree_5_4_0_non_unique

        # Run function and test
        testthat::expect_error(
          make_ids(otree2,
                   pmake = TRUE,
                   gmake = TRUE,
                   from_var = "dictator.1.group.id_in_subsession",
                   emptyrows = "yes"),
          "length of participant codes is not equal the length of unique participant")
      })

      testthat::test_that("Make IDs (e) - wrong from_app", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = TRUE,
                                        from_app = "Chats",
                                        emptyrows = "yes"),
                               "You are not supposed to use")
      })

      testthat::test_that("Make IDs (e) - no participant.code", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$participant.code <- NULL

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = FALSE,
                                        emptyrows = "yes"),
                               "There is no participant.code in")
      })

      testthat::test_that("Make IDs (e) - participant.code is NA", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$participant.code <- NA

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = FALSE,
                                        emptyrows = "yes"),
                               "There are NAs in your participant.code")
      })

      testthat::test_that("Make IDs (e) - from_app not found", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = TRUE,
                                        from_app = "xyz",
                                        emptyrows = "yes"),
                               "not found")
      })

      testthat::test_that("Make IDs (e) - from_app not data frame", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2$notadataframe <- c(1L, 2L, 3L)

        # Run function
        testthat::expect_error(make_ids(otree2, gmake = TRUE,
                                        from_app = "notadataframe"),
                               "not a data frame")
      })

      testthat::test_that("Make IDs (e) - from_app empty data frame", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$emptydataframe <- data.frame()

        # Run function and test
        testthat::expect_error(
          make_ids(otree2,
                  gmake = TRUE,
                  from_app = "emptydataframe"),
         "Your from_app is not a normal oTree all_apps_wide or apps data frame.")
      })

      testthat::test_that("Make IDs (e) - from_app empty data frame", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Make empty data frame
        otree2$survey <- otree2$survey[NULL, ]

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = TRUE,
                                        from_app = "survey"),
                               "has no entries")
      })

      testthat::test_that("Make IDs (e) - no variable", {
        # Prepare data (delete group_id)
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- otree2$all_apps_wide[
          , !(endsWith(names(otree2$all_apps_wide), "group.id_in_subsession"))]

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = TRUE),
                               "No variable that ends with")
      })

      testthat::test_that("Make IDs (e) - group_id cannot be calculated", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(make_ids(otree2, gmake = TRUE),
                               "roup_id can not be calculated")
      })

      testthat::test_that("Make IDs (e) - participant_code
                          and participant__code", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          make_ids(otree2, gmake = TRUE),
          "You combined data.*messy_time.*messy_chat")
      })

      testthat::test_that("Make IDs (e) - participant_code and
                          participant__code", {
        # Prepare data
        otree2 <- otree_all
        otree2 <- messy_time(otree2, combine = TRUE)

        # Run function
        error <- tryCatch(
          make_ids(otree2, gmake = TRUE),
          error = function(e) e)

        # Test
        testthat::expect_true(grepl(
          "You combined data from old and new oTree versions", error))
        testthat::expect_false(grepl(
          "messy_time", error))
        testthat::expect_true(grepl(
          "messy_chat", error))
      })

      testthat::test_that("Make IDs (e) - participant_code
                          and participant__code", {
        # Prepare data
        otree2 <- otree_all
        otree2 <- messy_chat(otree2, combine = TRUE)

        # Run function
        error <- tryCatch(
          make_ids(otree2, gmake = TRUE),
          error = function(e) e)

        # Test
        testthat::expect_true(grepl(
          "You combined data from old and new oTree versions", error))
        testthat::expect_false(grepl(
          "messy_chat", error))
        testthat::expect_true(grepl(
          "messy_time", error))
      })

      testthat::test_that("Make IDs (e) - NA in participant__code", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Only NAs in participant_code
        otree2$Chats$participant__code[1L] <- NA

        # Run function and test
        testthat::expect_error(
          make_ids(otree2,
                   gmake = TRUE),
          "There are NAs in your participant__code variable in the")
      })

      testthat::test_that("Make IDs (e) - NA in participant_code", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Chats$participant_code[1L] <- NA

        # Run function and test
        testthat::expect_error(
          make_ids(otree2,
                   gmake = TRUE),
          "There are NAs in your participant_code variable in the")
      })

      testthat::test_that("Make IDs (e) - session_code and session__code", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          make_ids(otree2,
                   gmake = FALSE),
          "You combined data from old and new oTree versions")
      })

      testthat::test_that("Make IDs (e) - group_id not the same", {
        otree2 <- otree_5_4_0
        testthat::expect_error(
          make_ids(otree2,
                   gmake = TRUE),
          "You don't have the same group.id_in_subsession in every app")
      })

      testthat::test_that("Make IDs (e) - only use from_app or from_var", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          make_ids(oTree = otree2,
                   from_app = "dictator",
                   from_var = "dictator.1.group.id_in_subsession",
                   gmake = TRUE),
          "Please only use")
      })

      testthat::test_that("Make IDs (e) - from_var constant - nondistinct", {
        # Prepare data
        otree2 <- otree_5_4_0_non_unique

        # Run function and test
        testthat::expect_error(
          otree2 <- make_ids(otree2,
                             from_var = "survey.1.group.id_in_subsession"),
          "length of participant codes is not equal the length of unique participant")
      })

      testthat::test_that("Make IDs (e) - from_var not found", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(make_ids(otree2,
                                        gmake = TRUE,
                                        from_var = "donotfind"),
                               "not found")

        # Test if variables exist / don't exist
        test1 <- is.null(otree2$all_apps_wide$group_id)
        test2 <- is.null(otree2$all_apps_wide$participant_id)
        test3 <- is.null(otree2$all_apps_wide$session_id)
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs (e) - from_var not found,
                          session codes Na", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$session.code[c(1L, 5L, 3L)] <- NA

        # Run function
        testthat::expect_error(

          otree2 <- make_ids(otree2,
                             gmake = TRUE,
                             from_var = "donotfind"),
          "from_var.*not found")

        # Test if variables exist / don't exist
        test1 <- is.null(otree2$all_apps_wide$group_id)
        test2 <- is.null(otree2$all_apps_wide$participant_id)
        test3 <- is.null(otree2$all_apps_wide$session_id)
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("Make IDs (e) - chat_warning", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Chats$participant_code <- 1L

        # Run function and test
        testthat::expect_error(make_ids(otree2, icw = FALSE),
                               "bug")
      })

      testthat::test_that("Make IDs (e) - wrong from_var", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(make_ids(otree2, from_var = "gender"),
                               "not found")
      })

      testthat::test_that("Make IDs (e) - from_app & data there twice", {
        # Prepare data
        otree3 <- otree_5_4_0
        otree3$all_apps_wide <- rbind(otree3$all_apps_wide,
                                      otree3$all_apps_wide)
        otree3$dictator <- rbind(otree3$dictator, otree3$dictator)

        # Run function and test
        testthat::expect_error(
          otree3 <- make_ids(otree3, gmake = TRUE, from_app = "dictator"),
          "The length of participant codes is not equal")
      })

      testthat::test_that("Make IDs - from_app (old)", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- delete_duplicate(otree2)
        otree2$all_apps_wide$session.code
        otree2$dictator$session.code
        order_aaw_before <- otree2$all_apps_wide$participant.code
        order_dictator_before <- otree2$dictator$participant.code

        # Run function
        otree2 <- make_ids(otree2,
                           gmake = TRUE,
                           from_app = "dictator",
                           emptyrows = "yes")
        # Test
        testthat::expect_equal(max(otree2$dictator$participant_id),
                               length(unique(otree2$dictator$participant.code)))
        testthat::expect_equal(max(otree2$dictator$group_id), 6L)
        testthat::expect_equal(max(otree2$dictator$session_id), 2L)

        # Test if variables exist / don't exist
        test1 <- !is.null(otree2$all_apps_wide$participant_id)
        test2 <- !is.null(otree2$dictator$group_id)
        test3 <- !is.null(otree2$survey$participant_id)
        test4 <- !is.null(otree2$survey$group_id)
        test5 <- !is.null(otree2$survey$session_id)

        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))

        # Test if order of DF entries did not change
        order_aaw_after <- otree2$all_apps_wide$participant.code
        order_dictator_after <- otree2$dictator$participant.code

        test1 <- all(order_aaw_before == order_aaw_after)
        test2 <- all(order_dictator_before == order_dictator_after)
        testthat::expect_true(all(c(test1, test2)))

        # Test - check if no two session codes have the same session id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$session_id,
          otree2$all_apps_wide$session.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        testthat::expect_true(test1)
        test2 <- !anyDuplicated(strings)
        testthat::expect_true(test2)

        # Test - check if no two participant codes have the same participant id
        id_code_pairs <- unique(paste(
          otree2$all_apps_wide$participant_id,
          otree2$all_apps_wide$participant.code))

        numbers <- as.integer(sub("^([0-9]+).*", "\\1", id_code_pairs))
        strings <- sub("^[0-9]+ (.*)", "\\1", id_code_pairs)
        test1 <- !anyDuplicated(numbers)
        testthat::expect_true(test1)
        test2 <- !anyDuplicated(strings)
        testthat::expect_true(test2)
      })

      print("---- extime -----")

      # Experiment time   ####
      testthat::test_that("extime - all oTree", {
        # Prepare data
        otree2 <- otree_all
        testthat::expect_warning(
          otree2 <- messy_time(otree2, combine = TRUE, info = TRUE),
          "referred to the time stamp.*participant code")

        # Run function
        output <- extime(otree2)

        # Test
        # Test if variables exist / don't exist
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - new otree", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2, startat = 1L)

        # Test

        # Test if variables exist / don't exist
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - new oTree - secondsonpage2", {
        # Calculation not with time stamp but with secondsonpage2
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2)
        otree3 <- pagesec(otree2)
        otree3$Time$epoch_time_completed <- NULL
        output2 <- extime(otree3)

        # Test
        test1 <- output$mean_duration == output2$mean_duration
        test2 <- output$min_duration & output2$min_duration
        test3 <- output$min_duration & output2$min_duration
        test4 <- exists("single_durations", output2)

        test6 <- round(mean(output2$single_durations$duration),
                       digits = 2L) == output2$mean_duration

        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test6)))
      })

      testthat::test_that("extime - new otree seconds on page error", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        otree2 <- pagesec(otree2)
        otree2$Time$epoch_time_completed <- NULL

        testthat::expect_error(
          extime(otree2,
                 startat = "real",
                 sinfo = NULL),
          "There is no variable referring to the time stamp"
        )
      })

      testthat::test_that("extime - messy not included", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          output <- extime(otree2, combine = FALSE), # Is automatically TRUE
          "using data from different "
        )
      })

      testthat::test_that("extime - messy included", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        output <- extime(otree2, combine = TRUE)  # Is automatically TRUE

        # Test

        # Test if variables exist / don't exist
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - 3 digits", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2, digits = 3L)

        # Test
        test1 <- round(mean(output$single_durations$duration), digits = 3L) ==
          output$mean_duration
        test2 <- round(min(output$single_durations$duration), digits = 3L) ==
          output$min_duration
        test3 <- round(max(output$single_durations$duration), digits = 3L) ==
          output$max_duration
        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - for a specific group", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Run
        output <- extime(otree2, group_id = 1L)

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("extime - experiment just one page", {
        otree2 <- otree_2_2_4

        # Run
        output <- extime(otree2, sinfo = NULL)

        # Test

        # Test if variables exist / don't exist
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        test5 <- grepl(
          pattern = "the experiment only has one page",
          x = output$messages)
        test6 <- is.vector(output$only_one_page)

        # Test if the values and the single_durations align
        test7 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test8 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test9 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7, test8,
                                    test9)))
      })

      testthat::test_that("extime - with sinfo session_code", {
        otree2 <- otree_all
        testthat::expect_warning(
          otree2 <- messy_time(otree2, combine = TRUE, info = TRUE),
          "More than one variable referred to")

        # Run function
        output <- extime(otree2, sinfo = "session_code")

        # Test

        # Test if variables exist / don't exist
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - session_id", {
        # Prepare data
        otree2 <- otree_all
        otree2 <- delete_duplicate(otree2)
        testthat::expect_warning(
          otree2 <- messy_chat(otree2, combine = TRUE, info = TRUE),
          "More than one variable referred to")

        testthat::expect_warning(
          otree2 <- messy_time(otree2, combine = TRUE, info = TRUE),
          "More than one variable referred to")

        otree2 <- make_ids(otree2, pmake = TRUE)

        # Run function
        output <- extime(otree2,
                         combine = TRUE,
                         sinfo = "session_id")

        # Test
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)
        test4 <- is.numeric(output$single_durations$session)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)

        # Test warnings
        test8 <- !is.null(output$only_one_page)
        test9 <- any(grepl(
          "For at least one participant, the experiment only has one page",
          output$messages))

        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7, test8,
                                    test9)))
      })

      testthat::test_that("extime - otree all: no session_id", {
        # Prepare data
        otree2 <- otree_all
        otree2 <- delete_duplicate(otree2)

        # Run function
        output <- extime(otree2,
                         combine = TRUE,
                         sinfo = "session_id")
        # Test
        test1 <- is.numeric(output$single_durations$session)
        testthat::expect_true(test1)
      })

      testthat::test_that("extime - otree all: session_code", {
        # Prepare data
        otree2 <- otree_all
        otree2 <- delete_duplicate(otree2)

        # Run function
        output <- extime(otree2,
                         combine = TRUE,
                         sinfo = "session_code")
        # Test
        test1 <- is.vector(output$single_durations$session)
        testthat::expect_true(test1)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test5, test6, test7)))
      })

      testthat::test_that("extime - one person only one page", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- aggregate(otree2$Time,
                            list(person = otree2$Time$participant__code),
                            FUN = "max")
        person <- unlist(person[person$page_index == 1L, ][1L])

        # Run function
        testthat::expect_warning(
          output <- extime(otree2,
                           combine = TRUE,
                           sinfo = NULL,
                           person), "the experiment only has one page")

        # Test
        test1 <- is.na(output)
        testthat::expect_true(test1)
      })

      testthat::test_that("extime - startat real", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2,
                         startat = "real",
                         seconds = TRUE,
                         sinfo = NULL)

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - startat comparison - comparison
                          real und 1", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2,
                         startat = "real",
                         seconds = TRUE,
                         sinfo = NULL)

        output2 <- extime(otree2,
                          startat = 1L,
                          seconds = TRUE,
                          sinfo = NULL)

        # The difference to startat = 1 is that startat = 1 does
        # not result in decimal numbers.
        # startat = "real" has a more precise start time!!!

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - seconds", {
        otree2 <- otree_5_4_0

        # Run function
        output <- extime(otree2, seconds = TRUE)

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - old otree", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        output <- extime(otree2, sinfo = NULL)

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - old otree seconds on page", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        output <- extime(otree2, sinfo = NULL)
        otree2$Time$time_stamp <- NULL
        output2 <- extime(otree2, sinfo = NULL)

        # Test
        test1 <- output$mean_duration == output2$mean_duration
        test2 <- output$min_duration & output2$min_duration
        test3 <- output$min_duration & output2$min_duration
        test4 <- exists("single_durations", output2)

        test6 <- round(mean(output2$single_durations$duration),
                       digits = 2L) == output2$mean_duration

        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test6)))
      })

      testthat::test_that("extime - old otree seconds on page error", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        otree2$Time$time_stamp <- NULL
        testthat::expect_error(
          extime(otree2,
                 startat = "real",
                 sinfo = NULL),
          "There is no variable referring to the time stamp"
        )
      })

      testthat::test_that("extime - oTree version 2.2.4", {
        # Prepare data
        otree2 <- otree_2_2_4
        # otree2$Time$session_id # There is already a variable called session_id
        otree2 <- make_ids(otree2)
        # otree2$Time$session_id  # The new session_id looks the same

        # Run function
        output <- extime(otree2, sinfo = "session_id")

        # Test
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("extime - old specific", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function
        output <- extime(otree2,
                         pcode = person,
                         sinfo = NULL)

        # Test
        test <- !is.null(output) & !is.na(output)
        testthat::expect_true(test)
      })

      testthat::test_that("extime - old startatreal without session info", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Run function
        output <- extime(otree2, startat = "real", sinfo = NULL)

        # Test
        testthat::expect_output(str(output), "List of 6")

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test5, test6, test7)))
      })

      testthat::test_that("extime - old startatreal with session info", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Run function and test
        # If sinfo is not NULL, there will be an error in some of the old oTree
        # versions because they don't save the session_code there
        testthat::expect_error(
          output <- extime(otree2, startat = "real", sinfo = "session_code"),
          "There is no variable called session_code or session__code")
      })

      testthat::test_that("extime - for a specific individual - code", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function
        output <- extime(otree2, person)

        # Test
        # Last entry for the person
        max <- max(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person])

        # First entry for the person
        min <- min(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person])

        # Duration
        duration <- max - min
        duration <- duration / 60L
        testthat::expect_identical(output, round(duration, 2L))
      })

      testthat::test_that("extime - for a specific individual - label", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Make unique participant labels
        otree2$all_apps_wide$participant.label <-
          sapply(1L:length(otree2$all_apps_wide$participant.label),
                 function(x) paste("Person ", x))

        person <- otree2$all_apps_wide$participant.label[1L]

        # Run function
        output <- extime(otree2,
                         plabel = person,
                         sinfo = NULL)

        pcode <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant.label == person]

        # Test
        max <- max(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == pcode])

        min <- min(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == pcode])

        duration <- max - min
        duration <- duration / 60L
        testthat::expect_identical(output, round(duration, 2L))
      })

      testthat::test_that("extime - for a specific individual (real)", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")
        person1 <- unique(otree2$Time$participant_code)[1L]

        # Run function
        output <- extime(otree2, person1, startat = "real")

        # Test
        max <- max(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person1])
        min <- (as.numeric(
          as.POSIXct(otree2$all_apps_wide$participant.time_started[
            otree2$all_apps_wide$participant.code == person1], tz = "UTC")))

        duration <- max - min
        duration <- duration / 60L
        testthat::expect_identical(output, round(duration, 2L))
      })

      testthat::test_that("extime - old specific, warning not enough entries", {
        # Prepare data (get a person with only one index entry)
        otree2 <- otree_2_2_4
        person <- aggregate(otree2$Time,
                            list(person = otree2$Time$participant__code),
                            FUN = "max")
        person <- unlist(person[person$page_index == 1L, ][1L])

        # Run function and test
        testthat::expect_warning(
          output <- extime(otree2,
                           pcode = person,
                           sinfo = NULL),
          "the experiment only has one page")

        test1 <- is.na(output)
        testthat::expect_true(test1)
      })

      testthat::test_that("extime - old specific, startat", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function
        output <- extime(otree2,
                         pcode = person,
                         sinfo = NULL,
                         startat = "real")

        # Test
        test <- !is.null(output) & !is.na(output)
        testthat::expect_true(test)
      })

      testthat::test_that("extime - old startatreal", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        output <- extime(otree2, startat = "real", sinfo = NULL)

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)
        test4 <- !is.null(output$single_durations)
        test5 <- is.null(output$single_durations$session)
        test6 <- !is.null(output$messages)
        test7 <- !is.null(output$only_one_page)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))

        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test5, test6, test7)))
      })

      testthat::test_that("extime - old startat and 0 comparison", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function
        startattime_real <- extime(otree2,
                                   startat = "real",
                                   seconds = TRUE,
                                   digits = 5L,
                                   sinfo = NULL)
        startattime_0 <- extime(otree2,
                                startat = 1L,
                                seconds = TRUE,
                                digits = 5L,
                                sinfo = NULL)

        # Test
        val_real <- otree2$all_apps_wide$participant.time_started_utc[
          otree2$all_apps_wide$participant.code == person]

        # Delete last part of val_real (more detailed time than val0)
        # val0 only seconds
        val_real <- stringr::str_remove(val_real, ".[0-9]{6}")

        val0 <- otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person &
            otree2$Time$page_index == 0L]

        val0 <- as.POSIXct(val0, tz = "UTC", origin = "1970-01-01")
        # Remove UTC
        val0 <- stringr::str_remove(val0, " UTC")
        test1 <- val_real == val0
        testthat::expect_true(test1)
      })

      testthat::test_that("extime (w) - warning only
                          one page for old specific", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- aggregate(otree2$Time,
                            list(person = otree2$Time$participant__code),
                            FUN = "max")
        person <- unlist(person[person$page_index == 1L, ][1L])

        # Run function and test
        testthat::expect_warning(
          output <- extime(otree2,
                           pcode = person,
                           sinfo = NULL),
          "the experiment only has one page")
        test <- is.na(output)
        testthat::expect_true(test)
      })

      testthat::test_that("extime (e) - plabel and group_id", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 plabel = "Person 1",
                 group_id = 1L,
                 sinfo = NULL),
          "Please enter only plabel or group_id")
      })

      testthat::test_that("extime (e) - plabel and pcode", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 plabel = "Person 1",
                 pcode = person,
                 sinfo = NULL),
          "Please enter only pcode or plabel")
      })

      testthat::test_that("extime (e) - several plabels", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 plabel = c("Person 1", "Person 2"),
                 sinfo = NULL),
          "Please enter only one participant label")
      })

      testthat::test_that("extime (e) - plabel but no aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 plabel = "Person 1",
                 sinfo = NULL),
          "if there is an all_apps_wide-data frame")
      })

      testthat::test_that("extime (e) - old specific: TIME empty", {
        # Prepare data (delete everything from time)
        otree2 <- otree_2_2_4
        otree2$Time <- otree2$Time[NULL, ]

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      sinfo = NULL),
                               "data frame is empty")
      })

      testthat::test_that("extime (e) - old specific: TIME for one empty", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- otree2$all_apps_wide$participant.code[2L]
        otree2$Time <- otree2$Time[otree2$Time$participant__code != person, ]

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      pcode = person,
                                      sinfo = NULL),
                               "The participant is not in the")
      })

      testthat::test_that("extime (e) - starting value old specific", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- otree2$all_apps_wide$participant.code[2L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 pcode = person,
                 sinfo = NULL,
                 startat = 20L),
          "The chosen starting value startat is higher than the total")
      })

      testthat::test_that("extime (e) - startat specific - old", {
        # Prepare data
        otree2 <- otree_2_2_4
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 startat = 400L,
                 pcode = person,
                 sinfo = NULL),
          "The chosen starting value startat is higher than the total number")
      })

      testthat::test_that("extime (e) - startat: old", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 startat = 400L,
                 sinfo = NULL),
          "The chosen starting value startat is higher than the total number")
      })

      testthat::test_that("extime (e) - startat: new", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 startat = 400L,
                 sinfo = NULL),
          "The chosen starting value startat is higher than the total number")
      })

      testthat::test_that("extime (e) - startat: new specific", {
        # Prepare data
        otree2 <- otree_5_4_0
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 pcode = person,
                 startat = 400L,
                 sinfo = NULL),
          "The chosen starting value startat is higher than the total number")
      })

      testthat::test_that("extime (e) - startat: old specific", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- delete_duplicate(otree2)
        person <- otree2$all_apps_wide$participant.code[1L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 pcode = person,
                 startat = 400L,
                 sinfo = NULL),
          "The chosen starting value startat is higher than the total number")
      })

      testthat::test_that("extime (e) - startat: new too low", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2$Time <- otree2$Time[otree2$Time$page_index > 2L, ]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 startat = 1L,
                 sinfo = NULL),
          "is lower than")
      })

      testthat::test_that("extime (e) - startat: new too low", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2$Time <- otree2$Time[otree2$Time$page_index > 2L, ]
        person <- otree2$Time$participant__code[otree2$Time$page_index > 5L][1L]

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 pcode = person,
                 startat = 1L,
                 sinfo = NULL),
          "is lower than")
      })

      testthat::test_that("extime (e) - startat real and no aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      startat = "real",
                                      sinfo = NULL),
                               "only works if there is a")
      })

      testthat::test_that("extime (e) - nonexistent specific individual ", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      "fake_individual"),
                               "The participant is not in the")
      })

      testthat::test_that("extime (e) - individual + group specified", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      pcode = "3ttf7yix",
                                      group_id = 1L),
                               "specify either the pcode or the group_id")
      })

      testthat::test_that("extime (e) - variable group_id not there", {
        # Run function and test
        otree2 <- otree_5_4_0

        # Run function and test error if group_id is not defined yet
        testthat::expect_error(extime(otree2, group_id = 7L))
      })

      testthat::test_that("extime (e) - this group_id not there", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Run function and test error if chosen group_id is not in data frame
        testthat::expect_error(extime(otree2, group_id = 47L))
      })

      testthat::test_that("extime (e) - no session_code", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time$session_code <- NULL

        # Run function and test error if chosen group_id is not in data frame
        testthat::expect_error(extime(otree2),
                               "no variable called session_code")
      })

      testthat::test_that("extime (e) - valid session info", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      sinfo = "any"),
                               "Please specify a valid sinfo")
      })

      testthat::test_that("extime (e) - no session id", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time$session_id <- NULL

        # Run function and test
        testthat::expect_error(extime(otree2,
                                      sinfo = "session_id"),
                               "There is no session_id in the")
      })

      testthat::test_that("extime (e) - only one participant", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 pcode = c("164r1hs4", "7wa8kk3d")),
          "Please enter only one participant")
      })

      testthat::test_that("extime (e) - no Time data frame", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time <- NULL

        # Run function and test
        testthat::expect_error(extime(otree2),
                               "There is no \"Time\" data frame")
      })

      testthat::test_that("extime (e) - participant not there", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          extime(otree2,
                 sinfo = "session_id",
                 combine = TRUE,
                 pcode = "wrongcode"), "The participant is not in the ")
      })

      testthat::test_that("extime (e) - messy data", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test error
        testthat::expect_error(
          extime(otree2,
                 sinfo = "session_id",
                 combine = FALSE),
          "It seems as if you are using data from different oTree versions")
      })

      testthat::test_that("extime (e) - starting value", {
        # Prepare data
        otree2 <- otree_all
        namesbefore <- names(otree2$Time)

        # Run function
        testthat::expect_error(extime(otree2,
                                      combine = TRUE,
                                      startat = 400L),
                               "The chosen starting value startat")

        # Test
        namesafter <- names(otree2$Time)
        test1 <- length(namesbefore) == length(namesafter) # of course
        testthat::expect_true(test1)
      })

      testthat::test_that("extime (e) - otree new - no session_id", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- delete_duplicate(otree2)

        # Run function
        testthat::expect_error(
          extime(otree2,
                 combine = TRUE,
                 sinfo = "session_id"),
          "no session_id in the")
      })

      testthat::test_that("extime (e) - no time stamp", {
        otree2 <- otree_5_4_0
        otree2$Time$epoch_time_completed <- NULL
        testthat::expect_error(extime(otree2),
                               "There is no variable")
      })

      testthat::test_that("extime (e) - no valid starttime", {
        otree2 <- otree_5_4_0
        testthat::expect_error(extime(otree2,
                                      startat = -3L),
                               "Please choose a valid")
      })

      testthat::test_that("extime (e) - no paticipant var", {
        otree2 <- otree_5_4_0
        otree2$Time$participant_code <- NULL
        testthat::expect_error(extime(otree2),
                               "There is no variable")
      })

      print("---- apptime -----")

      # App time   ####

      testthat::test_that("App time - all apps old oTree specified person", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)
        person <- otree_2_2_4$Time$participant__code[
          otree_2_2_4$Time$app_name == "survey"][1L]

        # Run function
        output <- apptime(otree2,
                          pcode = person,
                          sinfo = "session_id")

        # Test
        test1 <- is.numeric(output$chatapp)
        test2 <- is.numeric(output$dictator)
        test3 <- is.numeric(output$start) # NA

        testthat::expect_true(all(c(test1, test2, test3)))
        testthat::expect_output(str(output), "List of 4")
      })

      testthat::test_that("App time - all apps, seconds = FALSE", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- apptime(otree2,
                          sinfo = NULL)

        # Test
        testthat::expect_output(str(output), "List of 6")

        # Important. In new oTree, there is no need for a first-app-one-stage
        # warning, because there is a index number 0!
        # Also: firstappwarning is also not shown for new oTree.
        test1 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test2 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test3 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)
        test4 <- (nrow(output$dictator$single_durations) +
                    length(output$dictator$warnings)) ==
          nrow(otree2$dictator) / 3L
        # Test if the values and the single_durations align
        test5 <- round(output$dictator$min_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test6 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))

        testthat::expect_true(
          grepl(pattern =
                  "or some participants, no duration could be calculated",
                x = output$chatapp$message))

        testthat::expect_true(exists("warnings", output$chatapp))
      })

      testthat::test_that("App time - all apps, without session info", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- apptime(otree2)

        # Test
        testthat::expect_output(str(output), "List of 4")
        # Test fi values are there
        test1 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test2 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test3 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)
        test4 <-
          (nrow(output$dictator$single_durations) +
             length(output$dictator$warnings)) == nrow(otree2$dictator) / 3L

        # Test if the values and the single_durations align
        test5 <- round(output$dictator$min_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test6 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)
        # Test warnings
        test8 <-
          !(any(output$survey$first_app_one_page %in% output$survey$warnings))
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7, test8)))
      })

      testthat::test_that("App time - all apps, seconds = TRUE", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output_second <- apptime(otree2, seconds = TRUE, digits = 4L)
        output_minutes <- apptime(otree2, seconds = FALSE, digits = 4L)

        # Test
        testthat::expect_output(str(output_second), "List of 4")

        test1 <-
          round(output_minutes$dictator$mean_duration * 60L, 2L) ==
          round(output_second$dictator$mean_duration, 2L)
        test2 <-
          round(output_minutes$dictator$min_duration  * 60L, 2L) ==
          round(output_second$dictator$min_duration, 2L)
        test3 <-
          round(output_minutes$dictator$max_duration * 60L, 2L) ==
          round(output_second$dictator$max_duration, 2L)
        testthat::expect_true(all(c(test1, test2, test3)))
      })

      testthat::test_that("App time - all apps, old oTree", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2,
                          sinfo = "session_id")

        # Test
        testthat::expect_output(str(output$start), "List of 2")
        testthat::expect_output(str(output), "List of 4")

        test1 <- grepl(pattern = "Durations not calculated",
                       x = output$start$message)
        test2 <- is.vector(output$start$first_app_one_page)
        nrow(otree2$dictator)  # 36 / 3 = 12
        test3 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test4 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test5 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)
        # Solche Tests gehen im alten oTree nicht,
        # weil nicht alle Personen auch in der Time-File sind.
        # test4 <- (nrow(output$dictator$single_durations) +
        # length(output$dictator$warnings)) == nrow(otree2$dictator) / 3
        # Test if the values and the single_durations align

        test6 <- round(output$dictator$min_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test8 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7, test8)))
      })

      testthat::test_that("App time - one app firstappproblemparticipant", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Delete rows to make the first app to only have one page
        otree2$Time <- otree2$Time[otree2$Time$page_index != 0L, ]

        # Run function
        output <- apptime(otree2,
                          "start")

        test1 <- expect_true(grepl(
          pattern = "If the first app only has one page, the indices",
          x = output$start$message))
      })

      testthat::test_that("App time - several apps
                          firstappproblemparticipant", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Delete rows to make the first app to only have one page
        otree2$Time <- otree2$Time[otree2$Time$page_index != 0L, ]

        # Run function
        output <- apptime(otree2)

        test1 <- output$start$message <-
          "If the first app only has one page, the indices"

      })

      testthat::test_that("App time - one app", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- apptime(otree2,
                          "dictator")

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- !is.null(output$mean_duration) &
          !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) &
          !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) &
          !is.na(output$max_duration)
        # Test if the values and the single_durations align

        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test5, test6, test7)))

        testthat::expect_true(
          grepl("or some participants, no duration could be calculated",
                output$messages))

        testthat::expect_true(exists("warnings", output))

      })

      testthat::test_that("App time - all apps, old oTree seconds on page2", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        otree2 <- pagesec(otree2)

        otree2$Time$time_stamp <- NULL

        # Run function
        output <- apptime(otree2,
                          sinfo = "session_id")

        # Test
        testthat::expect_output(str(output$start), "List of 4")
        testthat::expect_output(str(output), "List of 4")

        nrow(otree2$dictator)  # 36 / 3 = 12
        test3 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test4 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test5 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)
        # Solche Tests gehen im alten oTree nicht,
        # weil nicht alle Personen auch in der Time-File sind.
        # test4 <- (nrow(output$dictator$single_durations) +
        # length(output$dictator$warnings)) == nrow(otree2$dictator) / 3
        # Test if the values and the single_durations align

        test6 <- round(output$dictator$min_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test8 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)

        testthat::expect_true(all(c(test3, test4,
                                    test5, test6, test7, test8)))

      })

      testthat::test_that("App time - all apps, old oTree seconds on page", {

        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        otree2$Time$time_stamp <- NULL

        # Run function
        output <- apptime(otree2,
                          sinfo = "session_id")

        # Test
        testthat::expect_output(str(output$start), "List of 4")
        testthat::expect_output(str(output), "List of 4")

        nrow(otree2$dictator)  # 36 / 3 = 12
        test3 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test4 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test5 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)
        # Solche Tests gehen im alten oTree nicht,
        # weil nicht alle Personen auch in der Time-File sind.
        # test4 <- (nrow(output$dictator$single_durations) +
        # length(output$dictator$warnings)) == nrow(otree2$dictator) / 3
        # Test if the values and the single_durations align

        test6 <- round(output$dictator$min_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test8 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)

        testthat::expect_true(all(c(test3, test4,
                                    test5, test6, test7, test8)))

      })

      testthat::test_that("App time - one app - seconds on page 2", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- pagesec(otree2)
        otree2$Time$epoch_time_completed <- NULL

        # Run function
        output <- apptime(otree2,
                          "dictator")

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- !is.null(output$mean_duration) &
          !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) &
          !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) &
          !is.na(output$max_duration)
        # Test if the values and the single_durations align

        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)

        testthat::expect_true(all(c(test1, test2, test3,
                                    test5, test6, test7)))

        warningmessage <- grepl("For some participants, no duration could be",
                                output$messages)
        testthat::expect_true(warningmessage)

      })

      testthat::test_that("App time - one app", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- apptime(otree2,
                          "dictator")

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- !is.null(output$mean_duration) &
          !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) &
          !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) &
          !is.na(output$max_duration)
        testthat::expect_true(all(c(test1, test2, test3)))
        # Test if the values and the single_durations align
        test5 <- round(output$min_duration, 2L) ==
          round(min(output$single_durations$duration), 2L)
        test6 <- round(output$max_duration, 2L) ==
          round(max(output$single_durations$duration), 2L)
        test7 <- round(output$mean_duration, 2L) ==
          round(mean(output$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test5, test6, test7)))
      })

      testthat::test_that("App time - Time df empty", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2$Time <- otree2$Time[NULL, ]

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       sinfo = NULL),
                               "data frame is empty")
      })

      testthat::test_that("App time - epoch time ", {
        # Prepare data
        otree2 <- otree_old_one

        # Run function
        output <- apptime(otree2)

        # Test
        testthat::expect_output(str(output), "List of 4")

        test1 <- "chatapp" %in% names(output)
        test2 <- "survey" %in% names(output)
        test3 <- "dictator" %in% names(output)
        test4 <- "survey" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test if the values and the single_durations align
        test5 <- round(output$dictator$mean_duration, 2L) ==
          round(min(output$dictator$single_durations$duration), 2L)
        test6 <- round(output$dictator$max_duration, 2L) ==
          round(max(output$dictator$single_durations$duration), 2L)
        test7 <- round(output$dictator$mean_duration, 2L) ==
          round(mean(output$dictator$single_durations$duration), 2L)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test5, test6, test7)))
      })

      testthat::test_that("App time - all apps old oTree group", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2,
                           gmake = TRUE,
                           from_var = "dictator.1.group.id_in_subsession")

        # Run function
        output <- apptime(otree2,
                          group_id = 3L,
                          sinfo = "session_id")

        # Test
        test <- setequal(names(output),
                         c("chatapp", "dictator", "start", "survey"))
        testthat::expect_true(test)

        # Test for start
        test1 <- setequal(names(output$start),
                          c("first_app_one_page", "message"))
        testthat::expect_true(test1)

        # Test for chatapp
        test1 <- is.numeric(output$chatapp$mean_duration)
        test2 <- is.numeric(output$chatapp$max_duration)
        test3 <- is.numeric(output$chatapp$min_duration)
        test4 <- !is.null(output$chatapp$single_durations)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test for dictator
        test1 <- is.numeric(output$dictator$mean_duration)
        test2 <- is.numeric(output$dictator$max_duration)
        test3 <- is.numeric(output$dictator$min_duration)
        test4 <- !is.null(output$dictator$single_durations)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test for survey
        test1 <- is.numeric(output$survey$mean_duration)
        test2 <- is.numeric(output$survey$max_duration)
        test3 <- is.numeric(output$survey$min_duration)
        test4 <- !is.null(output$survey$single_durations)
        testthat::expect_true(all(c(test1, test2, test3, test4)))
      })

      testthat::test_that("App time - all apps old oTree session code", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        # Run function and test
        testthat::expect_error(apptime(otree2, sinfo = "session_code"),
                               "There is no session_code or session__code")
      })

      testthat::test_that("App time - all apps no session info ", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2, sinfo = NULL)

        # Test
        testthat::expect_output(str(output), "List of 4")
        test1 <- setequal(names(output),
                          c("chatapp", "dictator", "start", "survey"))
        testthat::expect_true(test1)

        # Test for survey
        test1 <- is.numeric(output$survey$mean_duration)
        test2 <- is.numeric(output$survey$max_duration)
        test3 <- is.numeric(output$survey$min_duration)
        test4 <- is.null(output$survey$single_durations$session)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test for dictator
        test1 <- is.numeric(output$dictator$mean_duration)
        test2 <- is.numeric(output$dictator$max_duration)
        test3 <- is.numeric(output$dictator$min_duration)
        test4 <- is.null(output$dictator$single_durations$session)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

        # Test for start (only old otree not startreal)
        test1 <- setequal(names(output$start),
                          c("first_app_one_page", "message"))
      })

      testthat::test_that("App time - one app: one wrong app", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)

        # Run function and test
        testthat::expect_error(
          apptime(otree2,
                  apps = "wrongapp"),
          "The apps specified in the argument apps are not in the")
      })

      testthat::test_that("App time - one app: no error one app okay", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)

        # Run function
        testthat::expect_warning(
          output <- apptime(otree2,
                            apps = c("wrongapp", "dictator")),
          "not in the list of oTree data fram")

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        testthat::expect_true(all(c(test1, test2, test3, test4)))

      })

      testthat::test_that("App time - one app:
                          single durations didnt make it", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2,
                          pcode = "2scvem7a",
                          apps = "survey")

        # Test
        test <- is.na(output)
        testthat::expect_true(test)

      })

      testthat::test_that("App time - two apps:
                          single durations didnt make it", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)
        person <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant._current_app_name == "dictator"][1L]

        # Run function
        output <- apptime(otree2,
                          pcode = "2scvem7a",
                          apps = c("survey", "chatapp"))

        # Test
        test1 <- setequal(names(output), c("survey", "chatapp"))
        test2 <- is.na(output$chatapp)
        test3 <- is.na(output$survey)
        testthat::expect_true(all(c(test1, test2, test3)))

      })

      testthat::test_that("App time - all apps:
                          single durations didnt make it", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)
        person <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant._current_app_name == "dictator"][1L]

        # Run function
        testthat::expect_warning(
          output <- apptime(otree2,
                            pcode = person),
          "Duration could not be calculated for the person"
        )

        # Test
        test1 <- is.numeric(output$start)
        test2 <- is.numeric(output$dictator)
        test3 <- is.na(output$chatapp)
        test4 <- is.na(output$survey)
        testthat::expect_true(all(test1, test2, test3, test4))
      })

      testthat::test_that("App time - one app: several
                          durations didnt make it", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2,
                          apps = "survey")

        # Test
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)
        test4 <- !is.null(output$single_durations)
        test5 <- grepl("For some participants, no duration could",
                       output$messages)
        test6 <- is.vector(output$warnings)
        test7 <- !(output$warnings %in% output$single_durations)
        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5, test6,
                                    test7)))
      })

      testthat::test_that("App time - several apps: several didnt make it", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2)

        # Test
        warningmessage <- grepl("For some participants, no duration could",
                                output$survey$messages)

        testthat::expect_true(warningmessage)

        sum1 <- nrow(output$survey$single_durations) +
          length(output$survey$warnings)
        sum2 <- nrow(output$dictator$single_durations) +
          length(output$dictator$warnings)
        sum3 <- nrow(output$chatapp$single_durations) +
          length(output$chatapp$warnings)

        testthat::expect_identical(sum1, sum2)
        testthat::expect_identical(sum1, sum3)
        # Dictator should have more cases than survey
        testthat::expect_gte(nrow(output$dictator$single_durations),
                             nrow(output$survey$single_durations))
      })

      testthat::test_that("App time - one app oTree old", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2, "dictator", sinfo = "session_id")

        # Test
        testthat::expect_output(str(output), "List of 6")
        test1 <- !is.null(output$mean_duration) & !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) & !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) & !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        test5 <- exists("warnings", output)
        testthat::expect_true(all(c(test1, test2, test3, test4, test5)))
      })

      testthat::test_that("App time - all duplicate participants", {
        # Prepare data (make duplicate data)
        otree2 <- otree_5_4_0
        otree2$Time <- rbind(otree2$Time, otree2$Time)

        # Run function
        output <- apptime(otree2,
                          apps = "survey")

        # Test
        test <- grepl("Durations not calculated", output$survey)
        testthat::expect_true(test)
      })

      testthat::test_that("App time - all duplicate participants all apps", {
        # Prepare data (make duplicate data)
        otree2 <- otree_5_4_0
        otree2$Time <- rbind(otree2$Time, otree2$Time)

        # Run function
        output <- apptime(otree2)

        # Test
        test <- grepl("Durations not calculated", output$survey)
        testthat::expect_true(test)
      })

      testthat::test_that("App time - one duplicate
                          participant - all data one app", {
        # Prepare data
        otree2 <- otree_5_4_0
        person1 <- unique(otree2$Time$participant_code)[1L]

        # Make duplicate data
        otree2$Time <-
          rbind(otree2$Time,
                otree2$Time[otree2$Time$participant_code == person1, ])

        # Run function and test dictator
        output <- apptime(otree2,
                          apps = "dictator")

        test0 <- all(output$duplicate_participants == person1)
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)

        test4 <- all(output$duplicate_participants %in% (c(person1)))
        test5 <- any(grepl("have duplicate data", output$messages))
        test6 <- any(grepl("no duration could be calculated", output$messages))

        test7 <- !(output$single_durations$participant %in%
                     output$duplicate_participants)
        test8 <- !(output$single_durations$participant %in%
                     output$warnings)
        test9 <- !(output$duplicate_participants %in%
                     output$warnings)
        test10 <- !(output$warnings %in%
                      output$duplicate_participants)

        testthat::expect_true(all(c(test0, test1, test2, test3,
                                    test4, test5, test6,
                                    test7, test8, test9,
                                    test10)))

        # Run function and test start
        output <- apptime(otree2,
                          apps = "start")
        test0 <- all(output$duplicate_participants == person1)
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)
        test4 <- all(output$duplicate_participants %in% (c(person1)))
        test5 <- any(grepl("have duplicate data", output$messages))

        test7 <- !(output$single_durations$participant %in%
                     output$duplicate_participants)
        test8 <- !("warnings" %in% names(output))

        testthat::expect_true(all(c(test0, test1, test2, test3,
                                    test4, test5, test6,
                                    test7, test8)))
      })

      testthat::test_that("App time - some duplicate participants", {
        # Prepare data (make duplicate data)
        otree2 <- otree_5_4_0
        person1 <- unique(otree2$Time$participant_code)[1L]
        person2 <- unique(otree2$Time$participant_code)[2L]
        otree2$Time <-
          rbind(otree2$Time,
                otree2$Time[otree2$Time$participant_code == person1, ],
                otree2$Time[otree2$Time$participant_code == person2, ])

        # Run function
        output <- apptime(otree2,
                          apps = "dictator")

        # Test
        test1 <- is.numeric(output$mean_duration)
        test2 <- is.numeric(output$max_duration)
        test3 <- is.numeric(output$min_duration)
        test4 <- all(output$duplicate_participants %in% (c(person1, person2)))
        test5 <- any(grepl("have duplicate data", output$messages))
        test6 <- any(grepl("no duration could be calculated", output$messages))
        test7 <- !(output$single_durations$participant %in%
                     output$duplicate_participants)
        test8 <- !(output$single_durations$participant %in%
                     output$warnings)
        test9 <- !(output$duplicate_participants %in%
                     output$warnings)
        test10 <- !(output$warnings %in%
                      output$duplicate_participants)

        testthat::expect_true(all(c(test1, test2, test3,
                                    test4, test5, test6,
                                    test7, test8, test9,
                                    test10)))
      })

      testthat::test_that("App time - a whole session not there", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time$app_name[otree2$Time$app_name == "survey"] <- NA

        # Run function
        output <- apptime(otree2)

        # Test
        test <- grepl("Durations not calculated", output$survey)
        testthat::expect_true(test)  # Die überall schön machen
      })

      testthat::test_that("App time - participant is there more often ", {
        # Prepare data
        otree2 <- otree_5_4_0
        persons <- unique(otree2$Time$participant_code)[c(1L, 2L, 3L, 4L)]

        otree2$Time <- rbind(otree2$Time,
                             otree2$Time[
                               otree2$Time$participant_code %in% persons, ])

        # Run and test dictator
        output <- apptime(otree2,
                          apps = "dictator")
        test1 <- all(output$duplicate_participants == persons)
        testthat::expect_true(test1)
        test2 <- any(grepl("have duplicate data", output$messages))
        testthat::expect_true(test2)
        test3 <- any(grepl("no duration could be calculated", output$messages))
        testthat::expect_true(test3)

        # Rund and test start
        output <- apptime(otree2,
                          apps = "start")
        test1 <- all(output$duplicate_participants == persons)
        testthat::expect_true(test1)
        test2 <- any(grepl("have duplicate data", output$messages))
        testthat::expect_true(test2)
      })

      testthat::test_that("App time - participant code", {
        # Prepare data
        otree2 <- otree_5_4_0

        person1 <- unique(otree2$Time$participant_code[
          otree2$Time$page_index > 10L])[1L]

        # Expect
        x <- otree2$Time[otree2$Time$participant_code == person1  &
                           otree2$Time$app_name == "dictator", ]
        max <- max(x$epoch_time_completed)

        min <- min(x$epoch_time_completed)
        min <- max(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person1 &
            otree2$Time$epoch_time_completed < min])

        diff <- max - min
        diff <- diff / 60L

        # Run function
        output <- apptime(otree2, pcode = c(person1))

        # Test
        testthat::expect_identical(
          output$dictator,
          round(diff, digits = 2L))
      })

      testthat::test_that("App time - participant code did not make it", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Person with only certain entries
        person1 <- unique(otree2$Time$participant_code[
          otree2$Time$page_index > 3L &
            otree2$Time$page_index < 7L])[1L]

        # Delete the last pages
        otree2$Time <- otree2$Time[!(otree2$Time$participant_code == person1 &
                                       otree2$Time$page_index >= 7L), ]

        # Expect for dictator app
        x <- otree2$Time[otree2$Time$participant_code == person1  &
                           otree2$Time$app_name == "dictator", ]
        max <- max(x$epoch_time_completed)

        min <- min(x$epoch_time_completed)
        min <- max(otree2$Time$epoch_time_completed[
          otree2$Time$participant_code == person1 &
            otree2$Time$epoch_time_completed < min])

        diff <- max - min
        diff <- diff / 60L

        # Run function
        testthat::expect_warning(
          output <- apptime(otree2,
                            pcode = c(person1)),
          "Duration could not be calculated for the person in app")

        # Test
        testthat::expect_identical(
          output$dictator,
          round(diff, digits = 2L))
      })

      testthat::test_that("App time - participant code
                          not in any + combine yes", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        testthat::expect_warning(
          output <- apptime(otree2,
                            combine = TRUE,
                            pcode = "something"),
          "referred to")

        # Test
        test <- is.na(output$survey)
        testthat::expect_true(test)
      })

      testthat::test_that("App time - participant code did not make it", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        testthat::expect_warning(
          output <- apptime(otree2,
                            combine = TRUE,
                            pcode = "something"),
          "referred to")

        # Test
        test <- is.na(output$survey)
        testthat::expect_true(test)
      })

      testthat::test_that("App time - two apps oTree old", {
        # Prepare data
        otree2 <- otree_2_2_4
        otree2 <- make_ids(otree2)

        # Run function
        output <- apptime(otree2,
                          apps = c("survey", "dictator"),
                          sinfo = "session_id")

        # Test
        testthat::expect_output(str(output), "List of 2")
      })

      testthat::test_that("App time - group", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE, from_app = "dictator")

        # Run function
        output <- apptime(otree2, group_id = 2L, apps = "dictator")

        # Test
        test1 <- !is.null(output$mean_duration) &
          !is.na(output$mean_duration)
        test2 <- !is.null(output$min_duration) &
          !is.na(output$min_duration)
        test3 <- !is.null(output$max_duration) &
          !is.na(output$max_duration)
        test4 <- exists("single_durations", output)
        test5 <- round(mean(output$single_durations$duration),
                       digits = 2L) == output$mean_duration

        testthat::expect_true(all(c(test1, test2,
                                    test3, test4,
                                    test5)))
      })

      testthat::test_that("App time - one app durations not calculated", {
        # Prepare data
        otree2 <- otree_5_4_0
        # Keep only cases that didn't make it to the app
        delcases <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant._current_app_name == "survey"
        ]
        otree2 <- delete_cases(otree2,
                               delcases,
                               reason = "test")
        # Run function
        output <- apptime(otree2,
                          apps = "survey")

        # Test
        test1 <- setequal(names(output$survey), "message")
        test2 <- grepl("Durations not calculated", output$survey$message)

        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("App time - participant code not in any", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        testthat::expect_error(
          output <- apptime(otree2,
                            combine = FALSE,
                            pcode = "something"),
          "referred to the time stamp.*referred to the participant code")
      })

      testthat::test_that("App time - participant code did not make it", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        testthat::expect_error(
          output <- apptime(otree2,
                            combine = FALSE,
                            pcode = "something"),
          "referred to the time stamp.*referred to the participant code")
      })

      testthat::test_that("App time - participant label", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, pmake = TRUE)

        # Make new participant labels
        otree2$all_apps_wide$participant.label <-
          paste("Person",
                data.table::rleidv(otree2$all_apps_wide$participant.code))

        # Expect
        person_code <- otree2$all_apps_wide$participant.code[
          otree2$all_apps_wide$participant._current_app_name == "survey" &
            otree2$all_apps_wide$participant.label != ""][1L]

        person_label <- otree2$all_apps_wide$participant.label[
          otree2$all_apps_wide$participant._current_app_name == "survey" &
            otree2$all_apps_wide$participant.label != ""][1L]

        # Comparison data frame
        xminus1 <- max(otree2$Time[
          otree2$Time$participant_code == person_code &
            otree2$Time$app_name == "chatapp", ]$epoch_time_completed)

        x <- otree2$Time[
          otree2$Time$participant_code == person_code &
            otree2$Time$app_name == "survey", ]

        max <- max(x$epoch_time_completed)

        diff <- max - xminus1
        diff <- diff / 60L

        # Run function
        output <- apptime(otree2,
                          plabel = person_label,
                          digits = 2L)

        # Test
        testthat::expect_identical(
          output$survey,
          round(diff, digits = 2L))
      })

      testthat::test_that("App time - two apps", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- apptime(otree2,
                          apps = c("survey", "dictator"))

        # Test
        testthat::expect_output(str(output), "List of 2")
        test1 <- !is.null(output$dictator$mean_duration) &
          !is.na(output$dictator$mean_duration)
        test2 <- !is.null(output$dictator$min_duration) &
          !is.na(output$dictator$min_duration)
        test3 <- !is.null(output$dictator$max_duration) &
          !is.na(output$dictator$max_duration)

        test4 <- !is.null(output$survey$mean_duration) &
          !is.na(output$survey$mean_duration)
        test5 <- !is.null(output$survey$min_duration) &
          !is.na(output$survey$min_duration)
        test6 <- !is.null(output$survey$max_duration) &
          !is.na(output$survey$max_duration)

        test7 <- exists("single_durations", output$dictator)
        test8 <- exists("single_durations", output$survey)

        testthat::expect_true(test1)
        testthat::expect_true(test2)
        testthat::expect_true(test3)
        testthat::expect_true(test4)
        testthat::expect_true(test5)
        testthat::expect_true(test6)
        testthat::expect_true(test7)
        testthat::expect_true(test8)
      })

      testthat::test_that("App time (e) - two apps no session_code - old", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function and test
        testthat::expect_error(
          apptime(otree2,
                  apps = c("survey", "dictator")),
          "no session_code or session__code")
      })

      testthat::test_that("App time (e) - no group_id", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          apptime(otree2, group_id = 6L, apps = "dictator"),
          "Variable group_id is not in \"Time\" data frame")
      })

      testthat::test_that("App time (e) - nonunique participant label", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        testthat::expect_error(
          apptime(otree2,
                  plabel = "Person2"),
          "You do not have unique participant labels in your")
      })

      testthat::test_that("App time (e) - app", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        testthat::expect_error(apptime(otree2, apps = "Hinz"))
      })

      testthat::test_that("App time (e) - no epoch time old", {
        # Prepare data
        otree2 <- otree_old_one
        otree2$Time$epoch_time <- NULL

        # Run function and test
        testthat::expect_error(apptime(otree2),
                               "There is no variable referring to")
      })

      testthat::test_that("App time (e) - no participant code old", {
        # Prepare data
        otree2 <- otree_old_one
        otree2$Time$participant_code <- NULL

        # Run function and test
        testthat::expect_error(apptime(otree2),
                               "No variable referring to")
      })

      testthat::test_that("App time (e) - no Time data frame", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time <- NULL

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       apps = "survey"),
                               "There is no")
      })

      testthat::test_that("App time (e) - wrong group_id", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2 <- make_ids(otree2, gmake = TRUE,
                           from_var = "dictator.1.group.id_in_subsession")

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       group_id = 400L,
                                       apps = "survey"),
                               "group_id is not in \"Time\" data frame")
      })

      testthat::test_that("App time (e) -
                          participant.label and no all_apps_wide", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       plabel = "Person1",
                                       apps = "survey"),
                               "You can only use")
      })

      testthat::test_that("App time (e) -
                          no session_id in the Time data frame", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       sinfo = "session_id",
                                       apps = "survey"),
                               "There is no session_id in the Time data frame")
      })

      testthat::test_that("App time (e) -
                          no session_id in the Time data frame", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       sinfo = "session.id",
                                       apps = "survey"),
                               "Please specify a valid sinfo")
      })

      testthat::test_that("App time (e) - only group or participant", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       plabel = "Person1",
                                       group_id = 1L,
                                       apps = "survey"),
                               "Please enter only")
      })

      testthat::test_that("App time (e) - specified duplicate participant", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Make duplicate data
        person1 <- unique(otree2$Time$participant_code)[1L]
        otree2$Time <-
          rbind(otree2$Time,
                otree2$Time[otree2$Time$participant_code == person1, ])

        # Run function
        testthat::expect_error(
          apptime(otree2,
                  pcode = person1,
                  apps = "dictator"),
          "This person has duplicate data in their Time data")
      })

      testthat::test_that("App time (e) - only participant.code or group_id", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       pcode = "46kxib6w",
                                       group_id = 1L,
                                       apps = "survey"),
                               "Please enter only pcode or group_id")
      })

      testthat::test_that("App time (e) - only p.label or p.code", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(apptime(otree2,
                                       plabel = "Person1",
                                       pcode = "46kxib6w",
                                       apps = "survey"),
                               "Please enter only")
      })

      testthat::test_that("App time (e) - two participants (label)", {
        # Prepare data
        otree2 <- otree_5_4_0
        part <- c("xx78b3x0", "46kxib6w")

        # Run function and test
        part <- c("Person1", "Person2")
        testthat::expect_error(
          apptime(otree2,
                  plabel = part),
          "Please enter only one participant")
      })

      testthat::test_that("App time (e) - two participants (code)", {
        # Prepare data
        otree2 <- otree_5_4_0
        part <- c("xx78b3x0", "46kxib6w")

        # Run function and test
        testthat::expect_error(
          apptime(otree2,
                  pcode = part),
          "Please enter only one participant")
      })

      print("---- pagesec -----")

      # Page seconds   ####
      testthat::test_that("pagesec", {
        # Run function
        # Make minutes
        otree2 <- pagesec(otree_5_4_0, minutes = TRUE)
        # Make seconds
        otree2 <- pagesec(otree2, minutes = FALSE)

        # Test
        test1 <- mean(otree2$Time$minutes_on_page,
                      na.rm = TRUE) > 0L

        test2 <- mean(otree2$Time$seconds_on_page2,
                      na.rm = TRUE) > 0L

        test3 <- testthat::expect_identical(
          otree2$Time$minutes_on_page[!is.na(otree2$Time$minutes_on_page)],
          round(otree2$Time$seconds_on_page2[
            !is.na(otree2$Time$seconds_on_page2)] / 60L, 2L))

        testthat::expect_true(all(c(test1, test2)))
      })

      testthat::test_that("pagesec - minutes ", {
        # Run function
        otree2 <- pagesec(otree_5_4_0, minutes = TRUE)
        otree2 <- pagesec(otree2, minutes = FALSE)

        # Test
        variable <- otree2$Time$seconds_on_page2

        test1 <- testthat::expect_true(
          mean(variable, na.rm = TRUE) > 0
        )

        testthat::expect_identical(
          otree2$Time$minutes_on_page[!is.na(otree2$Time$minutes_on_page)],
          round(otree2$Time$seconds_on_page2[
            !is.na(otree2$Time$seconds_on_page2)] / 60L, 2L))

      })

      testthat::test_that("pagesec - oTree 2.2.4", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        otree2 <- pagesec(otree2)

        # Test
        testthat::expect_false(is.null(otree2$Time$seconds_on_page2))

        test1 <- all(otree2$Time$seconds_on_page2[
          !is.na(otree2$Time$seconds_on_page2)] ==
            otree2$Time$seconds_on_page[!is.na(otree2$Time$seconds_on_page2)])

        testthat::expect_true(test1)
      })

      testthat::test_that("pagesec - messy time", {
        # Prepare data
        otree2 <- otree_all

        # Run function
        testthat::expect_warning(
          otree2 <- pagesec(otree2,
                            minutes = TRUE,
                            combine = TRUE),
          "referred to ")

        otree2 <- pagesec(otree2,
                          minutes = FALSE)

        # Test
        test1 <- mean(otree2$Time$minutes_on_page,
                      na.rm = TRUE) > 0.0

        test2 <- mean(otree2$Time$seconds_on_page2,
                      na.rm = TRUE) > 0.0
        test3 <- all(
          otree2$Time$seconds_on_page2[!is.na(otree2$Time$seconds_on_page2) &
                                         !is.na(otree2$Time$seconds_on_page)] ==
            otree2$Time$seconds_on_page[!is.na(otree2$Time$seconds_on_page2) &
                                          !is.na(otree2$Time$seconds_on_page)])

        test4 <- testthat::expect_identical(
          otree2$Time$minutes_on_page[!is.na(otree2$Time$minutes_on_page)],
          round(otree2$Time$seconds_on_page2[
            !is.na(otree2$Time$seconds_on_page2)] / 60L, 2L))

        testthat::expect_true(all(c(test1, test2, test3)))

      })

      testthat::test_that("pagesec (e) - messy time", {
        # Prepare data
        otree2 <- otree_all

        # Run function and test
        testthat::expect_error(
          pagesec(otree2,
                  minutes = TRUE),
          "referred to the time stamp.*referred to the participant code")
      })

      testthat::test_that("pagesec (e) - no epoch time", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time$epoch_time_completed <- NULL

        # Run function and test
        testthat::expect_error(
          pagesec(otree2, minutes = TRUE),
          "no variable referring to the time stamp")
      })

      testthat::test_that("pagesec (e) - no participant code", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time$participant_code <- NULL

        # Run function and test
        testthat::expect_error(
          pagesec(otree2, minutes = TRUE),
          "no variable referring to the participant")
      })

      testthat::test_that("pagesec (e) - no Time files", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$Time <- NULL

        # Run function and test
        testthat::expect_error(pagesec(otree2), "No time data frame")
      })

      print("---- show_constant -----")

      # Show constant  ####
      testthat::test_that("Show constant - empty ", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- show_constant(otree2, value = "donotfind")

        # Test
        test1 <- "all_apps_wide" %in% names(output)
        test2 <- "chatapp" %in% names(output)
        test3 <- "dictator" %in% names(output)
        test4 <- "start" %in% names(output)
        test5 <- "survey" %in% names(output)
        test6 <- "Time" %in% names(output)
        test7 <- "Chats" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))

        test1 <- length(output$all_apps_wide) == 0L
        test2 <- length(output$chatapp) == 0L
        test3 <- length(output$dictator) == 0L
        test4 <- length(output$start) == 0L
        test5 <- length(output$survey) == 0L
        test6 <- length(output$Time) == 0L
        test7 <- length(output$Chats) == 0L
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("Show constant - NA ", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        output <- show_constant(otree2, value = NA)

        # Test
        test1 <- "all_apps_wide" %in% names(output)
        test2 <- "chatapp" %in% names(output)
        test3 <- "dictator" %in% names(output)
        test4 <- "start" %in% names(output)
        test5 <- "survey" %in% names(output)
        test6 <- "Time" %in% names(output)
        test7 <- "Chats" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("Show constant - any", {
        # Run function
        output <- show_constant(otree_5_4_0, value = "any")

        # Test
        test1 <- "all_apps_wide" %in% names(output)
        test2 <- "chatapp" %in% names(output)
        test3 <- "dictator" %in% names(output)
        test4 <- "start" %in% names(output)
        test5 <- "survey" %in% names(output)
        test6 <- "Time" %in% names(output)
        test7 <- "Chats" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("Show constant - -99", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide$constant_col <- -99L # Make fake column

        # Run function
        output <- show_constant(otree2, value = -99L)

        # Test
        test1 <- "all_apps_wide" %in% names(output)
        test2 <- "chatapp" %in% names(output)
        test3 <- "dictator" %in% names(output)
        test4 <- "start" %in% names(output)
        test5 <- "survey" %in% names(output)
        test6 <- "Time" %in% names(output)
        test7 <- "Chats" %in% names(output)
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))

        test1 <- length(output$all_apps_wide) == 1L
        test2 <- length(output$chatapp) == 0L
        test3 <- length(output$dictator) == 0L
        test4 <- length(output$start) == 0L
        test5 <- length(output$survey) == 0L
        test6 <- length(output$Time) == 0L
        test7 <- length(output$Chats) == 0L
        testthat::expect_true(all(c(test1, test2, test3, test4,
                                    test5, test6, test7)))
      })

      testthat::test_that("Show constant (e) - only one value", {
        # Run function and test
        testthat::expect_error(show_constant(otree_5_4_0,
                                             c(-99L, NA)),
                               "Please only enter only one value")
      })

      testthat::test_that("Show constant (e) - valid value", {
        # Run function and test
        testthat::expect_error(show_constant(otree_5_4_0,
                                             NULL),
                               "Please enter a valid value!")
      })

      print("---- assignv -----")

      # Assign variable  ####
      testthat::test_that("Assign variable - new", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        otree2 <- assignv(oTree = otree2,
                          variable = "survey.1.player.gender",
                          newvar = "gender")
        # Test
        testthat::expect_vector(otree2$survey$gender)
      })

      testthat::test_that("Assign variable - new, one random df", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Run function
        otree2 <- assignv(oTree = otree2,
                          variable = "survey.1.player.gender",
                          newvar = "gender")
        # Test
        testthat::expect_vector(otree2$survey$gender)
        testthat::expect_null(otree2$random_dataframe$gender)
      })

      testthat::test_that("Assign variable - old", {
        # Prepare data
        otree2 <- otree_2_2_4

        # Run function
        otree2 <- assignv(oTree = otree2,
                          variable = "survey.1.player.gender",
                          newvar = "gender")
        # Test
        testthat::expect_vector(otree2$survey$gender)
      })

      testthat::test_that("Assign variable - res after", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function
        otree2 <- assignv_to_aaw(otree2,
                                 app = "survey",
                                 variable = "younger30",
                                 newvar = "younger30",
                                 resafter = "survey.1.player.age")

        # Test
        diff <- match("younger30", names(otree2$all_apps_wide)) -
          match("survey.1.player.age", names(otree2$all_apps_wide))
        testthat::expect_identical(diff, 1L)
      })

      testthat::test_that("Assign variable (e) - no aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(assignv(oTree = otree2,
                                       variable = "survey.1.player.gender",
                                       newvar = "gender"),
                               "There is no \"all_apps_wi")
      })

      testthat::test_that("Assign variable (e) - too many vars", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          assignv(oTree = otree2,
                  variable = c("survey.1.player.gender", "asdfasfd"),
                  newvar = "gender"), "Plase enter only one variable name")
      })

      testthat::test_that("Assign variable (e) - too many vars", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          assignv(oTree = otree2,
                  variable = c("survey.1.player.gender", "asdfasfd"),
                  newvar = c("gender", "asfasdf")),
          "Plase enter only one variable name")
      })

      testthat::test_that("Assign variable (e) - too many newvars", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          assignv(oTree = otree2,
                  variable = "asdfasfd",
                  newvar = c("gender", "asfasdf")),
          "Plase enter only one new variable name")
      })

      testthat::test_that("Assign variable (e) - aaw not there", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)
        otree2$all_apps_wide <- NULL

        # Run function
        testthat::expect_error(
          otree2 <- assignv_to_aaw(otree2,
                                   app = "survey",
                                   variable = "younger30",
                                   newvar = "younger30",
                                   resafter = "survey.1.player.age"),
          "There is no")
      })

      testthat::test_that("Assign variable (e) - nonexistent variable", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(
          assignv(oTree = otree2,
                  variable = "fake_variable",
                  newvar = "gender"),
          "The variable does not exist")
      })

      print("---- assign to AAW ----")

      # Assign to AAW  ####
      testthat::test_that("Assign variable to aaw", {
        # Prepare data
        otree2 <- otree_5_4_0

        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function
        otree2 <- assignv_to_aaw(otree2,
                                 app = "survey",
                                 variable = "younger30",
                                 newvar = "younger30")

        # Test
        testthat::expect_vector(otree2$all_apps_wide$younger30)
      })

      testthat::test_that("Assign variable to aaw - random df", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Run function
        testthat::expect_error(
          otree2 <- assignv_to_aaw(otree2,
                                   app = "random_dataframe",
                                   variable = "a",
                                   newvar = "a"),
          "This function does not work with random_dataframe"
        )
      })

      testthat::test_that("Assign variable to aaw - one person missing", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$player.age[1L] <- 15L  # Make sure the first player has a value
        person <- otree2$survey$participant.code[1L]
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)
        otree2$survey <- otree2$survey[3L:nrow(otree2$survey), ]

        # Run function
        testthat::expect_warning(
          otree2 <- assignv_to_aaw(otree2,
                                   app = "survey",
                                   variable = "younger30",
                                   newvar = "younger30"),
          "However, there is an unequal number of participants")

        # Test
        testthat::expect_vector(otree2$all_apps_wide$younger30)
        testthat::expect_true(is.na(otree2$all_apps_wide$younger30[
          otree2$all_apps_wide$participant.code == person
        ]))
      })

      testthat::test_that("Assign variable to aaw - all NA", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- NA

        # Run function
        otree2 <- assignv_to_aaw(otree2,
                                 app = "survey",
                                 variable = "younger30",
                                 newvar = "younger30")

        # Test
        testthat::expect_vector(otree2$all_apps_wide$younger30)
        testthat::expect_true(all(is.na(otree2$all_apps_wide$younger30)))
      })

      testthat::test_that("Assign variable to aaw (w)", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Create new variable
        person <- otree2$survey$participant.code[1L]
        otree2$survey <- otree2$survey[2L:nrow(otree2$survey), ]  # Delete first row
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function
        testthat::expect_warning(
          otree2 <- assignv_to_aaw(otree2,
                                   app = "survey",
                                   variable = "younger30",
                                   newvar = "younger30"),
          "unequal number of")

        # Test
        testthat::expect_vector(otree2$all_apps_wide$younger30)
        test1 <- is.na(otree2$all_apps_wide$younger30[
          otree2$all_apps_wide$participant.code == person])
        testthat::expect_true(test1)
      })

      testthat::test_that("Assign variable to aaw (e) - no aaw", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)
        otree2$all_apps_wide <- NULL

        # Run function and test
        testthat::expect_error(assignv_to_aaw(otree2,
                                              app = "survey",
                                              variable = "younger30",
                                              newvar = "younger30"),
                               "There is no \"all_apps_wide\" in your oTree")
      })

      testthat::test_that("Assign variable to aaw (e) - only one var", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function and test
        testthat::expect_error(
          assignv_to_aaw(otree2,
                        app = "survey",
                        variable = c("younger30", "asdfsadf"),
                        newvar = "younger30"),
         "Plase enter only one variable name!")
      })

      testthat::test_that("Assign variable to aaw (e) -
                          only one var and newvar", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function and test
        testthat::expect_error(
          assignv_to_aaw(otree2,
                        app = "survey",
                        variable = c("younger30", "asdfsadf"),
                        newvar = c("younger30", "asdfasdf")),
         "Plase enter only one variable name!")
      })

      testthat::test_that("Assign variable to aaw (e) - only one newvar", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$survey$younger30 <- as.integer(otree2$survey$player.age < 30L)

        # Run function and test
        testthat::expect_error(
          assignv_to_aaw(otree2,
                         app = "survey",
                         variable = "younger30",
                         newvar = c("younger30", "asdfasdf")),
         "Plase enter only one new variable name!")
      })

      testthat::test_that("Assign variable to aaw (e) - nonexistent variable", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(assignv_to_aaw(otree2,
                                              app = "survey",
                                              variable = "fake_variable",
                                              newvar = "younger30"
        ), "The variable does not exist in the app")
      })

      testthat::test_that("Assign variable to aaw (e) - Chats", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function and test
        testthat::expect_error(assignv_to_aaw(otree2,
                                              app = "Chats",
                                              variable = "nickname",
                                              newvar = "nickname"),
                               "This function does not work with Chats")
      })

      print("---- delete_plabels ----")

      # Delete participant labels   ####
      testthat::test_that("Delete participant labels", {
        # Prepare data
        otree2 <- otree_5_4_0

        # Run function
        otree2 <- delete_plabels(otree2)

        # Test
        testthat::expect_null(otree2$all_apps_wide$participant.label)
        testthat::expect_null(otree2$Time$participant_label)
        testthat::expect_null(otree2$Time$participant__label)
        testthat::expect_null(otree2$Chats$participant_label)
        testthat::expect_null(otree2$Chats$participant__label)
        testthat::expect_null(otree2$dictator$participant.label)
        testthat::expect_null(otree2$survey$participant.label)
      })

      testthat::test_that("Delete participant labels - random df", {
        # Prepare data
        otree2 <- otree_5_4_0
        otree2$random_dataframe <- data.frame(
          a = c(1L, 2L, 3L),
          b = c(4L, 5L, 6L),
          c = c(7L, 8L, 9L))

        # Run function
        otree2 <- delete_plabels(otree2)

        # Test
        testthat::expect_null(otree2$all_apps_wide$participant.label)
        testthat::expect_null(otree2$Time$participant_label)
        testthat::expect_null(otree2$Time$participant__label)
        testthat::expect_null(otree2$Chats$participant_label)
        testthat::expect_null(otree2$Chats$participant__label)
        testthat::expect_null(otree2$dictator$participant.label)
        testthat::expect_null(otree2$survey$participant.label)
      })

}
