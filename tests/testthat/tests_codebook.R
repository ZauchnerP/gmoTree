if (rlang::is_installed(c("withr", "testthat"))) {

    testthat::test_that("Codebook - double brackets", {
      # Here we see that if a value starts and ends with brackets, it makes no difference
      # TODO 
      
      testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        output = "list"))

      testthat::expect_true(cbook$rankaversion$Player$acceptance$choices$value[1] == "(Yes)")
      
      
    })

    testthat::test_that("Codebook (e) - wrong output", {
      
      # Wrongt text
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          output = "WRONG",
          fsource = "init")}, "Output should be")

      # NULL
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          output = "NULL",
          fsource = "init")}, "Output should be")
      

      # Two inputs
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          fsource = "init",
          output = c("file", "list"))},
        "Output should be")
      
    })
      
    testthat::test_that("Codebook (e) - wrong fsource", {
      
      # Wrongt text
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          fsource = "none",
          output = "list")}, "fsource must be either")
      
      # NULL
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          fsource = NULL,
          output = "list")}, "fsource must be either")
      
      # Wrong length
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          output_dir = NULL,
          fsource = c("init", "model"),
          output = "list")}, "Please enter only one fsource")

    })

    testthat::test_that("Codebook (w) - user_settings", {

      # Try user Var
      testthat::expect_warning(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          user_settings = list(error = "Test",
                               showupToken = 2,
                               max_payoffPart1_ab = 1,
                               ExchangeMainCurrency = 1,
                               payoff_survey = 1,
                               MinutesForSurvey = 20,
                               ExchangeToken = 1,
                               timer123 = 12,
                               payoffPart1_c = 1),
          settings_replace = "user",
          doc_info = FALSE,
          output = "list"), "too complex for this function")

      test1 <- cbook$rankaversion$Constants$errorvariable == "Test"
      test2 <- cbook$rankaversion$Constants$MinutesForSurvey == 20

      testthat::expect_true(test1)
      testthat::expect_true(test2)
    })

    testthat::test_that("Codebook (w) - float in variable name", {
      testthat::expect_warning(
        cb <- codebook(
          path = testthat::test_path("testdata", "ocode_y1"),
          fsource = "models",
          output = "list"
        ), paste0(
          "too complex for this function.*",
          "\\$test\\$Constants\\$min_payoffAB.*",
          "\\$test\\$Player\\$min_payoffAB\\$max.*",
          "\\$test\\$Group\\$min_payoffAB\\$max"
        ))

      # Info: Correct result but a string instead of a float
      testthat::expect_true(cb$test$Group$variable2$max == "3.3")

      test1 <- cb$test$Constants$min_payoffAB ==
        "(float(1.0)) + (float(1.1)) + (float(1.2))"
      test2 <- cb$test$Group$min_payoffAB$max ==
        "(float(1.0)) + (float(1.1)) + (float(1.2))"
      test3 <- cb$test$Player$min_payoffAB$max ==
        "(float(1.0)) + (float(1.1)) + (float(1.2))"
      test4 <- cb$test$Group$variable2$max == 3.3

      testthat::expect_true(all(c(test1, test2, test3, test4)))

    })

    testthat::test_that("Codebook (e) - only list, app and app rm specified", {

      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          doc_info = FALSE,
          app = "dictator",
          app_rm = "public_goods_simple"),
        "Please specify only \"app\" or \"app_rm")

    })

    testthat::test_that("Codebook - only list, new oTree, app_rm", {

      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output = "list",
        app_rm = "dictator",
        doc_info = FALSE)

      test1 <- !is.null(cbook$bargaining)
      test2 <- is.null(cbook$dictator)
      testthat::expect_true(test1)
      testthat::expect_true(test2)
    })
    
    testthat::test_that("Codebook (e) - apps don't exist", {
      
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          app = c("dictator", "nonexistingapp"),
          doc_info = FALSE)}, "At least one app")
    })
    
    testthat::test_that("Codebook (e) - wrong format", {
      
      testthat::expect_error({
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "both",
          output_format = "WrongFormat",
          doc_info = FALSE)}, "Output format should be")
    })

    testthat::test_that("Codebook (e) - params", {
      warnings <- testthat::capture_warnings(
        testthat::expect_error(

          cbook <- codebook(
            path = testthat::test_path("testdata", "ocode_z2"),
            fsource = "model",
            params = "ab",
            output = "both"),
          "params must be a list"))

    })

    testthat::test_that("Codebook (w) - constants var not there", {

      # Try user Var
      testthat::expect_message(
        testthat::expect_warning(
          cbook <- codebook(
            path = testthat::test_path("testdata", "ocode_new2"),
            fsource = "init",
            output = "list"), "is not in Constants"),
        "Variables without documentation.*bargaining\\$Group\\$total_requests")

      test1 <-
        cbook$bargaining$Player$request$max == "C.AMOUNT_SHARED_false"
      
      # Check if leading and trailing brackets and comments are removed
      test2 <- cbook$bargaining$Player$request$label == 
        "Please enter an amount from 0 to 100"
      test3 <- cbook$bargaining$Player$request2$label == 
        "Please enter an amount from 0 to 100"
      test4 <- cbook$bargaining$Player$request3$label == 
        "Please enter an amount from 0 to 100"

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
    })
    
    testthat::test_that("Codebook (w) - folder without settings", {

      warnings <- testthat::capture_warnings(
        cbook <- codebook(
          fsource = "model",
          path = testthat::test_path("testdata", "ocode_z3.1"),
          output = "list",
          doc_info = FALSE))

      test1 <- any(grepl(
        pattern = "There is no settings.* in your path.*Folder .*rankaversion.* reference: .*StartToken",
        x = warnings))
      test2 <- any(grepl(
        pattern = "too complex",
        x = warnings))

      testthat::expect_true(test1)
      testthat::expect_true(test2)

    })

    testthat::test_that("Codebook (e) - path", {

      # Wrong path
      testthat::expect_error(
        cbook <- codebook(
          path = NULL,
          output = "list",
          doc_info = TRUE), "Path must not be NULL")
    })

    testthat::test_that("Codebook (e) - relative path", {

      # Relative path
      testthat::expect_error(
        cbook <- codebook(
          output_dir  = "../",
          output = "list",
          doc_info = TRUE), "Please don't use relative paths in output_dir")

      # Relative path
      testthat::expect_error(
        cbook <- codebook(
          output_dir  = "..",
          output = "list",
          doc_info = TRUE), "Please don't use relative paths in output_dir")

      # Relative path
      testthat::expect_error(
        cbook <- codebook(
          output_dir  = ".",
          output = "list",
          doc_info = TRUE), "Please don't use relative paths in output_dir")

      # Relative path
      testthat::expect_error(
        cbook <- codebook(
          output_dir  = "./",
          output = "list",
          doc_info = TRUE), "Please don't use relative paths in output_dir")

    })

    testthat::test_that("Codebook (w) - old oTree, only list", {

      testthat::expect_message(
          testthat::expect_warning(
            cbook <- codebook(
              path = testthat::test_path("testdata", "ocode_z"),
              fsource = "model",
              output = "list",
              doc_info = TRUE),
          ".*too complex for this function.*App rankaversion\\(Constants\\).*App rankend\\(Constants\\) \\(read_csv\\).*\\$settings\\$showupToken \\(float\\).*\\$end\\$Constants\\$showupToken \\(float\\).*\\$end\\$Constants\\$showupTokenTest \\(float\\)"
          ), "Variables without.*rankaversion\\$Player\\$choicenumber.*rankend\\$Player\\$done_distribution")

      testthat::expect_true(cbook$rankaversion$Constants$randomtest == 35L)
      test1 <- is.list(cbook)

      # Test settings in settings replacements
      test2 <- cbook$settings$payoffPart1_c ==
        6L * cbook$settings$ExchangeToken

      # Test settings in Constants replacement
      test3 <- cbook$end$Constants$payoffPart1_c ==
        cbook$settings$payoffPart1_c

      test4 <- cbook$end$Constants$ExchangeToken ==
        cbook$settings$ExchangeToken

      test5 <- cbook$end$Constants$ExchangeToken == 4L
      # Test constants in Constants replacement
      test6 <- cbook$end$Constants$ExchangeRate ==
        as.numeric(cbook$end$Constants$ExchangeToken) /
        as.numeric(cbook$end$Constants$ExchangeMainCurrency)

      test7 <- cbook$end$Constants$showupToken ==
        cbook$end$Constants$showupTokenTest

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)

    })

    testthat::test_that("Codebook (w) - i - with example, no settings.py", {
      # Tests code that contains "with" + no settings.py

      mywarnings <- testthat::capture_warnings(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_g"),
          fsource = "init", # wrong because old otree
          output = "list",
          doc_info = FALSE)
      )

      test1 <- any(grepl(
        pattern = "not in settings and cannot be replaced",
        x = mywarnings))

      test2  <- any(grepl(
        pattern = "too complex for this function",
        x = mywarnings))

      test3  <- cbook$effort_add$Constants$NUM_ROUNDS == 1L
      test4  <- cbook$effort_add$Constants$NAME_IN_URL == "effort_add"
      test5  <- cbook$effort_add$Constants$NUM_TASKS == "len(INTS)"
      test6  <- cbook$effort_add$Constants$TASK_TIMER == "settings.task_timer"

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
    })

    testthat::test_that("Codebook (w) - setting var not there 4", {

      # Try user Var
      message <- capture_messages(
        testthat::expect_warning(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new3"),
          settings_replace = "global",
          fsource = "init",
          output = "list") , "are not in settings and"))

      testthat::expect_true(
        grepl(x = message,
              pattern = "Variables without documentation.*bargaining\\$Group\\$total_requests"))

      test1 <-
        cbook$bargaining$Player$request$max == "settings.AMOUNT_SHARED"

      testthat::expect_true(test1)
    })

    testthat::test_that("Codebook (e) - app not there", {

      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z"),
          fsource = "init", # wrong because old otree
          output = "list",
          doc_info = TRUE,
          app = "xyp"),
        "not in oTree code"
      )
    })

    testthat::test_that("Codebook (e) - settings var. not there3", {

      # Try user Var but with some missing
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          user_settings = list(error = "Test"),
          settings_replace = "user",
          output = "list"),
        "is not in user_settings")
    })
    
    testthat::test_that("Codebook (e) - wrong settings replace", {
      
      # Wrong text
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          user_settings = list(error = "Test"),
          settings_replace = "wrongString",
          output = "list"),
        "must be either")
    })

    testthat::test_that("Codebook (e) - user settings and global_s mismatch", {

      # Wrong settings_replace
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          user_settings = list(error = "Test",
                               showupToken = 2L),
          settings_replace = "global",
          output = "list"),
        "settings_replace must be set to"
      )
      
      # Wrong variable
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          user_settings = list(error = "Test",
                               showupToken = 2L),
          settings_replace = "user",
          output = "list"),
        "is not in user_settings"
      )
    })

    testthat::test_that("Codebook (w) - settings var. not there 1", {
      # Settings not there

      warnings <- testthat::capture_warnings(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          output = "list",
          doc_info = FALSE)
      )

      test1 <- any(grepl(pattern = "not in settings and cannot be replaced",
                         x = warnings))
      test2 <- any(grepl(pattern = "too complex for this function",
                         x = warnings))

      testthat::expect_true(test1)
      testthat::expect_true(test2)

    })

    testthat::test_that("Codebook (e) - wrong source", {

      # Wrong source
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "xx",
          output = "list",
          doc_info = TRUE,
          app = "public_goods_simple"),
        "fsource must be either")

      # If source is NULL
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = NULL,
          output = "list",
          doc_info = TRUE,
          app = "public_goods_simple"),
        "fsource must be either")
    })

    testthat::test_that("Codebook (e) - init instead of models", {

      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z"),
          fsource = "init", # wrong because old otree
          output = "list",
          doc_info = TRUE),
        "At least one of your init-files is empty"
      )
    })

    testthat::test_that("Codebook (e) - models instead of init", {

      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "model", # wrong because new otree
          output = "list",
          doc_info = TRUE,
          app = "public_goods_simple"), "No files to process"
      )
    })

    testthat::test_that("Codebook (e) - path", {
      testthat::expect_error(
        cbook <- codebook(
          path = testthat::test_path("testdata", "nopath"),
          fsource = "init",
          output = "list",
          doc_info = TRUE,
          app = "public_goods_simple"),
        "does not exist")
    })
    
    testthat::test_that("Codebook (w) - vector variable in settings and constants", {
      warning <- testthat::expect_warning(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z3"),
          fsource = "models",
          output = "list",
          output_format = "html_document",
          doc_info = FALSE), "too complex for this function"
      )

      # Since R only saves the same type in a vector
      test1 <- cbook$settings$Something == c("1", "3", "test", "3", "1")

      test2 <-
        cbook$rankaversion$Constants$Something == c("1", "3", "test", "3", "1")

      # Otherwise
      test3 <- cbook$settings$StartToken == c(9.0, 14.0, 17.0, 23.0, 38.0)

      test4 <-
        cbook$rankaversion$Constants$StartToken ==
        c(9.0, 14.0, 17.0, 23.0, 38.0)

      test5 <- cbook$rankaversion$Constants$StartToken2 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 2.0
      test6 <- cbook$rankaversion$Constants$StartToken3 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 10.0

      # Calculations
      test7 <- cbook$settings$Othertest1 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0)
      test8 <- cbook$settings$Othertest2 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 5L

      test9 <-
        cbook$rankaversion$Constants$players_per_group == "None"

      test10 <-
        cbook$rankaversion$Constants$TestToken ==
        cbook$settings$StartToken * 5.0

      test11 <- all(cbook$rankaversion$Player$Acc_DistributionChange$value ==
                      c("Ich hätte mich dennoch für Verfahren > xyz < entschieden.",
                        "Ich hätte mich für das alternative Verfahren > aaa <  entschieden.",
                        "Ich hätte mich für das alternative Verfahren > bbb < entschieden."))

      test12 <- all(cbook$rankaversion$Player$acceptance$choices$value ==
                      c("xyz", "aaa", "Wrong, trick answer", "Correct trick answer"))

      testthat::expect_true(all(test1))
      testthat::expect_true(all(test2))
      testthat::expect_true(all(test3))
      testthat::expect_true(all(test4))
      testthat::expect_true(all(test5))
      testthat::expect_true(all(test6))
      testthat::expect_true(all(test7))
      testthat::expect_true(all(test8))
      testthat::expect_true(all(test9))
      testthat::expect_true(all(test10))
      testthat::expect_true(test11)
      testthat::expect_true(test12)

    })

    testthat::test_that("Codebook (w) - vector variable in settings and constants", {
      # Dissertation code

      testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z3"),
        fsource = "models",
        output = "list",
        doc_info = FALSE), "too complex for this function")

      # Since R only saves the same type in a vector
      test1 <- cbook$settings$Something == c("1", "3", "test", "3", "1")

      test2 <-
        cbook$rankaversion$Constants$Something == c("1", "3", "test", "3", "1")

      # Otherwise
      test3 <- cbook$settings$StartToken == c(9.0, 14.0, 17.0, 23.0, 38.0)
      test4 <-
        cbook$rankaversion$Constants$StartToken ==
        c(9.0, 14.0, 17.0, 23.0, 38.0)

      test5 <- cbook$rankaversion$Constants$StartToken2 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 2.0
      test6 <- cbook$rankaversion$Constants$StartToken3 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 10.0

      # Calculations
      test7 <- cbook$settings$Othertest1 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0)
      test8 <- cbook$settings$Othertest2 ==
        c(9.0, 14.0, 17.0, 23.0, 38.0) * 5L

      test9 <-
        cbook$rankaversion$Constants$players_per_group == "None"

      test10 <-
        cbook$rankaversion$Constants$TestToken ==
        cbook$settings$StartToken * 5L

      test11 <- all(
        cbook$rankaversion$Player$Acc_DistributionChange$value ==
          c("Ich hätte mich dennoch für Verfahren > xyz < entschieden.",
            "Ich hätte mich für das alternative Verfahren > aaa <  entschieden.",
            "Ich hätte mich für das alternative Verfahren > bbb < entschieden."))

      test12 <- all(cbook$rankaversion$Player$acceptance$choices$value ==
                      c("xyz", "aaa", "Wrong, trick answer", "Correct trick answer"))

      testthat::expect_true(all(test1))
      testthat::expect_true(all(test2))
      testthat::expect_true(all(test3))
      testthat::expect_true(all(test4))
      testthat::expect_true(all(test5))
      testthat::expect_true(all(test6))
      testthat::expect_true(all(test7))
      testthat::expect_true(all(test8))
      testthat::expect_true(all(test9))
      testthat::expect_true(all(test10))
      testthat::expect_true(test11)
      testthat::expect_true(test12)

    })

    testthat::test_that("Codebook - only list, new oTree, correct sort", {

      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output = "list",
        doc_info = FALSE,
        sort =  c("traveler_dilemma", "survey",
                  "bargaining", "volunteer_dilemma", "bertrand",
                  "common_value_auction", "cournot",
                  "dictator", "guess_two_thirds",
                  "matching_pennies", "payment_info",
                  "prisoner", "public_goods_simple",
                  "trust", "trust_simple"))

      test1 <- is.list(cbook)
      test2 <- !is.null(cbook$public_goods_simple$Constants)
      test3 <- !is.null(cbook$public_goods_simple$Group)
      test4 <- !is.null(cbook$public_goods_simple$Player)

      test5 <-
        cbook$public_goods_simple$Constants$NAME_IN_URL ==
        "public_goods_simple"
      test6 <-
        cbook$public_goods_simple$Group$total_contribution$noargs ==
        "TRUE"
      test7 <-
        cbook$public_goods_simple$Player$contribution$label ==
        "How much will you contribute?"
      test8 <-
        cbook$public_goods_simple$Player$contribution$noargs ==
        "FALSE"

      # If there is only one group field without information in it
      test9 <-
        cbook$bertrand$Group$winning_price$field ==
        "CurrencyField"

      # If there is one group field that has information in it
      test10 <- cbook$dictator$Group$kept$field ==
        "CurrencyField"

      # If there are two group fields with no information in them
      test11 <-
        cbook$public_goods_simple$Group$total_contribution$field ==
        "CurrencyField"

      test12 <-
        all(names(cbook) ==
              c("settings", "traveler_dilemma", "survey",
                "bargaining", "volunteer_dilemma", "bertrand",
                "common_value_auction", "cournot",
                "dictator", "guess_two_thirds",
                "matching_pennies", "payment_info",
                "prisoner", "public_goods_simple",
                "trust", "trust_simple"))

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)
      testthat::expect_true(test8)
      testthat::expect_true(test9)
      testthat::expect_true(test10)
      testthat::expect_true(test11)
      testthat::expect_true(test12)
    })

    testthat::test_that("Codebook (w) - only list, new oTree, too many sort 2", {

      original_sort <-
        c("bargaining", "bertrand", "common_value_auction", "cournot",
          "dictator", "guess_two_thirds", "matching_pennies", "payment_info",
          "prisoner", "public_goods_simple", "survey", "traveler_dilemma",
          "trust", "trust_simple", "volunteer_dilemma")

      new_sort <- c("thisisnothere", "payment_info",
                    "bargaining", "volunteer_dilemma",
                    "bertrand", "common_value_auction",
                    "cournot", "dictator",
                    "guess_two_thirds", "matching_pennies",
                    "prisoner", "public_goods_simple",
                    "survey", "traveler_dilemma",
                    "trust", "trust_simple")

      thiswarning <- capture_warnings(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          doc_info = FALSE,
          sort = new_sort)
      )

      testthat::expect_true(any(
        grepl(x = thiswarning, pattern = "Sort elements not in apps are")))

      testthat::expect_true(
        any(grepl("Sort apps are not equal to all apps", thiswarning)))

      testthat::expect_true(any(grepl("Sort elements not in apps are",
                                      thiswarning)))

      test1 <- is.list(cbook)
      test2 <- !is.null(cbook$public_goods_simple$Constants)
      test3 <- !is.null(cbook$public_goods_simple$Group)
      test4 <- !is.null(cbook$public_goods_simple$Player)

      test5 <-
        cbook$public_goods_simple$Constants$NAME_IN_URL ==
        "public_goods_simple"
      test6 <-
        cbook$public_goods_simple$Group$total_contribution$noargs ==
        "TRUE"
      test7 <-
        cbook$public_goods_simple$Player$contribution$label ==
        "How much will you contribute?"
      test8 <-
        cbook$public_goods_simple$Player$contribution$noargs ==
        "FALSE"

      # If there is only one group field without information in it
      test9 <- cbook$bertrand$Group$winning_price$field == "CurrencyField"

      # If there is one group field that has information in it
      test10 <- cbook$dictator$Group$kept$field == "CurrencyField"

      # If there are two group fields with no information in them
      test11 <-
        cbook$public_goods_simple$Group$total_contribution$field == "CurrencyField"

      test12 <-
        all(names(cbook) == c("settings", original_sort))

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)
      testthat::expect_true(test8)
      testthat::expect_true(test9)
      testthat::expect_true(test10)
      testthat::expect_true(test11)
      testthat::expect_true(test12)
    })

    testthat::test_that("Codebook (w) - only list, new oTree, false sort", {

      thiswarning <- capture_warnings(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          doc_info = FALSE,
          sort = c("public_goods_simple", "dictator")
        ))

      testthat::expect_true(
        any(grepl("Sort apps are not equal to all apps", thiswarning)))

      test1 <- is.list(cbook)
      test2 <- !is.null(cbook$public_goods_simple$Constants)
      test3 <- !is.null(cbook$public_goods_simple$Group)
      test4 <- !is.null(cbook$public_goods_simple$Player)

      test5 <-
        cbook$public_goods_simple$Constants$NAME_IN_URL ==
        "public_goods_simple"
      test6 <-
        cbook$public_goods_simple$Group$total_contribution$noargs ==
        "TRUE"
      test7 <-
        cbook$public_goods_simple$Player$contribution$label ==
        "How much will you contribute?"
      test8 <-
        cbook$public_goods_simple$Player$contribution$noargs ==
        "FALSE"

      # If there is only one group field without information in it
      test9 <- cbook$bertrand$Group$winning_price$field == "CurrencyField"

      # If there is one group field that has information in it
      test10 <- cbook$dictator$Group$kept$field == "CurrencyField"

      # If there are two group fields with no information in them
      test11 <-
        cbook$public_goods_simple$Group$total_contribution$field == "CurrencyField"

      test12 <-
        all(names(cbook) ==
              c("settings", "bargaining", "bertrand",
                "common_value_auction", "cournot",
                "dictator", "guess_two_thirds",
                "matching_pennies", "payment_info",
                "prisoner", "public_goods_simple",
                "survey", "traveler_dilemma",
                "trust", "trust_simple", "volunteer_dilemma"))

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)
      testthat::expect_true(test8)
      testthat::expect_true(test9)
      testthat::expect_true(test10)
      testthat::expect_true(test11)
      testthat::expect_true(test12)
    })

    testthat::test_that("Codebook - only list, new oTree, only one app", {

      testthat::expect_message(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          doc_info = TRUE,
          app = "public_goods_simple"),
        "Variables without documentation, label, or verbose name:"
      )

      test1 <- is.list(cbook)
      test2 <- !is.null(cbook$public_goods_simple$Constants)
      test3 <- !is.null(cbook$public_goods_simple$Group)
      test4 <- !is.null(cbook$public_goods_simple$Player)

      test5 <-
        cbook$public_goods_simple$Constants$NAME_IN_URL ==
        "public_goods_simple"
      test6 <-
        cbook$public_goods_simple$Group$total_contribution$noargs ==
        "TRUE"
      test7 <-
        cbook$public_goods_simple$Player$contribution$label ==
        "How much will you contribute?"
      test8 <-
        cbook$public_goods_simple$Player$contribution$noargs ==
        "FALSE"

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)
      testthat::expect_true(test8)
    })

    testthat::test_that("Codebook (w) - only list, too many sort", {
      # Without trust_simple

      original_sort <-
        c("bargaining", "bertrand", "common_value_auction", "cournot",
          "dictator", "guess_two_thirds", "matching_pennies", "payment_info",
          "prisoner", "public_goods_simple", "survey", "traveler_dilemma",
          "trust", "trust_simple", "volunteer_dilemma")

      new_sort <- c("thisisnothere", "payment_info",
                    "bargaining", "volunteer_dilemma",
                    "bertrand", "common_value_auction",
                    "cournot", "dictator",
                    "guess_two_thirds", "matching_pennies",
                    "prisoner", "public_goods_simple",
                    "survey", "traveler_dilemma",
                    "trust")

      thiswarning <- capture_warnings(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new"),
          fsource = "init",
          output = "list",
          output_format = "html_document",
          doc_info = FALSE,
          sort = new_sort)
      )

      testthat::expect_true(any(
        grepl(x = thiswarning, pattern = "Sort elements not in apps are")))

      testthat::expect_true(
        any(grepl("Sort apps are not equal to all apps", thiswarning)))

      testthat::expect_true(any(grepl("Sort elements not in apps are",
                                      thiswarning)))

      test1 <- is.list(cbook)
      test2 <- !is.null(cbook$public_goods_simple$Constants)
      test3 <- !is.null(cbook$public_goods_simple$Group)
      test4 <- !is.null(cbook$public_goods_simple$Player)

      test5 <-
        cbook$public_goods_simple$Constants$NAME_IN_URL ==
        "public_goods_simple"
      test6 <-
        cbook$public_goods_simple$Group$total_contribution$noargs ==
        "TRUE"
      test7 <-
        cbook$public_goods_simple$Player$contribution$label ==
        "How much will you contribute?"
      test8 <-
        cbook$public_goods_simple$Player$contribution$noargs ==
        "FALSE"

      # If there is only one group field without information in it
      test9 <- cbook$bertrand$Group$winning_price$field == "CurrencyField"

      # If there is one group field that has information in it
      test10 <- cbook$dictator$Group$kept$field == "CurrencyField"

      # If there are two group fields with no information in them
      test11 <-
        cbook$public_goods_simple$Group$total_contribution$field == "CurrencyField"

      test12 <-
        all(names(cbook) == c("settings", original_sort))

      testthat::expect_true(test1)
      testthat::expect_true(test2)
      testthat::expect_true(test3)
      testthat::expect_true(test4)
      testthat::expect_true(test5)
      testthat::expect_true(test6)
      testthat::expect_true(test7)
      testthat::expect_true(test8)
      testthat::expect_true(test9)
      testthat::expect_true(test10)
      testthat::expect_true(test11)
      testthat::expect_true(test12)

      # Check if "yes, I will" is one element
      testthat::expect_true(
        length(cbook$bargaining$Player$level2$choices) == 3L)

      testthat::expect_true(
        cbook$bargaining$Player$level2$choices[1] == "No, I won't")
      testthat::expect_true(
        cbook$bargaining$Player$level2$choices[3] == "Maybe I'll do")

      # Test for ''' documentations
      test1 <- grepl(
        pattern = "^A bat and a ball cost 22 dollars in tota.*the ball cost\\?$",
        x = cbook$survey$Player$crt_bat$label)

      # Test for """ documentations
      test2 <- grepl(
        pattern = "^If it takes 5 machines 5.*make 100 widgets\\?$",
        x = cbook$survey$Player$crt_widget$label)

      testthat::expect_true(test1)
      testthat::expect_true(test2)

      # Test for ' choices
      testthat::expect_true(
        cbook$survey$Player$gender$choices$key[1L] == "Male")
      testthat::expect_true(
        cbook$survey$Player$gender$choices$key[2L] == "Female")
      testthat::expect_true(
        cbook$survey$Player$gender$choices$value[1L] == "Male")
      testthat::expect_true(
        cbook$survey$Player$gender$choices$value[2L] == "Female")

      # Test for " choices
      testthat::expect_true(
        cbook$survey$Player$gender2$choices$key[1L] == "Male")
      testthat::expect_true(
        cbook$survey$Player$gender2$choices$key[2L] == "Female")
      testthat::expect_true(
        cbook$survey$Player$gender2$choices$value[1L] == "Male")
      testthat::expect_true(
        cbook$survey$Player$gender2$choices$value[2L] == "Female")

      # Further
      testthat::expect_equal(length(cbook$prisoner$Player$cooperate$choices), 2L) # True: Cooperate, False Defect
      testthat::expect_true(is.data.frame(cbook$prisoner$Player$cooperate$choices))
    })
}
