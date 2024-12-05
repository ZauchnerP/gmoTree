if (rlang::is_installed(c("withr", "testthat"))) { 
  
  print("---- start codebook tests -----")

  testthat::test_that("Codebook (w) - unmatched square brackets", {
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z13"),
        fsource = "model",
        output = "list",
        doc_info = FALSE), 
      "\\$rankaversion\\$Player\\$gender \\(unmatched square brackets\\).*\\(unmatched brackets\\)")
    
    # Brackets
    cbook$rankaversion$Player$variable$doc == "(A doc in brackets))"
    cbook$rankaversion$Player$variable2$doc == "A doc with one closing bracket"
    
    
    # No warning when it comes to constants
    testthat::expect_true(
      cbook$rankaversion$Constants$Something == "[[information]")
    
    # Even if there is a warning, it is still shown
    testthat::expect_true(
      cbook$rankaversion$Player$gender$doc == "[[Gender]")
  })

  testthat::test_that("Codebook - quotes", {
    
    warning <- testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z4"),
        fsource = "models",
        output_format = "latex",
        output_file = "cb_quotestest",
        output = "list",  
        settings_replace = NULL,
        doc_info = FALSE)
    )
    
    warning <- testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z4"),
        fsource = "models",
        output_format = "pdf_simple",
        output_file = "cb_quotestest_simple",
        output = "list",  
        settings_replace = NULL,
        doc_info = FALSE)
    )
    
    warning <- testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z4"),
        fsource = "models",
        output_format = "pdf",
        output_file = "cb_quotestest",
        output = "list",  
        settings_replace = NULL,
        doc_info = FALSE)
    )
    
    
    # Doc with """ in them
    testthat::expect_true(cbook$end$Player$hierarchy_of_principlesArg$doc ==
                            "Argument for Rejection")
    
    # Doc with ''' in them
    testthat::expect_true(cbook$end$Player$hierarchy_of_principlesArg2$doc ==
                            "Argument for Rejection")
    
    testthat::expect_true(cbook$end$Constants$without == "This is a without test")
    # Normal string  - Single values with single string def
    testthat::expect_true(cbook$end$Constants$name_in_url == "end")
    # Normal string  - Single values with double string def
    testthat::expect_true(cbook$end$Constants$name_in_url2 == "end")
    # Normal string - single quoted strings within double string def
    testthat::expect_true(cbook$end$Constants$singlequoteinsidedouble ==
                            "This is a 'test'")
    # Normal string - escaped quoted strings within double string def
    testthat::expect_true(cbook$end$Constants$doublequoteinsidesingle ==
                            "This is another \"test\"")
    testthat::expect_true(cbook$end$Constants$doublequoteinsidesingle2 ==
                            "This is another \"test\" inside \"another test\"")
    
    testthat::expect_true(cbook$end$Constants$escapedsinglequote ==
                            "This ia a 'single quote test'")
    
    testthat::expect_true(cbook$end$Constants$escapeddoublequote ==
                            "This is a \"double quote test\"")
    
    # Choices with single quotation marks
    testthat::expect_true(cbook$end$Player$Acc_DistributionChange$choices$value[1] ==
                            "Ich hätte mich dennoch für Verfahren >settings.TokenAdd_1dName< entschieden.")
    # Test next argument because they have different quotations:
    
    testthat::expect_true(cbook$end$Player$Acc_DistributionChange$doc ==
                            "Former variable name: AnerkennungPrivat")
    
    # Choices with double quotation marks
    
    testthat::expect_true(
      cbook$end$Player$Acc_DistributionChange2$choices$value[1] ==
        "Ich hätte mich dennoch für Verfahren >settings.TokenAdd_1dName< entschieden.")
    testthat::expect_true(cbook$end$Player$Acc_DistributionChange2$doc ==
                            "Former variable name: AnerkennungPrivat")
  })

  testthat::test_that("Codebook - print", {
    
    outputfilewithgetwd <- paste0(getwd(), "codebooktestprint")
    
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_new"),
      fsource = "init",
      output_dir = "getwd()",
      output_format = "latex_document",
      output_file = outputfilewithgetwd,
      output = "list",
      app = "dictator",
      doc_info = FALSE)
    
    # Check for print removal in Constants
    testthat::expect_true(cbook$dictator$Constants$NAME_IN_URL == "dictator")
    testthat::expect_true(cbook$dictator$Constants$PLAYERS_PER_GROUP == 2) 
    testthat::expect_true(cbook$dictator$Constants$NUM_ROUNDS == 1) # Here is print
    testthat::expect_true(cbook$dictator$Constants$ENDOWMENT == "cu(100)")
    
    # Check for print removal in Group
    testthat::expect_true(all(
      names(cbook$dictator$Group$kept) == 
        c("noargs", "doc", "min", "max", "label", "field")))
    testthat::expect_true(cbook$dictator$Group$kept$noargs == FALSE)
    testthat::expect_true(cbook$dictator$Group$kept$doc == 
                            "Amount dictator decided to keep for himself")
    testthat::expect_true(cbook$dictator$Group$kept$min == 0)
    testthat::expect_true(cbook$dictator$Group$kept$label == "I will keep")
    
    # Check for print removal in Player
    
    testthat::expect_true(all(
      names(cbook$dictator$Player) == c("gender", "chandler")))
    
    testthat::expect_true(cbook$dictator$Player$gender$field == "IntegerField")
    testthat::expect_true(cbook$dictator$Player$gender$noargs == FALSE)
    testthat::expect_true(cbook$dictator$Player$gender$doc == "Gender")
    
    testthat::expect_true(all(
      names(cbook$dictator$Player$chandler) == c("noargs","doc", "field")))
    testthat::expect_true(cbook$dictator$Player$chandler$field == "IntegerField")
    testthat::expect_true(cbook$dictator$Player$chandler$noargs == FALSE)
    testthat::expect_true(cbook$dictator$Player$chandler$doc == "Chandler")
    
    
  })
  
  testthat::test_that("Codebook - new oTree, app_rm", {
    
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_new"),
      fsource = "init",
      output = "list",
      output_file = "newappapprm",
      app_rm = "dictator",
      doc_info = FALSE)
    
    test1 <- !is.null(cbook$bargaining)
    test2 <- is.null(cbook$dictator)
    testthat::expect_true(test1)
    testthat::expect_true(test2)
    
    testthat::expect_true(cbook$bargaining$Player$level$doc ==
                            "choices with only [1,2,3]")
  })

  testthat::test_that("Codebook - brackets", {
    # TODO also check in PDF and word
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output = "list",
        output_dir = NULL,
        output_format = "pdf_simple",
        output_file = "cb_bracket_newline",
        fsource = "models",
        splitvarname = TRUE,
        sep_list = "newline",
        output_open = FALSE
        ), "Some variables")
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output = "list",
        output_dir = NULL,
        output_format = "latex",
        output_file = "cb_bracket_newline",
        fsource = "models",
        splitvarname = TRUE,
        sep_list = "newline",
        output_open = FALSE), "Some variables")
    
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output = "list",
        output_dir = NULL,
        output_format = "word",
        output_file = "bracket_newline_word",
        fsource = "models",
        splitvarname = TRUE,
        sep_list = "newline",
        output_open = FALSE), "Some variables")
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output = "list",
        output_dir = NULL,
        output_file = "bracket_vector",
        sep_list = "vector",
        fsource = "models",
        splitvarname = TRUE), "Some variables")
    
    # First and last bracket
    # First and last brackets are not removed for normale variables!
    testthat::expect_true(
      cbook$rankend$Constants$firstlastbr == "(firstlastbracketexample)")
    
    # Empty list
    testthat::expect_identical(cbook$rankend$Constants$emptylist, list())
    testthat::expect_identical(cbook$rankend$Constants$emptylist2, list())
    
    # Vector with numbers and strings
    testthat::expect_true(is.list(cbook$rankaversion$Constants$onelevelvectorl))
    testthat::expect_identical(
      cbook$rankaversion$Constants$onelevelvectorl,
      list("a", "b", 3))
    
    testthat::expect_identical(
      cbook$rankaversion$Constants$onelevelvectorl2,
      list("a", "b", 3, "asdfadsf", "adsfadsfasdf", "00asasdf", 2342, "wqerwerqewr"))
    testthat::expect_true(is.list(cbook$rankaversion$Constants$onelevelvectorl2))
    
    # Original oTree code has brackets:  (Yes)
    testthat::expect_true(cbook$rankaversion$Player$acceptance$choices$value[1] == "(Yes)")
    
    # Test for numeric vectors in Constants
    testthat::expect_identical(cbook$rankend$Constants$TestVector,
                               list(1, 10, 3, 4, 5))
    
    # Test for numeric vectors in Settings
    testthat::expect_identical(cbook$rankend$Constants$TestVector2,
                               list(99, 66, 33))
    
    
    # Test for character vectors in Constants
    testthat::expect_identical(cbook$rankend$Constants$TestVectorC,
                               list("a", "b", 6))
    
    # Test for character vectors in Settings
    testthat::expect_identical(cbook$rankend$Constants$TestVectorC2,
                               list("d", "e", 4))
    
    # Test for two-level lists 
    testthat::expect_true(is.list(cbook$rankaversion$Constants$choices))
    
    testthat::expect_true(identical(
      cbook$rankaversion$Constants$choices[[1]],
      list(4, 1, 1, 0.5, "noRR_needSat", 3, 2)
    ))
    
    # If closing bracket is at next row
    testthat::expect_true(all(cbook$rankaversion$Constants$nextrowbracket[[1]] == 
                                c(69, 43, 5)))
    
    # If closing bracket is at same row
    testthat::expect_true(all(cbook$rankaversion$Constants$samerowbracke[[1]] == 
                                c(69, 43, 5)))
    
    
    # If there is a space before the second opening bracket
    testthat::expect_true(all(cbook$rankaversion$Constants$spacebeforebracket[[1]] == 
                                c(2004, 43, 5)))
    
    # If there is a comment before the second opening bracket
    testthat::expect_true(all(cbook$rankaversion$Constants$commentafterbracket[[1]] == 
                                c(69, 43, 5)))
    
    # If there is a space before the last closing bracket
    testthat::expect_true(all(cbook$rankaversion$Constants$spaceafterbracket[[1]] == 
                                c(69, 43, 5)))
    
    
    # If there is a space after a comma
    testthat::expect_true(all(cbook$rankaversion$Constants$spaceaftercomma[[1]] == 
                                c(2004, 43, 5)))
    
    # If there is a comment after a comma
    testthat::expect_true(all(cbook$rankaversion$Constants$commentaftercomma[[1]] == 
                                c(2004, 43, 5)))
    
    
  })

  testthat::test_that("Codebook (e) - three level list in constants", {
  
    
    # In constants
    testthat::expect_error({
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_z10"),
      output_dir = NULL,
      output = "list",
      fsource = "model")
    }, "This function does not support lists with more.*Constants")
  })
  
  testthat::test_that("Codebook - old oTree", {
    
    testthat::expect_message(
      testthat::expect_warning(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z"),
          fsource = "model",
          output = "list",
          doc_info = TRUE),
        ".*too complex for this function.*rankaversion\\$Constants.*\\$rankend\\$Constants \\(read_csv\\).*\\$settings\\$showupToken \\(float\\).*\\$end\\$Constants\\$showupToken \\(float\\).*\\$end\\$Constants\\$showupTokenTest \\(float\\)"
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

  testthat::test_that("Codebook - several lines", {
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_count"),
      fsource = "init",
      output_open = FALSE,
      output = "list",
      output_file = "cb_severallines",
      doc_info = TRUE)
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
    testthat::expect_identical(
      cbook$settings$Something,
      list(1, 3, "test", 3, 1))
    
    testthat::expect_identical(
      cbook$rankaversion$Constants$Something,
      list(1, 3, "test", 3, 1))
    
    # Otherwise
    testthat::expect_identical(
      cbook$settings$StartToken, 
      list(9.0, 14.0, 17.0, 23.0, 38.0))
    
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
      unlist(cbook$settings$StartToken) * 5L
    
    test11 <- all(
      cbook$rankaversion$Player$Acc_DistributionChange$value ==
        c("Ich hätte mich dennoch für Verfahren > xyz < entschieden.",
          "Ich hätte mich für das alternative Verfahren > aaa <  entschieden.",
          "Ich hätte mich für das alternative Verfahren > bbb < entschieden."))
    
    test12 <- all(cbook$rankaversion$Player$acceptance$choices$value ==
                    c("xyz", "aaa", "Wrong, trick answer", "Correct trick answer"))
    
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

  testthat::test_that("choices ohne key", {
    
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_new"),
      fsource = "init",
      output = "list",
      doc_info = FALSE,
      app = "bargaining")
    
   cbook$bargaining$Player$level2$choices[1] == "No, I won't"
   cbook$bargaining$Player$level2$choices[2] == "yes, I will"
   cbook$bargaining$Player$level2$choices[3] == "Maybe I'll do"
            
    
  })
  
  testthat::test_that("Codebook - documentation of variables", {
    
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_z8"),
      output_dir = NULL,
      fsource = "models",
      splitvarname = TRUE,
      output = "list")
    
    testthat::expect_true(cbook$rankaversion$Player$gender$doc == "Gender documentation with double quotations")
    
    testthat::expect_true(cbook$rankaversion$Player$gender2$doc == "Gender documentation with single quotations")
    
    testthat::expect_true(
      cbook$rankaversion$Player$gender3$doc == 
        "Gender documentation with tripple double quotations")
    
    
    
    testthat::expect_true(
      cbook$rankaversion$Player$gender4$doc == 
        "Gender documentation with tripple single quotations")
    
    
    
    
    
    
    
    
  
    testthat::expect_true(grepl(
      cbook$rankaversion$Player$crt_bat$label, 
      pattern = "^A bat and a ball.*cost\\?$"))
    
    
    testthat::expect_true(grepl(
      cbook$rankaversion$Player$crt_bat2$label,
      pattern = "^A bat and a ball.* cost\\?$"))
    
    
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
    
    # Check if leading and trailing brackets and comments are removed   # TODO compara with prior verison
    # Original "(Please enter an amount from 0 to 100)"
    test2 <- cbook$bargaining$Player$request$label == 
      "(Please enter an amount from 0 to 100)"

    
    test3 <- cbook$bargaining$Player$request2$label == 
      "Please enter an amount from 0 to 100"
    test4 <- cbook$bargaining$Player$request3$label == 
      "Please enter an amount from 0 to 100"
    
    testthat::expect_true(test1)
    testthat::expect_true(test2)
    testthat::expect_true(test3)
    testthat::expect_true(test4)
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
    testthat::expect_true(cb$test$Group$variable2$max == 3.3)

    test1 <- cb$test$Constants$min_payoffAB ==
      "(float(1.0)) + (float(1.1)) + (float(1.2))"
    test2 <- cb$test$Group$min_payoffAB$max ==
      "(float(1.0)) + (float(1.1)) + (float(1.2))"
    test3 <- cb$test$Player$min_payoffAB$max ==
      "(float(1.0)) + (float(1.1)) + (float(1.2))"
    

    test4 <- cb$test$Group$variable2$max == 3.3

    testthat::expect_true(all(c(test1, test2, test3, test4)))
    
  })
  
  testthat::test_that("Codebook - new oTree, only one app", {
    
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
    
    # Remove leading and trailing """ from doc
    test9 <- cbook$volunteer_dilemma$Player$volunteer$doc == 
      "Whether player volunteers"
    
    # Remove leading and trailing ' from label
    test9 <- cbook$volunteer_dilemma$Player$volunteer$label == 
      "Do you wish to volunteer?"
    
    testthat::expect_true(test1)
    testthat::expect_true(test2)
    testthat::expect_true(test3)
    testthat::expect_true(test4)
    testthat::expect_true(test5)
    testthat::expect_true(test6)
    testthat::expect_true(test7)
    testthat::expect_true(test8)
  })

  testthat::test_that("Codebook - key value", {

    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_z7"),
      output_dir = NULL,
      fsource = "models",
      splitvarname = TRUE,
      output = "list")
    
    # Key and value in double quotations
testthat::expect_true(all(
  cbook$rankaversion$Player$gender$choices$key == c("Male1", "Female")))

testthat::expect_true(all(
  cbook$rankaversion$Player$gender$choices$value == c("Male", "Femaleval")))


# Key and value in single quotations
testthat::expect_true(all(
  cbook$rankaversion$Player$gender2$choices$key == c("Male", "Female")))

testthat::expect_true(all(
  cbook$rankaversion$Player$gender2$choices$value == c("Male", "Femaleval")))
  })

  testthat::test_that("Codebook - empty arguments", {
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        splitvarname = TRUE,
        output = "list"))
    
    
    
    # Empty argument after choices
    testthat::expect_true(
      cbook$rankaversion$Player$a_richer$choices$value[[2]] ==
        "B is the richer person")
    
    testthat::expect_true(all(
      names(cbook$rankaversion$Player$a_richer) == c("noargs", "doc", "choices", "field")))
    
    testthat::expect_true(all(
      cbook$rankaversion$Player$a_richer$choices$value == 
        c("A is the richer person", "B is the richer person")))
    
    # Empty arguments after doc
    testthat::expect_true(
      cbook$rankaversion$Player$testempty$doc == "This is an example doc")
    testthat::expect_true(all(names(cbook$rankaversion$Player$testempty) == 
                                c("noargs", "doc", "field")))
    
    # Variable after that
    testthat::expect_true(exists("is_trick", cbook$rankaversion$Player))
    
  })

  testthat::test_that("Codebook - from-to string values", {

      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z5"),
        fsource = "models",
        output_format = "html",
        output_file = "fromtovalues",
        output = "list",  
        settings_replace = NULL,
        doc_info = FALSE)
    
    
    cbook$MIG1$Player$money_in_month$choices[1] == "0-399"
    cbook$MIG1$Player$money_in_month$choices[2] == "400-699"
    
    
    
  })
  
  testthat::test_that("Codebook - newlines in doc", {
    
    warning <- testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z4"),
        fsource = "models",
        output = "list",  
        settings_replace = NULL,
        doc_info = FALSE))
    
    testthat::expect_true(cbook$end$Player$completionCode$doc == 
                            "Completion Code for MTurk. Has no role for the experiment and will not be validated unless there are legal problems with the participants")
    testthat::expect_true(cbook$end$Player$completionCodespaces$doc == 
                            "Completion Code for MTurk. Has no role for the experiment and will not be validated unless there are legal problems with the participants")
    
  })
  
  testthat::test_that("Codebook (e) - settings var. not there3", {
    
    # Try user Var but with some missing
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        fsource = "model",
        user_settings = list(error = "Test"),
        settings_replace = "user",
        output = "list")
      , " not in user\\_settings and cannot be replaced.*\\$rankaversion\\$MinutesForSurvey") 
  })
  
  testthat::test_that("Codebook - pdf brackets", {
    
    # TODO hier überall checken wie rankaversion$Constants$choices in output angezeigt wird 
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        output_file = "newline",  # Pretty :)
        splitvarname = TRUE,
        doc_info = FALSE,
        output = "list", 
        sep_list = "newline"), "Some variables")
    

    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        splitvarname = TRUE,
        output_file = "cb_vector_complex", # Not pretty yet
        doc_info = FALSE,
        output = "list",
        sep_list = "vector"), "Some variables") # TODO not working properly because the cell is too large, bzw. kommt da keine warning message
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        splitvarname = TRUE,
        output_format = "pdf_document_simple",  # Not pretty yet
        output_file = "cb_vector_simple",
        output = "list",
        doc_info = FALSE,
        sep_list = "vector"), "Some variables") 
    
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        output_dir = NULL,
        fsource = "models",
        splitvarname = TRUE,
        output_format = "word_document",  # Word works
        output_file = "vector_word",
        output = "list",
        doc_info = FALSE,
        sep_list = "vector"), "Some variables")
  })

  # Equal sign
  testthat::test_that("Codebook - escaped equal signs in doc values", {
    
    cbook <- codebook(testthat::test_path("testdata", "ocode_f2"), 
                      fsource = "model", 
                      app = "MIG1",
                      doc_info = FALSE,
                      output = "list")
    
    # Vales in oTree code should be written with  \= 
    testthat::expect_true(
      cbook$MIG1$Player$P1_ConsumptionFinal$doc == 
        "Final Consumption \\=  with TokenAdd as voted for by group")
    testthat::expect_true(
      cbook$MIG1$Player$P1_ConsumptionNoDis$doc == 
        "Initial Consumption \\=  no Redistribution")
    testthat::expect_true(
      cbook$MIG1$Player$P1_ConsumptionEqual$doc == 
        "Initial Consumption \\=  with TokenAdd as suggested by Experimenter") 
    testthat::expect_true(
      cbook$MIG1$Player$P1_ConsumptionWeakerFirst$doc == 
        "Initial Consumption \\=  with TokenAdd as suggested by Weaker First")
    testthat::expect_true(
      cbook$MIG1$Player$P1_ConsumptionMixed$doc == 
        "Initial Consumption \\=  with TokenAdd as suggested by Mixed")
  })
  
  testthat::test_that("Codebook (e)- equal signs in doc values", {
    # If there are unescaped equal signs, errors are thrown
    
    testthat::expect_error({
      
      cbook <- codebook(testthat::test_path("testdata", "ocode_f1"), 
                        fsource = "model", 
                        doc_info = FALSE,
                        app = "MIG1",
                        output_file = "esindoc",
                        output = "list")
      
    }, "read properly by gmoTree.*\\$MIG1\\$Player\\$P1_ConsumptionFinal")
  })
  
  testthat::test_that("Codebook (e)- equal signs in widgets", {
    # If there are unescaped equal signs, errors are thrown
    
    # testthat::expect_error({ 
      
      cbook <- codebook(testthat::test_path("testdata", "ocode_f4"), 
                        fsource = "model", 
                        doc_info = FALSE,
                        app = "MIG1",
                        output_file = "cb_of4",  # TODO achtung. hier wird Group blöd angezeigt. 
                        output = "list")
      
    # }, " cannot be read properly by gmoTree.*(MIG1\\$Player\\$user_total")  # TODO hier wird kein error gezegit
  })
  
  testthat::test_that("Codebook (e)- equal signs in choices values", {
    # If there are unescaped equal signs, errors are thrown
    
    testthat::expect_error({
      
      cbook <- codebook(testthat::test_path("testdata", "ocode_f3"), 
                        fsource = "model", 
                        doc_info = FALSE,
                        app = "MIG1",
                        output = "list")
      
    }, "cannot be read properly by gmoTree.*\\$MIG1\\$Group\\$treatment_ec.*\\$MIG1\\$Player\\$P1_AckVote")
  })
  
  testthat::test_that("Codebook - escaped equal signs in choices values", {
    # If there are unescaped equal signs, errors are thrown
    
    cbook <- codebook(testthat::test_path("testdata", "ocode_f4"), 
                      fsource = "model", 
                      doc_info = FALSE,
                      app = "MIG1",
                      output = "list")
    
    # TODO Also check in output file
    # TODO check auch wie viele antworten
    testthat::expect_true(length(cbook$MIG1$Group$treatment_ec$choices) == 2)
    testthat::expect_true(cbook$MIG1$Group$treatment_ec$choices$key[1] == 0)
    testthat::expect_true(cbook$MIG1$Group$treatment_ec$choices$value[1] ==
                            "0 \\= tolle Anfangssausstattung")
    testthat::expect_true(length(cbook$MIG1$Player$P1_AckVote$choices) == 2)
    testthat::expect_true(cbook$MIG1$Player$P1_AckVote$choices$value[1] == "Das Verfahren x \\= 1 soll anerkannt werden")
    
    # Other tests 
    testthat::expect_true(all(
      names(cbook$MIG1$Group$P1_Rej_nArgAkzNo_2) == c("noargs", "initial", "field")))
    
  })
  
  # Output
  testthat::test_that("Codebook (e) - wrong output format", {
    
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output = "both",
        output_open = TRUE,
        output_format = "WrongFormat",
        doc_info = FALSE)}, "Output format should be")
  })
  
  testthat::test_that("Codebook (e) - dots in names", {
    
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_file = "coebook.new",
        output = "both",
        output_open = TRUE,
        fsource = "init")
      
    }, "You are not allowed to use dots in your output_file names")
    
  })
  
  testthat::test_that("Codebook (e) - dir not there", {
    
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_file = "codebooknothappening",
        output_dir = "E:/folder",
        output = "both",
        output_open = TRUE,
        fsource = "init")
      
    }, "The directory.*does not exist yet.")
    
  })
  
  testthat::test_that("Codebook (e) - wrong output", {
    
    # Wrong text
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
    
    # Output dir and file don't align
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_file = "E:/folder/filename",
        output_dir = "E:/folder2",
        output = "both",
        output_open = TRUE,
        fsource = "init")}, "When using an absolute path for")
    
  })
  
  testthat::test_that("Codebook (e) - output file contains invalid path", {
    
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output_dir = NULL,
        output_file = "E:/GMO/gmoTree2/codebooktestprint/ocode_newivalpath",
        output = "both",
        output_open = TRUE,
        app = "dictator",
        doc_info = FALSE), "The directory .* does not exist.")
  })
  
  testthat::test_that("Codebook (e) - empty output_file", {
    
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_file = NULL,
        output = "both",
        output_open = TRUE,
        fsource = "init")
      
    }, "Please enter a output_file name!")
    
  })
  
  testthat::test_that("Codebook (e) - params", {
    warnings <- testthat::capture_warnings(
      testthat::expect_error(
        
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_z2"),
          fsource = "model",
          params = "ab",
          output = "list"),
        "params must be a list"))
    
  })
  
  # Choosing apps
  testthat::test_that("Codebook (e) - app and app rm specified", {
    
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
  
  testthat::test_that("Codebook (e) - apps don't exist", {
    
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output = "list",
        app = c("dictator", "nonexistingapp"),
        doc_info = FALSE)}, "At least one app")
  })
  
  testthat::test_that("Codebook (e) - one app not there", {
    
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

  # Settings
  testthat::test_that("Codebook - three level list in settings work", {
    
    # If settings_replace is global
    testthat::expect_error({
      cbook1 <- codebook(
        path = testthat::test_path("testdata", "ocode_z11"),
        output_dir = NULL,
        output = "list",
        settings_replace = "global",
        fsource = "model")
    }, "not support lists.*settings")
    
    # If settings_replace is NULL
    cbook2 <- codebook(
      path = testthat::test_path("testdata", "ocode_z11"),
      output_dir = NULL,
      output = "list",
      settings_replace = NULL,
      fsource = "model")
    
    testthat::expect_null(cbook2$rankaversion$settings)
    testthat::expect_true(
      cbook2$rankaversion$Constants$Something == "settings.Something")
    
  })
  
  testthat::test_that("Codebook (w) - setting var not there 4", {
    
    # Try user Var
    message <- capture_messages(
      testthat::expect_warning(
        cbook <- codebook(
          path = testthat::test_path("testdata", "ocode_new3"),
          settings_replace = "global",
          fsource = "init",
          output = "list") 
        , "are not in settings and"))
    
    testthat::expect_true(
      grepl(x = paste(message, collapse = ""),
            pattern = "Variables without documentation.*bargaining\\$Group\\$total_requests"))
    
    test1 <-
      cbook$bargaining$Player$request$max == "settings.AMOUNT_SHARED"
    
    testthat::expect_true(test1)
  })
  
  testthat::test_that("Codebook - setting, constants type", {
    
    cbook <- codebook(
      path = testthat::test_path("testdata", "ocode_z9"),
      fsource = "models",
      output = "list",
      doc_info = FALSE)
    
    
    testthat::expect_identical(
      cbook$settings$Something,
      list(1, 3, "test", 3, 1))
    
    testthat::expect_identical(
      cbook$settings$Something,
      cbook$rankaversion$Constants$Something)
    
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
        output = "list")
      , "settings_replace must be set to"
    )
    
    # Wrong variable
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        fsource = "model",
        user_settings = list(error = "Test",
                             showupToken = 2L),
        settings_replace = "user",
        output = "list"),
      "not in user_settings and"
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
  
  testthat::test_that("Codebook (w) - folder without settings", {
    
    warnings <- testthat::capture_warnings(
      cbook <- codebook(
        fsource = "model",
        path = testthat::test_path("testdata", "ocode_z3.1"),
        output = "list",
        doc_info = FALSE))
    
    test1 <- any(grepl(
      pattern = "There is no settings.* in your path..*\\$rankaversion\\$Testnumber, reference \"settings.Testnumber",
      x = warnings))
    
    test2 <- any(grepl(
      pattern = "too complex",
      x = warnings))
    
    testthat::expect_true(test1)
    testthat::expect_true(test2)
    
    # Check if variables are there only once
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$Testnumber") == 1)
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$Something") == 1)
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$TestToken") == 1)
    
    # User settings
    warnings <- testthat::capture_warnings(
      cbook <- codebook(
        fsource = "model",
        path = testthat::test_path("testdata", "ocode_z3.1"),
        output = "list",
        settings_replace = "user",
        doc_info = FALSE))
    
    # Check if variables are there only once
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$Testnumber") == 1)
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$Something") == 1)
    testthat::expect_true(
      stringr::str_count(warnings, "\\$rankaversion\\$TestToken") == 1)
    
  })
  
  testthat::test_that("Codebook - user_settings", {
    
    # Comparison without settings.py variable
    testthat::expect_warning(
      cbook_default <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        fsource = "model",
        settings_replace = "global",
        doc_info = FALSE,
        output = "list"))
    
    
    # Try user Var
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z2"),
        fsource = "model",
        user_settings = list(error2 = "Test",
                             showupToken = 2,
                             max_payoffPart1_ab = 1,
                             ExchangeMainCurrency = 1,
                             payoff_survey = 1,
                             MinutesForSurvey = 20,
                             TestVector2 = c(1,2,3),
                             TestVectorC2 = c("a", "b"),
                             ExchangeToken = 1,
                             timer123 = 12,
                             payoffPart1_c = 1),
        settings_replace = "user",
        doc_info = FALSE,
        output = "list"), "too complex for this function")
    
    cbook$rankend$Constants$settings.TestVector2
    
    # Models.py: errorvariable <- settings.error
    
    testthat::expect_true(cbook_default$rankend$Constants$Test1 == 1000)
    testthat::expect_identical(cbook_default$rankend$Constants$TestVector,
                               list(1,10,3,4,5))
    testthat::expect_identical(cbook_default$rankend$Constants$TestVectorC,
                               list("a", "b", 6))
    
    # Variables in settings.py
    test1 <- cbook_default$rankaversion$Constants$error2 == "Error2insettings"
    test2 <- cbook_default$rankaversion$Constants$MinutesForSurvey == 18
    # User setting variables
    testthat::expect_identical(
      cbook_default$rankend$Constants$TestVector2,
      list(99, 66, 33))   # settings.TestVector2
    testthat::expect_identical(cbook_default$rankend$Constants$TestVectorC2, 
                               list("d", "e", 4))
    test3 <- cbook$rankaversion$Constants$error2 == "Test"  # This variable is not in settings.py
    test4 <- cbook$rankaversion$Constants$MinutesForSurvey == 20
    
    testthat::expect_true(test1)
    testthat::expect_true(test2)
    testthat::expect_true(test3)
    testthat::expect_true(test4)
  })
  
  # Fsource
  testthat::test_that("Codebook (e) - fsource not valid", {
    
    # Wrong source text
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
    
    # Wrong length
    testthat::expect_error({
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_dir = NULL,
        fsource = c("init", "model"),
        output = "list")}, "Please enter only one fsource")
    
  })
  
  testthat::test_that("Codebook (e) - fsource init instead of models", {
    
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z"),
        fsource = "init", # wrong because old otree
        output = "list",
        doc_info = TRUE),
      "At least one of your init-files is empty"
    )
  })
  
  testthat::test_that("Codebook (e) - fsource models instead of init", {
    
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "model", # wrong because new otree
        output = "list",
        doc_info = TRUE,
        app = "public_goods_simple"), "No files to process"
    )
  })
  
  # List variables
  testthat::test_that("Codebook - vector variable in settings and constants", {
    warning <- testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z3"),
        fsource = "models",
        output = "list",  
        doc_info = FALSE)
      , "code that is too complex for this function" # read csv problem
    )  
    
    # Since R only saves the same type in a vector
    testthat::expect_identical(cbook$settings$Something,
                               list(1, 3, "test", 3, 1))
    
    testthat::expect_identical(
      cbook$rankaversion$Constants$Something,list(1, 3, "test", 3, 1))
    
    # Otherwise
    testthat::expect_identical(cbook$settings$StartToken,
                               list(9.0, 14.0, 17.0, 23.0, 38.0))
    
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
    
    testthat::expect_identical(
      cbook$rankaversion$Constants$TestToken,
      unlist(cbook$settings$StartToken) * 5.0
    )

    
    test11 <- all(cbook$rankaversion$Player$Acc_DistributionChange$value ==
                    c("Ich hätte mich dennoch für Verfahren > xyz < entschieden.",
                      "Ich hätte mich für das alternative Verfahren > aaa <  entschieden.",
                      "Ich hätte mich für das alternative Verfahren > bbb < entschieden."))
    
    test12 <- all(cbook$rankaversion$Player$acceptance$choices$value ==
                    c("xyz", "aaa", "Wrong, trick answer", "Correct trick answer"))

    testthat::expect_true(all(test4))
    testthat::expect_true(all(test5))
    testthat::expect_true(all(test6))
    testthat::expect_true(all(test7))
    testthat::expect_true(all(test8))
    testthat::expect_true(all(test9))
    testthat::expect_true(test11)
    testthat::expect_true(test12)
    
  })
  
  testthat::test_that("Codebook - check sublists with different types", {
    # Other sublist tests are in brackets tests 
    
    testthat::expect_warning(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_z6"),
        output_dir = NULL,
        fsource = "models",
        splitvarname = TRUE,
        output = "list")
    )
    
    # Currently, numbers are still in strings if one of the elements is a string element
    
    # Check if original numbers are numbers
    testthat::expect_true(cbook$rankaversion$Constants$choices[[1]][[1]] == 8)
    testthat::expect_true(cbook$rankaversion$Constants$choices[[2]][[1]] == 80)
    testthat::expect_true(cbook$rankaversion$Constants$choices[[3]][[1]] == "8")
    testthat::expect_true(cbook$rankaversion$Constants$choices[[1]][[6]] == "replacingsettingsworked")
    
    testthat::expect_true(cbook$rankaversion$Constants$choices[[2]][[6]] == "settings.shouldnotwork")
    
  })

  # Path
  testthat::test_that("Codebook (e) - path does not exist", {
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "nopath"),
        fsource = "init",
        output = "list",
        doc_info = TRUE,
        app = "public_goods_simple"),
      "does not exist")
  })
  
  testthat::test_that("Codebook (e) - path is NULL", {
    
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
        path = testthat::test_path("testdata", "ocode_new"),
        fsource = "init",
        output_dir  = "../",
        output = "both",
        output_open = TRUE,
        doc_info = TRUE), "Please don't use relative paths in output_dir")
    
    # Relative path
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_dir  = "..",
        output = "both",
        output_open = TRUE,
        doc_info = TRUE), "Please don't use relative paths in output_dir")
    
    # Relative path
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_dir  = ".",
        output = "both",
        output_open = TRUE,
        doc_info = TRUE), "Please don't use relative paths in output_dir")
    
    # Relative path
    testthat::expect_error(
      cbook <- codebook(
        path = testthat::test_path("testdata", "ocode_new"),
        output_dir  = "./",
        output = "both",
        output_open = TRUE,
        doc_info = TRUE), "Please don't use relative paths in output_dir")
    
  })
  
  # Sort
  testthat::test_that("Codebook - new oTree, correct sort", {
    
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
  
  testthat::test_that("Codebook (w) - new oTree, too many sort 2", {
    
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
  
  testthat::test_that("Codebook (w) - new oTree, false sort", {
    
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
  
  testthat::test_that("Codebook (w) - too many sort", {
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
        output_format = "word_document",
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
      length(cbook$bargaining$Player$level2$choices) == 3L)  # ACHTUNG. Bei einlistigen Sachen funzt was nicht
    
    
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
      x = cbook$survey$Player$crt_widget$label)   # TODO removal of three """
    
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
