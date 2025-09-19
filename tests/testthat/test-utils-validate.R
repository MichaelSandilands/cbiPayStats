testthat::test_that("validate_cbi_download errors with wrong input", {
  good_df <- cbi_tbl
  testthat::expect_silent(validate_cbi_download(good_df))

  wrong_ncol_tbl <- good_df[, -1]
  testthat::expect_error(validate_cbi_download(wrong_ncol_tbl),
                         "must have exactly 11 columns")

  wrong_name_tbl <- good_df
  names(wrong_name_tbl)[names(wrong_name_tbl) == "Reporting Period"] <-
    "Wrong Name"
  testthat::expect_error(validate_cbi_download(wrong_name_tbl),
                         "must have the expected column names")

  wrong_class_tbl <- good_df
  wrong_class_tbl$"Reporting Period" <-
    as.character(wrong_class_tbl$"Reporting Period")
  testthat::expect_error(validate_cbi_download(wrong_class_tbl),
                         "must have the expected column classes")

})

testthat::test_that("validate_cbi_clean errors with wrong input", {
  good_df <- cbi_clean_tbl
  testthat::expect_silent(validate_cbi_clean(good_df))

  wrong_ncol_tbl <- good_df[, -1]
  testthat::expect_error(validate_cbi_clean(wrong_ncol_tbl),
                         "must have exactly 11 columns")

  wrong_name_tbl <- good_df
  names(wrong_name_tbl)[names(wrong_name_tbl) == "Reporting Period"] <-
    "Wrong Name"
  testthat::expect_error(validate_cbi_clean(wrong_name_tbl),
                         "must have the expected column names")

  wrong_class_tbl <- good_df
  wrong_class_tbl$"Reporting Period" <-
    as.character(wrong_class_tbl$"Reporting Period")
  testthat::expect_error(validate_cbi_clean(wrong_class_tbl),
                         "must have the expected column classes")

  contains_na <- good_df
  contains_na[1, "Sector"] <- NA
  testthat::expect_error(validate_cbi_clean(contains_na),
                         "Error: The data frame contains NA values.")

})

test_that("validate_characters passes for all character arguments", {
  expect_invisible(validate_characters("a", "b", "c"))
  expect_invisible(validate_characters(c("x", "y"))) # still character
})

test_that("validate_characters errors for non-character arguments", {
  expect_error(validate_characters("a", 1, "c"))
  expect_error(validate_characters(TRUE))
  expect_error(validate_characters(list("a")))
})

test_that("validate_single_string passes for single values", {
  expect_invisible(validate_single_string("a"))
  expect_invisible(validate_single_string(1))
  expect_invisible(validate_single_string(TRUE))
})

test_that("validate_single_string errors for length > 1", {
  expect_error(validate_single_string(c("a", "b")))
  expect_error(validate_single_string(c(1, 2)))
  expect_error(validate_single_string(logical(0)))
})

