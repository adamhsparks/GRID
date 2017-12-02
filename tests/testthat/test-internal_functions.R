
# Check that get_inventory functions properly ----------------------------------
context(".check_year()")
test_that(".check_year() defaults to current year if none specified", {
  year_list <- NULL
  year_list <- .check_year(year_list)
  expect_equal(year_list, format(Sys.Date(), "%Y"))
})

context(".validate_dsn()")
test_that(".validate_dsn defaults to user's `home` dir if none specified", {
  dsn <- NULL
  dsn <- .validate_dsn(dsn)
  expect_equal(dsn, path.expand("~"))
})

context(".validate_cores()")
test_that(".validate_cores() sets to 1 if NULL", {
  cores <- NULL
  cores <- .validate_cores(cores)
  expect_equal(cores, 1)
})

context(".check_vars()")
test_that(".check_vars sets vars to TEMP if NULL", {
  vars <- NULL
  vars <- .check_vars(vars)
  expect_equal(vars, "TEMP")
})

context(".check_vars()")
test_that(".check_vars sets vars errors if wrong value is entered", {
  vars <- c("TEMP", "MAX", "MIN", "RH", "PRCP")
  expect_error(.check_vars(vars))
})

context(".check_bz2()")
test_that(".check_bz2 errors if no list of files is given", {
  file_list <- NULL
  expect_error(.check_bz2(file_list))
})
