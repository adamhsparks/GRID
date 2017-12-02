
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
  skip_on_cran()
  cores <- NULL
  cores <- .validate_cores(cores)
  expect_equal(cores, 1)
})
