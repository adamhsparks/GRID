
context(".validate_year()")
test_that(".validate_year() defaults to current year if none specified", {
  year_list <- NULL
  year_list <- .validate_year(year_list)
  expect_equal(year_list, as.numeric(format(Sys.Date(), "%Y")))
})

context(".validate_vars()")
test_that(".validate_vars sets vars to TEMP if NULL", {
  vars <- NULL
  vars <- .validate_vars(vars)
  expect_equal(vars, "TEMP")
})

context(".validate_vars()")
test_that(".validate_vars sets vars errors if wrong value is entered", {
  vars <- c("TEMP", "MAX", "MIN", "RH", "PRCP")
  expect_error(.validate_vars(vars))
})

context(".validate_bz2()")
test_that(".validate_bz2 errors if no list of files is given", {
  file_list <- NULL
  expect_error(.validate_bz2(file_list))
})

context(".validate_max_missing()")
test_that(".validate_max_missing errors with invalid values", {
  missing <- .5
  expect_error(.validate_max_missing(missing))

  missing <- NA
  expect_error(.validate_max_missing(missing))
})

context(".validate_resolution()")
test_that(".validate_resolution errors if wrong value is entered", {

  resolution <- NULL
  agg <- .validate_resolution(resolution)
  expect_equal(agg, 12)

  resolution <- 9
  expect_error(.validate_resolution(resolution))

  resolution <- 1
  agg <- .validate_resolution(resolution)
  expect_equal(agg, 12)

  resolution <- .5
  agg <- .validate_resolution(resolution)
  expect_equal(agg, 6)

  resolution <- .25
  agg <- .validate_resolution(resolution)
  expect_equal(agg, 3)
})
