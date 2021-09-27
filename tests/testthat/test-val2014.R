test_that("the API is working", {
  expect_silent(val2014())
})

test_that("the returned object inherits from val2014", {
  val = val2014$new()
  expect_true(inherits(val, "val2014"))
})

test_that("wrong county name is rejected", {
  val = val2014()
  expect_error(val$county("Sverige"))
})

test_that("wrong type is rejected", {
  val = val2014()
  expect_error(val$county(123))
})

test_that("first county returns correct results", {
  val = val2014()
  expect_equal(round(unname(val$county("Blekinge")), 2),
               c(19.21, 5.61, 3.86, 3.37, 36.80, 4.60, 4.81, 18.37, 1.65, 0.76))
})

test_that("middle county returns correct results", {
  val = val2014()
  expect_equal(round(unname(val$county("Skane")), 2),
               c(24.48, 4.70, 5.61, 3.35, 28.00, 4.37, 6.78, 17.71, 2.97, 1.04))
})

test_that("last county returns correct results", {
  val = val2014()
  expect_equal(round(unname(val$county("Ostergotland")), 2),
               c(22.02, 5.92, 5.04, 5.01, 32.22, 4.76, 6.52, 14.24, 2.22, 0.99))
})
