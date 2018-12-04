context("aoristic fraction method works")


test_that("Utilities are sensible", {

  # We can use the uniform circular distribution.
  expect_equal(dcunif(0, 2*pi - 1, 1), 1/2)

  # We can calculate the mean direction
  expect_error(meanDirAoristic(generateAoristicData(n = 10)), NA)

})


test_that("Aoristic Functions work", {


  dat <- generateAoristicData(n = 10)
  expect_is(dat, "data.frame")


  fun <- getCircAoristicFunction(dat)
  expect_is(fun, "function")
  expect_gte(fun(3), 0)


  aodf <- aoristic_df(1:10, 11:20)
  expect_is(aodf, "aoristic_df")


})
