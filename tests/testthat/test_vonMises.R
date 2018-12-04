
context("Von Mises functions")


test_that("Aoristic Maximum Likelihood Estimates", {

  # By default, mean = 1, kp = 10.
  set.seed(10)
  dat <- generateAoristicData(n = 100)

  expect_is(vecVMCDF(dat$t_end, froms = dat$t_start, 3, 10), "numeric")

  # The von mises cdf works
  expect_true(vecVMCDF(1, 3, 3, 10) - integrate(dvm, 1, 3, mu = 3, kp = 10)$value < .01)

  # Get MLE's
  ao_mle <- aoristic_vm_mle(dat)
  expect_is(ao_mle, "numeric")

  # Likelihood function
  expect_is(aoristicVML(dat)(ao_mle), "numeric")
  expect_is(aoristicVMLL(dat)(ao_mle), "numeric")




})

