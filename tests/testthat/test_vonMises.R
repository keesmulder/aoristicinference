
context("Von Mises functions")


test_that("Aoristic Maximum Likelihood Estimates for fully aoristic", {

  # By default, mean = 1, kp = 10.
  set.seed(10)
  dat <- generateAoristicData(n = 100)

  expect_is(vecVMCDF(dat$t_end, froms = dat$t_start, 3, 10), "numeric")

  # The von mises cdf works
  expect_true(vecVMCDF(1, 3, 3, 10) - integrate(dvm, 1, 3, mu = 3, kp = 10)$value < .01)

  # Likelihood functions
  expect_is(aoristicVML(dat)(c(1, 10)), "numeric")
  expect_is(aoristicVMLL(dat)(c(1, 10)), "numeric")

  expect_is(aoristicKpVMLL(dat, 3)(3), "numeric")
  expect_length(aoristicKpVMLL(dat, 3)(3), 1)


  # Maximum likelihood front-end
  ao_mle <- aoristic_vm_mle(dat)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)


  # Changing tolerance
  ao_mle <- aoristic_vm_mle(dat, kp_max = 5, tol = .001)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)

})



test_that("Aoristic Maximum Likelihood Estimates for partial aoristic", {

  # By default, mean = 1, kp = 10.
  set.seed(10)
  dat <- generateAoristicData(n = 100, aoristicProportion = .5)

  expect_is(vecVMCDF(dat$t_end, froms = dat$t_start, 3, 10), "numeric")

  # The von mises cdf works
  expect_true(vecVMCDF(1, 3, 3, 10) - integrate(dvm, 1, 3, mu = 3, kp = 10)$value < .01)

  # Likelihood functions
  expect_is(aoristicVML(dat)(c(1, 10)), "numeric")
  expect_is(aoristicVMLL(dat)(c(1, 10)), "numeric")

  expect_is(aoristicKpVMLL(dat, 3)(3), "numeric")
  expect_length(aoristicKpVMLL(dat, 3)(3), 1)


  # Maximum likelihood front-end
  ao_mle <- aoristic_vm_mle(dat)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)


  # Changing tolerance
  ao_mle <- aoristic_vm_mle(dat, kp_max = 5, tol = .001)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)

})



test_that("Aoristic Maximum Likelihood Estimates for fully observed", {

  # By default, mean = 1, kp = 10.
  set.seed(10)
  dat <- generateAoristicData(n = 100, aoristicProportion = 0)

  # Likelihood functions
  expect_is(aoristicVML(dat)(c(1, 10)), "numeric")
  expect_is(aoristicVMLL(dat)(c(1, 10)), "numeric")

  expect_is(aoristicKpVMLL(dat, 3)(3), "numeric")
  expect_length(aoristicKpVMLL(dat, 3)(3), 1)


  # Maximum likelihood front-end
  ao_mle <- aoristic_vm_mle(dat)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)


  # Changing tolerance
  ao_mle <- aoristic_vm_mle(dat, kp_max = 5, tol = .001)
  expect_is(ao_mle, "numeric")
  expect_length(ao_mle, 2)

})
