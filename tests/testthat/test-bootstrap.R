context("Aoristic Von Mises bootstrap")

test_that("Aoristic Bootstrap Works", {

  dat <- generateAoristicData()

  aoboot <- aoristic_vm_bootstrap(data = dat, R = 3, tol = .1)

  expect_is(aoboot, "aovmboot")

  # Bootstrap finds original results.
  expect_true(abs(aoboot$tab[, "original"] - aoristic_vm_mle(dat))[1] < .1)
  expect_true(abs(aoboot$tab[, "original"] - aoristic_vm_mle(dat))[2] < .1)



})
