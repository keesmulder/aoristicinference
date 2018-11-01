
context("Von Mises functions work")


test_that("Aoristic Maximum Likelihood Estimates", {

  dat <- generateAoristicData(n = 10)
  aoristic_vm_mle(dat)
})

