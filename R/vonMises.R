
# Auxiliary function to compute the von Mises CDF with a vector of angles and a
# vector of 'from' values. We have to make sure that the two vectors are of equal length.
vecVMCDF <- function(ths, froms, mu, kp) {
  sapply(1:length(ths), function(i) circular::pvonmises(q = ths[i], mu = mu, kappa = kp, from = froms[i]))
}

# create an aoristic von Mises likelihood function.
aoristicVML <- function(d) {
  function(param) {
    suppressWarnings(
      prod(
        vecVMCDF(ths = d$t_end, froms = d$t_start, mu = param[1], kp = param[2]) /
          ((d$t_end - d$t_start) %% (2*pi))
      )
    )
  }
}
