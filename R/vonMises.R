
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


# create an aoristic von Mises log-likelihood function.
aoristicVMLL <- function(d) {
  # isAoristic <- !identical(d$t_start, d$t_end)
  function(param) {
    suppressWarnings(
      sum(
        log(vecVMCDF(ths = d$t_end, froms = d$t_start, mu = param[1], kp = param[2])) -
          log( (d$t_end - d$t_start) %% (2*pi))
      )
    )
  }
}


#' Get maximum likelihood estimates for the von Mises distribution based on aoristic data.
#'
#' @param d Data frame with columns `t_start` and `t_end`, which are the start and end times.
#'
#' @return Numeric vector of length two; estimates for mean direction `mu` and concentration parameter `kp`, respectively.
#' @export
#'
#' @examples
#' dat <- generateAoristicData(n = 30)
#' aoristicVMMLE(dat)
aoristicVM_mle <- function(d, startingValues = c(0, 1)) {
  aVMLL <- aoristicVMLL(d)
  VMMLE <- optim(startingValues,
                 aVMLL,
                 control = list(fnscale = -1),
                 lower = c(-(2*pi), 0), upper = c(4*pi, Inf),
                 method = "L-BFGS-B")$par
  VMMLE[1] <- VMMLE[1] %% (2*pi)

  names(VMMLE) <- c("mu", "kp")

  VMMLE
}
