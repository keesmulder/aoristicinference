
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

# create an aoristic von Mises log-likelihood function using the mean direction
# estimate for mu.
aoristicKpVMLL <- function(d, muhat) {
  # isAoristic <- !identical(d$t_start, d$t_end)

  function(kp) {
    suppressWarnings(
      sum(
        log(vecVMCDF(ths = d$t_end, froms = d$t_start, mu = muhat, kp = kp)) -
          log( (d$t_end - d$t_start) %% (2*pi))
      )
    )
  }
}


#' Get maximum likelihood estimates for the von Mises distribution based on
#' aoristic data.
#'
#' @param d Data frame with columns \code{t_start} and \code{t_end}, which are
#'   the start and end times.
#' @param kp_max Maximum value of \code{kp} to search for.
#' @param ... Additional arguments for the \code{optimize}.
#'
#' @return Numeric vector of length two; estimates for mean direction \code{mu}
#'   and concentration parameter \code{kp}, respectively.
#' @export
#'
#' @examples
#' dat <- generateAoristicData(n = 200)
#' aoristic_vm_mle(dat)
aoristic_vm_mle <- function(d, kp_max = 100, ...) {

  mu_hat <- meanDirAoristic(d)

  this_kp_ll <- aoristicKpVMLL(d, mu_hat)

  kp_hat <- optimize(this_kp_ll,
                     c(0, kp_max),
                     maximum = TRUE,
                     ...)$`maximum`

  c(mu = mu_hat, kp = kp_hat)
}

