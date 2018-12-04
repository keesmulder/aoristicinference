pvm.mu0 <- function(q, kappa, tol) {
  # This algorithm for computation of the von Mises cumulative distribution was adapted
  # from the internal function `PVonMisesRad` R package `circular`, and used here instead of the front-end
  # `pvonmises` for speed considerations. Original code licensed under GPL-2, by
  # Claudio Agostinelli, Ulric Lund and Harry Southworth.

  flag <- TRUE
  p <- 1
  sum <- 0
  while (flag) {
    term <- (besselI(x = kappa, nu = p, expon.scaled = FALSE) *
               sin(p * q))/p
    sum <- sum + term
    p <- p + 1
    if (abs(term) < tol)
      flag <- FALSE
  }
  return(q/(2 * pi) + sum/(pi * besselI(x = kappa, nu = 0,
                                        expon.scaled = FALSE)))
}


pvmrad <- function(q, from, mu, kappa, tol = 1e-20) {
  # This algorithm for computation of the von Mises cumulative distribution was adapted
  # from the internal function `PVonMisesRad` R package `circular`, and used here instead of the front-end
  # `pvonmises` for speed considerations. Original code licensed under GPL-2, by
  # Claudio Agostinelli, Ulric Lund and Harry Southworth.

  mu <- (mu - from) %% (2 * pi)
  q <- (q - from) %% (2 * pi)
  n <- length(q)

  result <- rep(NA, n)
  if (mu == 0) {
    for (i in 1:n) {
      result[i] <- pvm.mu0(q[i], kappa, tol)
    }
  } else {

    for (i in 1:n) {
      if (q[i] <= mu) {
        upper <- (q[i] - mu)%%(2 * pi)
        if (upper == 0)
          upper <- 2 * pi
        lower <- (-mu)%%(2 * pi)
        result[i] <- pvm.mu0(upper, kappa, tol) - pvm.mu0(lower,
                                                          kappa, tol)
      } else {
        upper <- q[i] - mu
        lower <- mu%%(2 * pi)
        result[i] <- pvm.mu0(upper, kappa, tol) + pvm.mu0(lower,
                                                          kappa, tol)
      }
    }
  }
  return(result)
}


logBesselI <- function(x, nu) {
  x + log(besselI(x, nu, expon.scaled = TRUE))
}

dvm <- function(x, mu = 0, kp = 1, log = FALSE) {
  logp <- kp * cos(x - mu) - log(2 * pi) - logBesselI(kp, 0)

  if (log) {
    return(logp)
  } else {
    return(exp(logp))
  }
}

# Auxiliary function to compute the von Mises CDF with a vector of angles and a
# vector of 'from' values. We have to make sure that the two vectors are of equal length.
vecVMCDF <- function(froms, tos, mu, kp, tol = 1e-10) {
  vapply(1:length(tos), function(i) pvmrad(q = tos[i], mu = mu, kappa = kp, from = froms[i], tol = tol), FUN.VALUE = 0)
}




# create an aoristic von Mises likelihood function.
aoristicVML <- function(d) {

  # Split the observed and aoristic datapoints
  isAoristic <- !(d$t_start == d$t_end)
  daor <- d[isAoristic, , drop = FALSE]
  dobs <- d[!isAoristic, "t_start", drop = FALSE]

  function(param) {
      prod(
        vecVMCDF(froms = daor$t_start, tos = daor$t_end, mu = param[1], kp = param[2]) /
          ((daor$t_end - daor$t_start) %% (2*pi))
      ) * prod(
        dvm(dobs$t_start, mu = param[1], kp = param[2], log = FALSE)
      )
  }
}


# create an aoristic von Mises log-likelihood function.
aoristicVMLL <- function(d) {

  # Split the observed and aoristic datapoints
  isAoristic <- !(d$t_start == d$t_end)
  daor <- d[isAoristic, , drop = FALSE]
  dobs <- d[!isAoristic, "t_start", drop = FALSE]

  function(param) {
    sum(
      log(vecVMCDF(froms = daor$t_start, tos = daor$t_end,  mu = param[1], kp = param[2])) -
        log( (daor$t_end - daor$t_start) %% (2*pi))
    ) + sum(
      dvm(dobs$t_start, mu = param[1], kp = param[2], log = TRUE)
    )
  }
}

# create an aoristic von Mises log-likelihood function using the mean direction
# estimate for mu.
aoristicKpVMLL <- function(d, muhat) {

  # Split the observed and aoristic datapoints
  isAoristic <- !(d$t_start == d$t_end)
  daor <- d[isAoristic, , drop = FALSE]
  dobs <- d[!isAoristic, "t_start", drop = FALSE]

  function(kp) {
    sum(
      log(vecVMCDF(froms = daor$t_start, tos = daor$t_end, mu = muhat, kp = kp)) -
        log( (daor$t_end - daor$t_start) %% (2*pi))
    ) + sum(
      dvm(dobs$t_start, mu = muhat, kp = kp, log = TRUE)
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

