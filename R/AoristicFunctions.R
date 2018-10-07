#' Generate aoristic data
#'
#' This function generates aoristic circular data from a true distribution
#' generation function and two function to generate the lower and upper
#' distances from the true point.
#'
#' @param n The desired sample size.
#' @param trueDistGen A function that takes a single argument n and samples n
#'   random values from the desired true distribution.
#' @param intervalSampler A function that takes a single argument n and samples
#'   n random values that determine how far away the interval bounds are from
#'   the true sampled datapoint on either side. By default the same function is
#'   used on both sides, but this can be overwritten by specifying LBSampler and
#'   UBSampler.
#' @param LBSampler Optional function of the same form as intervalSampler that
#'   specifies the way in which the distance between the lower bound and the
#'   true value is sampled. Should sample positive values which represent the
#'   difference.
#' @param UBSampler  Optional function of the same form as intervalSampler that
#'   specifies the way in which the distance between the upper bound and the
#'   true value is sampled.Should sample positive values which represent the
#'   difference.
#'
#' @return A data frame containing the start, end, and true values.
#' @export
#'
#' @examples
#' generateAoristicData()
#'
generateAoristicData <- function(n = 30,
                                 trueDistGen = function(n) rvmc(n, 0, 3),
                                 intervalSampler = function(n) runif(n, 0, 2),
                                 LBSampler = intervalSampler,
                                 UBSampler = intervalSampler) {
  t_actual <- trueDistGen(n)
  t_start  <- (t_actual - LBSampler(1)) %% (2*pi)
  t_end    <- (t_actual + UBSampler(1)) %% (2*pi)
  data.frame(t_start = t_start,
             t_end = t_end,
             t_actual = t_actual)
}


# Create a function for circular interval censored analysis.
getCircAoristicFunction <- function(d) {
  # The aoristic function is a mean of separate circular uniform distributions.
  Vectorize(function(x) mean(dcunif(x,
                                    d$t_start,
                                    d$t_end)))
}

