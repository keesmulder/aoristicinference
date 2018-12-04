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
                                 trueDistGen = function(n) as.numeric(suppressWarnings(circular::rvonmises(n, 1, 10))),
                                 intervalSampler = function(n) runif(n, 0, 2),
                                 LBSampler = intervalSampler,
                                 UBSampler = intervalSampler,
                                 aoristicProportion = 1) {

  t_actual <- trueDistGen(n)

  if (aoristicProportion > 1 || aoristicProportion < 0) stop("Invalid aoristic proportion.")
  n_aor <- round(n * aoristicProportion)


  rep()
  is_aoristic <-
    sample(1:n), n, replace = TRUE, aoristicProportion)

  t_start  <- (t_actual - LBSampler(n)) %% (2*pi)
  t_end    <- (t_actual + UBSampler(n)) %% (2*pi)
  data.frame(t_start  = t_start,
             t_end    = t_end,
             t_actual = t_actual)
}

#' Create an aoristic data frame.
#'
#' @param start Start of the interval.
#' @param end End of the interval.
#' @param rest Data frame; All other columns to add to the aoristic data frame.
#'
#' @return A data frame of class \code{"aoristic_df"} with at least columns
#'   \code{t_start} and \code{t_end},
#' @export
#'
aoristic_df <- function(start, end, rest = NULL) {
  aodf <- data.frame(t_start = start, t_end = end)
  if (!is.null(rest)) aodf <- cbind(aodf, rest)
  class(aodf) <- c("aoristic_df", class(aodf))
  aodf
}


#' Aoristic Fraction function
#'
#' Create a function for circular interval censored analysis. The created
#' function takes a vector `x` and returns the height of the aoristic fraction
#' given that function.
#'
#' @param df A `data.frame` or matrix of which the first column represents the
#'   lower bound and the second column represents the upper bound of the
#'   observed intervals.
#'
#' @return The aoristic function.
#' @export
#'
#' @examples
#' df <- generateAoristicData(n = 5)
#' myfun <- getCircAoristicFunction(df)
#' ggplot(data.frame(x = c(0, 2*pi)), aes(x)) +
#'   geom_hline(yintercept = 0, color = "gray") +
#'   stat_function(fun = myfun, n = 1000) +
#'   coord_polar() +
#'   ylim(-1, NA) +
#'   theme_void()
getCircAoristicFunction <- function(df) {
  # The aoristic function is a mean of separate circular uniform distributions.
  Vectorize(function(x) mean(dcunif(x,
                                    df$t_start,
                                    df$t_end)))
}

