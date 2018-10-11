
#' Circular Uniform Distribution
#'
#' Evaluate the circular uniform distribution.
#'
#' @param x Angle to evaluate.
#' @param from Lower bound of the circular uniform distribution.
#' @param to Upper bound of the circular uniform distribution.
#'
#' @return Numeric; probability of x.
#' @export
#'
#' @examples
#' dcunif(1, 6, 2)
#'
dcunif <- function(x, from = 0, to = 2*pi) {

  # Take the modulus here so that the ranges provided are irrelevant.
  x    <- x    %% (2*pi)
  from <- from %% (2*pi)
  to   <- to   %% (2*pi)

  # Check whether the uniform distribution crosses zero, and if so, ensure that
  # the length is computed correctly and that the correct set is in the uniform
  # distribution.
  ifelse(from > to,
         (x > from | x < to) / (to + 2*pi - from),
         (x > from & x < to) / (to - from))
}





#' Mean direction of aoristic data.
#'
#' Compute the mean direction of an aoristic data frame. It is the mean
#' direction of the midpoints.
#'
#' @param d Aoristic data frame.
#' @param na.rm Logical; If \code{FALSE}, returns NA if there are missing
#'   values.
#'
#' @return The mean direction in radians.
#' @export
#'
#' @examples
#' meanDirAoristic(generateAoristicData())
#'
meanDirAoristic <- function(d, na.rm = TRUE) {
  atan2(sum(sin(d$t_start) + sin(d$t_end), na.rm),
        sum(cos(d$t_start) + cos(d$t_end), na.rm)) %% (2*pi)
}
