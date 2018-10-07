
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

