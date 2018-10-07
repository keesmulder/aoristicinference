
# Function to generate uniform distribution pdf values that have a range on the circle.
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

