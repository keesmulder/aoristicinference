



# Create a function for linear interval censored analysis.
getLinearAoristicFunction <- function(d) {
  Vectorize(function(x) mean(dunif(x, d$t_start, d$t_end)))
}


# Generate a list of uniform distributions, this only works for linear
# functions.
aoristicLinearHist <- function(d, ...) {
  # The aoristic function is a mean of separate uniform distributions.
  aoFun <- getLinearAoristicFunction(d)

  # Plot the curve of the aoristic function
  curve(aoFun(x), ...)
}
