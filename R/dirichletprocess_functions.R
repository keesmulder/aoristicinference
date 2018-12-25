
dpm_mean_direction_sample <- function(dpobj, xpts = 100, fpts = 100) {

  x_grid <- seq(0, 2*pi, length.out = xpts)

  its <- length(dpobj$alphaChain)
  inds <- round(seq(its/2, 2, length.out = fpts))

  posteriorFit <- sapply(inds, function(i) dirichletprocess::PosteriorFunction(dpobj, i)(x_grid))

  # Riemann sum approximation
  apply(posteriorFit, 2, function(x) sum(x * x_grid)) * (2*pi/xpts)

}
