
# Plot circular densities with ggplot.
ggCircDens <- function(FUN = function(x) dvonmises(x, mu = pi, kappa = 2),
                       ggtheme = theme_distr_polar, r = 1, nxticks = 8,
                       res = 200, axisLabels = "hours", plotFun = TRUE) {

  # Places to put breaks on the axis labels.
  brks <- 2*pi*(0:(nxticks - 1))/nxticks

  if (axisLabels == "radians") {
    scaleGeom <- scale_x_continuous(limits = c(0, 2*pi), breaks = round(brks, 2))
  } else if (axisLabels == "hours") {
    scaleGeom <- scale_x_continuous(limits = c(0, 2*pi),
                                    breaks = brks,
                                    labels = conversion.circular(circular(brks), units = "hours"))
  } else {
    stop('Wrong axislabels.')
  }

  if (plotFun) funGeom <- stat_function(fun = FUN, n = res) else funGeom <- NULL

  # Properties of coord_polar that depend on whether we display a clock or radians.
  c_pdir   <- ifelse(axisLabels == "hours", 1, -1)
  c_pstart <- ifelse(axisLabels == "hours", 0, 3*pi/2)

  ggplot(data.frame(x = c(0, 2*pi)), aes(x)) + ylim(-r, NA) +
    funGeom +
    geom_ribbon(aes(ymin = -r, ymax = 0), fill = "gray95") +
    theme_distr_polar +
    coord_polar(start = c_pstart, direction = c_pdir) +
    scaleGeom
}



scale_x_circular <- function(units = "degrees", nticks = 8, ...) {
  brks <- 2*pi*(0:nticks)/nticks
  scale_x_continuous(breaks = brks,
                     labels = conversion.circular(circular(brks),
                                                  units = units),
                     ...)
}

scale_y_circular <- function(units = "degrees", nticks = 8, ...) {
  brks <- 2*pi*(0:nticks)/nticks
  scale_y_continuous(breaks = brks,
                     labels = conversion.circular(circular(brks),
                                                  units = units),
                     ...)
}
