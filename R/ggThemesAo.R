# Two useful ggplot themes.
theme_spartan <- ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(colour="black",
                                                          fill="white"),
                 panel.spacing = ggplot2::unit(0, "lines"))

theme_distr_polar <- ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(size = 12),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_line(),
                 panel.border = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(colour="black",
                                                          fill="white"),
                 panel.spacing = ggplot2::unit(0, "lines"))


scale_x_circular <- function(units = "degrees", nticks = 8, ...) {
  brks <- 2*pi*(0:nticks)/nticks
  ggplot2::scale_x_continuous(breaks = brks,
                              labels = circular::conversion.circular(
                                circular::circular(brks),
                                units = units
                              ),
                              ...)
}

scale_y_circular <- function(units = "degrees", nticks = 8, ...) {
  brks <- 2*pi*(0:nticks)/nticks
  ggplot2::scale_y_continuous(breaks = brks,
                              labels = circular::conversion.circular(
                                circular::circular(brks),
                                units = units
                              ),
                              ...)
}
