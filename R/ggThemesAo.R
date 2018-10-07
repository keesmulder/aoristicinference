# Two useful ggplot themes.
theme_spartan <- theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(0, "lines"))

theme_distr_polar <- theme_bw() +
  theme(text = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        panel.spacing = unit(0, "lines"))


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