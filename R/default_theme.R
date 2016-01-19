theme_plain1 <-  function () {
  theme(
    text = element_text(family="mono"),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust=-0.25),
    axis.title.y = element_text(family="sans"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.14, 0.80),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )
}

theme_plain <-  function () {
  theme(
    text = element_text(family="mono", size = 13),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust = -1.25, size = 13),
    axis.title.y = element_text(family="sans", vjust = +2, size = 13),
    legend.background = element_rect(fill = "white"),
    legend.position = "bottom",
    #legend.position = c(0.14, 0.80),
    plot.margin = unit(c(.6, .6, 1.3, 1), "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f5f5f5", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )
}
