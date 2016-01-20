theme_grey_new <-  function () {
  theme(
    text = element_text(family="mono"),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(family="sans",vjust=-0.25),
    axis.title.y = element_text(family="sans"),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "#e5e5e5"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#efefef", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )
}

theme_plain <-  function () {
  theme(
    text = element_text(family="sans", size = 13),
    plot.title = element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = element_text(margin=margin(10,0,0,0)),
    axis.title.y = element_text(margin=margin(0,15,0,0)),
    legend.background = element_rect(fill = "white"),
    legend.position = "top",
    legend.margin=unit(0.1, "cm"),
    legend.key = element_rect(fill = "#ffffff", colour=NA),
    legend.key.width = unit(.5, "cm"),
    plot.margin = unit(c(.1, .6, .3, 1), "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#f5f5f5", colour = NA),
    strip.background = element_rect(fill = "#444444", colour = NA),
    strip.text = element_text(face="bold", colour = "white")
  )
}
