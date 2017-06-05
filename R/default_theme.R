theme_grey_new <-  function () {
  ggplot2::theme(
    text = ggplot2::element_text(family="mono"),
    plot.title = ggplot2::element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = ggplot2::element_text(family="sans",vjust=-0.25),
    axis.title.y = ggplot2::element_text(family="sans"),
    legend.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major = ggplot2::element_line(colour = "#e5e5e5"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#efefef", colour = NA),
    strip.background = ggplot2::element_rect(fill = "#444444", colour = NA),
    strip.text = ggplot2::element_text(face="bold", colour = "white")
  )
}

theme_plain <-  function () {
  ggplot2::theme(
    text = ggplot2::element_text(family="sans", size = 13),
    plot.title = ggplot2::element_text(family="sans", size = 16, vjust = 1.5),
    axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10,0,0,0)),
    axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0,15,0,0)),
    legend.background = ggplot2::element_rect(fill = "white"),
    legend.position = "top",
    legend.spacing = ggplot2::unit(0.1, "cm"),
    legend.key = ggplot2::element_rect(fill = "#ffffff", colour=NA),
    legend.key.width = ggplot2::unit(.5, "cm"),
    plot.margin = ggplot2::unit(c(.1, .6, .3, 1), "cm"),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "#f5f5f5", colour = NA),
    strip.background = ggplot2::element_rect(fill = "#444444", colour = NA),
    strip.text = ggplot2::element_text(face="bold", colour = "white")
  )
}
