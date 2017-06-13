#' Generate an overlay for ggplot2 plots from a PNG file
#'
#' @param png PNG file
#' @param x_sel relative x-viewwindow for PNG, default `c(0,1)`
#' @param y_sel relative y-viewwindow for PNG, default `c(0,1)`
#' @param x_range x-range on scale of ggplot to show / stretch the PNG
#' @param y_range y-range on scale of ggplot to show / stretch the PNG
#'
#' @export
png_overlay <- function(
  png = NULL,
  x_sel = c(0, 1),
  y_sel = c(0, 1),
  x_range = c(0, 24),
  y_range = c(0, 100)
) {
  if(is.null(png)) {
    stop("No PNG file specified!")
  }
  img <- png::readPNG(png)
  y_len <- length(img[,1,1])
  x_len <- length(img[1,,1])
  g <- grid::rasterGrob(img[
    round(y_len * (1-y_sel[2])):round(y_len*(1-y_sel[1])),
    round(x_len * x_sel[1]):round(x_len*x_sel[2]),
    1:3],
    interpolate=TRUE, width = grid::unit(1,"npc"), height = grid::unit(1,"npc"))
  ggplot2::annotation_custom(g, x_range[1], x_range[2], y_range[1], y_range[2])
}
