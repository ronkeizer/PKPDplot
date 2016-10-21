#' Create a customized PKPDsim plot theme
#'
#' @param update list containing the plot elements to be updated. Run `new_plot_theme()` with no arguments to show an overview of available plot elements.
#'
#' @return A list with vpc theme specifiers
#' @export
new_plot_theme <- function (update = NULL) {
  tmp <- structure(list(

    spaghetti_color = rgb(0.5, 0.5, 0.5, 0.5),
    dose_fill       = rgb(0.2, 0.2, 0.2, 0.2),
    target_fill     = rgb(0.3, 0.4, 0.6, 0.15),
    target_color    = rgb(0.4, 0, 0, 0.5),
    ci_fill         = rgb(0.8, 0.5, 0.8, 0.2),
    median_color    = rgb(0.15, 0.2, 0.6, 0.6),
    obs_size        = 2,
    obs_color       = rgb(0, 0, 0, 0.5)

  ), class = "plot_theme")
  n <- names(tmp)
  if(is.null(update)) {
    return(tmp)
  }
  if(!is.null(update) & length(names(update)) > 0) {
    for(i in seq(names(update))) {
      if(names(update)[i] %in% n) {
        tmp[[names(update)[i]]] <- update[[names(update)[i]]]
      } else {
        warning(paste0("`", names(update)[i],"` is not recognized as a plot element, ignoring."))
      }
    }
  }
  tmp
}
