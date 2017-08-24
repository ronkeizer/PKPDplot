#' Expose plot function also as regular function
#'
#' @param ... pass on arguments
#' @export
plot <- function(...) {
  plot.PKPDsim_data(...)
}
