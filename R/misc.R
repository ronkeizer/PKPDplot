#' Convert safely to numeric
#'
#' @param x value
#' @export
as.num <- function(x) {
  return(as.numeric(as.character(x)))
}

`%>%` <- dplyr::`%>%`

## hacky way of circumventing erroneous CRAN check:
globalVariables("y")
