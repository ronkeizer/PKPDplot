#' Generic plotting function for PKPDsim objects
#'
#' @param data PKPDsim-simulated data
#' @param only_obs Only plot observation compartment
#' @param labels list with `x` and `y` labels
#' @param target vector of target (dependent variable)
#' @param ... rest
#' @export
plot.PKPDsim_data <- function(
  data,
  only_obs = TRUE,
  show = list(
    obs = FALSE,
    spaghetti = TRUE,
    ci = FALSE,
    median = TRUE
  ),
  target = NULL,
  target_as_ribbon = TRUE,
  labels = list(x = "Time (hours)", y = "Concentration"),
  ...) {
  if(only_obs) {
    data_pl <- data[data$comp == "obs",]
  } else {
    data_pl <- data
  }
  if(length(unique(data_pl$id)) > 1) {
    pl <- ggplot(data_pl, aes(x = t, y = y, group = id))
  } else {
    pl <- ggplot(data_pl, aes(x = t, y = y))
  }
  if(!is.null(show$spaghetti) && show$spaghetti) {
    pl <- pl + geom_line(colour = rgb(0.5, 0.5, 0.5, 0.5))
  }
  if(!is.null(show$ci) && show$ci) {
    ci_data <- data.frame(data_pl %>% group_by(t) %>% summarise(quantile(y, 0.05), quantile(y, 0.95)))
    colnames(ci_data) <- c("t", "lower", "upper")
    pl <- pl +
      geom_ribbon(data = ci_data, aes(x = t, y = NULL, ymin = lower, ymax = upper, group = NULL),
                  colour = 0, fill = rgb(0.3, 0.5, 0.8, 0.3))
  }
  if(!is.null(show$median) && show$median) {
    median_data <- data.frame(data_pl %>% group_by(t) %>% summarise(quantile(y, 0.5)))
    colnames(median_data) <- c("t", "median")
    pl <- pl +
      geom_line(data = median_data, aes(x = t, y = median, group = NULL),
                size = 1.5, colour=rgb(0.3, 0.3, 0.8, 0.9))
  }
  if(!is.null(target)) {
    if(target_as_ribbon) {
      if(length(target) != 2) {
        stop("To plot ribbon target should be vector of length 2.")
      } else {
        target_ribbon <- data.frame(cbind(t = c(0, max(data_pl$t)), ymin = target[1], ymax = target[2]))
        pl <- pl + geom_ribbon(data = target_ribbon, aes(x = t, y = NULL, ymin = ymin, ymax = ymax, group = NULL),
                              colour = 0, fill = rgb(0.3, 0.5, 0.8, 0.35))
      }
    } else {
      pl <- pl + geom_hline(yintercept = target,
                            colour = rgb(0.4, 0, 0), size = 0.5, linetype = 'dotted')
    }
  }
  if(!is.null(labels)) {
    pl <- pl + xlab(labels$x)
    pl <- pl + ylab(labels$y)
  }
  pl <- pl + theme_plain()
  return(pl)
}
