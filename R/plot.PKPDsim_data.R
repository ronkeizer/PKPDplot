#' Generic plotting function for PKPDsim objects
#'
#' @param data PKPDsim-simulated data
#' @param only_obs Only plot observation compartment
#' @param labels list with `x` and `y` labels
#' @param target vector of target values (dependent variable), will be shown as vertical lines
#' @param target_as_ribbon show target (only when vector of 2) as ribbon (TRUE), or just as two lines (FALSE)
#' @param ... rest
#' @export
plot.PKPDsim_data <- function(
  data,
  only_obs = TRUE,
  show_single = list(
    obs = TRUE,
    spaghetti = TRUE,
    ci = FALSE,
    median = FALSE,
    regimen = TRUE
  ),
  show_population = list(
    obs = FALSE,
    spaghetti = TRUE,
    ci = FALSE,
    median = TRUE,
    regimen = TRUE
  ),
  show = NULL,
  target = NULL,
  target_as_ribbon = TRUE,
  labels = list(x = "Time (hours)", y = "Concentration (mg/L)"),
  ...) {
    single <- TRUE
    if(length(unique(data$id)) > 1) {
      single <- FALSE
    }
    if(is.null(show)) {
    if(single) {
        show <- show_single
      } else {
        show <- show_population
      }
    }
  regimen <- attr(data, "regimen")
  if(only_obs) {
    data_pl <- data[data$comp == "obs",]
  } else {
    data_pl <- data
  }
  pl <- ggplot()
  if(!is.null(regimen) && show$regimen) {
    dat_reg <- data.frame(cbind(t_start = regimen$dose_times,
                                t_end = regimen$dose_times + regimen$t_inf,
                                dose = regimen$dose_amts))
    pl <- pl + geom_rect(data = dat_reg,
                         aes(xmin = t_start, xmax = t_end,
                             ymin = -Inf, ymax = +Inf),
                         fill = rgb(0.2, 0.2, 0.2, 0.2))
  }
  if(!is.null(show$spaghetti) && show$spaghetti) {
    col <- rgb(0.5, 0.5, 0.5, 0.5)
    if(single) {
      col <- rgb(0.1, 0.1, 0.1)
    }
    pl <- pl + geom_line(data = data_pl,
                         aes(x=t, y=y, group=id),
                         colour = col, size = 1)
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
                size = 1.5, colour=rgb(0.3, 0.3, 0.8, 0.6))
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
