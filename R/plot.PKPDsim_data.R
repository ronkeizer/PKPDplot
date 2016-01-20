#' Generic plotting function for PKPDsim objects
#'
#' @param data PKPDsim-simulated data
#' @param only_obs Only plot observation compartment
#' @param obs_data Also add observation (e.g. TDM) data
#' @param labels list with `x` and `y` labels
#' @param legend list of labels that override defaults, e.g. list("individual" = "ind.pred")
#' @param target vector of target values (dependent variable), will be shown as vertical lines
#' @param target_as_ribbon show target (only when vector of 2) as ribbon (TRUE), or just as two lines (FALSE)
#' @param regimen regimen specification from PKPDsim::new_regimen()
#' @param show_single definition for plots of single patients
#' @param show_population definition for plots of populations
#' @param show definition of what to show in plot, overrides `show_single` and `show_population`. NULL by default
#' @param return_svg return the svg plot definition instead of ggplot2 object. FALSE by default
#' @param ... rest
#' @export
plot.PKPDsim_data <- function(
  data,
  only_obs = TRUE,
  obs_data = NULL,
  regimen = NULL,
  lines = NULL,
  show_single = list(
    obs = TRUE,
    spaghetti = TRUE,
    ci = FALSE,
    median = FALSE,
    regimen = TRUE
  ),
  xlim = NULL,
  ylim = NULL,
  width = 6,
  height = 4,
  legend = NULL,
  show_series = NULL,
  show_population = list(
    obs = FALSE,
    spaghetti = TRUE,
    ci = FALSE,
    median = TRUE,
    regimen = TRUE
  ),
  show = NULL,
  log_y = FALSE,
  target = NULL,
  target_as_ribbon = TRUE,
  labels = list(x = "Time (hours)", y = "Concentration (mg/L)"),
  return_svg = FALSE,
  ...) {
    if(!("id") %in% tolower(colnames(data))) {
      data$id <- 1
    }
    if(!("comp") %in% tolower(colnames(data))) {
      data$comp <- "obs"
    }
    data$type <- "Concentration"
    single <- TRUE
    if(length(unique(data$id)) > 1) {
      single <- FALSE
    }
    if(is.null(show)) {
    if(single) {
        show <- show_single
        if("ipred" %in% tolower(colnames(data))) {
          tmp <- data
          tmp$y <- data$ipred
          tmp$type <- "individual"
          if(!is.null(data$y) && length(data$y) > 0) {
            data <- data.frame(rbind(data, tmp))
          } else {
            data <- tmp
          }
          data <- data[data$type != "Concentration",]
        }
        if("pred" %in% tolower(colnames(data))) {
          tmp <- data
          tmp$y <- data$pred
          tmp$type <- "population"
          if(!is.null(data$y) && length(data$y) > 0) {
            data <- data.frame(rbind(data, tmp))
          } else {
            data <- tmp
          }
          data <- data[data$type != "Concentration",]
        }
    } else {
        show <- show_population
      }
    }
    if(!is.null(attr(data, "regimen")) && is.null(regimen)) {
      regimen <- attr(data, "regimen")
    }
  if(only_obs) {
    data_pl <- data[data$comp == "obs",]
  } else {
    data_pl <- data
  }
  if(!is.null(lines)) {
    for (i in names(lines)) {
      templ <- data_pl[rep(seq_len(nrow(data_pl)), each=length(lines[[i]]$t)),]
      tmp <- lines[[i]]
      for(j in names(tmp)) {
        templ[[j]] <- tmp[[j]]
      }
      templ$type <- i
      data_pl <- data.frame(rbind(data_pl, templ))
    }
  }
  if(!is.null(legend)) {
    for(key in names(legend)) {
      data_pl[data_pl$type == as.character(key), ]$type <- as.character(legend[[key]])
    }
  }
  if(!is.null(show_series)) {
    data_pl <- data_pl[data_pl$type %in% show_series,]
  }
  ## /end data formatting
  ## start plotting
  pl <- ggplot()
  if(!is.null(regimen) && show$regimen) {
    dat_reg <- data.frame(cbind(t_start = regimen$dose_times,
                                t_end = regimen$dose_times + regimen$t_inf,
                                dose = regimen$dose_amts))
    if(log_y) {
      miny <- min(data_pl$y)
      maxy <- max(data_pl$y)
    } else {
      miny <- -Inf
      maxy <- +Inf
    }
      pl <- pl + geom_rect(data = dat_reg,
                           aes(xmin = t_start, xmax = t_end),
                           ymin = miny, ymax = maxy,
                           fill = rgb(0.2, 0.2, 0.2, 0.2))
  }
  if(!is.null(target)) {
    if(target_as_ribbon) {
      if(length(target) != 2) {
        stop("To plot ribbon target should be vector of length 2.")
      } else {
        target_ribbon <- data.frame(cbind(t = c(0, max(data_pl$t)), ymin = target[1], ymax = target[2]))
        pl <- pl + geom_ribbon(data = target_ribbon, aes(x = t, y = NULL, ymin = ymin, ymax = ymax, group = NULL,colour = NULL),
                              fill = rgb(0.3, 0.4, 0.6, 0.25))
      }
    } else {
      pl <- pl + geom_hline(yintercept = target,
                            colour = rgb(0.4, 0, 0, 0.5), size = 0.5, linetype = 'dotted')
    }
  }
  if(!is.null(show$spaghetti) && show$spaghetti) {
    col <- rgb(0.5, 0.5, 0.5, 0.5)
    if(single) {
      col <- rgb(0.1, 0.1, 0.1)
    }
    if(single) {
      pl <- pl + geom_line(data = data_pl,
                           aes(x=t, y=y, group = as.factor(type), colour = as.factor(type)),
                           size = 1)
    } else {
      pl <- pl + geom_line(data = data_pl,
                           aes(x=t, y=y, group = as.factor(id)),
                           colour = rgb(0.5, 0.5, 0.5, 0.5),
                           size = 1)
    }
  }
  pl <- pl + scale_colour_discrete(guide = guide_legend(title = NULL))
  if(!is.null(show$ci) && show$ci) {
    ci_data <- data.frame(data_pl %>% group_by(t) %>% summarise(quantile(y, 0.05), quantile(y, 0.95)))
    colnames(ci_data) <- c("t", "lower", "upper")
    pl <- pl +
      geom_ribbon(data = ci_data,
                  aes(x = t, y = NULL, ymin = lower, ymax = upper, colour=NULL, group=NULL),
                  fill = rgb(0.8, 0.5, 0.8, 0.3))
  }
  if(!is.null(show$median) && show$median) {
    median_data <- data.frame(data_pl %>% group_by(t) %>% summarise(quantile(y, 0.5)))
    colnames(median_data) <- c("t", "median")
    pl <- pl +
      geom_line(data = median_data, aes(x = t, y = median, group = NULL, colour = type),
                size = 1.5, colour=rgb(0.15, 0.2, 0.6, 0.6))
  }
  if(!is.null(obs_data)) {
    pl <- pl + geom_point(data = obs_data, aes(x = t, y = y), size = 2)
  }
  if(!is.null(labels)) {
    pl <- pl + xlab(labels$x)
    pl <- pl + ylab(labels$y)
  }
  pl <- pl + theme_plain()
  if(log_y) {
    pl <- pl + scale_y_log10()
  }
  pl <- pl + coord_cartesian(xlim = xlim, ylim = ylim)
  if(return_svg) {
    filename <- paste0(tempfile(pattern="plot_"), ".svg")
    ggsave(filename = filename, plot = pl, width=width, height=height)
    pl_contents <- readChar(filename, file.info(filename)$size)
    unlink(filename)
    return(pl_contents)
  } else {
    return(pl)
  }
}
