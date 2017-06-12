#' Generic plotting function for PKPDsim objects
#'
#' @param x PKPDsim-simulated x
#' @param only_obs Only plot observation compartment
#' @param obs_x Also add observation (e.g. TDM) x
#' @param labels list with `x` and `y` labels
#' @param legend list of labels that override defaults, e.g. list("individual" = "ind.pred")
#' @param target vector of target values (dependent variable), will be shown as vertical lines
#' @param target_as_ribbon show target (only when vector of 2) as ribbon (TRUE), or just as two lines (FALSE)
#' @param regimen regimen specification from PKPDsim::new_regimen()
#' @param lines show lines?
#' @param show_single definition for plots of single patients
#' @param show_population definition for plots of populations
#' @param ci confidence interval, specified as vector of two values between 0 and 1, default is `c(0.05, 0.95)`.
#' @param theme theme
#' @param show definition of what to show in plot, overrides `show_single` and `show_population`. NULL by default
#' @param scale_colour_values values for colour scale
#' @param scale_linetype_values values for linetype scale
#' @param xlim xlim passed to ggplot
#' @param ylim ylim passed to ggplot
#' @param return_svg return the svg plot definition instead of ggplot2 object. FALSE by default
#' @param width width of plot if saved
#' @param height height of plot if saved
#' @param show_series show only specified types / series
#' @param overlay add a overlay, e.g. from a PNG file created with `png_overlay()`
#' @param log_y log y axis?
#' @param ... rest
#' @export

plot.PKPDsim_data <- function(
  x,
  only_obs = TRUE,
  obs_x = NULL,
  regimen = NULL,
  lines = NULL,
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
  ci = c(0.05, 0.95),
  theme = NULL,
  xlim = NULL,
  ylim = NULL,
  width = 6,
  height = 4,
  legend = NULL,
  show_series = NULL,
  show = NULL,
  log_y = FALSE,
  target = NULL,
  target_as_ribbon = FALSE,
  scale_colour_values = NULL,
  scale_linetype_values = NULL,
  overlay = NULL,
  labels = list(x = "Time (hours)", y = "Concentration (mg/L)"),
  return_svg = FALSE,
  ...) {
    if(!("id") %in% tolower(colnames(x))) {
      x$id <- 1
    }
    if(!("comp") %in% tolower(colnames(x))) {
      x$comp <- "obs"
    }
    if(is.null(ci) || length(ci) != 2 || any(ci < 0) || any(ci > 1)) {
      stop("`ci` argument has to be of vector of length two and specify the CI (between 0 and 1).")
    }
    x$type <- "Concentration"
    single <- TRUE
    if(length(unique(x$id)) > 1) {
      single <- FALSE
    }
    if(is.null(theme)) {
      theme <- new_plot_theme()
    }
    if(is.null(show)) {
    if(single) {
        show <- show_single
        if("ipred" %in% tolower(colnames(x))) {
          tmp <- x
          tmp$y <- x$ipred
          tmp$type <- "individual"
          if(!is.null(x$y) && length(x$y) > 0) {
            x <- data.frame(rbind(x, tmp))
          } else {
            x <- tmp
          }
          x <- x[x$type != "Concentration",]
        }
        if("pred" %in% tolower(colnames(x))) {
          tmp <- x
          tmp$y <- x$pred
          tmp$type <- "population"
          if(!is.null(x$y) && length(x$y) > 0) {
            x <- data.frame(rbind(x, tmp))
          } else {
            x <- tmp
          }
          x <- x[x$type != "Concentration",]
        }
    } else {
        show <- show_population
      }
    }
    if(!is.null(attr(x, "regimen")) && is.null(regimen)) {
      regimen <- attr(x, "regimen")
    }
  if(only_obs) {
    x_pl <- x[x$comp == "obs",]
  } else {
    x_pl <- x
  }
  if(!is.null(lines)) {
    for (i in names(lines)) {
      templ <- x_pl[rep(seq_len(nrow(x_pl)), each=length(lines[[i]]$t)),]
      tmp <- lines[[i]]
      for(j in names(tmp)) {
        templ[[j]] <- tmp[[j]]
      }
      templ$type <- i
      x_pl <- data.frame(rbind(x_pl, templ))
    }
  }
  if(!is.null(legend)) {
    for(key in names(legend)) {
      x_pl[x_pl$type == as.character(key), ]$type <- as.character(legend[[key]])
    }
  }
  if(!is.null(show_series)) {
    x_pl <- x_pl[x_pl$type %in% show_series,]
  }
  tmp_y <- x_pl$y
  if(!is.null(obs_x)) {
    tmp_y <- c(tmp_y, obs_x$y)
  }
  ## /end x formatting
  ## start plotting
  pl <- ggplot2::ggplot()
  if(!is.null(overlay)) {
    pl <- pl + overlay
  }
  if(!is.null(regimen) && show$regimen) {
    t_end <-  regimen$dose_times
    if(any(regimen$type == "infusion")) {
      t_end[!is.na(regimen$t_inf)] <- t_end[!is.na(regimen$t_inf)] + regimen$t_inf[!is.na(regimen$t_inf)]
      t_end[is.na(regimen$t_inf)] <- t_end[is.na(regimen$t_inf)] + 1
    } else {
      t_end <- t_end + 1
    }
    dat_reg <- data.frame(cbind(t_start = regimen$dose_times,
                                t_end = regimen$dose_times + regimen$t_inf,
                                dose = regimen$dose_amts))
      if(log_y) {
        miny <- min(x_pl$y)
        maxy <- max(x_pl$y)
      } else {
        miny <- -Inf
        maxy <- +Inf
      }
      pl <- pl + ggplot2::geom_rect(data = dat_reg,
                           ggplot2::aes_string(xmin = "t_start", xmax = "t_end"),
                           ymin = miny, ymax = maxy,
                           fill = theme$dose_fill)
  }
  if(!is.null(target)) {
    if(target_as_ribbon) {
      if(length(target) != 2) {
        stop("To plot ribbon target should be vector of length 2.")
      } else {
        target_ribbon <- data.frame(cbind(t = c(0, max(x_pl$t)), ymin = target[1], ymax = target[2]))
        pl <- pl + ggplot2::geom_ribbon(data = target_ribbon, ggplot2::aes_string(x = "t", y = NULL, ymin = "ymin", ymax = "ymax", group = NULL, colour = NULL),
                               fill = theme$target_fill, show_guide=FALSE)
      }
    } else {
      pl <- pl + ggplot2::geom_hline(yintercept = target,
                            colour = theme$target_color,
                            size = 0.75,
                            linetype = 'dotted')
    }
  }
  pl <- pl + ggplot2::theme(legend.key = ggplot2::element_blank())
  if(!is.null(show$spaghetti) && show$spaghetti) {
    col <- grDevices::rgb(0.5, 0.5, 0.5, 0.5)
    if(single) {
      col <- grDevices::rgb(0.1, 0.1, 0.1)
    }
    if(single) {
      x_pl$type <- as.factor(x_pl$type)
      pl <- pl + ggplot2::geom_line(
        data = x_pl,
        ggplot2::aes_string(x="t", y="y", group = "type", colour = "type", linetype="type"),
                           size = .75)
      if(is.null(scale_colour_values)) {
        pl <- pl + ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = NULL))
      } else {
        pl <- pl + ggplot2::scale_colour_manual(guide = ggplot2::guide_legend(title = NULL), values = scale_colour_values)
      }
      if(is.null(scale_linetype_values)) {
        scale_linetype_values <- rep("solid", length(unique(x_pl$type)))
      }
      pl <- pl + ggplot2::scale_linetype_manual(guide = ggplot2::guide_legend(title = NULL), values=scale_linetype_values)
    } else {
      x_pl$id <- as.factor(x_pl$id)
      pl <- pl + ggplot2::geom_line(
        data = x_pl,
        ggplot2::aes_string(x="t", y="y", group = "id"),
        colour = theme$spaghetti_color,
        size = .75)
    }
  }
  if(!is.null(show$ci) && show$ci) {
    ci_x <- data.frame(x_pl %>%
               dplyr::group_by(t) %>%
               dplyr::summarise(stats::quantile(y, ci[1]), stats::quantile(y, ci[2])))
    colnames(ci_x) <- c("t", "lower", "upper")
    pl <- pl +
      ggplot2::geom_ribbon(data = ci_x,
                           ggplot2::aes_string(x = "t", y = NULL, ymin = "lower", ymax = "upper", colour = NULL, group=NULL),
                  fill = theme$ci_fill)
  }
  if(!is.null(show$median) && show$median) {
    median_x <- data.frame(x_pl %>%
                             dplyr::group_by(t) %>%
                             dplyr::summarise(stats::quantile(y, 0.5)))
    colnames(median_x) <- c("t", "median")
    pl <- pl +
      ggplot2::geom_line(data = median_x,
                         ggplot2::aes_string(x = "t", y = "median", group = NULL, colour = "type"),
                size = 1.5, colour = theme$median_color)
  }
  if(!is.null(obs_x)) {
    pl <- pl +
      ggplot2::geom_point(data = obs_x,
                          ggplot2::aes_string(x = "t", y = "y"), colour = theme$obs_color, size = theme$obs_size)
  }
  if(!is.null(labels)) {
    pl <- pl + ggplot2::xlab(labels$x)
    pl <- pl + ggplot2::ylab(labels$y)
  }
  pl <- pl + theme_plain()
  if(log_y) {
    pl <- pl + ggplot2::scale_y_log10()
  }
  if(is.null(ylim)) {
    ylim <- c(0.1, max(tmp_y))
  }
  pl <- pl +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  if(return_svg) {
    filename <- paste0(tempfile(pattern="plot_"), ".svg")
    ggplot2::ggsave(filename = filename, plot = pl, width=width, height=height)
    pl_contents <- readChar(filename, file.info(filename)$size)
    unlink(filename)
    return(pl_contents)
  } else {
    return(pl)
  }
}
