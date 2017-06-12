library(png)
library(grid)
library(ggplot2)
library(PKPDsim)
library(pkfosphenytointanaka)
library(dplyr)
setwd("/home/ron/git/PKPDplot")

covs <- list(weight = 70)
reg <- new_regimen(amt = 18 * covs$weight, n = 1, interval = 24, type = "infusion", t_inf = .25)
mod_t <- pkfosphenytointanaka::model()
par_t <- pkfosphenytointanaka::parameters()
res_t <- sim_ode(mod_t,
                 covariates = list(
                   WT = new_covariate(value = covs$weight, time = 0)
                 ),
                 t_obs = seq(0, 24, .1),
                 regimen = reg,
                 only_obs = TRUE,
                 parameters = par_t)

overlay <- png_overlay(png = "tests/pk_tanaka.png",
                       x_sel = c(0.09, 0.95), y_sel = c(0.133, .71),
                       x_range = c(0,24), y_range = c(0, 50))
plot(res_t, overlay = overlay, ylim=c(0, 50)) + xlim(c(0, 24))

