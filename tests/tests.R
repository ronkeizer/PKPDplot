Sys.setenv("R_TESTS" = "")
library(testit)
library(PKPDsim)
library(PKPDplot)

p <- list(CL = 1, V  = 10, KA = 0.5)
pk1 <- new_ode_model("pk_1cmt_oral")
r1 <- new_regimen(amt = 100,
                  times = c(0, 12, 24, 36), t_inf = 2)
dat <- sim_ode (ode = "pk1",
                par = p,
                n = 50,
                t_obs = seq(from = 0, to = 54, by = 0.5),
                omega = c(0.1, 0.05, 0.1),
                regimen = r1)

plot(dat)
plot(dat,
     target = c(5, 11))
plot(dat[dat$id == 1,])
