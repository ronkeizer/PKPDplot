library(testit)
library(PKPDsim)
library(PKPDplot)

p <- list(CL = 1, V  = 10, KA = 0.5, S2=.1)
pk  <- new_ode_model(code = "
                     dAdt[1] = -KA * A[1];
                     dAdt[2] = KA*A[1] -(CL/V) * A[2]
                     dAdt[3] = S2*(A[2]-A[3])
                     ",
                     obs = list(cmt=2, scale="V"),
                     dose = list(cmt = 2), cpp_show_code = FALSE)
r <- new_regimen(amt = 100, times = c(0), type = "infusion")
dat <- sim_ode (ode = "pk", n_ind = 1,
                omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
                par = p, regimen = r,
                verbose = FALSE, t_max=48)
pl <- dat %>% plot()
assert("plot created", "ggplot" %in% class(pl))
