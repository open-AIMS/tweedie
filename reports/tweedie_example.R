library(brms)
library(tidyverse)
library(tweedie)
library(patchwork)
dir.create("output", showWarnings = FALSE)
# source("https://raw.githubusercontent.com/open-AIMS/stats_helpers/main/R/tweedie.R")
options(mc.cores = parallel::detectCores())

# Visualise different distributions based on parameter combinations
set.seed(10)
tpars <- read_csv("https://raw.githubusercontent.com/open-AIMS/stats_helpers/main/pars_tweedie_viz.csv") |>
  dplyr::group_by(mu, phi, theta) |>
  dplyr::reframe(
    vals = tweedie::rtweedie(1e4, mu = mu, power = theta, phi = phi),
    mu_dep = deparse(substitute(mu == a, list(a = mu))),
    phi_dep = deparse(substitute(phi == a, list(a = phi)))
  ) |>
  ungroup()
# tpars_plots <- tpars |>
#   split(f = ~ theta) |>
#   purrr::map(function(x) {
#     ggplot(data = x) +
#       geom_density(mapping = aes(x = vals), fill = "black") +
#       labs(title = substitute(theta == a, list(a = unique(x$theta))),
#            x = "") +
#       facet_grid(mu_dep ~ phi_dep, scales = "free", labeller = label_parsed)
#   })

# (tpars_plots[[1]] + tpars_plots[[2]]) /
# (tpars_plots[[3]] + tpars_plots[[4]]) +
#   patchwork::plot_annotation(
#     caption = "Response variable",
#     theme = theme(plot.caption = element_text(
#       hjust = 0.5, size = 16, vjust = 5
#     ))
#   )

# For a zero-inflated case, which family performs better?
#   Tweedie or hurdle-gamma?
test_dat <- tpars |>
  dplyr::filter(theta == 1.05, mu == 0.3, phi == 1)

t1 <- Sys.time()
mod_tw <- brms::brm(
  vals ~ 1, 
  data = test_dat, 
  family = tweedie, 
  stanvars = stanvars,
  backend = "cmdstanr"
)
t2 <- Sys.time()
t2 - t1

mod_hg <- brms::brm(
  vals ~ 1, data = test_dat, family = hurdle_gamma,
  backend = "cmdstanr"
)
mod_tw <- brms::add_criterion(mod_tw, "loo")
mod_hg <- brms::add_criterion(mod_hg, "loo")
brms::loo_compare(mod_tw, mod_hg)
pptw <- brms::pp_check(mod_tw, type = "dens_overlay", ndraws = 1e3) +
  brms::pp_check(mod_tw, type = "scatter_avg") +
  patchwork::plot_annotation(title = "Tweedie ppchecks")
pphg <- brms::pp_check(mod_hg, type = "dens_overlay", ndraws = 1e3) +
  brms::pp_check(mod_hg, type = "scatter_avg") +
  patchwork::plot_annotation(title = "Hurdle-Gamma ppchecks")
patchwork::wrap_elements(pptw) /
  patchwork::wrap_elements(pphg)

# A linear regression with intercept and slope
set.seed(10)
testdf <- data.frame(x = rnorm(100)) |>
  dplyr::mutate(
    mu = 15 + x * 5,
    y = tweedie::rtweedie(n(), mu = mu, power = 1.5, phi = 0.1)
  )
ggplot(data = testdf) +
  geom_point(mapping = aes(x = x, y = y))
options(mc.cores = parallel::detectCores())
stanvars <- stanvars +
  brms::stanvar(scode = gen_qt, block = "genquant")
fit <- brms::brm(
  y ~ x, data = testdf, family = tweedie, stanvars = stanvars,
  backend = "cmdstanr"
)
brms::bayes_R2(fit)
brms::conditional_effects(fit) |>
  plot(points = TRUE)
brms::conditional_effects(fit, method = "posterior_predict") |>
  plot(points = TRUE)
fit <- add_criterion(fit, "loo")
brms::pp_check(fit, type = "dens_overlay", ndraws = 1e3)
brms::pp_check(fit, type = "scatter_avg", ndraws = 1e3)
# similar plot to above, but using posterior_predict equivalent
#  generated from within the stan code.
brms::as_draws_df(fit) |>
  dplyr::select(starts_with("r_tweedie")) |>
  colMeans() |>
  (\(x, y)data.frame(pred = x, obs = y))(y = testdf$y) |>
  (\(x)ggplot(data = x))() +
    geom_point(mapping = aes(x = pred, y = obs)) +
    geom_abline(linetype = 2) +
    labs(x = "Mean prediction", y = "Observed")
# check range of distributional parameters
brms::as_draws_df(fit) |>
  dplyr::pull(phi) |>
  range()
brms::as_draws_df(fit) |>
  dplyr::pull(mtheta) |>
  range()