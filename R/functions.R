# ===============================================
#                USER FUNCTIONS
# ===============================================
model_realdata <- function(tweedie_def, species_type, custom_priors = FALSE, ...) {
  source(tweedie_def)
  message(str(tweedie))
  data <- get_realdata(species_type)
  priors <- if (custom_priors) priors_realdata(species_type) else NULL
  x <- fit_model_realdata(data, priors)
  x$simres <- make_brms_dharma_res(x$mod)
  return(x)
}
# model_simdata <- function(tweedie_def, model_type, custom_priors = FALSE, ...) {
#   source(tweedie_def)
#   message(str(tweedie))
#   x <- model_simdata_simple(...)
#   x$simres <- make_brms_dharma_res(x$mod)
#   return(x)
# }


# ===============================================
#                HELPER FUNCTIONS
# ===============================================
#  1. REAL DATA
# =====> READ IN REAL BIOMASS DATA 
get_realdata <- function(species) {
  file <- paste0("data/realdata_", species, "_species.csv")
  data <- file |> 
    read_csv(col_types = "dffff") |> 
    mutate(site_year = paste(site, year, sep = "_"))
  return(data)
}
# =====> DEFINE CUSTOM PRIORS FOR REAL BIOMASS DATA
priors_realdata <- function(species) {
  if (species == "common") {
    priors <-
      brms::prior(normal(2, 1), class = "b", lb = 0) +
      brms::prior(gamma(2, 0.5), class = "sd") + 
      brms::prior(gamma(2, 0.5), class = "mphi") + 
      brms::prior(uniform(1, 2), class = "mtheta") 
  }
  if (species == "rare") {
    stop("priors not yet defined in R/functions.R:priors_realdata()")
  }
  return(priors)
}
# =====> MODEL REAL (CENSORED) BRUVS BIOMASS DATA
fit_model_realdata <- function(data, priors) {
  x <- list()
  t1 <- Sys.time()
  x$mod <- brm(
    formula = biomass ~ habitat - 1 + (1 | site / subsite) + (1 | year) + (1 | site_year), 
    data = data,
    family = tweedie, 
    stanvars = stanvars, 
    prior = priors,
    sample_prior = "yes", 
    backend = "cmdstanr",
    iter = 1e4, warmup = 5e3, chains = 4, cores = 4, seed = 73,
    control = list(adapt_delta = 0.99, max_treedepth = 20)
  )
  t2 <- Sys.time()
  x$t <- difftime(t2, t1, units = "mins")
  return(x)
}

# # 2. SIMULATED DATA
# # ======> SIMPLE MODEL (INTERCEPT ONLY)
# model_simdata_simple_io <- function(mu, phi, p, n, seed) {
#   require(tweedie)
#   x <- list()
#   set.seed(seed)
 
#   # Simulate data
#   data <- data.frame(y = tweedie::rtweedie(n, mu = mu, phi = phi, power = p))
  
#   # Fit model
#   t1 <- Sys.time()
#   if (structure == "simple") x$mod <- brms::brm(
#     y ~ 1, 
#     data = data, 
#     family = tweedie,
#     stanvars = stanvars, 
#     seed = seed
#   )
#   t2 <- Sys.time()

#   # Compile estimates and runtime
#   x$info <- data.frame(
#     data = "Simulated", 
#     model_structure = "simple", 
#     runtime = difftime(t1, t2, units = "mins"),
#     mu_true = mu, 
#     phi_true = phi, 
#     p_true = p,
#   )
#   return(x)
# }
# # ======> HEIRARCHICAL STRUCTURE
# model_simdata_ranef <- function(mu, phi, p, sd_site, sd_year, n, seed) {
#   require(tweedie)
#   x <- list()
#   set.seed(seed)
#   # Simulate data: y ~ 1 + (1 | site) + (1 | year)
#   if (structure == "mixed_io") {
#     site <- c("A", "B", "C", "D", "E")
#     delta_site <- rnorm(5, mean = 0, sd = site_sd)
#     year <- c("2001", "2002", "2003", "2004", "2005")
#     delta_year <- rnorm(5, mean = 0, sd = sd_year)

#     data <- data.frame(
#       y = tweedie::rtweedie(n, mu = mu, phi = phi, power = p), 
#       sd
#     ) 
#   }
#   # Fit model
#   t1 <- Sys.time()
#   if (structure == "simple") x$mod <- brms::brm(
#     y ~ 1, 
#     data = data, 
#     family = tweedie,
#     stanvars = stanvars, 
#     seed = seed
#   )
#   t2 <- Sys.time()

#   # Compile estimates and runtime
#   t <- difftime(t1, t2, units = "mins")
#   x$info <- data.frame(
#     data = "Simulated", 
#     model_structure = "simple", 
#     runtime = t,
#     mu_true = mu, 
#     phi_true = phi, 
#     p_true = p,
#   )
#   return(x)
# }


