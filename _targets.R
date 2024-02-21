library(targets)
library(brms)
library(tidyverse, quietly = TRUE)
source("R/functions.R")
source("https://raw.githubusercontent.com/open-AIMS/stats_helpers/main/R/dharma.R")

list(
  # =====> TWEEDIE FAMILY DEFINITIONS
  tar_target(identity_link, "brms-tweedie-defs/tweedie_identity.R", format = "file"),
  tar_target(logmu_link, "brms-tweedie-defs/tweedie_logmu.R", format = "file"),
  tar_target(logmu_logphi_link, "brms-tweedie-defs/tweedie_logmu_logphi.R", format = "file"),

  # =====> REAL BIOMASS DATA
  # Common species
  tar_target(real_cmn_idnt,           model_realdata(identity_link, "common")),
  tar_target(real_cmn_logmu,          model_realdata(logmu_link, "common")),
  tar_target(real_cmn_logmu_cpriors,  model_realdata(logmu_link, "common", custom_priors = TRUE)),
  tar_target(real_cmn_logmuphi,       model_realdata(logmu_logphi_link, "common")),
  # Rare species
  tar_target(real_rare_idnt,          model_realdata(identity_link, "rare")),
  tar_target(real_rare_logmu,         model_realdata(logmu_link, "rare")),
  # tar_target(real_rare_logmu_cpriors, model_realdata(logmu_link, "rare", custom_priors = TRUE)),
  tar_target(real_rare_logmuphi,      model_realdata(logmu_logphi_link, "rare"))


  # =====> SIMULATED BIOMASS DATA
  # Simple (intercept only) model
  # Mixed effects (intercept only) model

)

