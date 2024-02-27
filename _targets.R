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

  # =====> DATASETS
  # Real data
  tar_target(rd_common, get_realdata("common")),
  tar_targets(rd_rare,  get_realdata("rare")),

  # =====> PRIORS
  # Real data
  tar_target(pr_common, priors_realdata("common")),
  # tar_target(pr_rare,   priors_realdata("rare")),

  # =====> REAL BIOMASS DATA
  # Common species - default priors
  tar_target(real_cmn_idnt,           model_realdata(rd_common, NULL, identity_link)),
  tar_target(real_cmn_logmu,          model_realdata(rd_common, NULL, logmu_link)),
  tar_target(real_cmn_logmuphi,       model_realdata(rd_common, NULL, logmu_logphi_link)),
  # Common species - custom priors
  tar_target(real_cmn_logmu_cpriors,  model_realdata(rd_common, pr_common, logmu_link)),
  # Rare species - default priors
  tar_target(real_rare_idnt,          model_realdata(rd_rare, NULL, identity_link)),
  tar_target(real_rare_logmu,         model_realdata(rd_rare, NULL, logmu_link)),
  tar_target(real_rare_logmuphi,      model_realdata(rd_rare, NULL, logmu_logphi_link))
  # Rare species - custom priors
  # tar_target(real_rare_logmu_cpriors, model_realdata(rd_rare, pr_rare, logmu_link)),

  # =====> SIMULATED BIOMASS DATA
  # Simple (intercept only) model
  # Mixed effects (intercept only) model

)

