# tweedie
Workspace for the brms implementation of the tweedie distribution with power parameter in [1, 2] a.k.a. tweedie compound poisson, compound poisson-gamma (CPG) distribution.

Notation: 

* $Y \sim \mathrm{tweedie}(\mu, \phi, p)$
* `mu` = $\mu$
* `mphi` = $\phi$
* `mtheta` = $p$

Links: 

* `identity`: $\mu = \eta$
* `logmu`: $\log(\mu) = \eta$


## Repo overview: 

* `brms-tweedie-defs/`: working `brms::custom_family` definitions of the tweedie CPG distributions with different link functions (currently `tweedie_logmu.R` is the best)

* `data/`: 

  - real fish biomass data:
      - one dataset relating to a common species and another to a rare species
      - original sample attributes are censored and noise added to biomass values to obscure original values (zero values are preserved)

  - simulated data

* `R/functions.R`: functions used in `_targets.R`

* `reports/`

* `_targets.R`: the main script; fits models and runs tests, etc


## Resources

* [Tweedie to CPG parameter mapping](https://en.wikipedia.org/wiki/Compound_Poisson_distribution#Compound_Poisson_Gamma_distribution)

* [`brms::custom_family` docs](https://paul-buerkner.github.io/brms/reference/custom_family.html)

* [Define Custom Response Distributions with brms](https://cran.r-project.org/web/packages/brms/vignettes/brms_customfamilies.html)

* [Stan docs](https://mc-stan.org/users/documentation/)

* [On the CPG distribution(Withers and Nadarajah, 2011)](https://www.kybernetika.cz/content/2011/1/15/paper.pdf)

* [Video: How to code up a bespoke probability density in Stan](https://www.youtube.com/watch?v=CMSMtcMYHdM)

* [Likelihood-based and Bayesian methods for Tweedie compound Poisson linear mixed models (Zhang, 2012)](https://doi.org/10.1007/s11222-012-9343-7)

* [Bayesian hierarchical modelling of continuous non-negative longitudinal data with a spike at zero: An application to a study of birds visiting gardens in winter (Swallow et al, 2015)](https://doi.org/10.1002/bimj.201400081)


## To do: 

* Add functions at bottom of tweedie defs to custom_family call

* look into log link on mphi - doesnt seem to have any affect via `brms::custom_family(..., links = c(..., "log", ...))`

* decide on a single link structure
  - mu: log
  - mphi: ?
  - mtheta: ?

* add better default priors?

* create simulated random effect structured data and run tests to see how the estimates compare to true values