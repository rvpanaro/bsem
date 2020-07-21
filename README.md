[![Codecov test coverage](https://codecov.io/gh/rvpanaro/bsem/branch/master/graph/badge.svg)](https://codecov.io/gh/rvpanaro/bsem?branch=master)
## bsem <a href='https://rvpanaro.github.io/bsem'><img src='https://github.com/rvpanaro/bsem/blob/master/inst/figures/bsem.png' align="right" height="139" /></a>

---

### An R package for Bayesian structural equation modeling. 

The package allows Bayesian analysis for particular cases of structural equation models (SEMs) based on [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) integration. Examples in [get started](https://rvpanaro.github.io/spsurv/articles/bsem.html) and [exploring bsem](https://rvpanaro.github.io/spsurv/articles/exploring-bsem-class.html) include confirmatory factor analysis and confirmatory SEM. The full SEM model (outer and inner models), enables the evaluation of user-defined latent variables along with the analysis of established linear relationships among the latent scores.

## Install

```r
devtools::install_github("rvpanaro/bsem")
```

## Basic working example

```r
dt <- bsem::simdata()

semfit <- bsem::sem(
  data = dt$data,
  blocks = dt$blocks,
  paths = dt$paths,
  exogenous = dt$exogenous,
  signals = dt$signals,
  iter = 2000,
  warmup = 1000,
  chains = 4
)

plot(semfit)
summary(semfit)
```

## Access to ShinyApp

```r
bsem::runShiny()
```

## Future work and improvements

Feel free to collaborate or to open an issue.
