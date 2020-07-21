## bsem <a href='https://rvpanaro.github.io/bsem'><img src='https://github.com/rvpanaro/bsem/blob/master/inst/figures/bsem.png' align="right" height="139" /></a>

---

### An R package for Bayesian structural equation modeling. 

The package allows Bayesian analysis for particular cases of structural equation models (SEMs) based on [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) integration. Examples in [get started](https://rvpanaro.github.io/spsurv/articles/bsem.html) and [exploring bsem](https://rvpanaro.github.io/spsurv/articles/exploring-bsem-class.html) include confirmatory factor analysis and confirmatory SEM. The full SEM model (outer and inner models), enables the evaluation of user-defined latent variables along with the analysis of established linear relationships among the latent scores.

## Install

Due to R 4.0 chain info is not working properly. As discussed [here](https://github.com/tylermorganwall/rayshader/issues/113) a possible fix is to get the under development version with:

```r
remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan")
```

Afterwards, you can install bsem using:

```r
devtools::install_github("rvpanaro/bsem")
```

## Basic working example

- Get some simulated data.

```r
dt <- bsem::simdata()
```

- Fit your SEM model

```r
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
```

- Print the outcome:

```r
print(semfit) 
```

- Get posterior summary statistics:

```r
summary(semfit)
```

- Plot a diagram with the relatioships between variables:

```r
plot(semfit)
```

## Access to ShinyApp

```r
bsem::runShiny()
```

## Future work and improvements

Feel free to collaborate or to open an issue.
