[![Codecov test coverage](https://codecov.io/gh/rvpanaro/bsem/branch/master/graph/badge.svg)](https://codecov.io/gh/rvpanaro/bsem?branch=master)
## bsem <a href='https://rvpanaro.github.io/bsem/'><img src='https://raw.githubusercontent.com/rvpanaro/bsem/master/inst/figures/bsem.png' align="right" height="139" /></a>

---

### An R package for Bayesian structural equation modeling. 

The package allows Bayesian analysis for particular cases of structural equation models (SEMs) based on [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) integration. Examples in [get started](https://rvpanaro.github.io/bsem/articles/bsem.html) and [exploring bsem](https://rvpanaro.github.io/bsem/articles/exploring-bsem-class.html) include confirmatory factor analysis and confirmatory SEM. The full SEM model (outer and inner models), enables the evaluation of user-defined latent variables along with the analysis of established linear relationships among the latent scores.

## Install

Please install remotes package and devtools before installing:

```r
install.packages("remotes")
install.packages("devtools")
```

Due to R 4.0 chain info is not working properly. As discussed [here](https://github.com/tylermorganwall/rayshader/issues/113) a possible fix is to get the under development version with:

```r
remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan")
```

Afterwards, you can install bsem using:

```r
devtools::install_github("rvpanaro/bsem", dependencies = TRUE)
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


<img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img1.png width=33.3% />&nbsp; <img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img2.png width=33.3% /> &nbsp;<img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img3.png width=33.3% />&nbsp;<img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img4.png width=33.3% />&nbsp; <img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img5.png width=33.3% /> &nbsp;<img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img6.png width=33.3% />&nbsp;<img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img7.png width=33.3% />&nbsp; <img src=https://raw.githubusercontent.com/rvpanaro/bsem/master/docs/gallery/img8.png width=33.3% />


## Future work and improvements

Feel free to collaborate with me or to open an issue.
