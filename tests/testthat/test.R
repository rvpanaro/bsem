library(bsem)
library(magrittr)
library(testthat)

## Models (sem, print.bsem, summary.bsem, plot.bsem)
dt <- simdata(Nna = 1)

## factorial
fit <- bsem::sem(
  data = dt$data %>% na.omit(),
  blocks = dt$blocks,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "factorial"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

### factorialNA
fit <- sem(
  data = dt$data,
  blocks = dt$blocks,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "factorialNA"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

## factorialEX
fit <- sem(
  data = dt$data %>% na.omit(),
  blocks = dt$blocks,
  exogenous = dt$exogenous,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "factorialEX"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

### factorialNAEX
fit <- sem(
  data = dt$data,
  blocks = dt$blocks,
  exogenous = dt$exogenous,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "factorialNAEX"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

## sem
fit <- sem(
  data = dt$data %>% na.omit(),
  blocks = dt$blocks,
  paths = dt$paths,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "sem"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

### semNA
fit <- sem(
  data = dt$data,
  blocks = dt$blocks,
  paths = dt$paths,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "semNA"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

## semEX
fit <- sem(
  data = dt$data %>% na.omit(),
  blocks = dt$blocks,
  paths = dt$paths,
  exogenous = dt$exogenous,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "semEX"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

### semNAEX
fit <- sem(
  data = dt$data,
  blocks = dt$blocks,
  paths = dt$paths,
  exogenous = dt$exogenous,
  signals = dt$signals,
  iter = 2,
  warmup = 0,
  chains = 1
) %>%
  expect_s3_class(class = "bsem")

fit$model == "semNAEX"

print.bsem(fit)
fit %>% summary()
fit %>% plot()
fit$mean_alpha %>% arrayplot()

### handlers.R

## Prior options
expect_warning(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(
      beta = "cauchy(0,1)",
      sigma2 = "lognormal(-1,2)",
      gamma = "cauchy(0,1)",
      tau2 = "lognormal(-1,2)",
      gamma0 = "cauchy(0,1)"
    )
  )
) %>% expect_s3_class(class = "bsem")

expect_warning(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(
      beta = "normal(-2,1)",
      sigma2 = "gamma(.1,.1)",
      gamma = "cauchy(0,1)",
      tau2 = "inv_gamma(1.1,2.2)",
      gamma0 = "cauchy(0,1)"
    )
  )
) %>% expect_s3_class(class = "bsem")

expect_warning(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(
      beta = "normal(-2,1)",
      sigma2 = "inv_gamma(1.1,2.1)",
      gamma = "cauchy(0,1)",
      tau2 = "gamma(1.1,2.2)",
      gamma0 = "cauchy(0,1)"
    )
  )
) %>% expect_s3_class(class = "bsem")


## Prior misspecification
expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(beta = "gamma(0,1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma = "gamma(0,1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma0 = "gamma(0,1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(sigma2 = "normal(.1,.1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(tau2 = "normal(0,1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(beta = "gamma")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(taual = "gamma0(0,1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma = "gamma(0.1,.1)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma = "lognormal(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gammar = "normal(0,2)")
  )
)


expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(beta = "gammar(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma = "gammar(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma0 = "gammar(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(sigma2 = "nrlme(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(tau2 = "gammar(0,2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma = "normal(-1,-2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(gamma0 = "normal(-1,-2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(beta = "normal(-1,-2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(tau2 = "inv_gamma(-1,1.2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(tau2 = "inv_gamma(1,-1.2)")
  )
)


expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(sigma2 = "inv_gamma(-1,1.2)")
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    prior_specs = list(sigma2 = "inv_gamma(1,-1.2)")
  )
)

## Stanformals
expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    iter = 2,
    warmup = 0,
    chains = 1,
    coress = 1
  )
)

expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    signals = dt$signals[-1],
    iter = 2,
    warmup = 0,
    chains = 1
  )
)

aux <- dt$signals
aux[[3]] <- 1
expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    signals = aux,
    iter = 2,
    warmup = 0,
    chains = 1
  )
)
rm(aux)

aux <- dt$paths
names(aux)[2] <- "F6"
expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    paths = aux,
    signals = dt$signals,
    iter = 2,
    warmup = 0,
    chains = 1
  )
)
rm(aux)

aux <- dt$paths
aux[[1]][1] <- 3.5
expect_error(
  sem(
    data = dt$data,
    blocks = dt$blocks,
    paths = aux,
    signals = dt$signals,
    iter = 2,
    warmup = 0,
    chains = 1
  )
)
rm(aux)


