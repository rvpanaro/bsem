library(bsem)
library(magrittr)
library(testthat)

bsem::runShiny()

## factorial
dt <- simdata(exogenous = NULL, paths = NULL)
fit <- bsem::sem(data = dt$data, blocks = dt$blocks, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

### factorialNA
dt <- simdata(exogenous = NULL, paths = NULL, Nna = 1)
fit <- sem(data = dt$data, blocks = dt$blocks, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

## factorialEX
dt <- simdata(paths = NULL)
fit <- sem(data = dt$data, blocks = dt$blocks, exogenous = dt$exogenous, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

### factorialNAEX
dt <- simdata(paths = NULL, Nna = 1)
fit <- sem(data = dt$data, blocks = dt$blocks, exogenous = dt$exogenous, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

## sem
dt <- simdata(exogenous = NULL)
fit <- sem(data = dt$data, blocks = dt$blocks, paths = dt$paths, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

### semNA
dt <- simdata(exogenous = NULL, Nna = 1)
fit <- sem(data = dt$data, blocks = dt$blocks, paths = dt$paths, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

## semEX
dt <- simdata()
fit <- sem(data = dt$data, blocks = dt$blocks, paths = dt$paths, exogenous = dt$exogenous, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

### semNAEX
dt <- simdata(Nna = 1)
fit <- sem(data = dt$data, blocks = dt$blocks, paths = dt$paths, exogenous = dt$exogenous, signals = dt$signals, iter = 100, chains = 4)
fit
fit %>% summary()
fit %>% plot()

