rm(list=ls(all=TRUE)) # cleaning global environment
set.seed(2020)

# Data
#--------------------------------------------------------------------
devtools::load_all()

data("set1")
data("set2")

# CFA using bsem
#--------------------------------------------------------------------
# devtools::install()

# Blocks for confirmatory factor analysis
t <- Sys.time();fit1 <- bsem::sem(blocks = set1$blocks, data = set1$set,
                  iter = 20, chains = 4,
                  scaled = F); Sys.time()-t
fit1
summary(fit1)

gridExtra::grid.arrange(bsem::arrayplot(set1$real$lambda, -2,2),
                        bsem::arrayplot(fit1$mean_scores, -2,2),
                        layout_matrix = matrix(c(1,1,2,2), ncol= 2))

gridExtra::grid.arrange(bsem::arrayplot(set1$real$alpha, -2,2),
                        bsem::arrayplot(fit1$mean_loadings, -2,2))

t <- Sys.time();fit2 <- bsem::sem(blocks = set2$blocks, data = set2$set,
                  paths = set2$paths,
                  iter = 10000, warmup = 5000,
                  chains = 4,
                  scaled = F);Sys.time()-t
fit2

gridExtra::grid.arrange(bsem::arrayplot(set2$real$lambda, -4,4),
                      bsem::arrayplot(fit2$mean_scores, -4,4),
                      layout_matrix = matrix(c(1,1,2,2), ncol= 2))

gridExtra::grid.arrange(bsem::arrayplot(set2$real$alpha, -4,4),
                        bsem::arrayplot(fit2$mean_loadings, -4,4))

bayesplot::mcmc_trace(fit2$samples$alpha[,,1])
bayesplot::mcmc_trace(fit2$samples$alpha[,,2])
bayesplot::mcmc_trace(fit2$samples$beta)

