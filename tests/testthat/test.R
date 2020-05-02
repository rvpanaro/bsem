rm(list=ls(all=TRUE)) # cleaning global environment

#Packages + seed
#--------------------------------------------------------------------

if(!require("coda")){ install.packages("coda"); library(coda)}
if(!require("rstan")){ install.packages("rstan"); library(rstan)}
if(!require("lattice")){ install.packages("lattice"); library(lattice)}

set.seed(2019) # Fixar semente para reproducibilidade.

# Data
#--------------------------------------------------------------------

# Data -> 53 companies e 19 variables
dat <- read.csv("data/Bayes_A03/dadosANEEL.csv", header=TRUE)


X <- dat[,-c(1,2)]; rownames(X) <- dat[, 1]

DIF1 <- X[,"EBITDA.Ajst"]-X[,"QRR"]
DIF2 <- X[,"EBIT.Ajst"]-X[,"EBIT.Reg"]
DIF3 <- X[,"Perdas.Rea"]-X[,"Perdas.Reg"]

X <- cbind(X, DIF1, DIF2, DIF3)

# EFA using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

fit_efa <- bsem::sem(K = 3, data = na.omit(X))
fit_efa

plotMatrix(round(fit_efa$loadings, 2),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_efa)

# Missing EFA using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

fit_efaNA <- bsem::sem(K = 3, data = X)
fit_efaNA

round(fit_efaNA$loadings, 2)
plotMatrix(round(fit_efaNA$loadings, 2),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_efaNA)

# CFA using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

# Blocks for confirmatory factor analysis
b = list(indebtedness = c("DLR","DIF1"),
         eficiency = c("EBITDA.Ajst","VPB.Reg","PMSO.Ajst","PMSO.Reg"),
         investment = c("CAPEX","QRR"),
         profitability = c("DIF2","BRL","EBIT.Reg"),
         operational = c("DGC","Mercado.GWh","DIF3","N.Cons"),
         return = c("Fluxo.Acionista","BRLK.Proprio","Res.Liquido"))

fit_cfa <- bsem::sem(blocks = b, data = na.omit(X))
fit_cfa

plotMatrix(round(fit_cfa$loadings, 3),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_cfa)

# Missing CFA using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

# Blocks for confirmatory factor analysis
b = list(indebtedness = c("DLR","DIF1"),
         eficiency = c("EBITDA.Ajst","VPB.Reg","PMSO.Ajst","PMSO.Reg"),
         investment = c("CAPEX","QRR"),
         profitability = c("DIF2","BRL","EBIT.Reg"),
         operational = c("DGC","Mercado.GWh","DIF3","N.Cons"),
         return = c("Fluxo.Acionista","BRLK.Proprio","Res.Liquido"))

fit_cfaNA <- bsem::sem(blocks = b, data = X, iter = 4000)
fit_cfaNA

plotMatrix(round(fit_cfaNA$loadings, 3),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_cfaNA)

# SEM using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

# Paths for structural equation modeling
p <- list(eficiency = c("investment", "profitability"),
          return = c("operational", "indebtedness", "eficiency"))

fit_sem <- bsem::sem(blocks = b, paths = p,
                     data = na.omit(X))
fit_sem

plotMatrix(round(fit_sem$loadings, 3),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_sem)

# Missing SEM using bsem
#--------------------------------------------------------------------
# devtools::install()
devtools::load_all()

# Paths for structural equation modeling
p <- list(eficiency = c("investment", "profitability"),
          return = c("operational", "indebtedness", "eficiency"))

fit_semNA <- bsem::sem(blocks = b, paths = p,
                       data = X)
fit_semNA

plotMatrix(round(fit_semNA$loadings, 3),
           xlab = "factor", ylab = "variable", main = "loading")

summary(fit_semNA)

#----
## Outcome Analysis

plotMatrix(output$loadings, xlab = "factor", ylab = "variable", main = "loading")

plotrix::plotCI(output$loadings[,1],
                li = output$credint$loadings[[1]][,1],
                ui = output$credint$loadings[[1]][,2],
                xlab = "", ylab = "Estimate", main  = "loading",
                pch = 19, lwd = 2, cex.axis = 2,
                cex.lab = 2, bty = 'n', xaxt = "n",
                las = 1, ylim = c(-3,3)
                  )

text(x = 1:nrow(output$loadings),
     y = ifelse(abs(output$credint$loadings[[1]][,1])>output$credint$loadings[[1]][,2],
                output$credint$loadings[[1]][,2]+1,
                output$credint$loadings[[1]][,1]-1),
     labels = rownames(output$loadings), srt = 60)

abline(h = 0, col =  2, lty = 2, lwd = 2)

plotMatrix(output$scores, xlab = "observation", ylab = "factor", main = "score")
plotrix::plotCI(output$scores[1,],
                li = output$credint$scores[[1]][,1],
                ui = output$credint$scores[[1]][,2],
                xlab = "observation", ylab = "Estimate", main = "score",
                pch = 19, lwd = 2, cex.axis = 2,
                cex.lab = 2, bty = 'n',
                las = 1, ylim = c(-3,3)
)

abline(h = 0, col =  2, lty = 2, lwd = 2)

output$h^2 %>% round(2)

lattice::bwplot(output$PVTE, panel = function(...) {
  panel.abline(v = median(output$PVTE), col = "green", lty = 2)
  panel.bwplot(...)
}, key = list(space="top",
             text=list(paste0("median= ", median(round(output$PVTE, 4))))),
xlab = "PVTE"
)

output$SQT
output$AFR2

Rank <- apply(output$scores, 1, order, decreasing = T)
rownames(Rank) <- colnames(output$scores)
Rank %>% head
Rank %>% tail

# Library
library(fmsb)

aux <- rbind(min = rep(floor(min(output$scores)), 6),
             max = rep(round(max(output$scores)), 6),
             t(output$scores)) %>% data.frame()

colors_border=c( rgb(0.2,0.5,0.5,0.9),
                 rgb(0.8,0.2,0.5,0.9) ,
                 rgb(0.7,0.5,0.1,0.9) )

colors_in=c( rgb(0.2,0.5,0.5,0.4),
             rgb(0.8,0.2,0.5,0.4) ,
             rgb(0.7,0.5,0.1,0.4) )

radarchart(aux[c("min", "max", "CEMIG_D", "AME", "ELETROPAULO"), ], axistype=1 ,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey",
           caxislabels = seq(floor(aux[1,1]), round(aux[2,1],1), round(aux[2,1]-aux[1,1])/4),
           cglwd=0.8,
           #custom labels
           vlcex=0.8 )

legend(x=1.1, y=1.3, legend = c("CEMIG_D", "AME", "ELETROPAULO"),
       bty = "n", pch=20 , col=colors_in , text.col = "grey",
       cex=1, pt.cex=3)

rgl::plot3d(x = aux[c("CEMIG_D", "AME", "ELETROPAULO"), 1],
            y = aux[c("CEMIG_D", "AME", "ELETROPAULO"), 2],
            z = aux[c("CEMIG_D", "AME", "ELETROPAULO"), 3],
            type = 's', radius = .1,
            col = 1:55)

output1 <- bsem::sem(data = X, blocks = G, iter = 2)
output1

