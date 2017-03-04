

library("lfe")

# Monte Carlo

set.seed(1000)
n.obs <- 1000
m.iterations <- 10000

ret.mat <- matrix(NA, ncol = 3, nrow = m.iterations)

for (iter in 1:m.iterations) {


  x <- rnorm(n.obs)
  y <- x + rnorm(n.obs)

  x.star.1 <- x + rnorm(n.obs)
  x.star.2 <- x + rnorm(n.obs)
  x.star.3 <- x + rnorm(n.obs)
  x.star.4 <- x + rnorm(n.obs)
  x.star.5 <- x + rnorm(n.obs)
  x.star.6 <- x + rnorm(n.obs)

  # OLS
  #summary(lm(y ~ x.star.1))
  #summary(lm(y ~ x.star.2))
  #summary(lm(y ~ I((x.star.1 + x.star.2)/2)))
  #summary(lm(y ~ I((x.star.1 + x.star.2 + x.star.3)/3)))
  #summary(lm(y ~ I((x.star.1 + x.star.2 + x.star.3 + x.star.4)/4)))
  #summary(lm(y ~ I((x.star.1 + x.star.2 + x.star.3 + x.star.4 + x.star.5)/5)))
  #summary(lm(y ~ I((x.star.1 + x.star.2 + x.star.3 + x.star.4 + x.star.5 + x.star.6)/6)))

  # GMM
  ret.mat[iter, 1] <- coef(felm(y ~ 1 | 0 | (x.star.1 ~ x.star.2)))[2]
  ret.mat[iter, 2] <- coef(felm(y ~ 1 | 0 | (x.star.1 ~ x.star.2 + x.star.3 + x.star.4 + x.star.5 + x.star.6)))[2]
  ret.mat[iter, 3] <- coef(felm(y ~ 1 | 0 | (x.star.1 ~ I((x.star.2 + x.star.3 + x.star.4 + x.star.5 + x.star.6)/5))))[2]
}


hist(ret.mat[, 1], breaks = 50)
hist(ret.mat[, 2], breaks = 50)
hist(ret.mat[, 3], breaks = 50)

apply(ret.mat, 2, FUN = sd)
apply(ret.mat, 2, FUN = summary)

ks.test(ret.mat[, 2], ret.mat[, 3])
ks.test(ret.mat[, 1], ret.mat[, 3])
ks.test(ret.mat[, 1], ret.mat[, 2])

t.test(ret.mat[, 1] - 1 )
t.test(ret.mat[, 2] - 1 )
t.test(ret.mat[, 3] - 1 )
t.test(ret.mat[, 2], ret.mat[, 3])

var.test(ret.mat[, 2], ret.mat[, 3])
# var.test() assumes normality, which -- since this is an OLS variant -- should hold in this case.
mood.test(ret.mat[, 2], ret.mat[, 3])
# mood.test() does not assume normality

# Mood's test has higher power than the Ansari test:
# https://www.researchgate.net/profile/Boris_Lemeshko/publication/236170343_ABOUT_ROBUSTNESS_AND_POWER_OF_VARIANCE_HOMOGENEITY_TESTS/links/02e7e516cb9c8d3d78000000.pdf


0.029
0.022

# The formula specification is a response variable followed by a four part formula. The first part consists of ordinary covariates, the second part consists of factors to be projected out. The third part is an IV-specification. The fourth part is a cluster specification for the standard errors. I.e. something like y ~ x1 + x2 | f1 + f2 | (Q|W ~ x3+x4) | clu1 + clu2 where y is the response, x1,x2 are ordinary covariates, f1,f2 are factors to be projected out, Q and W are covariates which are instrumented by x3 and x4, and clu1,clu2 are factors to be used for computing cluster robust standard errors. Parts that are not used should be specified as 0, except if it's at the end of the formula, where they can be omitted.













load("/Users/travismcarthur/Desktop/From old comp/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata", verbose = TRUE)




library("ICC")
#ICCest(x, y, data = NULL, alpha = 0.05, CI.type = c("THD", "Smith"))

seccion.icc <- ICCest(inputs.df$seccion.full,
       ifelse(inputs.df$x19.fertilizante.bs.quintal == 0 , NA, inputs.df$x19.fertilizante.bs.quintal ),
       data = NULL, alpha = 0.05, CI.type = c("THD", "Smith"))

segmento.icc <- ICCest(inputs.df$segmento.full,
       log(ifelse(inputs.df$x19.fertilizante.bs.kg.impute.level == "itself", inputs.df$x19.fertilizante.bs.kg, NA )),
       data = NULL, alpha = 0.05, CI.type = c("THD", "Smith"))


c("x19.fertilizante.bs.kg.impute.level", "x19.sem.comprada.bs.kg.impute.level",
"x19.abono.bs.kg.impute.level", "x19.plagicidas.bs.kg.impute.level",
"hourly.wage.impute.level", "hourly.tractor.rental.impute.level")

input.prices <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg",
"x19.plagicidas.bs.kg", "hourly.wage", "hourly.tractor.rental")

geog.levels <- c("segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento" )

intra.cor.lev.mat <- matrix(NA, ncol = length(geog.levels), nrow = length(input.prices))
rownames(intra.cor.lev.mat) <- input.prices
colnames(intra.cor.lev.mat) <- geog.levels
intra.cor.log.mat <- intra.cor.lev.mat

for (targ.price in input.prices) {
  for (targ.geog in geog.levels) {
    targ.y <- ifelse(inputs.df[, paste0(targ.price, ".impute.level")] == "itself", inputs.df[, targ.price], NA )
    targ.x.y <- aggregate( targ.y ~ inputs.df$folio +  inputs.df[, targ.geog], FUN = mean, na.rm = TRUE)
    intra.cor.lev.mat[targ.price, targ.geog] <- ICCest(targ.x.y$`inputs.df[, targ.geog]`,
       targ.x.y$targ.y)$ICC
    cat(targ.price, targ.geog, "\n")
  }
}



for (targ.price in input.prices) {
  for (targ.geog in geog.levels) {
    targ.y <- ifelse(inputs.df[, paste0(targ.price, ".impute.level")] == "itself", inputs.df[, targ.price], NA )
    targ.x.y <- aggregate( targ.y ~ inputs.df$folio +  inputs.df[, targ.geog], FUN = mean, na.rm = TRUE)
    intra.cor.log.mat[targ.price, targ.geog] <- ICCest(targ.x.y$`inputs.df[, targ.geog]`,
       log(targ.x.y$targ.y))$ICC
  }
}


input.quants <- c("x19.fertilizante.cantidad.kg", "x19.sem.comprada.cantidad.kg", "x19.abono.cantidad.kg",
"x19.plagicidas.cantidad.kg", "paid.hours", "tractor.hrs.final")




cat("
> length(unique(inputs.df$folio)
[1] 7169
> length(unique(inputs.df$segmento.full))
[1] 1005
> length(unique(inputs.df$sector.full))
[1] 995
> length(unique(inputs.df$canton.full))
[1] 666
> length(unique(inputs.df$seccion.full))
[1] 272
> length(unique(inputs.df$provincia.full))
[1] 104
")


for (targ.input in 1:length(input.quants)) {
  fmla <- as.formula(paste0("log(", input.prices[targ.input], ") ~ log(", input.quants[targ.input], ") | segmento.full" ))
  print(summary(felm(fmla,
  data = inputs.df[inputs.df[input.quants[targ.input]] > 0,  ] )))

}



round(intra.cor.lev.mat, 2)
round(intra.cor.log.mat, 2)

























