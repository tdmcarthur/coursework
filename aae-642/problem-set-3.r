

library("foreign")

# install.packages("stargazer")
library("stargazer")

source("/Users/travismcarthur/git/misc/authored-functions/authored-R-fns.R")

# QUESTION 1.1

mex.df <- read.dta("/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/hw3data.dta")

mex.df$regionNW <- ifelse(mex.df$region==1, 1, 0)
mex.df$regionCN <- ifelse(mex.df$region==2, 1, 0)
mex.df$regionCS <- ifelse(mex.df$region==3, 1, 0)
mex.df$regionS  <- ifelse(mex.df$region==4, 1, 0)

mex.df$region.factor <- factor(mex.df$region , levels=1:4, labels=c("NW", "CN", "CS", "S"))


unprocessed.summary.table <- by(mex.df[, c("area", "pctpd0306", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")], INDICES=mex.df$treat1, 
  FUN=function(x) {
    cbind(
      sapply(x, FUN=mean),
      sapply(x, FUN=sd),
      sapply(x, FUN=min),
      sapply(x, FUN=max)
      )
  } 
)

# NOTE: Ok, in this version, there is no median

processed.summary.table <- cbind(
  unprocessed.summary.table[[1]][, 1],
  unprocessed.summary.table[[2]][, 1],
  unprocessed.summary.table[[1]][, 2],
  unprocessed.summary.table[[2]][, 2],
  unprocessed.summary.table[[1]][, 3],
  unprocessed.summary.table[[2]][, 3],
  unprocessed.summary.table[[1]][, 4],
  unprocessed.summary.table[[2]][, 4]
)



colnames(processed.summary.table) <- c("Mean, control", "Mean, treat", "Std dev, control", "Std dev, treat", "Min, control", "Min, treat", "Max, control", "Max, treat")


stargazer(processed.summary.table , summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table1.tex", 
  rownames=TRUE,  column.separate=c(4,4), column.labels =c("test1", "test2"), float.env = "sidewaystable", font.size="small", title="Summary statistics of treatment and control groups"
    )



# QUESTION 1.2

t.stat.check <- sapply(mex.df[, c("area", "pctpd0306", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")],  FUN=function(x) {
  ret <- t.test(x[mex.df$treat1==0], x[mex.df$treat1==1])
  c(mean(x[mex.df$treat1==0]), mean(x[mex.df$treat1==1]), ret$statistic, ret$p.value)
  }
)

t.stat.check.df <- as.data.frame(t(t.stat.check ))

colnames(t.stat.check.df ) <- c("Mean, control", "Mean, treatment", "t statistic", "p-value")

stargazer(t.stat.check.df , summary=FALSE, 
  out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table2.tex",
  rownames=TRUE, title=paste0("Test of differences in means between treatment group (n=", sum(mex.df$treat1), ") and control group (n=", sum(mex.df$treat1==0), ")")
  )

# QUESTION 1.3

pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/kernelDensity1.pdf", width=4, height=4)

density.1 <- density(mex.df$avgelev[mex.df$treat1==0])
density.2 <- density(mex.df$avgelev[mex.df$treat1==1])

plot(density.1, main="Fig. 1: Kernel density of \naverage elevation", xlab="",
  xlim=c(min(c(density.1$x, density.2$x)), max(c(density.1$x, density.2$x))), 
  ylim=c(min(c(density.1$y, density.2$y)), max(c(density.1$y, density.2$y))),
  lwd=2
  )

lines(density.2, lty=2, lwd=2)

legend("topleft", legend=c("Control", "Treatment"), cex=.7, col=c("black"), lty=c(1, 2), lwd=2)

dev.off()




pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/kernelDensity2.pdf", width=4, height=4)

density.1 <- density(mex.df$avgslope[mex.df$treat1==0])
density.2 <- density(mex.df$avgslope[mex.df$treat1==1])

plot(density.1, main="Fig. 2: Kernel density of \naverage slope", xlab="",
  xlim=c(min(c(density.1$x, density.2$x)), max(c(density.1$x, density.2$x))), 
  ylim=c(min(c(density.1$y, density.2$y)), max(c(density.1$y, density.2$y))),
  lwd=2
  )

lines(density.2, lty=2, lwd=2)

legend("topright", legend=c("Control", "Treatment"), cex=.7, col=c("black"), lty=c(1, 2), lwd=2)

dev.off()


# TEST


summary(lm(treat1 ~ area + dcarr + avgelev + avgslope + ejido + 
    region.factor + psemidecid + pselva, data=mex.df) )

summary(glm(treat1 ~ area + dcarr + avgelev + avgslope + ejido + 
    region.factor + psemidecid + pselva, data=mex.df, family=binomial) )


# QUESTION 2.1

library(censReg)

uncontrolled.tobit <- censReg(pctpd0306 ~ treat1, data=mex.df)

summary(uncontrolled.tobit )

summary(margEff(uncontrolled.tobit))


stargazer(as.data.frame(summary(margEff(uncontrolled.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table3.tex", 
  rownames=TRUE, title="Tobit marginal effect upon percent deforested",
  notes=c(paste0("Number of observations: ", uncontrolled.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df$pctpd0306), 2))
  )
)
  



#test<-noquote(uncontrolled.tobit$nObs)



#  notes=c("Observations:",
#    paste0(names(uncontrolled.tobit$nObs), collapse=" "),
#    paste0(uncontrolled.tobit$nObs, collapse=" ")
#    )

controlled.tobit <- censReg(pctpd0306 ~ treat1 + area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df, method="BFGS")

cor(mex.df[, c("treat1", "avgelev", "avgslope")])

summary(controlled.tobit)

summary(margEff(controlled.tobit))

stargazer(as.data.frame(summary(margEff(controlled.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table4.tex", 
  rownames=TRUE, title="Tobit marginal effect upon percent deforested",
    notes=c(paste0("Number of observations: ", controlled.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df$pctpd0306), 2))
    )
)

# I have "marginal effects of an explanatory variable on the expected value of the dependent variable"


# QUESTION 2.3

region.het.tobit <- censReg(pctpd0306 ~ treat1*region.factor, data=mex.df)

summary(margEff(region.het.tobit))

stargazer(as.data.frame(summary(margEff(region.het.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table5.tex", 
  rownames=TRUE,
  notes=c(paste0("Number of observations: ", region.het.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df$pctpd0306), 2))
    )
    )

ejido.het.tobit <- censReg(pctpd0306 ~ treat1*ejido, data=mex.df)

summary(margEff(ejido.het.tobit))


stargazer(as.data.frame(summary(margEff(ejido.het.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table6.tex", 
  rownames=TRUE
    )


elevation.het.tobit <- censReg(pctpd0306 ~ treat1*I(avgelev>median(avgelev)), data=mex.df)

summary(margEff(elevation.het.tobit))


stargazer(as.data.frame(summary(margEff(elevation.het.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table7.tex", 
  rownames=TRUE
    )


# Ok, now put all interactions in one:

all.het.tobit <- censReg(pctpd0306 ~ treat1*region.factor + treat1*ejido + treat1*I(avgelev>median(avgelev)), data=mex.df, method="BFGS")

all.het.tobit.test <- censReg(pctpd0306 ~ treat1*region.factor + treat1*ejido + treat1*I(avgelev>median(avgelev)) + area + dcarr + avgslope  + psemidecid + pselva, data=mex.df, method="BFGS")


summary(all.het.tobit.test )

summary(margEff(all.het.tobit))

summary(margEff(all.het.tobit.test))


stargazer(as.data.frame(summary(margEff(all.het.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table7b.tex", 
  rownames=TRUE, title="Tobit marginal effect upon percent deforested, heterogenous effects",
  notes=c(paste0("Number of observations: ", all.het.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df$pctpd0306), 2))
    )
    )


# stargazer(as.data.frame(summary(margEff(ejido.het.tobit))), summary=FALSE)

# QUESTION 3.1

# install.packages("Matching")
library("Matching")

# install.packages("MatchIt")
library("MatchIt")

set.seed(100)

treat.prop.score.glm <- glm( treat1 ~  area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, family=binomial, data=mex.df)


#mex.complete.df <- mex.df[complete.cases(mex.df[, c("treat1", "area", "dcarr", "avgelev", "ejido", "region.factor", "psemidecid", "pselva")]), ]

#match.test<- matchit(treat1 ~  area + dcarr + avgelev + ejido + 
#    region.factor + psemidecid + pselva, data=mex.df[, c("treat1", "area", "dcarr", "avgelev", "ejido", "region.factor", "psemidecid", "pselva")],
#   method = "nearest", distance = "logit")

# The fitted values are the propensity score

# QUESTION 3.2

pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/propensity-score-density.pdf", width=4, height=4)

density.1 <- density(treat.prop.score.glm$fitted[mex.df$treat1==0])
density.2 <- density(treat.prop.score.glm$fitted[mex.df$treat1==1])

plot(density.1, main="Fig. 3: Kernel density of \npropensity scores", xlab="",
  xlim=c(min(c(density.1$x, density.2$x)), max(c(density.1$x, density.2$x))), 
  ylim=c(min(c(density.1$y, density.2$y)), max(c(density.1$y, density.2$y))),
  lwd=2
  )



lines(density.2, lty=2, lwd=2)

legend("bottom", legend=c("Control", "Treatment"), cex=.6, col=c("black"), lty=c(1, 2), lwd=2, bty="n")

dev.off()

# QUESTION 3.3




match.outcome <- Match(Y=mex.df$pctpd0306, Tr=mex.df$treat1, X=treat.prop.score.glm$fitted, M=1, replace=TRUE, ties=FALSE)

summary(match.outcome )

mex.df$prop.score.matched.1 <- ifelse(1:nrow(mex.df) %in% match.outcome$index.control, 1, 0)

length(unique(match.outcome$index.control))
length(unique(match.outcome$index.treat))


test.matchit <- matchit(treat1 ~  area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df)





# install.packages("dgof")
# library("dgof")


t.stat.check.2 <- sapply(mex.df[, c("area", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "region", "psemidecid", "pselva")],  FUN=function(x) {
  ret <- t.test(x[mex.df$prop.score.matched.1==1], x[mex.df$treat1==1])
  
  if (all(x %in% 0:1)) {
    ret.2 <- prop.test(x=c(sum(x[mex.df$prop.score.matched==1]), sum(x[mex.df$treat1==1])), 
      n=c(sum(mex.df$prop.score.matched.1), sum(mex.df$treat1)) )
    
    ret[] <- NA
    
  } else {
    ret.2 <- ks.test(x[mex.df$prop.score.matched.1==1], x[mex.df$treat1==1])
  }
  
  if (all(x %in% 1:4)) {
    ret.2 <- chisq.test( table(x[mex.df$prop.score.matched.1==1 | mex.df$treat1==1 ],
      mex.df$prop.score.matched.1[mex.df$prop.score.matched.1==1 | mex.df$treat1==1 ])
    )
  }
  
  c(mean(x[mex.df$prop.score.matched.1==1]), mean(x[mex.df$treat1==1]), ret$statistic, ret$p.value, ret.2$statistic, ret.2$p.value)
  }
)

# prop.test() or chisq.test().

# chisq.test(x[mex.df$prop.score.matched==1], x[mex.df$treat1==1])
# TODO: not sure whether to use Pearson's chi-square or prop.test

# chisq.test( table(mex.df$region,mex.df$prop.score.matched ))

#chisq.test( table(mex.df$region[mex.df$prop.score.matched.1==1 | mex.df$treat1==1 ],
#      mex.df$prop.score.matched.1[mex.df$prop.score.matched.1==1 | mex.df$treat1==1 ]))

t.stat.check.2.df <- as.data.frame(t(t.stat.check.2 ))

t.stat.check.2.df[c("regionNW", "regionCN", "regionCS", "regionS"), 5:6] <- NA
t.stat.check.2.df["region", 1:4] <- NA

colnames(t.stat.check.2.df) <- c("Mean, control", "Mean, treatment", "t statistic", "p-val of t stat", "Distn. stat", "p-val of distn. stat")

stargazer(t.stat.check.2.df,
  summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table8.tex", 
  rownames=TRUE,
  title =paste0("Test statistics for matched treatment (n=",sum(mex.df$prop.score.matched.1), ") and control groups (n=", sum(mex.df$treat1), ") for each group"),
  notes=c("Note: The distribution statistic is the test statistic of the Kolmogorov-Smirnov test of equality of two distributions", "in the case of continous variables. For the binary variable (ejido), the distribution statistic is the statistic", "for a test that the proportions for the mean and control are the same. For the regions, the distribution",  "statistic is a Pearson's $\\chi^2$ test that the observations were drawn from the same distribution of categories."),
   font.size="small",
  column.sep.width = "1pt"
    )
mex.df$prop.score.matched.1==1], x[mex.df$treat1==1]

summary(lm( pctpd0306 ~ treat1, data=mex.df[mex.df$prop.score.matched.1 ==1 | mex.df$treat1==1, ]))

summary(match.outcome)
summary(lm( Y ~ Tr, data=match.outcome$mdata, weight=rep(match.outcome$weights,2)))

table(match.outcome$mdata$Tr)

table(duplicated(t(match.outcome$mdata$X)))


# QUESTION 3.4

tobit.3.4 <- censReg( match.outcome$mdata$Y[!duplicated(colnames(match.outcome$mdata$X))] ~ 
    match.outcome$mdata$Tr[!duplicated(colnames(match.outcome$mdata$X))])

tobit.3.4 <- censReg(Y ~ Tr, data=match.outcome$mdata)

# tobit.3.4 <- censReg( pctpd0306 ~ treat1, data=mex.df[mex.df$prop.score.matched.1 ==1 | mex.df$treat1==1, ] )

summary(margEff(tobit.3.4 ))



stargazer(as.data.frame(summary(margEff(tobit.3.4 ))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table9b.tex", 
  rownames=TRUE,
  title="Tobit marginal effect upon percent deforested; control and treatment matched by propensity score",
  notes=c(paste0("Number of observations: ", tobit.3.4$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df[mex.df$prop.score.matched.1 ==1 | mex.df$treat1==1, "pctpd0306"]), 2))
    )
    )



summary(match.outcome )

match.outcome.results <- c(match.outcome$est, match.outcome$se.standard, match.outcome$est/match.outcome$se.standard, 
  (1-pnorm(abs(match.outcome$est/match.outcome$se.standard)))*2)

match.outcome.results.df<- as.data.frame(t(match.outcome.results))

colnames(match.outcome.results.df) <- c("Estimate", "Std. Error", "t stat", "p-value")



stargazer(match.outcome.results.df,
  summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table9.tex", rownames=FALSE   )




# QUESTION 4.1


match.outcome.mah <- Match(Y=mex.df$pctpd0306, Tr=mex.df$treat1, X=mex.df[, c("area", "dcarr",
  "avgelev", "ejido", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")], 
  replace=TRUE, Weight = 2, ties=FALSE)


tobit.4.1 <- censReg( Y ~ Tr, data=match.outcome.mah$mdata)



# tobit.4.1 <- censReg( pctpd0306 ~ treat1, data=mex.df[c(match.outcome.mah$index.control, match.outcome.mah$index.treated), ] )

summary(margEff(tobit.4.1 ))



stargazer(as.data.frame(summary(margEff(tobit.4.1 ))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table10b.tex", 
  rownames=TRUE,
  title="Tobit marginal effect upon percent deforested; control and treatment matched by Mahalanobis method",
  notes=c(paste0("Number of observations: ", tobit.4.1$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df[c(match.outcome.mah$index.control, match.outcome.mah$index.treated), "pctpd0306"]), 2))
    )
    )


# TODO: can probably get rid of this OLS estimate:

summary(match.outcome.mah)

match.outcome.mah.results <- c(match.outcome.mah$est, match.outcome.mah$se.standard, match.outcome.mah$est/match.outcome.mah$se.standard, 
  (1-pnorm(abs(match.outcome.mah$est/match.outcome.mah$se.standard)))*2)

match.outcome.mah.results.df<- as.data.frame(t(match.outcome.mah.results))

colnames(match.outcome.mah.results.df) <- c("Estimate", "Std. Error", "t stat", "p-value")




stargazer(match.outcome.mah.results.df,
  summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table10.tex", rownames=FALSE   )


# QUESTION 4.2



#match.outcome.mah.limited <- Match(Y=mex.df$pctpd0306, 
#  Tr=mex.df$treat1, X=mex.df[, c("area", "dcarr",
#  "avgelev", "ejido", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")], 
#  replace=FALSE, Weight = 2, distance.tolerance = 20, exact=TRUE)

#summary(match.outcome.mah.limited)

# install.packages("optmatch")
library(optmatch)

matchon.limited <- match_on(treat1 ~  area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data = mex.df)

#test.pairmatch <- pairmatch(treat1 ~  area + dcarr + avgelev + ejido + 
#    regionCN + regionCS + regionS + psemidecid + pselva, data = mex.df, method= "euclidean")

mex.df$match.dist.mah  <- 999


#min(matchon.limited[2,])

#diag(matchon.limited[, as.character(match.outcome.mah$index.control)])[2]


#matchon.limited[, as.character(match.outcome.mah$index.control)][1:4,1:4]

# Below operation shows it is A-ok
#apply(matchon.limited, 1, FUN=which.min)[1:4] + length(match.outcome.mah$index.treat)
#match.outcome.mah$index.control[1:4]





#mex.df$match.dist.mah[c(match.outcome.mah$index.treated, match.outcome.mah$index.control)] <- 
#  rep(diag(matchon.limited[, as.character(match.outcome.mah$index.control)]), 2)


mex.df$match.dist.mah[c(match.outcome.mah$index.treated, match.outcome.mah$index.control)] <- 
  rep(apply(matchon.limited, 1, FUN=min), 2)

match.outcome.mah$mdata$match.dist <- rep(apply(matchon.limited, 1, FUN=min), 2)


#table(duplicated(rownames(match.outcome.mah$mdata$X)))

mex.df$match.dist.mah[c(match.outcome.mah$index.treated, match.outcome.mah$index.control)] <- 
  rep(diag(matchon.limited[, as.character(match.outcome.mah$index.control)]), 2)




mex.df$mah.matched.control <- ifelse(1:nrow(mex.df) %in% match.outcome.mah$index.control & mex.df$match.dist.mah < 1.5, 1, 0)

mex.df$mah.matched.treat <- ifelse(1:nrow(mex.df) %in% match.outcome.mah$index.treated & mex.df$match.dist.mah < 1.5, 1, 0)

match.outcome.mah$mdata$Y <- match.outcome.mah$mdata$Y[match.outcome.mah$mdata$match.dist < 1.5]

match.outcome.mah$mdata$Tr <- match.outcome.mah$mdata$Tr[match.outcome.mah$mdata$match.dist < 1.5]

sum(mex.df$mah.matched.control)
sum(mex.df$mah.matched.treat)
length(match.outcome.mah$index.treated)

sum(mex.df$mah.matched.treat) /length(match.outcome.mah$index.treated)

sum(mex.df$mah.matched.control) /sum(!duplicated(mex.df$mah.matched.control))


table(mex.df$region[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ],
      mex.df$mah.matched.control[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ])



t.stat.check.3 <- sapply(mex.df[, c("area", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "region", "psemidecid", "pselva")],  FUN=function(x) {
  ret <- t.test(x[mex.df$mah.matched.control==1], x[mex.df$mah.matched.treat==1])
  
  if (all(x %in% 0:1)) {
    ret.2 <- prop.test(x=c(sum(x[mex.df$mah.matched.control==1]), sum(x[mex.df$mah.matched.treat==1])), 
      n=c(sum(mex.df$mah.matched.control), sum(mex.df$mah.matched.treat)) )
    
    ret[] <- NA
    
  } else {
    ret.2 <- ks.test(x[mex.df$mah.matched.control==1], x[mex.df$mah.matched.treat==1])
  }
  
  if (all(x %in% 1:4)) {
    ret.2 <- chisq.test( table(x[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ],
      mex.df$mah.matched.control[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ])
    )
  }
  
  c(mean(x[mex.df$mah.matched.control==1]), mean(x[mex.df$mah.matched.treat==1]), ret$statistic, ret$p.value, ret.2$statistic, ret.2$p.value)
  }
)



table(mex.df$region[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ],
      mex.df$mah.matched.control[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1 ])



t.stat.check.3.df <- as.data.frame(t(t.stat.check.3 ))

t.stat.check.3.df[c("regionNW", "regionCN", "regionCS", "regionS"), 5:6] <- NA
t.stat.check.3.df["region", 1:4] <- NA

colnames(t.stat.check.3.df) <- c("Mean, control", "Mean, treatment", "t statistic", "p-val of t stat", "Distn. stat", "p-val of distn. stat")

t.stat.check.3.df

stargazer(t.stat.check.3.df,
  summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table11.tex", 
  rownames=TRUE, 
  title=paste0("Test statistics for subset of Mahalanobis-matched treatment (n=",sum(mex.df$mah.matched.control), ") and control groups (n=", sum(mex.df$mah.matched.treat), ") for each group"),
  notes=c("Note: The distribution statistic is the test statistic of the Kolmogorov-Smirnov test of equality of two distributions", "in the case of continous variables. For the binary variable (ejido), the distribution statistic is the statistic", "for a test that the proportions for the mean and control are the same. For the regions, the distribution",  "statistic is a Pearson's $\\chi^2$ test that the observations were drawn from the same distribution of categories."),
  font.size="small",
  column.sep.width = "1pt"
    )



# QUESTION 4.3



tobit.4.3 <- censReg(match.outcome.mah$mdata$Y ~ match.outcome.mah$mdata$Tr)

#tobit.4.3 <- censReg( pctpd0306 ~ treat1, data=mex.df[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1, ] )

summary(margEff(tobit.4.3 ))


stargazer(as.data.frame(summary(margEff(tobit.4.3 ))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table12b.tex", 
  rownames=TRUE,
  title="Tobit marginal effect upon percent deforested; Mahalanobis method restricted sample",
  notes=c(paste0("Number of observations: ", tobit.4.3$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df[mex.df$mah.matched.control==1 | mex.df$mah.matched.treat==1, "pctpd0306"]), 2))
    )
    )




summary(lm( pctpd0306 ~ treat1, data=mex.df[mex.df$mah.matched.control ==1 |  mex.df$mah.matched.treat==1, ] ))

stargazer(lm( pctpd0306 ~ treat1, data=mex.df[mex.df$mah.matched.control ==1 |  mex.df$mah.matched.treat==1, ] ),
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table12.tex"  )







# QUESTION 4.4


mex.df$treat3 <- ifelse(is.na(mex.df$treat2), 1, 0)


#unconfound.tobit <- censReg(pctpd0306 ~ treat3 + area + dcarr + avgelev + ejido + 
#    region.factor + psemidecid + pselva, data=mex.df[mex.df$treat1 == 0, ])

#summary(margEff(unconfound.tobit))

unconfound.tobit <- censReg(pctpd0306 ~ treat3 + area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df[mex.df$mah.matched.control ==1, ])


summary(margEff(unconfound.tobit))





stargazer(as.data.frame(summary(margEff(unconfound.tobit ))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table13b.tex", 
  rownames=TRUE,
  title="Test of unconfoundedness: Tobit marginal effects",
  notes=c(paste0("Number of observations: ", unconfound.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df[mex.df$mah.matched.control ==1, "pctpd0306"]), 2))
    )
    )


mean(mex.df$pctpd0306)
mean(mex.df$pctpd0306[mex.df$pctpd0306>0])


# END














summary(lm(pctpd0306 ~ treat3 + area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df[mex.df$treat1 == 0, ]))



stargazer(lm(pctpd0306 ~ treat3 + area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df[mex.df$treat1 == 0, ]),
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table13.tex"  )







#stargazer(as.data.frame(summary(margEff(controlled.tobit))), summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table4.tex", 
#  rownames=TRUE
#    )


























data(nuclearplants)

detach("package:Matching", character.only = TRUE)
detach("package:MatchIt", character.only = TRUE)
detach("package:stargazer", character.only = TRUE)
match_on(pr ~ t1 + t2, data = nuclearplants, method = "mahalanobis")
mdist(pr ~ t1 + t2, data = nuclearplants)

( pm1 <- pairmatch(pr ~ t1 + t2, data = nuclearplants) )
summary(pm1)

fullmatch.test <- fullmatch(treat1 ~  area + dcarr + avgelev + ejido + 
    regionCN + regionCS + regionS + psemidecid + pselva, min.controls=0, max.controls=1, data = mex.df, method="euclidean" )
summary(fullmatch.test)

fullmatch.test <- fullmatch(mex.df$treat1 ~  treat.prop.score.glm$fitted, min.controls=1, max.controls=1)
  summary(fullmatch.test)


fullmatch.test <- fullmatch(mex.df$treat1 ~  treat.prop.score.glm$fitted, min.controls=1, max.controls=1  ,  method="euclidean" )
  summary(fullmatch.test)

summary(attr(fullmatch.test, "contrast.group" ))




matched(pm1)

nuclearplants$pr











mex.df$prop.score.matched.1 <- ifelse(1:nrow(mex.df) %in% match.outcome$index.control, 1, 0)


 <- abs(t(match.outcome.mah$mdata$X)[,1] - t(match.outcome.mah$mdata$X)[,2] )


str(match.outcome.mah$mdata$X)





match.outcome.mah$ecaliper







#
# Estimate the propensity model
#
glm1  <- glm(treat~age + I(age^2) + educ + I(educ^2) + black +
             hispan + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2) , family=binomial, data=lalonde)


#
#save data objects
#
X  <- glm1$fitted
Y  <- lalonde$re78
Tr  <- lalonde$treat

#
# one-to-one matching with replacement (the "M=1" option).
# Estimating the treatment effect on the treated (the "estimand" option which defaults to 0).
#
rr  <- Match(Y=Y,Tr=Tr,X=X,M=1);

#Let's summarize the output
summary(rr)

# Let's check the covariate balance
# 'nboots' is set to small values in the interest of speed.
# Please increase to at least 500 each for publication quality p-values.  
mb  <- MatchBalance(treat~age + I(age^2) + educ + I(educ^2) + black +
             hispan + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2), data=lalonde, match.out=rr, nboots=10)

























# TODO: DELETE THE BELOW:
  
sum(runif(length(mex.df$pctpd0306)) != 1 & Tr != 0) > 0

summary(match.outcome.mah)

match.outcome.mah <- Match(Y=mex.df$pctpd0306, Tr=mex.df$treat1, X=scale(), M=1, replace=TRUE, Weight = 3, Weight.matrix = diag(diag(var(mex.df[, c("area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")]))))



X.mat <-  as.matrix(mex.df[, c("area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")])

match.outcome.mah <- Match(Y=mex.df$pctpd0306, Tr=mex.df$treat1, X=X.mat, M=1, replace=TRUE, Weight = 3, Weight.matrix = solve(t(X.mat) %*% X.mat/nrow(X.mat)))

summary(match.outcome.mah)


min(eigen(solve(t(X.mat) %*% X.mat/nrow(X.mat)), only.values = TRUE)$values) < 
        sqrt(.Machine$double.eps)


as.double(sum((solve(t(X.mat) %*% X.mat/nrow(X.mat)) != 
            diag(diag(solve(t(X.mat) %*% X.mat/nrow(X.mat)))))) == 0)




min(eigen(t(X.mat) %*% X.mat/nrow(X.mat), only.values = TRUE)$values) > 1e-07

solve(t(X.mat) %*% X.mat/nrow(X.mat))



test.matchit <-
matchit(treat1 ~  area + dcarr + avgelev + ejido + 
    psemidecid + pselva, data=mex.df[, c("treat1", "area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")], method = "nearest", distance = "logit")
"mahalanobis"
  

regionNW + regionCN + regionCS + regionS + 

solve(t(as.matrix(mex.df[, c("area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")])) %*% as.matrix(mex.df[, c("area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")])
)

cor(as.matrix(mex.df[, c("area", "dcarr", "avgelev", "ejido", 
    "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")]))

, Weight.matrix=diag(runif(10))

diag(rep(1, 10))


t(match.outcome.mah$mdata$X)

mex.df

mex.df$prop.score.matched.1 <- ifelse(1:nrow(mex.df) %in% match.outcome$index.control, 1, 0)


 <- abs(t(match.outcome.mah$mdata$X)[,1] - t(match.outcome.mah$mdata$X)[,2] )


rbind(X[index.treated,],X[index.control,])








