# library("data.table")

library("xtable")
library("stargazer")

get.bootstraps <- function(directory, param.subset.pattern) {

  bootstrap.files <- list.files(directory, full.names = TRUE)

  boot.params.ls <- list()

  for (targ.file in seq_along(bootstrap.files)) {
    boot.params.df.temp <- read.csv(bootstrap.files[targ.file], col.names = c("param", "value"), header = FALSE, stringsAsFactors = FALSE)
    names(boot.params.df.temp)[2] <- paste0("value.", gsub("[^0-9]", "", bootstrap.files[targ.file]))
    cat(targ.file, base::date(), "\n")
    boot.params.df.temp <- boot.params.df.temp[!grepl("(^p)|(^w)|(^Smat)", boot.params.df.temp$param), ]
    boot.params.ls[[targ.file]] <- boot.params.df.temp[grepl(param.subset.pattern, boot.params.df.temp$param), ]
  }

  # boot.params.df <- do.call(cbind, boot.params.ls)
  # param.names.save <- boot.params.df$param
  # boot.params.df <- cbind(param = param.names.save, boot.params.df[, colnames(boot.params.df) != "param"], stringsAsFactors = FALSE)
  
  boot.params.df <- boot.params.ls[[1]]
  for ( i in 2:length(boot.params.ls) ) {
    boot.params.df <- merge(boot.params.df, boot.params.ls[[i]], all = TRUE)
  }
  
  ret <- boot.params.df[, "param", drop = FALSE]
  for (targ.boot in  0:as.numeric( gsub("[^0-9]", "", bootstrap.files[targ.file]) ) ) {
    boot.cols <- as.numeric( gsub("[^0-9]", "", names(boot.params.df)))
    boot.cols[is.na(boot.cols)] <- (-1)
    if ( any(  boot.cols == targ.boot)) {
      ret[, paste0("value.", formatC(targ.boot, width = 5, flag = "0"))] <- 
        boot.params.df[, boot.cols == targ.boot]
    } else {
      ret[, paste0("value.", formatC(targ.boot, width = 5, flag = "0"))] <- NA
    }
  }
  ret
}

boot.dataset <- function(seed.number, target.top.crop.number) {

# 

## source("/Users/travismcarthur/git/coursework/aae-637/paper/initial-data-setup.r")
## source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-bootstrap.r")
## source("/Users/travismcarthur/git/coursework/aae-637/paper/gme-control-code.r")



# target.top.crop.number <- 3

#Including zero cost:
#Potatoes	4,058
#Maize	3,440
#Barley	2,048
#Wheat	1,647
#Fava Beans	1,484

M <- 1
N <- 6
# Standard N is 6
# J <- 3
J <- 5
# Standard J is 5 now that HH and hired labor are treated as the same



do.regimes <- TRUE
# {}

functional.form <- "SGM" # OR TRANSLOG
#functional.form <- "TRANSLOG"

do.tobit <- FALSE


#synthetic.data <-TRUE
 synthetic.data <- FALSE
if (!exists("global.max.seed")) { global.max.seed <- 0}
do.SUR <- FALSE
include.cost.fn <- FALSE
only.cost.fn <- TRUE
generate.synth.data.from.cost.fn <- TRUE
start.at.true.xi <- FALSE
start.nonlin.from.ignorance <- TRUE
convex.in.f.inputs <- FALSE
concave.in.prices <- TRUE
# NOTE: J, i.e. number of fixed inputs, is set via sgm-linear-sur-building.r



if (!synthetic.data) { 
  intended.seed <- 100 
  start.nonlin.from.ignorance <- FALSE
#  start.nonlin.from.ignorance <- TRUE
  global.max.seed <- 4
  do.SUR <- FALSE
  include.cost.fn <- FALSE
  only.cost.fn <- FALSE
  generate.synth.data.from.cost.fn <- FALSE
  start.at.true.xi <- FALSE
}



# Only posi cost observations:

#Papa (patatas)    3155 
#Maiz combined   1838 
#Cebada combined   950 
#Trigo             475 
#Haba (verde)       641 
#Oca               240 
#Arveja (verde)     217 
#Hoja de coca       363 
#Arroz con cascara          264
#Quinua            284 




# do.SUR <- TRUE

#functional.form <- "TRANSLOG"

if (functional.form =="SGM") {
  include.censored.cost <- FALSE
}

price.trim.quantile <- 0.95
demand.var.trim.quantile <- 0.95
output.var.trim.quantile <- 1
fixed.input.var.trim.quantile <- 1
#demand.var.trim.quantile <- 1


local.source.evaluation <- TRUE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


# saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

# saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil.Rdata"
# with soil

# saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

# saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain and no drive time and with mean imputation.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Bolivia alloc paper/Data/saved workspace only inputsDF with soil and rain and no drive time and with mean imputation.Rdata"


# GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir2/"
GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir3/"

# GAMS.exe.path <- "/Applications/GAMS/gams24.1_osx_x64_64_sfx/gams"
GAMS.exe.path <- "/Applications/GAMS24.7/sysdir/gams"


code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"



# GAMS.projdir.subdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/bootstrap/"

if (Sys.info()['sysname']=="Linux") {

saved.workspace.path <- "/home/k/kzaman/TravisImInYourInternets/input-data/saved workspace only inputsDF with soil and rain and no drive time and with mean imputation.Rdata" # "/home/k/kzaman/TravisImInYourInternets/bootstrap-output/saved workspace.Rdata" NEED TO FIX # saved workspace only inputsDF with soil.Rdata

GAMS.projdir <-  "/home/k/kzaman/TravisImInYourInternets/gamsdir/projdir/"

GAMS.exe.path <- "/home/k/kzaman/TravisImInYourInternets/gams24.7_linux_x64_64_sfx/gams"

code.dir <- "/home/k/kzaman/TravisImInYourInternets/git/coursework/aae-637/paper/"

.libPaths("/home/k/kzaman/TravisImInYourInternets/Rlib")

#detach("package:Matrix", unload = TRUE, force=TRUE)
#detach("package:lattice", unload = TRUE, force=TRUE)

#unloadNamespace("lattice")

#install.packages("lattice", repos="http://cran.us.r-project.org", 
#        lib="/home/k/kzaman/TravisImInYourInternets/Rlib")

# library(lattice, lib.loc ="/home/k/kzaman/TravisImInYourInternets/Rlib")
# The above line was there to get around the old version of R that was installed previously
library(lattice)
library(Matrix)

  for ( i in c("plyr", "gdata", "stringr", "systemfit", "arules") ) {
    if(!require(i, character.only=TRUE, lib.loc ="/home/k/kzaman/TravisImInYourInternets/Rlib")) {
      install.packages(i, repos="http://cran.us.r-project.org", 
        lib="/home/k/kzaman/TravisImInYourInternets/Rlib")
      while(!require(i, character.only=TRUE, lib.loc ="/home/k/kzaman/TravisImInYourInternets/Rlib")) {
        Sys.sleep(1)
  	    require(i, character.only=TRUE, lib.loc ="/home/k/kzaman/TravisImInYourInternets/Rlib")
  	  }
    }
  }

}






load(saved.workspace.path, verbose = TRUE)




log.plus.one.cost <- FALSE

#seed.file <- paste0(GAMS.projdir, "bootstrap-seed.txt")
# This file starts as a completely empty text file. 
# It can be produced by "touch" in bash

#seed <- readLines(seed.file)
#cat("\n1", file = seed.file, append = TRUE)

# bootstrap.iter <- 1
bootstrap.iter <- sum(as.numeric(seed.number), na.rm = TRUE)
cat(bootstrap.iter, "\n")
# This starts the very first iteration as numeric(0), which actually sums to 0, so we are good
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"), local = local.source.evaluation)
# Above is a bit hacky

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))

if (functional.form =="TRANSLOG") {

region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)


# Below makes use of the fact that we have the original dataframe floating in workspace
# from the above source()

log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r

input.scaling.orig <- c()
for ( i in 1:N) {

  input.scaling.orig  <- c( input.scaling.orig, log10_ceiling(
    sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
    combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
  )
  )
  # Got this idea from scale() function

}

scale.vars.on.orig.data <- TRUE

}





set.seed(100)

#bootstrap.replications <- 1
#bootstrap.replications <- 1500
#nrow(firm.df)
#nrow(inputs.df)
#length(unique(inputs.df$folio))


bootstrap.replications.v <- 1:1500
# 0:300 301:600 601:900 901:1200 1201:1500
# condor_R max-entropy-bootstrap.r bootmaiz1.log &

bootstrap.replications <- max(bootstrap.replications.v)


bootstrap.selection.mat<- matrix(sample( x=nrow(firm.df), size=nrow(firm.df)*bootstrap.replications, 
  replace=TRUE), nrow=nrow(firm.df))

time.counter <- c()

# 1:bootstrap.replications

# bootstrap.iter <- 0


file.flavor <- "mean-impute-no-cost-fn-no-SUR-logit-attempt"











#for ( bootstrap.iter in c(0, bootstrap.replications.v)) {
# for ( bootstrap.iter in 0) {

if( bootstrap.iter==0 ) {
  bootstrap.selection.v <- TRUE
} else {
  bootstrap.selection.v <- bootstrap.selection.mat[, bootstrap.iter]
}


#for (target.top.crop.number in c(2,4,5)) {

#if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "build-model-extract-parcels.r"), local = local.source.evaluation)


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
# Needed to tweak this a bit for the boot.dataset function
# Also set this to make it faster: do.tobit <- FALSE
# AND local.source.evaluation <- TRUE
# and added loacl argument here:
# source(paste0(code.dir, "build-model-extract-parcels.r"), local = local.source.evaluation)


source(paste0(code.dir, "regimes-full-power-set.r"), local = local.source.evaluation)

regime.cut <- as.numeric(factor(posi.vars.regime))
combined.df$posi.vars.regime <- factor(posi.vars.regime)

combined.df$regime.cut <- regime.cut

combined.df

}


results.dir <- "/Users/travismcarthur/git/private/Bolivia Allocative Efficiency Paper/"
# "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/tables/"


#boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes", "(^xi)")
#boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/simple nonlinear", "(^xi)")

#boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/regimes", "(^xi)")
#boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/simple nonlinear", "(^xi)")

#boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/regimes", "(^xi)")
#boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/simple nonlinear", "(^xi)")


boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed actual/regimes", "(^xi)")
boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed actual/simple nonlinear", "(^xi)")

#/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed actual/regimes/sgmGMEnonlinearRegimesCebada00000mean-impute-finally-correct-param-output.txt



# boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada NLS fam not fixed", "(^xi)")


for( i in 1:nrow(boot.simple.df)) {
  hist(t(boot.simple.df[, -1])[, i], breaks = 10)
}



for( i in 1:nrow(boot.simple.df)) {
  print(summary(t(boot.simple.df[, -1])[, i]))
}


for( i in 1:nrow(boot.simple.df)) {
  hist(t(boot.simple.df[, -1])[  t(boot.simple.df[, -1])[, i] <= 50   , i], breaks = 10)
}




# boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/regimes", "(^xi)")
# boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/simple nonlinear", "(^xi)")



boot.regimes.sd <- apply(boot.regimes.df[, -1], 1, FUN = sd, na.rm = TRUE)
boot.simple.sd <- apply(boot.simple.df[, -1], 1, FUN = sd, na.rm = TRUE)

apply(boot.regimes.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)
apply(boot.simple.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)

# apply(boot.regimes.df[, -1] - boot.simple.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)



library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

data(tli)
xtable(tli[1:10, ])



boot.regimes.df[, -1]

xi.params.display.df <- cbind(boot.simple.df[, 1:2], boot.simple.sd, (boot.simple.df[, 2] - 1 ) / boot.simple.sd,
                              boot.regimes.df[, 2], boot.regimes.sd, (boot.regimes.df[, 2] - 1 ) / boot.regimes.sd)

colnames(xi.params.display.df) <- c("Parameter", "No selection correction", "SE", "t-stat", "Selection correction", "SE", "t-stat")
xi.params.display.df$Parameter <- paste0("$\\", gsub("xi", "xi_{", xi.params.display.df$Parameter), "}$")


boot.cov <- cov(t(boot.simple.df[, -1]), use = "complete.obs")

cov2cor(boot.cov)

R <- diag(1, 5)
r <- matrix(1, nrow = nrow(R))
theta <- boot.simple.df[, 2]
chi.sq.stat <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["chi2"]
p.val <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["P"]

chi.sq.stat.simple <- chi.sq.stat
p.val.simple <- p.val





theta.xi.06 <- c(theta, 1)
boot.cov.xi.06 <- boot.cov
boot.cov.xi.06 <- rbind(boot.cov.xi.06, 0)
boot.cov.xi.06 <- cbind(boot.cov.xi.06, 0)
# Ok so it's easy enough to add the last row and col

p.val.mat <- matrix(NA, ncol = nrow(boot.cov.xi.06), nrow = nrow(boot.cov.xi.06))


for (i in 1:(nrow(boot.cov.xi.06)) ) {
  for ( j in 1:(nrow(boot.cov.xi.06))) {
    if (i == j | i < j) { next } # | i > j
    
    R <- matrix(0, nrow = 1, ncol = nrow(boot.cov.xi.06))
    R[1, i] <- 1
    R[1, j] <- (-1)
    # print(R %*% theta.xi.06)
    r <- matrix(0, nrow = 1)
    chi.sq.stat <- aod::wald.test(Sigma = boot.cov.xi.06, b = theta.xi.06, L = R)$result$chi2["chi2"]
    p.val <- aod::wald.test(Sigma = boot.cov.xi.06, b = theta.xi.06, L = R)$result$chi2["P"]
    p.val.mat[i, j] <- p.val
    
  }
}

p.val.mat
t(t(theta.xi.06 ))









boot.cov <- cov(t(boot.regimes.df[, -1]), use = "complete.obs")
cov2cor(boot.cov)

R <- diag(1, 5)
r <- matrix(1, nrow = nrow(R))
theta <- boot.regimes.df[, 2]
chi.sq.stat <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["chi2"]
p.val <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["P"]

chi.sq.stat.regimes <- chi.sq.stat
p.val.regimes <- p.val

min.num.boots <- min(c(ncol(boot.simple.df), ncol(boot.regimes.df)))
boot.cov <- cov(t(rbind(boot.simple.df[, 2:min.num.boots], 
                        boot.regimes.df[, 2:min.num.boots])), use = "complete.obs")

# Ok how is this possible not positive definite? - aha I think this was due to use = "pairwise.complete.obs" in cov(). Now fixed
library("matrixcalc")
is.positive.semi.definite(boot.cov)
eigen(boot.cov)


R <- t(rbind(diag(1, 5), (-1) * diag(1, 5)))
r <- matrix(0, nrow = nrow(R))
theta <- c(boot.simple.df[, 2], boot.regimes.df[, 2])
chi.sq.stat <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["chi2"]
p.val <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["P"]

chi.sq.stat.simple.regimes.compare <- chi.sq.stat
p.val.simple.regimes.compare <- p.val

# (boot.cov/1685)

# Super useful:
# https://cran.r-project.org/web/packages/xtable/vignettes/xtableGallery.pdf

p.val.display <- ifelse(p.val.simple ==  0, "$<$ machine precision", paste0("= ", signif(p.val.simple, 3)))
note.1 <- paste0("No selection correction: $\\chi_{5}^{2}$ Wald statistic on $\\xi_{k} = 1$, $\\forall$ $k$: ", round(chi.sq.stat.simple, 2), " (p ", p.val.display, ").") 

p.val.display <- ifelse(p.val.regimes ==  0, "$<$ machine precision", paste0("= ", signif(p.val.regimes, 3)))
note.2 <- paste0("Selection correction: $\\chi_{5}^{2}$ Wald statistic on $\\xi_{k} = 1$, $\\forall$ $k$: ", round(chi.sq.stat.regimes, 2), " (p ", p.val.display, ").") 

p.val.display <- ifelse(p.val.simple.regimes.compare ==  0, "$<$ machine precision", paste0("= ", signif(p.val.simple.regimes.compare, 3)))
note.3 <- paste0("$\\chi_{5}^{2}$ Wald statistic on $\\xi_{k}^{No \\hphantom{o} correction} = \\xi_{k}^{Correction}$, $\\forall$ $k$: ", round(chi.sq.stat.simple.regimes.compare, 2), " (p ", p.val.display, ").") 

# Thanks to https://stackoverflow.com/questions/6163823/r-xtable-caption-or-comment
dfList <- list(xi.params.display.df)
#attr(dfList, "message") <- c("A caption", "Which can have multiple lines")
attr(dfList, "message") <- c(note.1, note.2, note.3)

to.output <- print(xtableList(dfList, caption = "Estimated shadow price parameters"), floating = TRUE, include.rownames = FALSE, sanitize.text.function = identity, caption.placement = "top")


results.dir <- "/Users/travismcarthur/git/private/Bolivia Allocative Efficiency Paper/"

cat(gsub("xi", "theta", to.output), 
      file = paste0(results.dir, "theta-cebada.tex"))


for( i in 1:nrow(boot.simple.df)) {
  hist(t(boot.simple.df[, -1])[, i], breaks = 10)
}















#boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/regimes", "(^xi)|(^lambda)")
#boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/simple nonlinear", "(^xi)|(^lambda)")


boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/regimes", "(^xi)|(^lambda)")
boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/simple nonlinear", "(^xi)|(^lambda)")

regime.key.file <- "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/regime-key.Rdata"
"/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regime-key.Rdata"




if (FALSE) {

regime.key.ls <- list()

for ( seed.number in 0:max(as.numeric( gsub("[^0-9]", "", colnames(boot.regimes.df))), na.rm = TRUE)) {
# for ( seed.number in 80:200) {
  add.family.labor.to.hired.labor <- FALSE
  combined.df <- boot.dataset(seed.number = seed.number, target.top.crop.number = 1)
  regime.key.df <- unique(combined.df[, c("posi.vars.regime", "regime.cut")])
  regime.key.df$regime.cut <- paste0("lambda", formatC(regime.key.df$regime.cut, width = 2, flag = "0"))
  colnames(regime.key.df)[2] <- "param"
  regime.key.ls[[as.character(seed.number)]] <- regime.key.df
  cat(seed.number, base::date(), "\n")
}

# save(regime.key.ls, file = "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regime-key.Rdata")
# save(regime.key.ls, file = "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/regime-key.Rdata")
# load("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regime-key.Rdata", verbose = TRUE)

}

load(regime.key.file, verbose = TRUE)
#load("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/regime-key.Rdata", verbose = TRUE) 

regime.params.ls <- list()

# for (i in as.numeric(names(regime.key.ls))) {
for (i in 0:(ncol(boot.regimes.df) - 2)) {
  #colnames(regime.key.ls[[i]])[2] <- "param"
  boot.regimes.df.temp <- boot.regimes.df[grepl("^lambda", boot.regimes.df$param), 
                                          c("param", paste0("value.", formatC(i, width = 5, flag = "0")))]
  regime.params.ls[[as.character(i)]] <- merge(regime.key.ls[[as.character(i)]], boot.regimes.df.temp)
  regime.params.ls[[as.character(i)]]$param <- NULL
}

regime.params.df <- regime.params.ls[[1]]



for (i in 2:length(regime.params.ls)) {
  regime.params.df <- merge(regime.params.df, regime.params.ls[[i]], all = TRUE)
}

apply(regime.params.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)


boot.cov <- cov(t(regime.params.df[, -1]), use = "complete.obs")


table(sapply(regime.params.df[, -1], FUN = function(x) sum(is.na(x))) )
# See how many missing lambdas we have

eigen(boot.cov)$value

# IDEA: Jitter the orig paras to make var-cov it a PSD matrix for cebada

# Previously, we have not-positive-semi-definite var-cov matrix due to use = "pairwise.complete.obs". 
# This fix will make things more likely to reject, I think, but will exclude some lambdas, I think.

#R <- matrix(0, nrow = nrow(boot.cov), ncol = nrow(boot.cov) - 1)
#for ( i in 1:(nrow(R) - 1)) {
#  R[i, i]     <- 1
#  R[i + 1, i] <- (-1)
#}
# R <- t(R)

R <- diag(1, nrow(boot.cov))
r <- matrix(0, nrow = nrow(R))
theta <- regime.params.df[, 2]
chi.sq.stat.lambda <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["chi2"]
p.val.lambda <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["P"]

chi.sq.stat.lambda
p.val.lambda

#chi.sq.stat <- t(R[1:2, 1:2] %*% theta[1:2] - r[1:2]) %*% solve(R[1:2, 1:2] %*% (boot.cov[1:2, 1:2]) %*% t(R[1:2, 1:2])) %*% (R[1:2, 1:2] %*% theta[1:2] - r[1:2])



# boot.regimes.all.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes", "06")
crop.dirs <- c(
  "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada fam labor fixed/simple nonlinear",
  "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/papa bootstrap fam labor fixed/simple nonlinear" # regimes
)


tech.params.ls <- list()

for (targ.crop.dir in crop.dirs) {


boot.regimes.all.df <- boot.regimes.all.df.save <- get.bootstraps(targ.crop.dir, "")
boot.regimes.all.df <- boot.regimes.all.df[!grepl("(xi)|(lambda)", boot.regimes.all.df$param), ]
# "(R)|(lambda)"
# "(R)|(lambda)|(b)|(s)"


boot.cov <- cov(t(boot.regimes.all.df[, -1]), use = "complete.obs")

tech.params.df  <- data.frame(boot.regimes.all.df[, 1], round(boot.regimes.all.df[, 2], 2), round(boot.regimes.all.df[, 2] / sqrt(diag(boot.cov)), 2))

tech.params.df  <- data.frame(boot.regimes.all.df[, 1], boot.regimes.all.df[, 2], sqrt(diag(boot.cov)), boot.regimes.all.df[, 2] / sqrt(diag(boot.cov)))

names(tech.params.df) <- c("Parameter", "Estimate", "SE", "t-stat") 

tech.params.ls[[targ.crop.dir]] <- tech.params.df

}

# do.call(cbind, tech.params.ls)
# This above doesnt work
tech.params.df.final <- cbind(tech.params.ls[[1]], tech.params.ls[[2]][, -1])


# Thanks to https://stackoverflow.com/questions/6163823/r-xtable-caption-or-comment
dfList <- list(tech.params.df.final)
#attr(dfList, "message") <- c("A caption", "Which can have multiple lines")
#attr(dfList, "message") <- c(note.1, note.2, note.3)


attr(dfList, "message") <- c(
  "See equation 2 for the model specification. Price $w_{i}$ are 1 = inorganic fertilizer in Bolivianos/kg; ",
  "2 = purchased seed in Bs/kg; 3 = tractor Bs/hours; 4 = plaguicidas Bs/kg; 5 = hired labor Bs/hours; ",
  "6 = organic fertilizer Bs/kg. Fixed inputs $q_{i}$ are 1 = hectares cultivated; 2 = has irrigation; ",
  "3 = family labor hours; 4 = soil quality; 6 = elevation in km; 5 = precipitation in cm/season."
)



tech.params.output <- print(xtableList(dfList, digits = 3, display = rep("g", 1 + ncol(tech.params.df.final)), 
                     caption = "Technological parameters of cost function"), include.rownames = FALSE,
          sanitize.text.function = identity, tabular.environment = "longtable", floating = FALSE, caption.placement = "top")

tech.params.output <- strsplit(tech.params.output, "\n" )[[1]]
tech.params.output <- c(tech.params.output[1:5], 
                        "\\multicolumn{1}{c}{} & \\multicolumn{3}{c}{Barley} & \\multicolumn{3}{c}{Potatoes} \\\\",
                        "\\hline",
                        tech.params.output[6:length(tech.params.output)])


cat( tech.params.output,  file = paste0(results.dir, "tech-params.tex"), sep ="\n")










R <- diag(1, nrow(boot.cov))
r <- matrix(0, nrow = nrow(R))
theta <- boot.regimes.all.df[, 2]
chi.sq.stat.q06 <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["chi2"]
p.val.q06 <- aod::wald.test(Sigma = boot.cov, b = theta, L = R)$result$chi2["P"]


boot.regimes.df$param

q06.params.display.df <- data.frame(boot.regimes.all.df[, 1], boot.regimes.all.df[, 2], sqrt(diag(boot.cov)), stringsAsFactors = FALSE)
colnames(q06.params.display.df) <- c("Parameter", "Estimate", "St Dev")



p.val.display <- ifelse(p.val.q06 ==  0, "$<$ machine precision", paste0("= ", signif(p.val.q06, 3)))
note.1 <- paste0("$\\chi_{5}^{2}$ Wald statistic on all params equal to $0$: ", round(chi.sq.stat.q06, 2), " (p ", p.val.display, ").") 


# Thanks to https://stackoverflow.com/questions/6163823/r-xtable-caption-or-comment
dfList <- list(q06.params.display.df)
#attr(dfList, "message") <- c("A caption", "Which can have multiple lines")
attr(dfList, "message") <- c(note.1)

cat(print(xtableList(dfList, digits = 3, display = rep("g", 1 + ncol(q06.params.display.df))), include.rownames = FALSE), 
      file = paste0(results.dir, "q06.tex"))














