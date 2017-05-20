


additional.cost <- function(seed.number = 0, target.top.crop.number, regimes.eqn = TRUE, set.params.zero = "NONE", result.filename ) {

  GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir3/"
  mle.GAMS.output <- TRUE
  
  demand.eqn.num <- 6
  
  # WARNING: This is a hack for   # Error in paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop,  : 
  #object 'GAMS.projdir' not found
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

do.tobit <- TRUE


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
  include.cost.fn <- TRUE
  # NOTE: I changed this to include cost function
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

n.regime.groups <- 6

source(paste0(code.dir, "regimes-full-power-set.r"), local = local.source.evaluation)

source(paste0(code.dir, "GAMS-construction-functions.r"), local = local.source.evaluation)

source(paste0(code.dir, "GAMS-multinomial-logit-construction-all-regimes.r"), local = local.source.evaluation)

regime.cut <- as.numeric(factor(posi.vars.regime))
combined.df$posi.vars.regime <- factor(posi.vars.regime)

combined.df$regime.cut <- regime.cut

if (regimes.eqn) {

  
  set.exp.correction.as.q07 <- TRUE
  normalize.cond.exp.coefs <- TRUE
  
  cat("The value of J is", J, "\n")
  # NOTE: A major thing that occurs with this file below is we get
  # J <- J + 1
  # which is used in every other operation below
  linear.GAMS.output <- FALSE
  source(paste0(code.dir, "prep-for-sgm-GAMS-regimes-construction.r"), local = local.source.evaluation)
  
  cat("The value of J is", J, "\n")
  
  # This below is to re-construct the nonlinear equations with the last q0[0-9] ready to be replaced
  # by the conditional expectation correction
  non.linear.GAMS.output <- FALSE
  
  
if (functional.form =="SGM") {
  other.param.endpoint <- round( max.abs.other.param * 2 , digits=1)

  other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=3)
  # NOTE: I changed this to 3
}

  
theta.param.support <- qlnorm(seq(.1, .999, length.out=13), meanlog= 0, sdlog = 1.5)
theta.param.support <- theta.param.support/mean(theta.param.support)
xi.param.support <- theta.param.support
  

  
  source(paste0(code.dir, "sgm-linear-sur-building.r"), local = local.source.evaluation) 
  source(paste0(code.dir, "sgm-GAMS-linear-construction.r"), local = local.source.evaluation)
  start.nonlin.from.ignorance <- TRUE 
  global.max.seed <- 0
  # Setting global.max.seed to 0 prevents the thing from trying to do the constrained optim because
  # of the start.nonlin.from.ignorance <- TRUE
  source(paste0(code.dir, "sgm-GAMS-nonlinear-construction.r"), local = local.source.evaluation)

  
  for ( i in 1:demand.eqn.num) {
    demand.eqns.nonlinear[[i]] <- gsub("q06", correction.factor, demand.eqns.nonlinear[[i]])
  }
}




params.df <- read.csv(result.filename, col.names = c("param", "value"), header = FALSE, stringsAsFactors = FALSE)
GAMS.nonlinear.results.params.full <- params.df$value
names(GAMS.nonlinear.results.params.full) <- params.df$param
GAMS.nonlinear.results.params.full[grepl(set.params.zero, names(GAMS.nonlinear.results.params.full))] <- 0






xi.unity <- rep(1, N)
names(xi.unity) <- paste0("xi", lead.zero(1:N))

non.distort.params <- GAMS.nonlinear.results.params.full[
  !grepl("xi", names(GAMS.nonlinear.results.params.full))]
non.distort.params <- c(non.distort.params, xi.unity)



xi06 <- 1

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(non.distort.params), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
}

ret.ls <- list()

ret.ls[["distort.cost.input.mat"]] <- y01 * distort.cost.input.mat
ret.ls[["non.distort.cost.input.mat"]] <- y01 * non.distort.cost.input.mat
# NOTE: This above relies on some syntactic quirks of R matrices and vectors. Example"
# c(1,2,3) * matrix(1:9, 3, 3)
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos

#summary(E.y01.data * y01)
# total cost based on actual data

#GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1
assign(paste0("xi", lead.zero(N)), 1)
assign(paste0("theta", lead.zero(N)), 1)

# Below is same thing, but non-neg constraints are imposed:

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(non.distort.params), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}


ret.ls[["distort.cost.input.mat.non.neg"]] <- y01 * distort.cost.input.mat
ret.ls[["non.distort.cost.input.mat.non.neg"]] <- y01 * non.distort.cost.input.mat

#summary(  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos
# rowSums(distort.cost.input.mat) - rowSums(non.distort.cost.input.mat)

#summary( (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
#  y01 * rowSums(non.distort.cost.input.mat)    ) 
# Percent increase in (predicted) cost
# Ok, so these values are not all posi since the non-negativity constraints mess up the 
# thing. We can have a few inputs hit non-neg constraints, and then the remaining 
# inputs happen to not result in the non-distort cost being lower than the distort cost.



#### Now just do input demand, not cost:

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <-  y01 * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))


# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  non.distort.cost.input.mat[, i] <-  y01 * with(as.list(non.distort.params), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}

ret.ls[["distort.input.demand.mat"]] <- y01 * distort.cost.input.mat
ret.ls[["non.distort.input.demand.mat"]] <- y01 * non.distort.cost.input.mat


ret.ls[["E.y01.data.actual"]] <-  E.y01.data  # Actual
ret.ls[["E.y01.data.predicted"]] <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
#print( cor(  actual.temp[actual.temp>0] ,  predicted.temp[actual.temp>0]))

ret.ls[["demand.eqns.nonlinear"]] <- demand.eqns.nonlinear
ret.ls[["distort.params"]] <- GAMS.nonlinear.results.params.full
ret.ls[["non.distort.params"]] <- non.distort.params


ret.ls[["data"]] <- combined.df

ret.ls
  
  
}



add.cost.results.regimes <- additional.cost(seed.number = 0, target.top.crop.number = 3, regimes.eqn = TRUE, set.params.zero = "NONE",
result.filename = "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes/sgmGMEnonlinearRegimesCebada00000mean-impute-no-cost-fn-no-SUR-logit-attempt-param-output.txt")

add.cost.results.non.regimes <- additional.cost(seed.number = 0, target.top.crop.number = 3, regimes.eqn = FALSE, set.params.zero = "NONE",
result.filename = "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes/sgmGMEnonlinearRegimesCebada00000mean-impute-no-cost-fn-no-SUR-logit-attempt-param-output.txt")



table(rowSums(add.cost.results.regimes$distort.cost.input.mat) > 
        rowSums(add.cost.results.regimes$non.distort.cost.input.mat))

table(rowSums(add.cost.results.non.regimes$distort.cost.input.mat) > 
        rowSums(add.cost.results.non.regimes$non.distort.cost.input.mat))

with(add.cost.results.regimes, {

  print(summary( (rowSums(distort.cost.input.mat) - 
              rowSums(non.distort.cost.input.mat) ) / 
             rowSums(non.distort.cost.input.mat)  ) )

  print(summary( (rowSums(distort.cost.input.mat.non.neg) - 
              rowSums(non.distort.cost.input.mat.non.neg) ) / 
             rowSums(non.distort.cost.input.mat.non.neg)    ) )
})


with(add.cost.results.non.regimes, {

  print(summary( (rowSums(distort.cost.input.mat) - 
              rowSums(non.distort.cost.input.mat) ) / 
             rowSums(non.distort.cost.input.mat)  ) )

  print(summary( (rowSums(distort.cost.input.mat.non.neg) - 
              rowSums(non.distort.cost.input.mat.non.neg) ) / 
             rowSums(non.distort.cost.input.mat.non.neg)    ) )
})


with(add.cost.results.regimes, {

  print(summary( (rowSums(distort.cost.input.mat) - 
              rowSums(non.distort.cost.input.mat) ) / 
             rowSums(non.distort.cost.input.mat)  ) )

  print(summary( (rowSums(distort.cost.input.mat.non.neg) - 
              rowSums(non.distort.cost.input.mat.non.neg) ) / 
             rowSums(non.distort.cost.input.mat.non.neg)    ) )
})



with(add.cost.results.regimes, {
  #print(cor(E.y01.data.actual, E.y01.data.predicted))
  for ( i in 1:6) {
  print(
  cor(data[, paste0("x", formatC(i, flag = "0", width = 2))], # Actual
      distort.input.demand.mat[, i] # Predicted
  ))
}
})


with(add.cost.results.non.regimes, {
  #print(cor(E.y01.data.actual, E.y01.data.predicted))
  for ( i in 1:6) {
  print(
  cor(data[, paste0("x", formatC(i, flag = "0", width = 2))], # Actual
      distort.input.demand.mat[, i] # Predicted
  ))
}
})

# So yes regimes does predict slightly better


library("stargazer")

results.dir <- "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/tables/"

test <- with(add.cost.results.regimes, {
  df <- data.frame(distort.cost.input.mat.non.neg = rowSums(distort.cost.input.mat.non.neg), 
                non.distort.cost.input.mat.non.neg = rowSums(non.distort.cost.input.mat.non.neg))
  df$perc.diff <- 100 * (df$distort.cost.input.mat.non.neg - df$non.distort.cost.input.mat.non.neg) / df$non.distort.cost.input.mat.non.neg
  df <- df[df$distort.cost.input.mat.non.neg > 0 & df$non.distort.cost.input.mat.non.neg > 0 & df$perc.diff > 0, ]
  
  colnames(df) <- c("Inefficient expenditure", "Efficient expenditure", "Percent difference")
  
  stargazer(df, summary = TRUE, median = TRUE, out = paste0(results.dir, "distort-cost.tex"),
            title = "Excess cost due to allocative inefficiency",
            nobs = FALSE, digits = 2, notes = "The 2008 exchange rate was about 7.4 Bolivianos per USD.")
  
  df
})
















add.cost.results.regimes$distort.cost.input.mat.non.neg[171, ]
add.cost.results.regimes$non.distort.cost.input.mat.non.neg[171, ]


test <- with(add.cost.results.regimes, {
  df <- data.frame(distort.cost.input.mat.non.neg = rowSums(distort.cost.input.mat), 
                non.distort.cost.input.mat.non.neg = rowSums(non.distort.cost.input.mat))
  df <- df[df$distort.cost.input.mat.non.neg > 0 & df$non.distort.cost.input.mat.non.neg > 0, ]
  df$perc.diff <- 100 * (df$distort.cost.input.mat.non.neg - df$non.distort.cost.input.mat.non.neg) / df$non.distort.cost.input.mat.non.neg
  
  print(stargazer(df, summary = TRUE, median = TRUE, type = "text"))
  
  df
})










if (FALSE) {



result.filename <- "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes/sgmGMEnonlinearRegimesCebada00000mean-impute-no-cost-fn-no-SUR-logit-attempt-param-output.txt"
params.df <- read.csv(result.filename, col.names = c("param", "value"), header = FALSE, stringsAsFactors = FALSE)
GAMS.nonlinear.results.params.full <- params.df$value
names(GAMS.nonlinear.results.params.full) <- params.df$param



demand.eqn.num <- 6


xi06 <- 1

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
}


summary(  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos

summary( (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
  y01 * rowSums(non.distort.cost.input.mat)    ) 
# Percent increase in (predicted) cost




summary(E.y01.data * y01)
# total cost based on actual data



#GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1
assign(paste0("xi", lead.zero(N)), 1)
assign(paste0("theta", lead.zero(N)), 1)

# Below is same thing, but non-neg constraints are imposed:

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}


summary(  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos
# rowSums(distort.cost.input.mat) - rowSums(non.distort.cost.input.mat)

summary( (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
  y01 * rowSums(non.distort.cost.input.mat)    ) 
# Percent increase in (predicted) cost
# Ok, so these values are not all posi since the non-negativity constraints mess up the 
# thing. We can have a few inputs hit non-neg constraints, and then the remaining 
# inputs happen to not result in the non-distort cost being lower than the distort cost.



#### Now just do input demand, not cost:

distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))

# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  distort.cost.input.mat[, i] <-  y01 * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(demand.eqn.num), nrow=nrow(combined.df))


# for ( i in 1:(length(all.eqns)-1)) {
for ( i in 1:demand.eqn.num) {
  non.distort.cost.input.mat[, i] <-  y01 * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}


summary(  as.data.frame(distort.cost.input.mat - non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Demand in units

summary(  as.data.frame(distort.cost.input.mat - non.distort.cost.input.mat)/as.data.frame(distort.cost.input.mat) )
# Percent increase in (predicted) demand





# Correlation between actual and predicted values

# for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
for ( i in 1:demand.eqn.num) {
  print(
  cor(get(paste0("x", lead.zero(i))) / y01, # Actual
    with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  ))
}

if (length(demand.eqns.nonlinear) != 7) {stop("do not have the cost fn in the eqn list")}
print(
 cor(E.y01.data, # Actual
   with(as.list(GAMS.nonlinear.results.params.full), 
     eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
))



# Among posi observations

# for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
for ( i in 1:demand.eqn.num) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print( cor(  actual.temp[actual.temp>0] ,  predicted.temp[actual.temp>0], method="spearman"))
  # NOTE: Spearman means that the rank correlation is computed
}

if (length(demand.eqns.nonlinear) != 7) {stop("do not have the cost fn in the eqn list")}
actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print( cor(  actual.temp[actual.temp>0] ,  predicted.temp[actual.temp>0]))

## Now taking logs of posi observations so that large outliers don't rule:


# Among posi observations

# for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
for ( i in 1:demand.eqn.num) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print( cor(  log(actual.temp[actual.temp>0]) ,  log(predicted.temp[actual.temp>0]), use="complete.obs"))
}

if (length(demand.eqns.nonlinear) != 7) {stop("do not have the cost fn in the eqn list")}
actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print( cor(  log(actual.temp[actual.temp>0]) ,  log(predicted.temp[actual.temp>0]), use="complete.obs"))




# Ch-sq on whether good prediction for posi vs. zero


# for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
for ( i in 1:demand.eqn.num) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print(mean(predicted.temp>0))
  print(table(actual.temp>0 ,  predicted.temp>0))
  print( chisq.test(  table(actual.temp>0 ,  predicted.temp>0)))
}

if (length(demand.eqns.nonlinear) != 7) {stop("do not have the cost fn in the eqn list")}
actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print(mean(predicted.temp>0))
print(table(actual.temp>0 ,  predicted.temp>0))
print( chisq.test(  table(actual.temp>0 ,  predicted.temp>0)))


}

