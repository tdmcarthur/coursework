
# 

## source("/Users/travismcarthur/git/coursework/aae-637/paper/initial-data-setup.r")
## source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-bootstrap.r")
## source("/Users/travismcarthur/git/coursework/aae-637/paper/gme-control-code.r")

add.family.labor.to.hired.labor <- FALSE

target.top.crop.number <- 3

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
if (add.family.labor.to.hired.labor) {
  J <- 5
} else {
  J <- 6
}
# Standard J is 5 now that HH and hired labor are treated as the same
# But 6 if add.family.labor.to.hired.labor == FALSE



do.regimes <- TRUE
# {}

functional.form <- "SGM" # OR TRANSLOG
#functional.form <- "TRANSLOG"

do.tobit <- TRUE
# Necessary


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
#demand.var.trim.quantile <- 1


local.source.evaluation <- FALSE
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

if (add.family.labor.to.hired.labor) {
  GAMS.projdir <-  "/home/k/kzaman/TravisImInYourInternets/gamsdir/projdir/"
} else {
  GAMS.projdir <-  "/home/k/kzaman/TravisImInYourInternets/gamsdir/projdir2/"
}

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

seed.file <- paste0(GAMS.projdir, "bootstrap-seed.txt")
# This file starts as a completely empty text file. 
# It can be produced by "touch" in bash

seed <- readLines(seed.file)
cat("\n1", file = seed.file, append = TRUE)

# bootstrap.iter <- 1
bootstrap.iter <- sum(as.numeric(seed), na.rm = TRUE)
# This starts the very first iteration as numeric(0), which actually sums to 0, so we are good
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"))
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
nrow(firm.df)
nrow(inputs.df)
length(unique(inputs.df$folio))


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
  source(paste0(code.dir, "build-model-extract-parcels.r"))
#}
#if (functional.form =="SGM") {
#  source(paste0(code.dir, "sur-var-building.r"), local=local.source.evaluation)  
    if (synthetic.data) {
    source(paste0(code.dir, "synthetic-data.r"), local=local.source.evaluation)
  }
#}

# If want to make censoring plots:
# source("/Users/travismcarthur/git/coursework/aae-637/paper/analyze-summary-stats.r")


source(paste0(code.dir, "GAMS-construction-functions.r"))


if (functional.form =="TRANSLOG") {

cost.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)


#share.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[!grepl("cost", 
#  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)

#share.err.endpoint <- 1.5 
share.err.endpoint <- 2

cost.err.support <- seq(from = -cost.err.endpoint, to = cost.err.endpoint, length.out=3)

# -round( max(combined.df$cost) * 5 )

share.err.support <- seq(from = -share.err.endpoint, to = share.err.endpoint, length.out=3)

other.param.endpoint <- round( max(abs(coef(linear.sur.est.region))) * 3 , digits=1)

other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=5)

}


if (functional.form =="SGM") {
  other.param.endpoint <- round( max.abs.other.param * 2 , digits=1)

  other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=3)
  # NOTE: I changed this to 3
}


linear.GAMS.output <- TRUE
non.linear.GAMS.output <- TRUE
# linear.GAMS.output <- FALSE



if (only.cost.fn) {
  demand.eqns <- demand.eqns[length(demand.eqns)]
  demand.eqns.nonlinear <- demand.eqns.nonlinear[length(demand.eqns.nonlinear)]
}




if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "GAMS-linear-construction.r"))
}

if (functional.form =="SGM" & !start.nonlin.from.ignorance) {
  source(paste0(code.dir, "sgm-GAMS-linear-construction.r"))
}



# system(paste0("cd ", GAMS.projdir, "\n", "ls" ) )

if (functional.form =="TRANSLOG") {
run.linear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "GMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
   " Ps=0 suppress=1")
}

if (functional.form =="SGM") {
run.linear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
   " Ps=0 suppress=1")
}

if (!start.nonlin.from.ignorance) {
  system(run.linear.from.shell)
}
#stop("END!")
# next

# elapsed 0:08:19.548
# elapsed 0:08:26.802


theta.param.support <- qlnorm(seq(.1, .999, length.out=13), meanlog= 0, sdlog = 1.5)
theta.param.support <- theta.param.support/mean(theta.param.support)
xi.param.support <- theta.param.support
#xi.param.support <- c(-8, 1, 10)
# NOTE: Changing support dratically

# plot(c(0,13), c(0,13))
# rug(theta.param.support, col="red")

if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "GAMS-nonlinear-construction.r"))
}

if (functional.form =="TRANSLOG") {
run.nonlinear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
   " Ps=0 suppress=1")
}


# In fact, we need the linear construction file to be created
# even if we "start nonlinear from ignorance"
# since the nonlinear relies on it for the var names.
# Can just run it a few secs and stop.

if (functional.form =="SGM") {
  source(paste0(code.dir, "sgm-GAMS-nonlinear-construction.r"))
}

if (functional.form =="SGM") {
run.nonlinear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "sgmGMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
   " Ps=0 suppress=1")
}




system(run.nonlinear.from.shell)

do.regimes <- TRUE


if ( do.regimes) {
  
  cat("The value of J is", J, "\n")

  n.regime.groups <- 6
  
  # This point right below here relies on there being on 
  # prep-for-sgm-GAMS-regimes-construction.r not having been run
  # Maybe want   source(paste0(code.dir, "build-model-extract-parcels.r"))   here
  
  # source(paste0(code.dir, "regimes-cluster-calc.r"))
  
  source(paste0(code.dir, "regimes-full-power-set.r"))
  
  # source(paste0(code.dir, "GAMS-multinomial-logit-construction.r"))
  mle.GAMS.output <- TRUE
  source(paste0(code.dir, "GAMS-multinomial-logit-construction-all-regimes.r"))
  
  
  run.multinom.logit.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
    GAMS.exe.path, " ", 
    "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
    formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
    " Ps=0 suppress=1")

  system(run.multinom.logit.from.shell)
  
  
  set.exp.correction.as.q07 <- TRUE
  normalize.cond.exp.coefs <- TRUE
  
  cat("The value of J is", J, "\n")
  # NOTE: A major thing that occurs with this file below is we get
  # J <- J + 1
  # which is used in every other operation below
  linear.GAMS.output <- FALSE
  source(paste0(code.dir, "prep-for-sgm-GAMS-regimes-construction.r"))
  
  cat("The value of J is", J, "\n")
  
  # This below is to re-construct the nonlinear equations with the last q0[0-9] ready to be replaced
  # by the conditional expectation correction
  non.linear.GAMS.output <- FALSE
  source(paste0(code.dir, "sgm-linear-sur-building.r"), local = local.source.evaluation) 
  source(paste0(code.dir, "sgm-GAMS-linear-construction.r"))
  source(paste0(code.dir, "sgm-GAMS-nonlinear-construction.r"))
  
   start.nonlin.regimes.from.ignorance <- FALSE
  #start.nonlin.regimes.from.ignorance <- TRUE
  # global.max.seed <- 0
   het.tech <- FALSE
   
   cat("The value of J is", J, "\n")
  
  source(paste0(code.dir, "sgm-GAMS-regimes-construction.r"))
  

  run.nonlinear.regimes.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
     GAMS.exe.path, " ", 
     "sgmGMEnonlinearRegimes", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
     " Ps=0 suppress=1")

  system(run.nonlinear.regimes.from.shell)
  
  # options decimals = 7;
  # ----  28655 VARIABLE byyalt1.L             =    0.0000001  parameters to be estimated



}




#time.counter <- c(time.counter, Sys.time())
#save(time.counter, file=paste0(GAMS.projdir, strsplit(target.crop, " ")[[1]][1], 
#  "bootstrapcounter.Rdata"))


#}


# }





# source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-postestimation.r")

# old: rvhess = 100
# old: rvstlm = 1.1

# Old-ish: lfmxns = 20000
# Old-ish: lfnicr = 10000
# Old-ish: lfstal = 10000



#load("/Users/travismcarthur/Desktop/gamsdir/projdir/Trigobootstrapcounter.Rdata")
#diff(time.counter)
#hist(diff(time.counter))




