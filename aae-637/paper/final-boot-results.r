# library("data.table")


get.bootstraps <- function(directory, param.subset.pattern) {

  bootstrap.files <- list.files(directory, full.names = TRUE)

  boot.params.ls <- list()

  for (targ.file in seq_along(bootstrap.files)) {
    boot.params.df.temp <- read.csv(bootstrap.files[targ.file], col.names = c("param", "value"), stringsAsFactors = FALSE)
    names(boot.params.df.temp)[2] <- paste0("value.", gsub("[^0-9]", "", bootstrap.files[targ.file]))
    cat(targ.file, base::date(), "\n")
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




boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes", "(^xi)|(^lambda)")
boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/simple nonlinear", "(^xi)|(^lambda)")

boot.regimes.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/regimes", "(^xi)")
boot.simple.df <- get.bootstraps("/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/simple nonlinear", "(^xi)")


apply(boot.regimes.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)
apply(boot.simple.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)

apply(boot.regimes.df[, -1] - boot.simple.df[, -1], 1, FUN = quantile, probs = c(0.05, 0.95), na.rm = TRUE)






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
# This starts the very first iteration as numeric(0), which actually sums to 0, so we are good
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"), local = local.source.evaluation)
# Above is a bit hacky


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) ), envir = .GlobalEnv))
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


boot.regimes.df

combined.df <- boot.dataset(seed.number = 0, target.top.crop.number = 3)

regime.key.df <- unique(combined.df[, c("posi.vars.regime", "regime.cut")])
regime.key.df$regime.cut <- paste0("lambda", lead.zero(regime.key.df$regime.cut))

boot.regimes.df$param




