

code.dir.control <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

if (Sys.info()['sysname']=="Linux") {
  code.dir.control <- "/home/k/kzaman/TravisImInYourInternets/git/coursework/aae-637/paper/"
}
# 

for (top.control.bootstrap.iter in 1:100) {
  source(paste0(code.dir.control, "max-entropy-bootstrap.r"))
}

# source("/Users/travismcarthur/git/coursework/aae-637/paper/initial-data-setup.r")


# load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata")


#target.top.crop.number <- 1

#source("/Users/travismcarthur/git/coursework/aae-637/paper/build-model-extract-parcels.r")

# If want to make censoring plots:
# source("/Users/travismcarthur/git/coursework/aae-637/paper/analyze-summary-stats.r")


#source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-construction-functions.r")

#source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-linear-construction.r")

#source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-nonlinear-construction.r")

#source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-postestimation.r")











