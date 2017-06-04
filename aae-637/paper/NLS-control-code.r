
code.dir.control <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

if (Sys.info()['sysname']=="Linux") {
  code.dir.control <- "/home/k/kzaman/TravisImInYourInternets/git/coursework/aae-637/paper/"
}
# 

for (top.control.bootstrap.iter in 1:200) {
  source(paste0(code.dir.control, "init-NLS.r"))
}

# source("/Users/travismcarthur/git/coursework/aae-637/paper/NLS-control-code.r")