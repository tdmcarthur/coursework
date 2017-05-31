


##### START FOR NONLINEAR::::



combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  

log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r



if ( !only.cost.fn) {
  for (i in 1:N) {
	combined.df[, paste0("dem", i)] <- with(combined.df, eval(parse(text=paste0("x", lead.zero(i), "/y01" ) ) ))
  }

  combined.df[, paste0("dem", N+1)] <- E.y01.data
  
} else {
  combined.df[, paste0("dem", 1)] <- E.y01.data
}




# combined.df <- scale(combined.df, center=FALSE)

top.before.data <- c(
"sets ",
paste0("  t number of observations  / 1 * ", nrow(combined.df), " /"),
paste0("  d number of variables in datafile  / 1 * ", ncol(combined.df), " /"),
paste0("  ss dimension of S matrix / 1 * ", N-1, "/"),
paste0("  sss second dimension of S matrix / 1 * ", N-1, "/"),
paste0("  cc dimension of C matrix / 1 * ", J, "/"),
paste0("  ccc second dimension of C matrix / 1 * ", J, "/"),
# "  m number of points in interval z / 1 * ", length(other.param.support), "/",
# "  j number of points in interval v / 1 * 3 /;",
"",
#"alias (s, ss);",
"",
"parameter",
"  datafile(t,d)   raw data")

# cost.dep.line <- "  cost(t)          log of cost"

# cost.share.parm.lines <- paste0("  s", 1:length(S.n),"(t)          cost share ", 1:length(S.n))

combined.df.GAMS <- make.GAMS.data(combined.df)

combined.df.GAMS <- c(combined.df.GAMS, ";")

# TRANSLOG colnames(combined.df)[colnames(combined.df)=="ln.E.data"] <- "cost"

data.declaration.lines <-  c( paste0(colnames(combined.df), "(t)"), paste0("xi", lead.zero(N)))




# TODO: do I need   indic(k)        copy of set k? Seems no.

top.before.data <- c(top.before.data,
  data.declaration.lines,
#   "  MM              number of points in support",
#   "  JJ              number of points in support",
  ";",
  "",
#   "MM=", length(other.param.support), ";",
#   "JJ=3;", # Eliminated theta here
  "", 
  paste0("xi", lead.zero(N), "=1;"),
  "table datafile(t,d)"
)




data.alloc.lines <- c()

for (i in 1:ncol(combined.df) ) {
  data.alloc.lines <- c(data.alloc.lines, 
    paste0(colnames(combined.df)[i], "(t) = datafile(t,\"", i, "\");")
  )
}


#other.param.support cost.err.support share.err.support

greeks.support.simple.lines <- paste0(
"parameter inputmean", lead.zero(1:N), "    input mean value", lead.zero(1:N),  "\n",
"/", "\n",
mean.of.inputs, "\n",
"/;", "\n"
)


#mean.of.inputs
#psi
#beta
#delta
#eta



all.params <- unique(unlist(str_extract_all(unlist(demand.eqns.nonlinear), 
"(xi.[0-9][0-9])|(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

# (psi.[0-9][0-9])| (beta.[0-9][0-9])| (delta.[0-9][0-9])| (eta.[0-9][0-9])|

all.params <- all.params[!grepl(paste0("xi.", lead.zero(N)), all.params)]

all.params <- gsub("([.])|( )", "", all.params)



# TRANSLOG all.eqns <- c("cost", paste0("s", 1:length(S.n)))
all.eqns <- paste0("dem", 1:length(demand.eqns.nonlinear))


cov.var.declarations <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.declarations.mat <- matrix(cov.var.declarations, nrow=length(all.eqns))

diag(cov.var.declarations.mat) <- ""
cov.var.declarations <- cov.var.declarations.mat
cov.var.declarations  <- cov.var.declarations[cov.var.declarations!= ""]  
  
cov.var.declarations <- paste0("  ", cov.var.declarations,  "    SUR covar parameter")

if (!do.SUR) { cov.var.declarations <- c() }
  

variable.declaration.lines <- c("variables",
  paste0("  ", all.params, "   parameters to be estimated"),
ifelse(concave.in.prices,  "  Smat(ss,sss)   S matrix to make cost function concave", ""),
ifelse(concave.in.prices,   "  SmatT(sss,ss)   transpose of S matrix to make cost function concave", ""),
ifelse(convex.in.f.inputs,  "  Cmat(cc,ccc)   C matrix to make cost function convex in fixed inputs", ""),
ifelse(convex.in.f.inputs,  "  CmatT(ccc,cc)   transpose of C matrix to make cost function convex in fixed inputs", ""),
#  "  errorrelax(t) small value to accomodate the zero error adding up restriction",
  cov.var.declarations,
#   paste0("  p", all.params, "(m)    probability corresponding param"),
#   paste0("  w", all.eqns, "(t,j)    probability corresponding error term"),
  "residual(t)",
  "  sse           nonlin least sq objective value",
  "",
#  "positive variable",
#  paste0("  p", all.params, "(m)"),
#  paste0("  w", all.eqns, "(t,j)"), # eliminated thetas here
  ";"
)




objective.fn.lines <- c("object.. sse =e= sum(t, sqr(residual(t)));")





#add.data.subscripts <- function(x) {
#  for (i in 1:99) {
#    ii <- formatC(i, width = 2, flag = "0")  
#    x <- gsub(paste0("w", ii), paste0("w", ii, "(t)"), x)
#    x <- gsub(paste0("q", ii), paste0("q", ii, "(t)"), x)
#    x <- gsub(paste0("x", ii), paste0("x", ii, "(t)"), x)
#    x <- gsub(paste0("y", ii), paste0("y", ii, "(t)"), x)
#  }
#  x
#}


# S.n.GAMS <- lapply(S.n, FUN=add.data.subscripts)

# S.n.GAMS <- lapply(S.n.GAMS, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))



# TRANSLOG S.n.linear <- lapply(S.n, FUN=function(x) gsub(
# TRANSLOG   pattern="[)] [/] [(][(]w[0-9][0-9].*$", replacement=")", x=x))

# TRANSLOG S.n.linear <- lapply(S.n.linear, FUN=add.data.subscripts)

# TRANSLOG S.n.linear <- lapply(S.n.linear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

# TRANSLOG S.n.GAMS <- gsub(" [*] theta[0-9][0-9]", "", S.n.linear)




model.restrictions <- list()


GAMS.demand.eqns.nonlinear <- lapply(demand.eqns.nonlinear, FUN=add.data.subscripts)

GAMS.demand.eqns.nonlinear <- lapply(GAMS.demand.eqns.nonlinear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))
GAMS.demand.eqns.nonlinear <- lapply(GAMS.demand.eqns.nonlinear, FUN=function(x) gsub(pattern="[{^]", replacement="**", x=x))


model.restrictions <-c("fit(t).. dem1(t)  =e=",
                       strwrap( GAMS.demand.eqns.nonlinear, indent=12, exdent=19, width=80),
                       "        + residual(t);")



covar.SUR.mat<- matrix("", ncol=length(all.eqns), nrow=length(all.eqns))

for ( i in 1:length(all.eqns)) {
  for ( j in 1:length(all.eqns)) {
    covar.SUR.mat[i,j] <- paste0( "restrcov", i, j, "..        ",
      "0 =e= surdelta", i, j, " * sqrt( ( sum(t, sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], 
      "(t, j)) * sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], "(t, j))) / ", nrow(combined.df), ") * ",
      " ( sum(t, sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], 
      "(t, j)) * sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], "(t, j))) / ", nrow(combined.df), ") ) - ",
      "sum(t, sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], "(t, j)) ",
      " * sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], "(t, j))) / ", nrow(combined.df), ";"
    )
    
  }
}

diag(covar.SUR.mat) <- ""

covar.SUR.v <- c(covar.SUR.mat)

covar.SUR.v <- strwrap( covar.SUR.v, indent=1, exdent=19, width=80)

covar.SUR.lines <- covar.SUR.v

if (!do.SUR) { covar.SUR.lines <- c() }

cov.rest.declarations <- paste0( "restrcov", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )
  
cov.rest.declarations.mat <- matrix(cov.rest.declarations, nrow=length(all.eqns))

diag(cov.rest.declarations.mat) <- ""
cov.rest.declarations <- cov.rest.declarations.mat
cov.rest.declarations  <- cov.rest.declarations[cov.rest.declarations != ""]

if (!do.SUR) { cov.rest.declarations <- c() }



double.s.params<- unique(unlist(str_extract_all(gsub("[.]", "", unlist(demand.eqns.nonlinear)), "s[0-9]{4}")))

double.s.indices <- str_extract_all(double.s.params, "[0-9]{2}")

concave.restriction.defn <- c()

for ( i in 1:length(double.s.indices)) {

 first.ind <- as.numeric(double.s.indices[[i]][1])
 second.ind <- as.numeric(double.s.indices[[i]][2])

  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcave", double.s.params[i], "..      ", double.s.params[i], " =e= - sum(sss, Smat(\"", 
  first.ind - 1, "\",sss)*SmatT(sss,\"", second.ind - 1, "\"));") )
  
#    concave.restriction.defn <- c(concave.restriction.defn,  paste0( 
#    "restrconcavelowertrinonzero", 
#   second.ind, first.ind, "..      ", 1e-08, " =l= sqr(Smat(\"", 
#  second.ind - 1, "\",", "\"", first.ind - 1, "\"));")) 
  # Above is basically specifying the transpose
  
  if (first.ind==second.ind) {next}
  # since we don't want to zero out the diagonal elements
    
  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcavelowertri", 
  first.ind, second.ind, "..      ", 0, " =e= Smat(\"", 
  first.ind - 1, "\",", "\"", second.ind - 1, "\");")) 
  
}

concave.restriction.declare <- c()

for ( i in 1:length(double.s.indices)) {
  concave.restriction.declare <- c(concave.restriction.declare, 
    paste0( "restrconcave", double.s.params[i]))
    
   first.ind <- as.numeric(double.s.indices[[i]][1])
   second.ind <- as.numeric(double.s.indices[[i]][2])
   
#   concave.restriction.declare <- c(concave.restriction.declare, 
#    paste0( "restrconcavelowertrinonzero", second.ind, first.ind))

  if (first.ind==second.ind) {next}
  
  concave.restriction.declare <- c(concave.restriction.declare, 
    paste0( "restrconcavelowertri", first.ind, second.ind))
    
}

Smat.transpose.restriction.defn <- "restrSmattrans(ss,sss).. Smat(ss,sss) =e= SmatT(sss,ss);"

Smat.transpose.restriction.declare <- "restrSmattrans"








double.c.params<- unique(unlist(str_extract_all(gsub("[.]", "", unlist(demand.eqns.nonlinear)), "c[0-9]{4}")))

double.c.indices <- str_extract_all(double.c.params, "[0-9]{2}")

convex.restriction.defn <- c()

for ( i in 1:length(double.c.indices)) {

 first.ind <- as.numeric(double.c.indices[[i]][1])
 second.ind <- as.numeric(double.c.indices[[i]][2])

  convex.restriction.defn <- c(convex.restriction.defn,  paste0( "restrconvex", double.c.params[i], "..      ", double.c.params[i], " =e= sum(ccc, Cmat(\"", 
  first.ind, "\",ccc)*CmatT(ccc,\"", second.ind, "\"));") )
  # Notice no negative since we want this to be _positive_ semidef
  
 #   convex.restriction.defn <- c(convex.restriction.defn,  paste0( 
 #   "restrconvexlowertrinonzero", 
 #  second.ind, first.ind, "..      ", 1e-08, " =l= sqr(Cmat(\"", 
 # second.ind - 1, "\",", "\"", first.ind - 1, "\"));"))
  # Above is basically specifying the transpose
  
  if (first.ind==second.ind) {next}
  # since we don't want to zero out the diagonal elements
    
  convex.restriction.defn <- c(convex.restriction.defn,  paste0( "restrconvexlowertri", 
  first.ind, second.ind, "..      ", 0, " =e= Cmat(\"", 
  first.ind, "\",", "\"", second.ind, "\");")) 
  
}

convex.restriction.declare <- c()

for ( i in 1:length(double.c.indices)) {
  convex.restriction.declare <- c(convex.restriction.declare, 
    paste0( "restrconvex", double.c.params[i]))
    
   first.ind <- as.numeric(double.c.indices[[i]][1])
   second.ind <- as.numeric(double.c.indices[[i]][2])
   
#   convex.restriction.declare <- c(convex.restriction.declare, 
#    paste0( "restrconvexlowertrinonzero", second.ind, first.ind))

  if (first.ind==second.ind) {next}
  
  convex.restriction.declare <- c(convex.restriction.declare, 
    paste0( "restrconvexlowertri", first.ind, second.ind))
    
}

Cmat.transpose.restriction.defn <- "restrCmattrans(cc,ccc).. Cmat(cc,ccc) =e= CmatT(ccc,cc);"

Cmat.transpose.restriction.declare <- "restrCmattrans"





if (!convex.in.f.inputs) {
  convex.restriction.declare <- ""
  Cmat.transpose.restriction.declare <- ""
}

if (!concave.in.prices) {
  concave.restriction.declare <- ""
  Smat.transpose.restriction.declare <- ""
}





equation.declarations <- c(
  "equations",   
  "  object             NLS objective function",
#   gsub("[.]{2}.*$", "", prob.weight.param.lines),
#   gsub("[.]{2}.*$", "",  prob.weight.error.lines),
# TRANSLOG   "restrcosta(t)",
# TRANSLOG   "restrcostb(t)",
#   paste0("restr", 1:length(demand.eqns.nonlinear), "dema(t)"),
#   paste0("restr", 1:length(demand.eqns.nonlinear), "demb(t)"),
  "fit(t)",
  concave.restriction.declare,
  convex.restriction.declare,
  Smat.transpose.restriction.declare,
  Cmat.transpose.restriction.declare,
#  restriction.that.err.sum.to.zero.declare,
#  errorrelaxrestrict.declare,
  cov.rest.declarations,
  ";"
)






set.seed(100)
start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = 1;")
# start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = ", jitter(1), ";")
# start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = 0;")
# added this to have correct (non-zero) starting vals for theta

if ( start.at.true.xi ) {
  start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = ", 
    synthetic.params[grepl("xi", names(synthetic.params)) ][-N], ";")
}



# install.packages("stringr")
library("stringr")





GAMS.linear.results<- readLines(paste0(GAMS.projdir, "sgmNLSlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".lst"))


GAMS.linear.results.params<- GAMS.linear.results[grep("(parameters to be estimated$)|(SUR covar parameter$)", GAMS.linear.results)]

GAMS.linear.results.params <- GAMS.linear.results.params[grep("VARIABLE", GAMS.linear.results.params)]

GAMS.linear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.linear.results.params, "[^ ]*[.]L") )


GAMS.linear.results.params.numbers <- as.numeric(gsub("(  parameters to be estimated)|(  SUR covar parameter)", "",
  str_extract(GAMS.linear.results.params, "([^ ]*  parameters to be estimated)|([^ ]*  SUR covar parameter)") ) )
  
  
combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

for ( i in 1:N) {
  combined.w.params.df[[ paste0("xi", lead.zero(i)) ]] <- 1
}







Smat.start.vals.mat <- read.fwf( 
  file=paste0(GAMS.projdir, "sgmNLSlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".lst"), 
   widths=c(2, rep(12, N-1) ),
    skip = (grep("VARIABLE Smat.L", GAMS.linear.results)+3),  
    nrows= N-1)
    

    
Smat.start.vals.mat <- as.matrix(Smat.start.vals.mat[, -1])
    

Smat.initiation.v <- Smat.start.vals.mat
Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
Smat.initiation.v[is.na(Smat.initiation.v)] <- 0
# sometimes zeros come back as blanks from GAMS, so need above line

Smat.initiation.v <- c(Smat.initiation.v)

if ( any(grepl("(Cmat)|(TIME)", gsub(" ", "", Smat.initiation.v)))) { Smat.initiation.v <- 0 }
# This is to deal with cse when whole matrix is zeroes

Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", Smat.initiation.v, ";")

if(convex.in.f.inputs) {

Cmat.start.vals.mat <- read.fwf( 
  file=paste0(GAMS.projdir, "sgmNLSlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".lst"), 
   widths=c(2, rep(12, J) ),
    skip = (grep("VARIABLE Cmat.L", GAMS.linear.results)+3),  
    nrows= J)
    
Cmat.start.vals.mat <- as.matrix(Cmat.start.vals.mat[, -1])


Cmat.initiation.v <- Cmat.start.vals.mat
Cmat.initiation.v[upper.tri(Cmat.initiation.v)] <- 0
Cmat.initiation.v[ is.na(Cmat.initiation.v)] <- 0
Cmat.initiation.v <- c(Cmat.initiation.v)

if ( any(grepl("TIME", gsub(" ", "", Cmat.initiation.v)))) { Cmat.initiation.v <- 0 }

Cmat.initiation.grid <- expand.grid(1:J, 1:J)

Cmat.initial.values <- paste0("Cmat.L(\"", Cmat.initiation.grid[, 1], "\",\"", Cmat.initiation.grid[, 2], "\") =  ", Cmat.initiation.v, ";")


} else {
  Cmat.initial.values <- ""
}




param.starting.vals <- paste0(GAMS.linear.results.params.names, ".l = ", GAMS.linear.results.params.numbers, ";")



if ( start.nonlin.from.ignorance) {



 start.vals.lines <- c( paste0("xi", lead.zero(1:(N-1)), ".l = 1;"), 
   paste0("  p", all.params[!grepl("xi", all.params)], ".l(m) = 1/MM;"),
   paste0("  p", all.params[grepl("xi", all.params)], ".l(h) = 1/HH;")  )

 

set.seed(global.max.seed)

Smat.initiation.v <- matrix(rnorm((N-1)^2), nrow=N-1)
Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
Smat.initiation.v <- c(Smat.initiation.v)

Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", Smat.initiation.v, ";")


#set.seed(100)

Cmat.initiation.v <- matrix(rnorm(J^2), nrow=J)
Cmat.initiation.v[upper.tri(Cmat.initiation.v)] <- 0
Cmat.initiation.v <- c(Cmat.initiation.v)

Cmat.initiation.grid <- expand.grid(1:J, 1:J)

Cmat.initial.values <- paste0("Cmat.L(\"", Cmat.initiation.grid[, 1], "\",\"", Cmat.initiation.grid[, 2], "\") =  ", Cmat.initiation.v, ";")




if (global.max.seed!=0 ) {



all.params.set.aside <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params.set.aside  <- gsub("([.])|( )", "", all.params.set.aside )


chosen.global.search.params <- rnorm(length(c(all.params.set.aside, paste0("xi", lead.zero(1:N))))) 
chosen.global.search.params <- as.numeric(chosen.global.search.params)
names(chosen.global.search.params) <- c(all.params.set.aside, paste0("xi", lead.zero(1:N)))

synthetic.S.mat <- matrix(0, N-1, N-1)
synthetic.T.mat <- matrix(0, N-1, N-1)

# synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <-  names(  chosen.global.search.params[1:(N*(N-1)/2)] )
# A-ok

synthetic.T.mat[lower.tri(synthetic.T.mat, diag=TRUE)] <- rnorm((N*(N-1)/2))

synthetic.S.mat <-  - synthetic.T.mat %*% t(synthetic.T.mat)

chosen.global.search.params[1:(N*(N-1)/2)] <- synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] 


synthetic.C.mat <- matrix(0, J, J)
synthetic.A.mat <- matrix(0, J, J)

chosen.global.search.params <- chosen.global.search.params[names(chosen.global.search.params)!="c0202"] 

chosen.global.search.params <- c(chosen.global.search.params[1:grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))], 0, 
  chosen.global.search.params[(grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))+1):length(chosen.global.search.params)])

names(chosen.global.search.params)[grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))+1] <- "c0202"
# Because "c0202" appear out of order, must fix it with these two lines above



synthetic.A.mat[lower.tri(synthetic.A.mat, diag=TRUE)] <- rnorm((J*(J+1)/2))

synthetic.C.mat <-   synthetic.A.mat %*% t(synthetic.A.mat)

chosen.global.search.params[grepl("c[0-9]{4}", names(chosen.global.search.params))] <- synthetic.C.mat[lower.tri(synthetic.C.mat, diag=TRUE)] 

chosen.global.search.params <- chosen.global.search.params[!grepl(paste0("xi", lead.zero(N)), names(chosen.global.search.params))]

chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] <- rlnorm(N-1)

while ( any(chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] > 10) ) {
  which.invalid<- which(chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] > 10)
  chosen.global.search.params[grepl("xi", names(chosen.global.search.params))][which.invalid] <- rlnorm(length(which.invalid))
}



chosen.global.search.params[grepl("(b[0-9][0-9])|((by[0-9][0-9]))", names(chosen.global.search.params))] <- 
  rlnorm(sum(grepl("(b[0-9][0-9])|(by[0-9][0-9])", names(chosen.global.search.params))))


chosen.global.search.params[grepl("byy", names(chosen.global.search.params))] <- 
  - rlnorm(sum(grepl("byy", names(chosen.global.search.params)))) / 100000
  # byy tends to have very low values in estimation, so divide by 100000

# NOTE: Some of these params are chosen to have good theoretical properties,
# and they mimic the properties in the synthetic params


param.starting.vals <- paste0(names(chosen.global.search.params), ".l = ", chosen.global.search.params, ";")

  
}


if (global.max.seed==0) { param.starting.vals<- c() }

cat(param.starting.vals, sep="\n")

# Problem: with random starting values, almost all error terms go as far negi as possible. 
# May be an issue with byy too high, or others too high

}






resid.starting.vals <- E.y01.data - with(combined.w.params.df, 
                                         eval(parse(text = unlist(gsub("[.]", "", demand.eqns.nonlinear)))))



resid.starting.vals.lines <- paste0("residual.L",
                                    "(\"", 1:length(resid.starting.vals), "\") = ", resid.starting.vals, ";")


  
parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
#  paste0("display p", all.params, ".l;"),
#  paste0("display w", all.eqns, ".l;"),
  cov.var.display ,
ifelse(concave.in.prices,   paste0("display Smat.l"), ""),
ifelse(convex.in.f.inputs,  paste0("display Cmat.l"), "")
#  paste0("display errorrelax.l")
  )




if (!convex.in.f.inputs) {
  Cmat.initial.values <- ""
}

if (!concave.in.prices) {
  Smat.initial.values <- ""
}



final.lines <- 
c(
"*Initial conditions",
# paste0("  p", all.params, ".l(m) = 1/MM;"),
# paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
param.starting.vals,
start.vals.lines,
Smat.initial.values,
Cmat.initial.values,
resid.starting.vals.lines,
"* primal approach",
"model nls /all/;",
"options domlim=5000;",
"*options seed=5;",
"*options iterlim=0;",
"*option bratio=1;",
"options iterlim=90000;",
"options reslim=470000;",
"*options work=900000;",
"* conopt optimizer option is below",
"*options solprint=off, nlp=conopt;",
"*options  nlp=conopt;",
"options solprint=off;",
"NLS.OPTFILE=1; ",
"* OPTION NLP=MINOS5;",
" OPTION NLP=CONOPTD;",
"$inlinecom /* */",
"",
"/* Turn off the listing of the input file */",
"$offlisting",
"",
"/* Turn off the listing and cross-reference of the symbols used */",
"$offsymxref offsymlist",
"",
"option",
"    limrow = 0,     /* equations listed per block */",
"    limcol = 0,     /* variables listed per block */",
"    solprint = off,     /* solver's solution output printed */",
"    sysout = off;       /* solver's system output printed */",
" ",
"solve nls using nlp minimizing sse; ",
"options decimals = 7;"
)
# Help from http://support.gams.com/doku.php?id=gams:how_do_i_reduce_the_size_of_my_listing_.lst_file
# on reducing size of list file

if (!convex.in.f.inputs) {
 convex.restriction.defn  <- ""
 Cmat.transpose.restriction.defn <- "" 
}

if (!concave.in.prices) {
 concave.restriction.defn  <- ""
 Smat.transpose.restriction.defn <- "" 

}


completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
#  param.support.simple.lines, " ", 
  greeks.support.simple.lines, " ",
#  vsupport.lines, " ", 
#  CE.q.support.lines, " ",
#  non.theta.param.support.lines, " ",  # Eliminated thetas here
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
# TRANSLOG  model.restrictions.cost, " ", 
#  prob.weight.param.lines, " ", 
#  prob.weight.error.lines, " ", 
  concave.restriction.defn, " ",
  Smat.transpose.restriction.defn, " ",
  convex.restriction.defn,
  Cmat.transpose.restriction.defn,
#  restriction.that.err.sum.to.zero.defn, " ",
#  errorrelaxrestrict.defn, " ",
  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines 
)




if (non.linear.GAMS.output) { 

  cat(completed.GAMS.file, 
    file=paste0(GAMS.projdir, "sgmNLSnonlinear", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms"), 
    sep="\n")
  
}




# "(recall that uninitialized parameters take on value zero)."
# http://www.me.utexas.edu/~bard/LP/LP%20Handouts/gams_intro.pdf

# ** Warning **  Memory Limit for Hessians exceeded.
# You can use the Conopt option "rvhess"

# rvhess = maxdouble




