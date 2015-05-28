#    inputs.df[, paste0(target.input, ".impute.level")] %in% "itself"


# load("/Users/travismcarthur/Desktop/gamsdir/projdir2/R workspace before test of correction factor model.Rdata")



library("stringr")



include.cost.fn <- TRUE
source(paste0(code.dir, "sgm-linear-sur-building.r"))  
# Must get the cost function if I shaved it off
include.cost.fn <- FALSE

cost.fn.expr <- demand.eqns[[7]]

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params <- gsub(" ", "", all.params)

start.params <- rep(0, length(all.params))
names(start.params) <- all.params

summary(with(as.list(start.params), eval(parse(text=cost.fn.expr))))

#'MODE_ID'    is 1,2,3,4
#'CHOICE' 0 -1 -- this is "mode"

nalts <- 2
mode_id <- rep(1:nalts, length(x01)) 
#mode <- ifelse(rep(x01, each=2) > 0, 1, 0)

mode <- c()
# Really hacky, below:
for ( i in 1:length(x01)) {
  if (x01[i]>0) {  
    mode <- c(mode, c(0, 1) )
  } else {
    mode <- c(mode, c(1, 0) )
  }
}


if (FALSE) {

nalts <- 3
mode_id <- rep(1:nalts, length(x01)) 
mode <- ifelse(rep(x01, each=2) > 0, 1, 0)

mode <- c()
#  Really hacky, below:
for ( i in 1:length(x01)) {
  if (x01[i]>0) {  
    mode <- c(mode, c(0, 1, 0) )
  } else {
    if (x02[i]>0) {
      mode <- c(mode, c(1, 0, 0) )
    } else {
      mode <- c(mode, c(0, 0, 1) )
    }
  }
}

}










start.params <- jitter( rep(0, length(all.params)) )
names(start.params) <- all.params
start.params.ls<- list(start.params, start.params)



with(as.list(start.params), eval(parse(text=cost.fn.expr)))



cond_logit_llf <- function(b0) {
#   global rhsvar nalts mode_id mode ;

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(as.data.frame(matrix(start.params, ncol=nalts)))
  for ( i in 1:nalts) {
    names(start.params.ls[[i]]) <- all.params
  }
  
  #start.params.ls<- list(start.params[1:length(all.params)], start.params[(length(all.params)+1):length(start.params)])
#print(start.params.ls)
 
  part_2=0
  inner_1=0
  inner_2=0
  for (i in 1:nalts)  {     # %*** Create LLF denom. ***         
        inner_2=inner_2 + exp(with(as.list(start.params.ls[[i]]), eval(parse(text=cost.fn.expr)))) #exp("EVALUATE i'th expression" ) #    %*** (T x 1) ***     
  }
#  print(log(inner_2))
  for (j in 1:nalts)  {                         #%*** No. of Alternatives ***
 
      dummy_j=mode[mode_id == j] # ; %**Dummy var.,jth model chosen **
      
      part_2=part_2 + dummy_j * log(inner_2);
   	  inner_1=inner_1 + dummy_j * with(as.list(start.params.ls[[j]]), eval(parse(text=cost.fn.expr))) # ("EVALUATE j'th expression") #; %***Sum if j consumed (T x 1) ***
   }  
#   print(inner_1)
   
   sum(inner_1-part_2)
   
}

set.seed(200)

start.val.f <- jitter( rep(0, length(all.params)*nalts), factor= 0.0001 )

cond_logit_llf( start.val.f  )
cond_logit_llf( start.val.f + 0.000001 )

set.seed(200)
multi.logit.results <- optim(par=jitter( rep(0, length(all.params)*nalts), factor= 0.0001 ), fn=cond_logit_llf, method = "BFGS", control=list(trace=10, fnscale= -1, maxit=10000000, ndeps = rep(1e-10, length(start.val.f ))))





cond_logit_predict <- function(b0, outcome) {

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(as.data.frame(matrix(start.params, ncol=nalts)))
  for ( i in 1:nalts) {
    names(start.params.ls[[i]]) <- all.params
  }

  inner_2 <- 0
  for (i in 1:nalts)  {     # %*** Create LLF denom. ***         
        inner_2=inner_2 + exp(with(as.list(start.params.ls[[i]]), eval(parse(text=cost.fn.expr))))
  }
  
  # Just show prob of being in 2nd category at first (i.e., use fert)
  exp( with(as.list(start.params.ls[[ outcome  ]]), eval(parse(text=cost.fn.expr))) ) / inner_2
}  


#summary(cond_logit_predict(results$par, outcome=2))

#predictions <- cond_logit_predict(results$par)



summary(cond_logit_predict(multi.logit.results$par, outcome=2))

predictions <- cond_logit_predict(multi.logit.results$par, outcome=2)

table( x01 >0, predictions> mean(predictions) )

table( x01 >0, predictions> 0.5 )

cor( x01 >0, predictions> mean(predictions) )


L_C = -length(x01) * log(nalts)   #; % Greene 7th ed. p. 807

1 - cond_logit_llf(multi.logit.results$par)/L_C





J <- J + 1
# This make room for the correction factor

source(paste0(code.dir, "sgm-linear-sur-building.r"))  





P01 <- cond_logit_predict(multi.logit.results$par, outcome=1)
P02 <- cond_logit_predict(multi.logit.results$par, outcome=2)

P01 <- ifelse(P01 < 0.001, 0.001, P01)
P02 <- ifelse(P02 < 0.001, 0.001, P02)
P01 <- ifelse(P01 > 0.999, 0.999, P01)
P02 <- ifelse(P02 > 0.999, 0.999, P02)
# This prevents divide-by-zero


RInd01 <- as.numeric(x01 ==0)
RInd02 <- as.numeric(x01 > 0)

correction.factor <- paste0("lambda", lead.zero(1:nalts), " * ( log(P", lead.zero(1:nalts), ") / (1-P", lead.zero(1:nalts), 
  ") ) * (P", lead.zero(1:nalts), " - RInd", lead.zero(1:nalts), ")", collapse=" + " )
# 2nd line of eqn 31 in Dubin & Mcfadden (1984)

correction.factor  <- paste0("( ", correction.factor, " )")


all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params <- gsub(" ", "", all.params)






demand.eqns.alt.ls <- list()

for ( targ.regime in 1:nalts) {

  demand.eqns.alt <- demand.eqns.nonlinear

  for (targ.param in all.params) {
    demand.eqns.alt <- gsub(paste0(targ.param, " "), 
      paste0(targ.param, "R", lead.zero(targ.regime), " "), demand.eqns.alt, fixed=TRUE)
      # The addition of space is so that e.g. c.02 doesnt replace the c.02.02 erroneously, 
    if (targ.param %in% paste0("b.", lead.zero(1:N)) ) {
      demand.eqns.alt <- gsub(paste0(targ.param, "/"), 
      paste0(targ.param, "R", lead.zero(targ.regime), "/"), demand.eqns.alt, fixed=TRUE)
    
    }
  }
  
  demand.eqns.alt.ls[[targ.regime]] <- demand.eqns.alt

}

demand.eqns.alt.mat <- do.call(cbind, demand.eqns.alt.ls)




demand.eqns.alt <- apply(demand.eqns.alt.mat, 1, function(x) {
  paste0( "RInd", lead.zero(1:nalts), " * ( ", x, " ) ", collapse= " + ")
} )

demand.eqns.alt <- as.list(demand.eqns.alt)

demand.eqns.alt <- gsub(paste0("q", lead.zero(J)), correction.factor,  demand.eqns.alt)

# save.image("/Users/travismcarthur/Desktop/gamsdir/projdir2/R workspace right before test of correction factor model.Rdata")


lambda.param.support <- c(-100, 0, 100)



start.nonlin.from.ignorance <- FALSE



run.nonlinear.regimes.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "sgmGMEnonlinearRegimes", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
   " Ps=0 suppress=1")


system(run.nonlinear.regimes.from.shell)










all.params.now <- unique(unlist(str_extract_all(unlist(demand.eqns.nonlinear[[2]]), 
"(xi.[0-9][0-9])|(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params.now <- gsub(" ", "", all.params.now)


all.params.now.vals <- runif(length(all.params.now))
all.params.now.vals <- rep(all.params.now.vals, 3)
names(all.params.now.vals) <- paste0(all.params.now, 
  rep(c("", "R01", "R02"), each=length(all.params.now) ) )
  
all.params.now.vals <- c(all.params.now.vals, lambda01=0, lambda02=0) 

q07 <- 0
  
summary( with(as.list(all.params.now.vals), eval(parse(text=demand.eqns.nonlinear[[2]])) ) )


summary( with(as.list(all.params.now.vals), eval(parse(text=demand.eqns.alt[[2]])) ) )
























combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.nonlinear.results.params.names)) {
  combined.w.params.df[[ GAMS.nonlinear.results.params.names[i] ]] <- GAMS.nonlinear.results.params.numbers[i]
}

# This part above seems not to really do anything

#for ( i in 1:N) {
#  combined.w.params.df[[ paste0("xi", lead.zero(i)) ]] <- 1
#}
























set.seed(200)
optim(par=jitter( rep(0, length(all.params)*nalts), factor= 0.0001 ), fn=cond_logit_llf, method = "Nelder-Mead", control=list(trace=10, fnscale= -1, maxit=10000000, ndeps = rep(1e-8, length(start.val.f ))))

set.seed(200)
optim(par=jitter( rep(0, length(all.params)*nalts), factor= 0.0001 ), fn=cond_logit_llf, method = "SANN", control=list(trace=10, fnscale= -1))

cond_logit_predict <- function(b0) {

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(as.data.frame(matrix(start.params, ncol=nalts)))
  for ( i in 1:nalts) {
    names(start.params.ls[[i]]) <- all.params
  }

  inner_2 <- 0
  for (i in 1:nalts)  {     # %*** Create LLF denom. ***         
        inner_2=inner_2 + exp(with(as.list(start.params.ls[[i]]), eval(parse(text=cost.fn.expr))))
  }
  
  # Just show prob of being in 2nd category at first (i.e., use fert)
  exp( with(as.list(start.params.ls[[2]]), eval(parse(text=cost.fn.expr))) ) / inner_2
}  


summary(cond_logit_predict(results$par))

predictions <- cond_logit_predict(results$par)

table( x01 >0, predictions> mean(predictions) )

table( x01 >0, predictions> 0.5 )

cor( x01 >0, predictions> mean(predictions) )


L_C = -length(x01) * log(nalts)   #; % Greene 7th ed. p. 807

1 - cond_logit_llf(results$par)/L_C









