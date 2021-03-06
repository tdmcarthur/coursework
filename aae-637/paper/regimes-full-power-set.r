

#posi.vars.df <- stacked.firm.df[, c("x19.sem.comprada.cantidad.kg.posi",
#  "tractor.hrs.final.posi",
#  "x19.plagicidas.cantidad.kg.posi",
#  "paid.hours.spread.posi",
#  "x19.abono.cantidad.kg.posi",
#  "x19.fertilizante.cantidad.kg.posi")]

# install.packages("arules")
# library("arules")



#firm.df$x19.fertilizante.cantidad.kg.posi <- firm.df$x19.fertilizante.cantidad.kg > 0 
#firm.df$x19.sem.comprada.cantidad.kg.posi <- firm.df$x19.sem.comprada.cantidad.kg > 0
#firm.df$tractor.hrs.final.posi <- firm.df$tractor.hrs.final > 0
#firm.df$x19.plagicidas.cantidad.kg.posi <- firm.df$x19.plagicidas.cantidad.kg > 0
#firm.df$paid.hours.spread.posi <- firm.df$paid.hours.spread > 0
#firm.df$x19.abono.cantidad.kg.posi <- firm.df$x19.abono.cantidad.kg > 0
  
#posi.vars.df <- firm.df[, c("x19.sem.comprada.cantidad.kg.posi",
#  "tractor.hrs.final.posi",
#  "x19.plagicidas.cantidad.kg.posi",
#  "paid.hours.spread.posi",
#  "x19.abono.cantidad.kg.posi",
#  "x19.fertilizante.cantidad.kg.posi")]

posi.vars.df <- data.frame(x01.posi = x01 > 0)
for ( i in 1:N) {
 posi.vars.df[, paste0("x", lead.zero(i), ".posi")] <- get(paste0("x", lead.zero(i))) > 0
}

# ftable(posi.vars.df)

posi.vars.class <- do.call(expand.grid, rep( list(c(FALSE, TRUE)), N ) )
colnames(posi.vars.class) <- paste0("x", lead.zero(1:N), ".posi")

posi.vars.regime <- rep(NA, nrow(posi.vars.df))

for ( i in 1:nrow(posi.vars.df)) {
  for ( j in 1:nrow(posi.vars.class)) {
    if (all(posi.vars.df[i, ] == posi.vars.class[j, ])) {
     posi.vars.regime[i] <- j
     next
    }
  }
  #cat(0)
}
# Ok kinda slow (one min), but this was the quickest to code





# trans1 <- as(posi.vars.df, "transactions")

# d_jaccard <- dissimilarity(trans1)
# hc <- hclust(d_jaccard)
# plot(hc)
## get 20 clusters and look at the difference of the item frequencies (bars)
## for the top 20 items) in cluster 1 compared to the data (line)


# regime.cut <- cutree(hc, n.regime.groups)

# colnames(posi.vars.df) <- c("Seed", "Tractor", "Plag", "Labor", "Abono", "Fert")
# NOTE: THIS ORDERING OF INPUTS ABOVE in the commented-out code
# IS "WRONG" - inconsistent with ordering in underlying dataframe
# as.data.frame( ftable(posi.vars.df[test.cut==1, ]) )

# regime.cut.tab <- table(regime.cut)


# all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
# "(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
#   ))
# )


# set.seed(100)
# for ( i in names(regime.cut.tab)[regime.cut.tab <= length(all.params)] ) {
#   regime.cut[ regime.cut==i ] <- sample(
#     x= as.numeric( names(regime.cut.tab)[regime.cut.tab > length(all.params)] ), 
#     size=sum(regime.cut==i), replace=TRUE
#   )
# }
# This above re-allocates a regime if the number of obs falls below the num of params, 
# so we do not get negative degrees of freedom
set.seed(100)

#table(regime.cut)


nalts <- length(unique(posi.vars.regime))
#mode_id <- rep(1:nalts, length(x01)) 
#mode <- ifelse(rep(x01, each=2) > 0, 1, 0)

#mode <- c()
#for ( i in 1:length(x01)) {
#  mode.vec <- rep(0, nalts)
#  mode.vec[ regime.cut[i] ] <- 1
#  mode <- c(mode, mode.vec)
#}

regime.cut <- as.numeric(factor(posi.vars.regime))
# This is a hack to ensure that regime.cut has values
# one to nalts rather than "skipping" a value
# because of the regime re-allocation
# Otherwise we could have a mode vector, defined below,
# that is all zeros.
# This was a major source of misestimation before, which
# I did not catch immediately.


for ( i in 1:nalts) {
  assign( paste0("mode", formatC(i, flag = "0", width = max(nchar(nalts)))), ifelse( regime.cut == i, 1, 0) )
}









