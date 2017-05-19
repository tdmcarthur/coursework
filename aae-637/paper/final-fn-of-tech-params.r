
library("numDeriv")


morishima.e.s <- function(i, j, data, params, cost.fn.string, shadow = FALSE) {

  # All this is from eqn 8 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    w_j <- params[ paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    w_j <- data[, paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
      
#  print(C_i)
#  print(names(w_i))
    
  C_j <- grad(
    temp.deriv.fn, 
    x=w_j, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(j))], 
      data[names(data)!= paste0(which.price, lead.zero(j))] ))  
    
  C_hess <- hessian(
    temp.deriv.fn, 
    x=c(w_i, w_j),
    data=c(params[! names(params) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))], 
      data[! names(data) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))] ))
    
  C_ij <- C_hess[1, 2]
  C_ii <- C_hess[1, 1]
  print(C_hess)
  
  w_i * C_ij / C_j - w_i * C_ii / C_i
  
}

# TODO: Am I properly taking the shadow prices into account? See Kumbhakar 1992
# Seems Ok, once I added it. I.e. shadow = TRUE and Shadow = FALSE are exactly the same.

test <- with(add.cost.results.regimes, {
morishima.e.s(1, 2, as.data.frame(t(colMeans(data))), 
              distort.params, 
              demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
})

morishima.mat <- matrix(NA, ncol=N, nrow=N)



for ( i in 1:N) {
  for (j in 1:N) {
    morishima.mat[i, j] <- with(add.cost.results.regimes, {
      morishima.e.s(i, j, as.data.frame(t(colMeans(data))), 
                                         distort.params, 
                                         demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
    })
    cat(i, j, "\n")
  }
}


colnames(morishima.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Labor", "Organic fert")
rownames(morishima.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Labor", "Organic fert")


library("xtable")

results.dir <- "/Users/travismcarthur/Desktop/Bolivia alloc paper/results/cebada bootstrap/tables/"

xtab.output <- print(xtable(morishima.mat,
  caption = "Morishima elasticties of substitution, evaluated at mean of data. Row is $i$. Column is $j$."), floating = TRUE, caption.placement = "top")
cat(xtab.output, sep = "\n",
      file = paste0(results.dir, "morishima-table.tex")
      )







