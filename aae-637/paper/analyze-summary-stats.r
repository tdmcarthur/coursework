

#saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

#saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF.Rdata"
saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"



load(saved.workspace.path)


inputs.df$x19.codigo.saved <- inputs.df$x19.codigo
#inputs.df$x19.codigo <- as.character(inputs.df$x19.codigo.saved)

inputs.df$x19.codigo <- as.character(inputs.df$x19.codigo)

inputs.df$x19.codigo[inputs.df$x19.codigo %in% 
  c("Maiz Choclo", "Maiz blando (dulce, blanco, chuspillo)", 
  "Maiz duro (cristalino, cubano)        ")] <- "Maiz combined"


inputs.df$x19.codigo[inputs.df$x19.codigo %in% 
  c("Cebada (berza) ", "Cebada en grano         " )] <- "Cebada combined"

inputs.df$x19.codigo <- factor(inputs.df$x19.codigo)

top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]

# for (target.crop in top.crops) {




region.inputs <- toupper(inputs.df$zona.agroproductiva)
region.inputs[region.inputs=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region.inputs <- gsub("Ú", "U", region.inputs)
region.inputs <- gsub(" *$", "", region.inputs)
region.inputs <- gsub(" ", ".", region.inputs)
region.inputs <- make.names(region.inputs)

#region.inputs[region.inputs %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ", "PAMPAS.DE.MOXOS")] <- "CHACO"
#region.inputs[region.inputs %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR", "YUMGAS.DEL.NORTE", "YUNGAS.DE.NORTE")] <- "YUNGAS"

# OK, we are going to avoid the groupings that we had for the actual estimation in
# order to get a better disaggregation


region.inputs[region.inputs %in% c("YUMGAS.DEL.NORTE", "YUNGAS.DE.NORTE")] <- "YUNGAS.DEL.NORTE"

region.inputs[region.inputs =="ALTIPANO.CENTRAL"] <- "ALTIPLANO.CENTRAL"

region.inputs <- factor(region.inputs)

inputs.df$zona.agroproductiva.cleaned <- region.inputs

full.sample.fert.use.df <- aggregate(inputs.df$x19.fertilizante.cantidad.kg, by=list(folio=inputs.df$folio), FUN=sum) 

colnames(full.sample.fert.use.df)[2] <- "fert.use.kg"

full.sample.fert.use.df <- merge(full.sample.fert.use.df, 
  inputs.df[!duplicated(inputs.df[, "folio"]), c("folio", "zona.agroproductiva.cleaned")])
# For some reason we still have 2 "bad" values for agroproductive zone, where 
# a given firm is listed as having two distinct zones, but we have dealt
# with that by having duplicated(inputs.df[, "folio"]) instead of 
# duplicated(inputs.df[, c("folio", "zona.agroproductiva.cleaned")])


table(region.inputs)


fert.region.use.tab <-  data.frame(round(prop.table(table(
  full.sample.fert.use.df$zona.agroproductiva.cleaned,
  full.sample.fert.use.df$fert.use.kg > 0), margin=1), digits=2)[, 2])
names(fert.region.use.tab) <- "prop.firms.use.fert"

fert.region.use.tab[, "num.firms"] <- 
  table(full.sample.fert.use.df$zona.agroproductiva.cleaned)

fert.region.use.tab




num.of.top.crops <- 5

top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]

# for (target.crop in top.crops) {

censored.cols.ls <- list()
fert.intensity.unconditional.ls <- list()
fert.intensity.conditional.ls <- list()
full.summary.stats.ls <- list()
number.obs.ls <- list()



for ( i in 1:num.of.top.crops) {


target.crop <- top.crops[i]
print(target.crop)

firm.df <- inputs.df[inputs.df$x19.codigo == target.crop & inputs.df$x19.produccion.obtenidad.kg>0 &
!is.na(inputs.df$x19.produccion.obtenidad.kg), ]

# Above, we are eliminating zero-harvest cases, mostly so we can take log

price.to.trim <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg",
   "x19.plagicidas.bs.kg", "hourly.wage",  "hourly.tractor.rental" )

# firm.df <- firm.df[!is.na(firm.df$hourly.tractor.rental), ]
# only kills 2 obseravtions for maiz and zero for Barley

price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=0.99) )
price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)
firm.df <- firm.df[price.trim.criteria, ]

uncensored.cost <- apply(firm.df[, c(
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", "tractor.hrs.final")], 
  # x107.hrs.tractor.spread
  1, FUN=function(x) {sum(x)!=0}
)

print(table(uncensored.cost))

#firm.df<- firm.df[uncensored.cost, ]
# try to see what happens when we eliminate censoring

number.obs.ls[[target.crop]] <- nrow(firm.df)

censored.cols <- c(sum(firm.df$x19.fertilizante.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.sem.comprada.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.abono.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.plagicidas.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$paid.hours.spread!=0)/nrow(firm.df),
sum(firm.df$tractor.hrs.final!=0)/nrow(firm.df)
)

censored.cols.ls[[i]] <- censored.cols

fert.intensity.unconditional.ls[[i]] <- 
  mean(firm.df$x19.fertilizante.cantidad.kg/firm.df$x19.superficie.cultivada.hectareas, na.rm=TRUE)

fert.intensity.conditional.ls[[i]] <- 
  median(firm.df$x19.fertilizante.cantidad.kg[firm.df$x19.fertilizante.cantidad.kg>0] / 
    firm.df$x19.superficie.cultivada.hectareas[firm.df$x19.fertilizante.cantidad.kg>0], na.rm=TRUE)
    
cat("number of firms with same crop in multiple plots", "\n")
print(table(table(firm.df$folio)))

w01 = firm.df$x19.fertilizante.bs.kg
w02 = firm.df$x19.sem.comprada.bs.kg
w03 = firm.df$hourly.tractor.rental
w04 = firm.df$x19.plagicidas.bs.kg
w05 = firm.df$hourly.wage
w06 = firm.df$x19.abono.bs.kg
y01 <- log( firm.df$x19.produccion.obtenidad.kg )

q01 = firm.df$x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

firm.df$x19.uso.riego.r = ifelse(firm.df$x19.uso.riego!="Si",  0, 1)

firm.df$ag.fam.labor.equiv.spread.r = firm.df$ag.fam.labor.equiv.spread
firm.df$ag.fam.labor.equiv.spread.r[firm.df$ag.fam.labor.equiv.spread.r == 0] = .5

full.summary.stats.ls[[target.crop]]<- do.call(rbind, lapply(firm.df[, c(
  "x19.produccion.obtenidad.kg",
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", 
  "tractor.hrs.final",
  "x19.fertilizante.bs.kg",
  "x19.sem.comprada.bs.kg",
  "hourly.tractor.rental",
  "x19.plagicidas.bs.kg",
  "hourly.wage",
  "x19.abono.bs.kg",
  "x19.superficie.cultivada.hectareas",
  "x19.uso.riego.r",
  "ag.fam.labor.equiv.spread.r"
  )  ], FUN=summary)
  )


}

censored.df <- do.call(rbind, censored.cols.ls)

colnames(censored.df) <- c("Inorganic Fert", "Seed", "Organic Fert", "Plaguicidas", "Labor", "Tractor Hrs")

barplot(as.matrix(censored.df), beside=TRUE, col=terrain.colors(num.of.top.crops),
  main="Proportion of uncensored observations")

legend("top", top.crops[1:num.of.top.crops], cex=1, 
       fill=terrain.colors(num.of.top.crops))
#cex=0.6

names(fert.intensity.unconditional.ls) <- top.crops[1:num.of.top.crops]
names(fert.intensity.conditional.ls) <- top.crops[1:num.of.top.crops]

# NOTE: Below is mean
unlist(fert.intensity.unconditional.ls)
# NOTE: Below is median
unlist(fert.intensity.conditional.ls)


save.image(file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/2008 summary stats.Rdata")


#load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/2008 summary stats.Rdata")

#

pdf("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/censored-inputs.pdf", width=5, height=5)

par(cex=.6)

barplot(as.matrix(censored.df), beside=TRUE, col=terrain.colors(num.of.top.crops),
  main="Proportion of uncensored observations")


top.crops.english <- top.crops
top.crops.english[top.crops.english=="Papa (patatas) "] <- "Potatoes"
top.crops.english[top.crops.english=="Maiz combined"] <- "Maize"
top.crops.english[top.crops.english=="Cebada combined" ] <- "Barley"
top.crops.english[top.crops.english=="Trigo          "] <- "Wheat"
top.crops.english[top.crops.english=="Haba (verde)    "] <- "Fava beans"

legend("topright", top.crops.english[1:num.of.top.crops], cex=1, 
       fill=terrain.colors(num.of.top.crops))

dev.off()

par(cex=1)





fert.ext.margin.agg <- aggregate(inputs.df$x19.fertilizante.cantidad.quintal, by = list(inputs.df$folio), FUN=sum, na.rm=TRUE)

colnames(fert.ext.margin.agg) <- c("folio", "farm.used.fertilizer")
fert.ext.margin.agg$farm.used.fertilizer <- fert.ext.margin.agg$farm.used.fertilizer > 0
 
inputs.for.fert.ext.margin.df <-  merge(inputs.df[!duplicated(inputs.df[, c("folio", "x19.codigo")]),] , fert.ext.margin.agg)


#extensive.margin.by.crop.mat <- as.matrix(with(inputs.for.fert.ext.margin.df[inputs.for.fert.ext.margin.df$x19.codigo %in% top.crops, ],
#  prop.table(table(factor(x19.codigo), farm.used.fertilizer), margin=1)) * 100
#)

extensive.margin.by.crop.mat <- as.matrix(with(inputs.for.fert.ext.margin.df[inputs.for.fert.ext.margin.df$x19.codigo %in% top.crops, ],
  prop.table(xtabs(factor.de.expansión ~ factor(x19.codigo) + farm.used.fertilizer), margin=1)) * 100
)

extensive.margin.by.crop.mat <- extensive.margin.by.crop.mat[, 2:1]

library("stargazer")
library("xtable")

xtab.output <- print(xtable(extensive.margin.by.crop.mat,
  caption="Percentage of farmers using fertilizer by type of crop planted in Bolivia", digits=1),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/bolivia-fert-use-by-crop.tex")
      )

fert.ext.margin.agg.w.weights <- merge(fert.ext.margin.agg, inputs.df[!duplicated(inputs.df[ , c("folio", "factor.de.expansión")]) , 
  c("folio", "factor.de.expansión")])

overall.extensive.margin.mat <- t(as.matrix(prop.table(xtabs(factor.de.expansión ~ farm.used.fertilizer, data=fert.ext.margin.agg.w.weights) )) * 100
)

overall.extensive.margin.mat <- overall.extensive.margin.mat[, 2:1, drop=FALSE]

xtab.output <- print(xtable(overall.extensive.margin.mat,
  caption="Percentage of farmers using fertilizer in Bolivia, overall", digits=1),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/bolivia-fert-use-overall.tex")
      )




# Check if survey weights changes things:


# Thanks to http://r.789695.n4.nabble.com/How-do-I-get-a-weighted-frequency-table-td3774665.html




# number of farms that have the same crop in multiple plots
       
num.same.crops <- 0 





for ( i in inputs.df$folio) {

  if( any(duplicated(inputs.df[inputs.df$folio==i & inputs.df$x19.codigo==top.crops[1],  "x19.mes.siembra"])) ) {
  
    x19.fertilizante.cantidad
    num.same.crops <-  num.same.crops + 1
  }

}



inputs.df[inputs.df$folio==i, ]

test.agg <- aggregate( x19.fertilizante.cantidad ~ folio + x19.codigo + x19.mes.siembra, data=inputs.df, 
  FUN=function(x) ifelse(any(x==0) & any(x!=0), 1, 0) )

test.agg[test.agg$x19.fertilizante.cantidad==1, ]

test.agg2 <- aggregate( x19.fertilizante.cantidad ~ folio + x19.codigo + x19.mes.siembra, data=inputs.df, 
  FUN=function(x) ifelse(sum(duplicated(x))!=(length(x)-1), 1, 0) )


t(inputs.df[inputs.df$folio=="2020142016" & inputs.df$x19.codigo=="Papa (patatas) ", ])





x19.abono.cantidad                    
x19.abono.bs 

sort(unique(inputs.df$x19.abono.bs ))

table(inputs.df$x19.abono.cantidad > 0 & (is.na(inputs.df$x19.abono.bs.quintal) | inputs.df$x19.abono.bs.quintal==0))
table(inputs.df$x19.abono.cantidad > 0 )

table(inputs.df$x19.fertilizante.cantidad > 0 & (is.na(inputs.df$x19.fertilizante.bs.quintal) | inputs.df$x19.fertilizante.bs.quintal==0))
table(inputs.df$x19.fertilizante.cantidad > 0 )

table(inputs.df$x19.plagicidas.cantidad > 0 & (is.na(inputs.df$x19.plagicidas.bs.quintal) | inputs.df$x19.plagicidas.bs.quintal==0))
table(inputs.df$x19.plagicidas.cantidad > 0 )

#!! prices not imputing properly - should take price from the same farm


inputs.df[inputs.df$folio=="2020142016" , c("x19.fertilizante.bs.quintal", "x19.fertilizante.bs.kg")]



sum(inputs.df$x19.fertilizante.cantidad) / sum(inputs.df$x19.superficie.cultivada.hectareas)


##################

require("rgdal") # requires sp, will use proj.4 if installed
  require("maptools")
  require("ggplot2")
  require("plyr")
  
setwd("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/1155/")

# Thanks to https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
nation.shp = readOGR(dsn=".", layer="bolivia")
nation.shp@data$id = rownames(nation.shp@data)
nation.points = fortify(nation.shp, region="id")
fert.price.agg <- aggregate( x19.fertilizante.bs.kg ~ provincia.full, data=inputs.df, FUN=median)
wage.agg <- aggregate( hourly.wage ~ provincia.full, data=inputs.df, FUN=median)
tractor.price.agg <- aggregate( hourly.tractor.rental ~ provincia.full, data=inputs.df, FUN=median)
# TODO: may want to restrict this to "itself" imputation levels


nation.shp@data$provincia.full <- paste0(substr( nation.shp@data$DCODE, 4, 5), substr( nation.shp@data$DCODE, 7,8))

nation.shp@data <- merge(nation.shp@data, wage.agg, all.x=TRUE)
nation.shp@data <- merge(nation.shp@data, fert.price.agg, all.x=TRUE)
nation.shp@data <- merge(nation.shp@data, tractor.price.agg, all.x=TRUE)


nation.df = join(nation.points, nation.shp@data, by="id")

require(gridExtra)

plot1 <- ggplot(nation.df) + 
  aes(long,lat,group=group,fill=hourly.tractor.rental) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()
  
plot2 <-ggplot(nation.df) + 
  aes(long,lat,group=group,fill=x19.fertilizante.bs.kg) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()

plot3 <-ggplot(nation.df) + 
  aes(long,lat,group=group,fill=hourly.wage) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()

grid.arrange(plot1, plot2, plot3, ncol=2)


# How to save to disk
 # draw your plots
# plot1 <- ggplot(...) # this specifies your first plot
# plot2 <- ggplot(...) # this specifies your second plot
# plot3 <- ggplot(...) # this specifies your third plot

 #merge all three plots within one grid (and visualize this)
# grid.arrange(plot1, plot2, plot3, nrow=3) #arranges plots within grid

 #save
# g <- arrangeGrob(plot1, plot2, plot3, nrow=3) #generates g
# ggsave(file="whatever.pdf", g) #saves g




  
   +
  scale_fill_brewer("Utah Ecoregion")











