
## Load library 
library(reshape2)
library(ggplot2)

## read monthly precipitation data from Tanzania and  Ghana
chirp_agrera5 <- read.csv("Data/chirp_agera5.csv")
Sum_Tamsat <- read.csv("Data/TAMSAP/Sum_Tamsat.csv")

# visualize seasonality with box plot  ( pattern comparison of gauge based precipitation data with gridded data)
seasonal_function<- function(dataset,country,product,var1,var2) {
   
   d<- dataset[,c("month",var1,var2)]
   colnames(d)<- c("month","gauge",product)
   d$month <- factor(d$month, levels = month.abb)
   dat.m <- melt(d,id.vars='month', measure.vars=c('gauge',product))
   p <- ggplot(dat.m) +geom_boxplot(aes(x=month, y=value, color=variable))+ylab("Cumulative rainfall (mm)")+labs(title = country)
   p
   
}

seasonal_function(chirp_agrera5,"Ghana","AgERA5","Prec_Ghana","prec_AgERA5_Ghana")

seasonal_function(chirp_agrera5,"Tanzania","AgERA5","prec_Tanz","prec_AgERA5_Tanz")

seasonal_function(Sum_Tamsat,"Ghana","TAMSAT","Prec_Ghana","prec_Tamsat_Ghana")

seasonal_function(Sum_Tamsat,"Tanzania","TAMSAT","prec_Tanz","prec_Tamsat_Tanz")

#cor(dchirp$Sum_Tanz,dchirp$Sum_Tanz_TS)