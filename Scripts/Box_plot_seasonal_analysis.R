library(reshape2)
library(ggplot2)
chirp_agrera5 <- read_csv("sum_data_chirp_agera5.csv")
chirp_agrera5$month[chirp_agrera5$month=="jan"]<- "Jan"
chirp_agrera5$month[chirp_agrera5$month=="Sept"]<- "Sep"

dchirp<- chirp_agrera5[,c("month","Sum" , "Sum_Ghana_ch")]

colnames(dchirp)<- c("month","gauge","CHIRPS")
dchirp$month <- factor(dchirp$month, levels = month.abb)
dat.m <- melt(dchirp,id.vars='month', measure.vars=c('gauge',"CHIRPS"))
p <- ggplot(dat.m) +geom_boxplot(aes(x=month, y=value, color=variable))+ylab("Cumulative rainfall (mm)")+labs(title = "Seasonal Analysis with boxplot in Ghana")
p

####################################
data_Tamsat <- read_csv("sum_data_Tamsat.csv")
data_Tamsat$month[data_Tamsat$month=="jan"]<- "Jan"
data_Tamsat$month[data_Tamsat$month=="Sept"]<- "Sep"

dchirp<- data_Tamsat[,c("month","Sum_Tanz","Sum_Tanz_TS")]

colnames(dchirp)<- c("month","gauge","TAMSAT")
dchirp$month <- factor(dchirp$month, levels = month.abb)
dat.m <- melt(dchirp,id.vars='month', measure.vars=c('gauge',"TAMSAT"))
p <- ggplot(dat.m) +geom_boxplot(aes(x=month, y=value, color=variable))+ylab("Cumulative rainfall (mm)")+labs(title = "Seasonal Analysis with boxplot in Tanzania")
p

#cor(dchirp$Sum_Tanz,dchirp$Sum_Tanz_TS)