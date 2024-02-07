library(Metrics) 
library(stringr)
#library(zoo)

#threshold<- 1 value of rainfall to be consider as rain ##
#Gauge<- ddd ## Historical weather data (stations)
#Estimate<-dd1 # Gridded products     
Qualitative-analysis<- function(threshold,Gauge,Estimate){
   s1<-s2<-s3<- 0
   df0<- data.frame()
   df10<- data.frame()
   Names<- colnames(Gauge)
   for ( i in Names){
      dataset<- Gauge[,c(i)]
      dataset<-data.frame(var1=dataset)
      colnames(dataset) <- c("var1")
      dataset1<- Estimate[,c(i)]
      dataset1<-data.frame(var1=dataset1)
      colnames( dataset1) <- c("var2")
      dataset$var2<- dataset1$var2
      dd<- dataset[!is.na(dataset$var2),]
      
      if (nrow(dd)==0) {
         message("NA in Estimate data")
      }
      
      else { 
         ###################################################################
         ### Categorical measure ######
         dataset<- dd
         dataset$filter <- ifelse(dataset$var1>threshold,1,0)
         rain_days_data <- dataset[dataset$filter==1,]
         No_rain_days_data <- dataset[dataset$filter==0,]
         ### calculate the HIT
         HITS <- rain_days_data[rain_days_data$var2>threshold, ] 
         Hit<- (nrow(HITS)/nrow(rain_days_data))*100
         ## calculate the MISS
         MISSES<- rain_days_data[rain_days_data$var2<=threshold, ] 
         MISS<- (nrow(MISSES)/nrow(rain_days_data))*100
         
         ### Calculate False Alarm
         FAd <-  No_rain_days_data[ No_rain_days_data$var2>threshold,] 
         False_Alarm<- (nrow(FAd)/nrow(No_rain_days_data))*100
         # Probability of Detection 
         POD <- Hit/(Hit+MISS)
         #print(POD)
         ### False Alarm Ratio
         FAR<- False_Alarm/(Hit+False_Alarm)
         ### Crirical sucess Index
         CSI<- Hit/(Hit+MISS+False_Alarm)
         ## Frequency bias index
         FBI<- (Hit+False_Alarm)/(Hit+MISS)
         
         ######################################################
         # Qualitative Result
         df<- data.frame(Station=i,POD=c(POD),FAR=c(FAR),CSI=c(CSI),FBI=c(FBI))
         
         d_f<-rbind(df0,df)
         df0<- d_f
      }
   }
   return(d_f)
}

Quantitative_analysis<- function(threshold,Gauge,Estimate){
   s1<-s2<-s3<- 0
   df0<- data.frame()
   df10<- data.frame()
   Names<- colnames(Gauge)
   for ( i in Names){
      dataset<- Gauge[,c(i)]
      dataset<-data.frame(var1=dataset)
      colnames(dataset) <- c("var1")
      dataset1<- Estimate[,c(i)]
      dataset1<-data.frame(var1=dataset1)
      colnames( dataset1) <- c("var2")
      dataset$var2<- dataset1$var2
      dd<- dataset[!is.na(dataset$var2),]
      
      if (nrow(dd)==0) {
         message("NA in Estimate data")
      }
      
      else { 
         
         ### Quantitative measure ######
         dataset<- dd
         ######################################################
         ### Ratio Bias and Relative Bias
         for(ii in 1:length(dataset$var1)){
            s1=s1+dataset$var1[ii]-dataset$var2[ii]
            s2=s2+dataset$var1[ii]
            s3=s3+dataset$var2[ii]
         }
         # Quantitative result
         
         df1<-data.frame(Station=i,RMSE=rmse(dataset$var1,dataset$var2),
                         CC=cor(dataset$var1, dataset$var2),
                         MAE=mae(dataset$var1,dataset$var2),
                         Rbias= (s1/s2)*100,
                         Bias=s3/s2)
         
         d_f1<-rbind(df10,df1)
         df10<- d_f1
      }
   }
   return(d_f1)
}







ff<-coord_region1[coord_region1$country=="Tanzania",]
ll<- na.omit(ff$name)
### preparing data for analysis and comparison 
chirps_1981_2010 <- read_csv("chirps_1981_2010.csv")
chirps_Month_1981_2010 <- read_csv("chirps_Month_1981_2010.csv")
Agera5_1980_2010 <- read_csv("Agera5_1980_2010.csv")
Agera5_Month_1980_2010 <- read_csv("Agera5_Month_1981_2010.csv")
prec_1981_2010<- read_csv("prec_1981_2010.csv")
prec_Month_1981_2010<- read_csv("prec_Month_1981_2010.csv")
dd<-prec_1981_2010[,ll]
ddm<- prec_Month_1981_2010[,ll]
dd1<-chirps_1981_2010[,ll]
dd2<-Agera5_1980_2010[,ll]
dd3<-chirps_Month_1981_2010[,ll]
dd4<-Agera5_Month_1980_2010[,ll]
# Southern data 
chirps_1998_2018 <- read_csv("chirps_1998_2018.csv")
prec_1998_2018<- read_csv("prec_1998_2018.csv")
dds<- prec_1998_2018[,c(2)]
dd_c<- chirps_1998_2018[,c(3)]

### subset the data in terms of wet season
dff<-ddd
dff1<-dd1
dff2<-dd2
dfff<-ddT
date<-data.frame(date=seq.Date(as.Date("1983-01-01"),as.Date("2010-12-31"),by="day"))
#dff1<- cbind(date,dff1)
date[c("year",'month', 'day')] <- str_split_fixed(date$date, '-', 3)
dff$month<-date$month 
dff1$month<-date$month 
dff2$month<-date$month 
dfff$month<-date$month 
l<-   dff$month %in% c("03","04","05","06","07","08","09","10")  #Ghana
#l<-   dff$month %in% c("01","02","03","12")
#dff$f[l]<- dff$var1[l]
dfs<-dff[l,]
dfss<-dfs[,-c(19)]
dfs1<- dff1[l,]
dfss1<- dfs1[,-c(19)]
dfs11<- dff2[l,]
dfss11<- dfs11[,-c(19)]
dfff<- dfff[l,]
dfff<- dfff[,-c(19)]
#### Tamsat data preparation
Tamsat_1983_2010 <- read_csv("Tamsat_1983_2010.csv")

ddT<-Tamsat_1983_2010[,ll]

dd<- prec_1981_2010[prec_1981_2010$date>="1983-01-01",]

ddd<-dd[,ll]

dG_1<-Evaluation_Rainfall_product(1,dfss,dfff)
write.xlsx(dG_1, file = "Ghana_Tamsat.xlsx", 
           sheetName="quantitative_ws", append=TRUE)
write_csv(dG_1,"Qualitative_Tanzania_Ag.csv")

dd4[,c("Musoma")]

########################## weekly####################"

dd_prec<-read_csv("dprec_week_1981_2010.csv")
dd_ch<-read_csv("chirps_week_1981_2010.csv")

dd_Ag <-read_csv("Agera5_week_1981_2010.csv")
dd_Ag$Musoma<-NA
dd_Ag$Pokuase<-NA

dd_prec_TS<-read_csv("Prec_TS_week_1983_2010.csv")
dd_TS <-read_csv("Tamsat_week_1983_2010.csv")

ddd<-dd_prec_TS[,ll]
ddd1<- dd_TS[,ll]

dG_1<-Quantitative_analysis(1,ddd,ddd1)
write.xlsx(dG_1, file = "Tanzania_week_Tamsat.xlsx", 
           sheetName="qualitative", append=TRUE)


########################## pendantal####################"

ff<-coord_region1[coord_region1$country=="Ghana",]
ll<- na.omit(ff$name)

dd_prec<-read_csv("dprec_Pendantal_1983_2010.csv")
dd_ch<-read_csv("chirps_Pendantal_1981_2010.csv")

dd_Ag <-read_csv("Agera5_Pendantal_1981_2010.csv")
dd_Ag$Musoma<-NA
dd_Ag$Pokuase<-NA

dd_prec_TS<-read_csv("Prec_TS_Pendantal_1983_2010.csv")
dd_TS <-read_csv("Tamsat_Pendantal_1983_2010.csv")

ddd<-dd_prec_TS[,ll]
ddd1<- dd_TS[,ll]

dG_1<-Evaluation_Rainfall_product(1,ddd,ddd1)
write.xlsx(dG_1, file = "Ghana_Pendantal_Tamsat.xlsx", 
           sheetName="qualitative", append=TRUE)
