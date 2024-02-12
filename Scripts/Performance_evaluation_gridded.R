library(Metrics) 
library(stringr)
#library(zoo)

#threshold<- 1 we assume that it is raining if rain value is greater than 1  ##
#Gauge<- ## Historical weather data (stations)
#Estimate<- ## Gridded products     
Qualitative_analysis<- function(threshold,Gauge,Estimate){
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
         ### Critical success Index
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



coord_region<- read.csv("Data/coord_region.csv")

### preparing data for analysis and comparison 

prec_1981_2010<-  read.csv("Data/Gauge data/prec_1981_2010.csv") ## read gauge data
l<- names(prec_1981_2010)
### filter station names coordinate present in the data from 1981 to 2010 
coord_region1<- data.frame()
for (i in l){
   coord_region2<- coord_region[coord_region$name==i,]
   d<- rbind(coord_region1,coord_region2)
   coord_region1<- d
   
}


ff<-coord_region1[coord_region1$country=="Tanzania",] 
#ff<-coord_region1[coord_region1$country=="Ghana",]
ll<- na.omit(ff$name)


########  CHIRPS daily time scale  ######################
chirps_1981_2010 <- read.csv("Data/CHIRPS/chirps_1981_2010.csv") 

dd<- prec_1981_2010[,ll]
dd1<- chirps_1981_2010[,ll]
dc<- Quantitative_analysis(1,dd,dd1) ## Quantitative result
dc1<- Qualitative_analysis(1,dd,dd1) ## Qualitative result

########  AgARA5 daily time scale  ######################

Agera5_1981_2010 <- read.csv("Data/AgERA5/Agera5_1981_2010.csv") ## read data
dd2<- Agera5_1981_2010[,ll]
dc_ag<- Quantitative_analysis(1,dd,dd2) ## Quantitative result
dc_ag1<- Qualitative_analysis(1,dd,dd2) ## Qualitative result


########  TAMSAT daily time scale  ######################
Tamsat_1983_2010 <- read.csv("Data/TAMSAP/Tamsat_1983_2010.csv")

ddT<-Tamsat_1983_2010[,ll]
dd<- prec_1981_2010[prec_1981_2010$date>="1983-01-01",]
ddd<-dd[,ll]
dc_TS<- Quantitative_analysis(1,ddd,ddT) ## Quantitative result
dc_TS1<- Qualitative_analysis(1,ddd,ddT) ## Qualitative result


