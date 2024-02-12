
#df<- chirps_1981_2010
#scale<-  15
#start_date<- "1981-01-01"  ## minimum date in the data
#End_date<- "2010-12-31"  ## maximun date in the data


aggragate_function <- function(df,scale,start_date,End_date) {
   
   l<- names(df)
   #dd2<- dd
   dd2<-data.frame(date=seq.Date(as.Date(start_date),as.Date(End_date),by=scale))
   for (i in l ) {
      
      if (i!= "date"){ 
         dfp<-df[,c("date",i)]
         colnames(dfp)<- c("date","var")
         dfp<- dfp[!is.na(dfp$var),]
         if (nrow(dfp)==0){
            message("missing data in station", i)
         }
         else{ 
            step <- as.numeric(as.Date(dfp$date) - as.Date(dfp$date[1])) %/% scale ## define the step 
            TimeP <- as.Date(dfp$date[1]) + scale * step
            dd<-aggregate(var ~ TimeP,dfp, sum) ## use the aggregate function
            dd<-dd[,c("TimeP","var")]
            colnames(dd)<- c("date",i) # change the variable name into station name
            week<- cbind(dd2,dd)
            dd2<- week
         }
      }
      dd2<- dd2[!duplicated(lapply(dd2, summary))]
   }
   return(dd2)
   
}

#################
chirps_1981_2010 <- read.csv("Data/CHIRPS/chirps_1981_2010.csv") 

## aggregate from daily to weekly time scale
chirps_weekly_1981_2010<- aggragate_function(chirps_1981_2010,7,"1981-01-01", "2010-12-31")

