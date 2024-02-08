aggragate_function <- function(df,scale,start_date,End_date){
   
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
            step <- as.numeric(dfp$date - dfp$date[1]) %/% scale ## define the step 
            TimeP <- dfp$date[1] + scale * step
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

#dpre_week<-norm_aggragate(dprec)
#dpre_week1<-norm_aggragate(dChirps)
dpre_week2<- aggragate_function(dAgera5,15,"1981-01-01", "2010-12-31")
#dpre_week3<-norm_aggragate(dTamsat1)