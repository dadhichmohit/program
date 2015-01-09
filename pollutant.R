pollutantmean <- function(directory, pollutant, id =1:332) {
 
   if(id[1]>=1&&id[length(id)]<=332)
    {
     possiblepollutants<-c("sulfate","nitrate")
     input<-tolower(pollutant)
     mresult<- input %in% possiblepollutants
      if(mresult==TRUE)
       {Myfactor<-NULL
        for(i in 1:length(id))
        { 
         x<-sprintf("specdata/%03d.csv",id[i])
         con<-read.csv(x)
         Myfactor<-c(Myfactor,con[!is.na(con[,pollutant]),pollutant])
        }
       
        mean(Myfactor)
        
      }
      else
      {
        print("no pollutant of this name exists")
      }
    }
   else
   {
     print("id out of range")
     break;
   }
}
  
