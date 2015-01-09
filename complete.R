complete<-function(directory,id=1:332)
{
  count<-0
  
  p<-numeric()
  d<-numeric()	
  for(id in id)
  {
    
    
    id<-sprintf('%03d',id)
    string<-paste("D:/",directory,"/",id,".csv",sep="")
    data<-read.csv(string)
    good<-complete.cases(data)
    data<-data[good,]
    number<- nrow(data)
    count<-count+1
    d[count]<-id
    p[count]<-number
    s<-data.frame("ids"=d,"nobs"=p)		
    
  }
  s
  
}