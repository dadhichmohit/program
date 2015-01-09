corre<-function(directory,id=1:332)
{
  count<-0
  a<-1
  p<-numeric()
  d<-numeric()	
  while(a<=length(id))
  {
    
    
    id<-sprintf('%03d',id)
    string<-paste(getwd(),"/",id,".csv",sep="")
    data<-read.csv(string)
    good<-complete.cases(data)
    data<-data[good,]
    number<- nrow(data)
    count<-count+1
    d[count]<-id
    p[count]<-number
    s<-data.frames("ids"=d,"nobs"=p)		
    
    a<-a+1
  }
  s
  
  
}