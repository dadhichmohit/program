corr1<-function(directory,thersold=150){

count<-0
p<-numeric()

for(id in 1:332)
{
	id<-sprintf('%03d',id)
	string<-paste("D:/",directory,"/",id,".csv",sep="")
	mydata<-na.omit(read.csv(string))
	
	if(nrow(mydata)>thersold)
	{
		count<-count+1
		p[count]<-cor(mydata$sulfate,mydata$nitrate)
	}



}

p

}

+## function corr 
corr <- function(directory, threshold = 0) {
       file_names <- list.files(directory)
       ans <- vector()
       for(i in 1:332)
       {
               addr <- paste(directory,file_names[i],sep ="/")
               data <- na.omit(read.csv(addr))
               if(nrow(data)>threshold)
               {
                       ans <- c(ans,cor(data$sulfate,data$nitrate))
               }
       }
       return(ans)
}
