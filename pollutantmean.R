pollutantmean<- function(directory, pollutant, id = 1:332)
	
		{	
			
		if(id[1]>=1 && id[length(id)]<=332)
		{
			pollutant<-tolower(pollutant)
		if( pollutant=="sulfate" || pollutant=="nitrate")
		{
			id<-sprintf('%03d',id)
		data<-sapply(paste("D:/",directory,"/",id,".csv",sep=""),read.csv)
		Mean<-sapply(data[pollutant, ],mean,na.rm=TRUE)
		print("Mean")
		print(Mean)
		Sum<-sapply(data[pollutant, ],sum,na.rm=TRUE)
		number<-sum(Sum/Mean)
		
		print("Number")
		print(number)
		
		totalsum<-sum(Sum)
		print("TotalSum")
		print(totalsum)
		answer<-totalsum/number
		}
		else
		{ print("no pollutant of this name exists")
		}
		}
		
		else
		{ 
		print("id out of range")
		}
		
		}
