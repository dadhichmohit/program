rankall<-function(outcome,num="best"){

# Validation of outcome
	outcome<-tolower(outcome)
	input<-function(outcome){
		if(outcome=="heart attack")
		{input<-11;	return(input)}
		if(outcome=="heart failure")
		{input<-17;return(input)}
		if(outcome=="pneumonia")
		{input<-23;return(input)}
		else{stop("invalid outcome") }
			}
# Defines the column to read via read.csv
		query<-input(outcome) 
	
# Readding of file 
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[ ,c(2,7,query)]
	names(data)<-c("hospital","state","mrate")

# Define empty vectors to store the state particular information
	
		p<-vector(); g<-vector();f<-vector()
# Caculate the number of rows in the main database
			row<-nrow(data)
# Read the state particular information and store them in the empty vectors(p,d,g)
	
	for(i in 1:row){
		
	if(data[i,3]!="Not Available"){	# Checking whether entities belong to the desired state
	 	f<-c(f,data[i,2])
		p<-c(p,data[i,1])	# Hospital Name saved
		g<-c(g,data[i,3])	# Mortality rate for the particular outcome saved
			}	
			}
# Create the dataframe of desired values
		g<-as.numeric(g)
		mydata<-data.frame("hospital"=p,"state"=f,"mrate"=g)
		mydata<-mydata[with(mydata, order(state,mrate,hospital,decreasing=F)),]
		data<-split(mydata$hospital,mydata$state)


# Define the conditions on rank varriable and assigning solution	
			k<-character()
	for(i in 1:length(data)){
		l <- length(data[[i]])
		
		k<-{
		if(num=="best"){
		rank<-1
		if(l<rank){
		k<-c(k,NA)
		}
		else{
		b<-as.character(data[[i]][[rank]])	
		k<-c(k,b)
		}
		}

		else if(num=="worst"){
		rank<-l
		b<-as.character(data[[i]][[rank]])	
		k<-c(k,b)
		}
		
		else if(num %in% 1:l){
		rank<-num
		b<-as.character(data[[i]][[rank]])	
		k<-c(k,b)
		}

		
		else {k<-c(k,NA)}
		}#if else ends
		
		}#for loop ends	

	string<-sort(levels(as.factor(mydata$state)))
  	solution<-data.frame("hospital"=k,"state"=string)
		solution
}






