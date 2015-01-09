best1<-function(state,outcome){

# Validation of outcome
	
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

# Validation of state name
# step 1 - change the class of State column(character) to factor
	
	 data[,2]<-as.factor(data[,2])
	 string<-levels(data[ ,2])
# step 2 - Check whether State name exist
		
	stname<-{if(toupper(state) %in% string)
		{stname <- toupper(state)}
		else stop("invalid state")
		}
# Define empty vectors to store the state particular information
	
		p<-vector(); g<-vector() 
# Caculate the number of rows in the main database
			row<-nrow(data)
# Read the state particular information and store them in the empty vectors(p,d,g)
	
	for(i in 1:row){
		
	if(data[i,2]==stname){	# Checking whether entities belong to the desired state
		p<-c(p,data[i,1])	# Hospital Name saved
		g<-c(g,data[i,3])	# Mortality rate for the particular outcome saved
			}	
			}
# Create the dataframe of desired values
		g<-as.numeric(g)
		mydata<-data.frame("hospital"=p,"mrate"=g)
# Removing missing values		
		mydata<-na.omit(mydata)
# Finds the lowest Mortality rate of the particular state
		ansrate<-min(mydata$mrate)
# Reading the Hospital Names correspoinding to the minimum mortality rate of the state
		solution<-character();id<-numeric()
		ro<-nrow(mydata)
		for(k in 1:ro){
		if(mydata[k,2]==ansrate){
		id<-c(id,k) 
			}
			}
#sorting of the hospital name in the case of tie
		solution<-sort(as.character(mydata[id,1]))
		
	
		print(solution[1])
}

















best<-function(state,outcome){

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

# Validation of state name
# step 1 - change the class of State column(character) to factor
	
	 data[,2]<-as.factor(data[,2])
	 string<-levels(data[ ,2])
# step 2 - Check whether State name exist
		
	stname<-{if(toupper(state) %in% string)
		{stname <- toupper(state)}
		else stop("invalid state")
		}
# Define empty vectors to store the state particular information
	
		p<-vector(); g<-vector() 
# Caculate the number of rows in the main database
			row<-nrow(data)
# Read the state particular information and store them in the empty vectors(p,d,g)
	
	for(i in 1:row){
		
	if(data[i,2]==stname && data[i,3]!="Not Available"){	# Checking whether entities belong to the desired state
	 	
		p<-c(p,data[i,1])	# Hospital Name saved
		g<-c(g,data[i,3])	# Mortality rate for the particular outcome saved
			
			}	
			}
# Create the dataframe of desired values
		g<-as.numeric(g)
		mydata<-data.frame("hospital"=p,"mrate"=g)
# Finds the lowest Mortality rate of the particular state
		ansrate<-min(mydata$mrate)
# Reading the Hospital Names correspoinding to the minimum mortality rate of the state
		solution<-character();id<-numeric()
		ro<-nrow(mydata)
		for(k in 1:ro){
		if(mydata[k,2]==ansrate){
		id<-c(id,k) 
			}
			}
#sorting of the hospital name in the case of tie
		solution<-sort(as.character(mydata[id,1]))
			
		print(solution[1])
}