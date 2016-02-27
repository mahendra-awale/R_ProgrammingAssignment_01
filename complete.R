##Function add user specified leading character to number
##num=Number for formating
##size=size of number
##flag=character to add
##Example, num=1, size=3, flag=0--> output=001
###############################################################
formatNum<-function(num, size=3, flag=0)
{
	sizeOfNum<-nchar(num)
	toAdd<-(size-sizeOfNum)
	toAddFlag=""

	if (toAdd<=0)
	{
		return(num)
	}
		
	for(i in seq_len(toAdd))
	{
		toAddFlag<-paste(toAddFlag,flag, sep="")
	}
	
	paste(toAddFlag, num, sep="")				
}
###############################################################
##Function takes the number and 
##create appropriate CSV filename for reading

getCSVName <-function(num, dir)
{
	paste(dir, "/", num, ".csv", sep="") 		
}
###############################################################
##Function get the number of observations with Complete cases
complete<-function (dir, id=1:332)
{

	df<-data.frame(id,id)
	colNames<-c("id", "nobs")
	names(df)<-colNames
	for (i in seq_along(id))
	{
		#read data
		num<-formatNum(id[i])
		file<-getCSVName(num, dir)
		data<-read.csv(file)
		good<-complete.cases(data)
		good_count<-length(data[good,1])
		df[i,2]<-good_count
	
	}
	print(df)
}
###############################################################
###############################################################