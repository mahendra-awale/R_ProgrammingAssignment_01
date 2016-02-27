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
##Function calculates mean for pollutant
pollutatnmean <-function (dir, pol, id=1:332)
{
	#defined variables to hold means
	means<-seq_along(id);

	#Now iterate through to get the means
	sum<-0
	count<-0
	for (i in seq_along(id))
	{
		#read data
		num<-formatNum(id[i])
		file<-getCSVName(num, dir)
		data<-read.csv(file)
		bad<-is.na(data[pol])
		good<-data[!bad,pol]
		for(j in seq_along(good))
		{
			sum<-sum+good[j]
			count<-count+1
		}	
	}
	meAN<-sum/count
	print(meAN)
}
###############################################################