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
corr <-function (dir, t=0)
{
	id<-1:332
	cr<-1:332
	countcases=0
	for (i in seq_along(id))
	{
		#read data
		num<-formatNum(id[i])
		file<-getCSVName(num, dir)
		data<-read.csv(file)
		good<-complete.cases(data)
		good_count<-length(data[good,1])
		if (good_count>t)
		{
			s<-data[good,2]
			n<-data[good,3]
			correlation<-cor(s,n)
			countcases<-countcases+1
			cr[countcases]<-correlation
		}		
	}

	cr[0:countcases]
}
###############################################################