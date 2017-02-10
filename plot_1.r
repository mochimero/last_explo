## ## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(data.table)
library(ggplot2)
library(dplyr)
NEISCC<-merge(NEI,SCC,by="SCC")


cutoff<-1e16
NEISCC.filtered<-filter(NEISCC,Emissions<cutoff)
NEISCC.filtered<-filter(NEISCC.filtered,Emissions!=0)


# Questions
# 
# You must address the following questions and tasks in your exploratory analysis. 
#For each question/task you will need to make a single plot. Unless specified, 
#you can use any plotting system in R to make your plot.
# 






NEISCC.2008<-filter(NEISCC.filtered,year==2008)
NEISCC.2005<-filter(NEISCC.filtered,year==2005)
NEISCC.2002<-filter(NEISCC.filtered,year==2002)
NEISCC.1999<-filter(NEISCC.filtered,year==1999)

# PLOT1: Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base 
#plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 
#2002, 2005, and 2008.

total.emissions<-c(sum(NEISCC.1999$Emissions),sum(NEISCC.2002$Emissions),
                   sum(NEISCC.2005$Emissions),sum(NEISCC.2008$Emissions))

png(filename="question1.png",height=800,width=800,bg="white")
barplot(total.emissions,ylab=" PM2.5 emissions per year in TONS",xlab="Years",
        names.arg=c("1999","2002","2005","2008"),col="blue",main="Total National PM2.5 emissions per year",
        axes=TRUE)

dev.off()
