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


NEISCC.2008<-filter(NEISCC.filtered,year==2008)
NEISCC.2005<-filter(NEISCC.filtered,year==2005)
NEISCC.2002<-filter(NEISCC.filtered,year==2002)
NEISCC.1999<-filter(NEISCC.filtered,year==1999)

# PLOT 2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

baltimore.1999<-filter(NEISCC.1999,fips=="24510")
baltimore.2002<-filter(NEISCC.2002,fips=="24510")
baltimore.2005<-filter(NEISCC.2005,fips=="24510")
baltimore.2008<-filter(NEISCC.2008,fips=="24510")
baltimore.emissions<-c(sum(baltimore.1999$Emissions),sum(baltimore.2002$Emissions),
                       sum(baltimore.2005$Emissions),sum(baltimore.2008$Emissions))


png(filename="question2.png",height=800,width=800,bg="white")
barplot(baltimore.emissions,ylab=" PM2.5 emissions per year in TONS",xlab="Years",
        names.arg=c("1999","2002","2005","2008"),col="blue",main="Total  PM2.5 emissions per year in Baltimore",
        axes=TRUE)


dev.off()