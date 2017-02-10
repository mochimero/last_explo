NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(data.table)
library(ggplot2)
library(dplyr)
NEISCC<-merge(NEI,SCC,by="SCC")

years<-c(1999,2002,2005,2008)
sources<-names(table(NEISCC$type))

# PLOT4: Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

coalIndex<-grep("[Cc]oal",NEISCC$EI.Sector)
coalbased.NEISCC<-slice(NEISCC,coalIndex)

coalUSperyear<-data.frame()
rowIndex<-1
for (i in years){
        coalUSperyear[rowIndex,1]<-i
        coalUSperyear[rowIndex,2]<-sum(filter(coalbased.NEISCC,coalIndex & year==i)$Emissions)
        rowIndex<-rowIndex+1
}

names(coalUSperyear)<-c("year","Emissions")
gUS<-ggplot(data=coalUSperyear,aes(x=year,y=Emissions))
gUS<-gUS+geom_line()+theme_bw()+labs(x="years",y="PM2.5 emissions per year in TONS")
gUS<-gUS+ggtitle("Emissions in the US between 1999 and 2008 from Coal combustion-related sources")
ggsave("question4.png",height=5,width=10)