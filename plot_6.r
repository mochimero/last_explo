NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(data.table)
library(ggplot2)
library(dplyr)
NEISCC<-merge(NEI,SCC,by="SCC")

years<-c(1999,2002,2005,2008)


# PLOT 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
# Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

baltimore.motor<-data.frame()
LA.motor<-data.frame()
rowIndex<-1
for(i in years){
        LA.motor[rowIndex,1]<-sum(filter(NEISCC,EI.Sector %in% motorStrings & fips=="06037" & year==i)$Emissions)
        LA.motor[rowIndex,2]<-i
        baltimore.motor[rowIndex,1]<-sum(filter(NEISCC,EI.Sector %in% motorStrings & fips=="24510" & year==i)$Emissions)
        baltimore.motor[rowIndex,2]<-i
        rowIndex<-rowIndex+1
}
names(baltimore.motor)<-c("Emissions","year")
names(LA.motor)<-c("Emissions_LA","year")


baltimore.motor.f<-cbind(baltimore.motor,rep("Baltimore",4))
LA.motor.f<-cbind(LA.motor,rep("LA",4))
names(baltimore.motor.f)<-c("Emissions","year","City")
names(LA.motor.f)<-c("Emissions","year","City")
LA_vs_BALT<-rbind(LA.motor.f,baltimore.motor.f)



gBalt_vs_LA<-ggplot(data=LA_vs_BALT,aes(x=year,y=Emissions,color=City))
gBalt_vs_LA<-gBalt_vs_LA+geom_line()+theme_bw()
gBalt_vs_LA<-gBalt_vs_LA+labs(x="years",y="PM2.5 emissions per year in TONS")
gBalt_vs_LA<-gBalt_vs_LA+ggtitle("Emissions from Motor Vehicles in Baltimore City and Los Angeles between 1999 and 2008 ")

ggsave("question6.png",height=5,width=10)