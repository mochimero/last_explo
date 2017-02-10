NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(data.table)
library(ggplot2)
library(dplyr)
NEISCC<-merge(NEI,SCC,by="SCC")

years<-c(1999,2002,2005,2008)

# PLOT 5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
motorStrings<-c("Mobile - On-Road Diesel Heavy Duty Vehicles","Mobile - On-Road Diesel Light Duty Vehicles",
                "Mobile - On-Road Gasoline Light Duty Vehicles","Mobile - On-Road Gasoline Heavy Duty Vehicles")

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

gBalt<-ggplot(data=baltimore.motor,aes(x=year,y=Emissions))
gBalt<-gBalt+geom_line(color="red")+theme_bw()
gBalt<-gBalt+labs(x="years",y="PM2.5 emissions per year in TONS")
gBalt<-gBalt+ggtitle("Emissions from Motor Vehicles in Baltimore City between 1999 and 2008 ")
ggsave("question5.png",height=5,width=10)