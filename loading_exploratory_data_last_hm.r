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
#boxplot(Emissions ~ year, NEISCC.filtered,log="y")

#NEISCC.tbl<-tbl_df(NEISCC)


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


# PLOT 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#variable, which of these four sources have seen decreases in emissions from 1999-2008 
#for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 
#plotting system to make a plot answer this question.
                       
years<-c(1999,2002,2005,2008)
sources<-names(table(NEISCC$type))
summary.Baltimore<-data.frame()

rowIndex=1
colIndex=1
for (i in c(1999,2002,2005,2008)){
        
        for (j in names(table(NEISCC$type))){
                # print(i)
                # print(j)
                summary.Baltimore[rowIndex,1]<-sum(filter(NEISCC,year==i & fips=="24510" & type==j)$Emissions)
                summary.Baltimore[rowIndex,2]<-i
                summary.Baltimore[rowIndex,3]<-j
                rowIndex<-rowIndex+1
        }
        
}
names(summary.Baltimore)<-c("Emissions","Year","Source")

g<-ggplot(data=summary.Baltimore,aes(x=Year,y=Emissions))
g+geom_line()+facet_grid(.~Source)+theme_bw()+labs(x="years",y="PM2.5 emissions per year in TONS")+ggtitle("Emissions in Baltimore City per source type between 1999 and 2008")

ggsave("question3.png",height=5,width=10)

g<-ggplot(data=summary.Baltimore,aes(x=Year,y=Emissions,color=Source))
g+geom_line()+theme_bw()+labs(x="years",y="PM2.5 emissions per year in TONS")+ggtitle("Emissions in Baltimore City per source type between 1999 and 2008")
ggsave("question3plus.png",height=5,width=10)
#print(g)
#dev.off()

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
gUS+geom_line()+theme_bw()+labs(x="years",y="PM2.5 emissions per year in TONS")+ggtitle("Emissions in the US between 1999 and 2008 from Coal combustion-related sources")
ggsave("question4.png",height=5,width=10)


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

# PLOT 6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
# Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?


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




