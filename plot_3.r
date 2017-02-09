# PLOT 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#variable, which of these four sources have seen decreases in emissions from 1999-2008 
#for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 
#plotting system to make a plot answer this question.


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(data.table)
library(ggplot2)
library(dplyr)
NEISCC<-merge(NEI,SCC,by="SCC")


# cutoff<-1e16
# NEISCC.filtered<-filter(NEISCC,Emissions<cutoff)
# NEISCC.filtered<-filter(NEISCC.filtered,Emissions!=0)
# 
# NEISCC.2008<-filter(NEISCC.filtered,year==2008)
# NEISCC.2005<-filter(NEISCC.filtered,year==2005)
# NEISCC.2002<-filter(NEISCC.filtered,year==2002)
# NEISCC.1999<-filter(NEISCC.filtered,year==1999)



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
g<-g+geom_line()+facet_grid(.~Source)+theme_bw()
g<-g+labs(x="years",y="PM2.5 emissions per year in TONS")
g<-g+ggtitle("Emissions in Baltimore City per source type between 1999 and 2008")

ggsave("question3.png",height=5,width=10)

g<-ggplot(data=summary.Baltimore,aes(x=Year,y=Emissions,color=Source))
g<-g+geom_line()+theme_bw()+labs(x="years",y="PM2.5 emissions per year in TONS")
g<-g+ggtitle("Emissions in Baltimore City per source type between 1999 and 2008")
ggsave("question3plus.png",height=5,width=10)