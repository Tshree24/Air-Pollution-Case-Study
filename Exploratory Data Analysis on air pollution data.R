#The most important task is to download the zip file and unzip them for analysis purpose

library("data.table")
path <- getwd()
download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", destfile = paste(path, "dataFiles.zip", sep = "/"))
unzip(zipfile = "dataFiles.zip")

#Reading the files

NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")



# Plot 1 Using the base plotting system to analyse total emissions from PM2.5 decreased in the United States from 1999 to 2008 

Total_Emissions <- summarise(group_by(NEI, year), Emissions=sum(Emissions))
png(filename='plot1.png')
x1<-barplot(height=Total_Emissions$Emissions/1000, names.arg= Total_Emissions$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,8000),
            main=expression('Total PM'[2.5]*' emissions for different years in kilotons'))
text(x = x1, y = round(Total_Emissions$Emissions/1000,2), label = round(Total_Emissions$Emissions/1000,2), pos = 3, cex = 0.8)
dev.off()

#Plot 2 analysing if total emissions have decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008 using the base plotting system 

Baltimorecity_Emissions <- summarise(group_by(filter(NEI, fips == "24510"), year), Emissions=sum(Emissions))

png(filename='plot2.png')
x2<-barplot(height=Baltimorecity_Emissions$Emissions/1000, names.arg=Baltimorecity_Emissions$year,
            xlab="years", ylab=expression('total PM'[2.5]*' emission in kilotons'),ylim=c(0,4),
            main=expression('Total PM'[2.5]*' emissions in Baltimore City in kilotons'))
text(x = x2, y = round(Baltimorecity_Emissions$Emissions/1000,2), label = round(Baltimorecity_Emissions$Emissions/1000,2), pos = 3, cex = 0.8)
dev.off()

#Plot 3 to analyse which of the four variable (point, nonpoint, onroad, nonroad) decreases the emissions

Year_Baltimorecity_Emissions<-summarise(group_by(filter(NEI, fips == "24510"), year,type), Emissions=sum(Emissions))

png(filename='plot3.png')
ggplot(Year_Baltimorecity_Emissions, aes(x=factor(year), y=Emissions, fill=type,label = round(Emissions,2))) +
  geom_bar(stat="identity") +
  #geom_bar(position = 'dodge')+
  facet_grid(. ~ type) +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emission in tons")) +
  ggtitle(expression("PM"[2.5]*paste(" emissions in Baltimore ",
                                     "City by various source types", sep="")))+
  geom_label(aes(fill = type), colour = "white", fontface = "bold")

dev.off()

#Plot 4 to analyse how have emissions from coal combustion-related sources changed from 1999–2008

Coal_combustion <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
Coal_combustion_sources <- SCC[Coal_combustion,]

Coal_combustion_emissions <- NEI[(NEI$SCC %in% Coal_combustion_emissions$SCC), ]

Coal_related_emissions <- summarise(group_by(Coal_combustion_emissions, year), Emissions=sum(Emissions))

png(filename='plot4.png')
ggplot(Coal_related_emissions , aes(x=factor(year), y=Emissions/1000,fill=year, label = round(Emissions/1000,2))) +
  geom_bar(stat="identity") +
  #geom_bar(position = 'dodge')+
  # facet_grid(. ~ year) +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions in kilotons")) +
  ggtitle("Emissions from coal combustion-related sources in kilotons")+
  geom_label(aes(fill = year),colour = "white", fontface = "bold")
dev.off()


#Plot 5 to see how have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City
Baltimorecitymary_Emissions <- NEI[(NEI$fips=="24510") & (NEI$type=="ON-ROAD"),]
Year_Baltimorecitymary_Emissions <- summarise(group_by(Baltimorecitymary_Emissions, year), Emissions=sum(Emissions))

png(filename='plot5.png')
ggplot(Year_Baltimorecitymary_Emissions, aes(x=factor(year), y=Emissions,fill=year, label = round(Emissions,2))) +
  geom_bar(stat="identity") +
  xlab("year") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) +
  ggtitle("Emissions from motor vehicle sources in Baltimore City")+
  geom_label(aes(fill = year),colour = "white", fontface = "bold")
dev.off()

#Plot 6 to compare

Baltimorecity_Emissions <- summarise(group_by(filter(NEI, fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
Losangels_emissions <- summarise(group_by(filter(NEI, fips == "06037"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))

Baltimorecity_Emissions$County <- "Baltimore City, MD"
Losangels_emissions$County <- "Los Angeles County, CA"
both.emissions <- rbind(Baltimorecity_Emissions, Losangels_emissions)

png(filename='plot6.png')
ggplot(both.emissions, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2))) +
  geom_bar(stat="identity") + 
  facet_grid(County~., scales="free") +
  ylab(expression("total PM"[2.5]*" emissions in tons")) + 
  xlab("year") +
  ggtitle(expression("Motor vehicle emission variation in Baltimore and Los Angeles in tons"))+
  geom_label(aes(fill = County),colour = "white", fontface = "bold")
dev.off()