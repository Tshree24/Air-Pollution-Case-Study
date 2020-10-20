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