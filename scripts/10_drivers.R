#### Woody encroachment across biomes 
#### Script 10. Drivers of woody cover change
#### Mariana García Criado
#### October 2018

## PACKAGES ----
library(ggplot2)
library(dplyr)

## DATA PREP ----

# loading driver data
driv.tundra <- read.csv("mastersheets/drivers_tundra_all_trends.csv")
driv.savanna <- read.csv("mastersheets/drivers_savanna_all_trends.csv")
driv.itex <- read.csv("mastersheets/drivers_itex_all_trends.csv")

# Combining biome databases
driv.tundra.s <- driv.tundra %>% dplyr::select(Biome_type, Latitude, Longitude, Annual.rate, Drivers) %>% na.omit()
driv.savanna.s <- driv.savanna %>% dplyr::select(Biome_type, Latitude, Longitude, Annual.rate, Drivers) %>% na.omit()
driv.itex.s <- driv.itex %>% dplyr::select(Biome_type, Latitude, Longitude, Annual.rate, Drivers) %>% na.omit()
drivers.all <- rbind(driv.savanna.s, driv.tundra.s, driv.itex.s)

# Adding trends
drivers.all$Trend <- ifelse(drivers.all$Annual.rate < -0.01, 'Decrease',
                         ifelse(drivers.all$Annual.rate >= -0.01 & drivers.all$Annual.rate <= 0.01, 'Stable',
                                ifelse(drivers.all$Annual.rate > 0.01, 'Increase', 'other')))

# Add 'Biome trend' column
drivers.all$Biome_trend[drivers.all$Biome_type == "Savanna" & drivers.all$Trend == "Decrease"] <- "Savanna_decrease"
drivers.all$Biome_trend[drivers.all$Biome_type == "Savanna" & drivers.all$Trend == "Stable"] <- "Savanna_stable"
drivers.all$Biome_trend[drivers.all$Biome_type == "Savanna" & drivers.all$Trend == "Increase"] <- "Savanna_increase"

drivers.all$Biome_trend[drivers.all$Biome_type == "Tundra" & drivers.all$Trend == "Decrease"] <- "Tundra_decrease"
drivers.all$Biome_trend[drivers.all$Biome_type == "Tundra" & drivers.all$Trend == "Increase"] <- "Tundra_increase"
drivers.all$Biome_trend[drivers.all$Biome_type == "Tundra" & drivers.all$Trend == "Stable"] <- "Tundra_stable"

# Tidying up and ordering the driver data
drivers.all.group <- drivers.all %>%
  dplyr::group_by(Biome_trend, Drivers) %>% 
  dplyr::count(Biome_trend, Drivers)

drivers.all.group.total <- drivers.all.group %>% group_by(Drivers) %>% summarise(total = sum(n))
drivers.final <- merge(drivers.all.group, drivers.all.group.total, by = "Drivers")


## FIGURE 6 ----
(drivers.plot <- ggplot(drivers.final, aes(x = reorder(Drivers, -total), y = n, fill = Biome_trend)) +
    geom_bar(stat = "identity", position = "stack") + ylab("Number of records\n") + xlab("\nDrivers") + 
    scale_fill_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                      guide = guide_legend(title = "Cover trends"), 
                      labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                   "Tundra decrease", "Tundra increase", "Tundra stable")) +
    theme(legend.position = c(0.8, 0.65), legend.title = element_text(face = "bold", size=22), 
          legend.text = element_text(size=22), legend.key.size = unit(1, "cm"),
          legend.key = element_blank(),
          axis.title.x = element_text(face = "bold", size = 22),
          axis.text.x  = element_text(angle = 45, hjust = 1, size=18, colour = "black"), 
          axis.title.y = element_text(face="bold", size=22),
          axis.text.y  = element_text(vjust=0.5, size=18, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

ggsave(drivers.plot, filename = "figures/Figure_6.png", 
       width = 47, height = 25, units = "cm")
