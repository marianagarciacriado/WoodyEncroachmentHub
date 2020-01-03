#### Woody encroachment across biomes 
#### Script 03. Woody cover change timelines
#### Mariana García Criado 
#### February 2018

## LIBRARIES ----
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MCMCglmm)


## DATA PREP ----
cover.ms.time <- read.csv("mastersheets/cover_ms_clean.csv")

# Adding the 'Colour' column
cover.ms.time$colour[cover.ms.time$Biome_trend == "Tundra_decrease"] <- "#8B0000"
cover.ms.time$colour[cover.ms.time$Biome_trend == "Tundra_increase"] <- "#00008B"
cover.ms.time$colour[cover.ms.time$Biome_trend == "Tundra_stable"] <- "#474747"

cover.ms.time$colour[cover.ms.time$Biome_trend == "Savanna_decrease"] <- "#EE6363"
cover.ms.time$colour[cover.ms.time$Biome_trend == "Savanna_increase"] <- "#63B8FF"
cover.ms.time$colour[cover.ms.time$Biome_trend == "Savanna_stable"] <- "#8f8f8f"

# Arrange records per start year
cover.ms.time <- arrange(cover.ms.time, Start_year)
write.csv(cover.ms.time, "mastersheets/timeline.csv")

# Filter per biome
sav_time <- filter(cover.ms.time, Biome_type == "Savanna")
tun_time <- filter(cover.ms.time, Biome_type == "Tundra")


## TIMELINE PLOTS ----

# Savanna timeline
(timeline.sav <- ggplot(sav_time) +
   geom_segment(aes(x=Start_year, xend=End_year, y = reorder(Plot.ID, Start_year), yend = Plot.ID, color = colour), size = 1) +
   labs(x = "\nYear", y = '') + expand_limits(x=(c(1870, 2020))) + scale_x_continuous(breaks = c(1900, 1940, 1980, 2020)) + 
    scale_colour_manual(values = c("steelblue1", "gray56", "indianred2"), 
                        guide = guide_legend(title = "Cover trends"), 
                        labels = c("Savanna increase", "Savanna stable", "Savanna decrease")) +
   theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
         panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
         panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.line.y = element_blank(),
         legend.position = "right", legend.title = element_text(size=24, face = "bold"), legend.text = element_text(size=20), 
         legend.key = element_blank(),
         axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
         axis.title.x = element_text(face="bold", size=24),
         axis.text.x  = element_text(vjust=0.5, size=20, colour = "black")))

# Tundra timeline
(timeline.tun <- ggplot(tun_time) +
    geom_segment(aes(x=Start_year, xend=End_year, y = reorder(Plot.ID, Start_year), yend = Plot.ID, color = colour),  size = 1) +
    labs(x = "\nYear", y = '') + expand_limits(x=(c(1870, 2020))) + scale_x_continuous(breaks = c(1900, 1940, 1980, 2020)) +
    scale_colour_manual(values = c("darkblue", "gray28", "darkred"), 
                        guide = guide_legend(title = "Cover trends"), 
                        labels = c("Tundra increase", "Tundra stable", "Tundra decrease")) +
    theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.line.y = element_blank(),
          legend.position = "right", legend.title = element_text(size=24, face = "bold"), legend.text = element_text(size=20), 
          legend.key = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
          axis.title.x = element_text(face="bold", size=24),
          axis.text.x  = element_text(vjust=0.5, size=20, colour = "black")))

# Timeline panel (Figure S3)
timeline.panel <- ggarrange(timeline.tun, timeline.sav, labels = c("(a)", "(b)"), 
                            font.label = list(size = 26), ncol = 1, nrow = 2, legend = "right")

ggplot2::ggsave(timeline.panel, filename = "figures/Figure_S3.png", 
       width = 70, height = 50, units = "cm")


## MEANS & MODELS ----

## Average length study period in each biome
duration_tun <- tun_time %>% summarise(mean = mean(Time_period), sd = sd(Time_period))
duration_tun #21.36 years +- 14.79 (TEXT)

duration_sav <- sav_time %>% summarise(mean = mean(Time_period), sd = sd(Time_period))
duration_sav #16.94 years +- 17.22 (TEXT)


## Comparison of study duration between biomes
duration.biomes <- MCMCglmm(Time_period ~ Biome_type, data = cover.ms.time, 
                            nitt = 100000, burnin = 5000, thin = 30)
summary(duration.biomes) # non-significant difference 
save(duration.biomes, file = "models/fixed/duration.biomes.RData")


## Average start date study period in each biome
start_tun <- tun_time %>% summarise(mean = mean(Start_year), sd = sd(Start_year))
start_tun #1987 +- 16.75

start_sav <- sav_time %>% summarise(mean = mean(Start_year), sd = sd(Start_year))
start_sav #1993 +- 21.28


## Mean start and end date overall
start_all <- cover.ms.time %>% summarise(mean = mean(Start_year), sd = sd(Start_year))
start_all

end_all <- cover.ms.time %>% summarise(mean = mean(End_year), sd = sd(End_year))
end_all


## Earliest start of a monitoring project record
which.min(tun_time$Start_year) #1920 for tundra
which.min(sav_time$Start_year) #1876 for savanna


## Increases reported in time
inc.tun <- tun_time %>% filter(Trend == "Increase")
hist.end.tun <- hist(inc.tun$End_year)
inc.tun.mean <- inc.tun %>% summarise(mean = mean(End_year))
inc.tun.mean #2008

inc.sav <- sav_time %>% filter(Trend == "Increase")
hist.end.sav <- hist(inc.sav$End_year)
inc.sav.mean <- inc.sav %>% summarise(mean = mean(End_year)) #2001
inc.sav.mean #2010

