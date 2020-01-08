#### Woody encroachment across biomes 
#### Script 02. Woody cover change map & rates
#### Mariana García Criado 
#### March 2019


## LIBRARIES ----
library(proj4)
library(scales)
library(ggalt)
library(ggplot2)
library(dplyr)
library(cowplot)
library(MCMCglmm)


## THEME ----
dense.theme <- theme(axis.title.x = element_text(face="bold", size=34),
                     axis.text.x  = element_text(vjust=0.5, size=28, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=34),
                     axis.text.y  = element_text(vjust=0.5, size=28, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                     legend.position = "left",
                     legend.title = element_text(size=34, face = "bold"), 
                     legend.text = element_text(size=34), legend.key = element_blank(),
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))


## DATA LOAD & PREP ----
cover.msc <- read.csv("mastersheets/cover_ms_clean.csv")

# Re-scaling the values so the points of change are proportional
cover.msc$Absolute_change <- abs(cover.msc$Annual.rate)
cover.msc$Rescaled_change <- rescale(cover.msc$Absolute_change, to = c(1,4))

# Creating my objects
savanna_decrease <- filter(cover.msc, Biome_trend == "Savanna_decrease")
savanna_increase <- filter(cover.msc, Biome_trend == "Savanna_increase")
savanna_stable <- filter(cover.msc, Biome_trend == "Savanna_stable")
tundra_decrease <- filter(cover.msc, Biome_trend == "Tundra_decrease")
tundra_increase <- filter(cover.msc, Biome_trend == "Tundra_increase")
tundra_stable <- filter(cover.msc, Biome_trend == "Tundra_stable")


## MAP OF WOODY COVER CHANGE ---- 
world <- map_data("world")
world <- world[world$region != "Antarctica",]

(cover.map <- ggplot(cover.msc) + geom_map(data = world, map = world,
                           aes(map_id=region), fill = "#D3D3D3") + 
    expand_limits(x = world$long, y = world$lat) + 
    coord_proj("+proj=wintri") +  
    geom_point(data = savanna_decrease, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "indianred2", alpha = 0.7) +
    geom_point(data = savanna_increase, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "steelblue1", alpha = 0.5) +
    geom_point(data = savanna_stable, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "gray56", alpha = 0.8) +
    geom_point(data = tundra_increase, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "darkblue", alpha = 0.5) +  
    geom_point(data = tundra_stable, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "gray28", alpha = 0.8) + 
    geom_point(data = tundra_decrease, aes(x = Longitude, y = Latitude, size = Rescaled_change), colour = "darkred", alpha = 0.5) +
    scale_size_continuous(range = c(8, 20)) + guides(size = FALSE) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
          axis.text.y = element_blank(), axis.title.y=element_blank(), 
          axis.title.x=element_blank(), legend.key = element_blank(), 
          panel.background = element_blank(), panel.border = element_blank()))


## RATES OF WOODY COVER CHANGE ----

## Savanna density plot
dp_sav <- filter(cover.msc, Biome_type == "Savanna")
m_sav <- plyr::ddply(dp_sav, "Trend", summarise, grop.mean=mean(Annual.rate))
head(m_sav)

(dp.sav <- ggplot(dp_sav, aes(x = Annual.rate, fill = Trend)) + geom_blank() +
    stat_density(data = dp_sav[dp_sav$Trend !="Stable",], 
                 aes(..count.., x = Annual.rate), geom = "area", colour = "black", trim = "TRUE", adjust = 2, alpha = 0.8) + 
    geom_rect(aes(xmin = -0.1, xmax = 0.1, ymax = length(dp_sav[dp_sav$Trend == "Stable",]$Annual.rate), ymin = 0), 
              alpha = 0.8, colour = "black", fill = "#8f8f8f") +    
    scale_fill_manual(values=c("#EE6363", "#63B8FF", "#8f8f8f"), 
                      labels = c("Decrease", "Increase", "Stable"),
                      guide = guide_legend(title = "Savanna trends")) + 
    scale_colour_manual(guide=FALSE) + 
    geom_vline(data = m_sav, aes(xintercept = grop.mean), linetype = "dashed") + 
    theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    ylab("Density\n") + xlab("\nCover change rate (% per year)") + 
    scale_x_continuous(breaks = seq(-4, +9, 1)) + expand_limits(x=(c(-4,9))) +
    dense.theme)


## Tundra density plot
dp_tun <- filter(cover.msc, Biome_type == "Tundra")
m_tun <- plyr::ddply(dp_tun, "Trend", summarise, grop.mean=mean(Annual.rate))
head(m_tun)

(dp.tun <- ggplot(dp_tun, aes(x = Annual.rate, fill = Trend)) + geom_blank() +
    stat_density(data = dp_tun[dp_tun$Trend != "Stable",], 
                 aes(..count.., x = Annual.rate), alpha = 0.8, adjust = 2, geom = "area", trim = "TRUE", colour = "black") + 
    geom_rect(aes(xmin = -0.1, xmax = 0.1, ymax = length(dp_tun[dp_tun$Trend == "Stable",]$Annual.rate), ymin = 0), 
              alpha = 0.8, colour = "black", fill = "#474747") +
    scale_fill_manual(values=c("darkred", "darkblue", "gray28"), 
                      labels = c("Decrease", "Increase", "Stable"),
                      guide = guide_legend(title = "Tundra trends")) + 
    scale_colour_manual(guide=FALSE) + 
    geom_vline(data = m_tun, aes(xintercept = grop.mean), linetype = "dashed") + 
    theme(legend.position = "left", panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
    ylab("Density\n") + xlab("\nCover change rate (% per year)") + 
    scale_x_continuous(breaks = seq(-4, +4, 1)) + expand_limits(x=(c(-4,+4))) + 
    dense.theme)


## FIGURE 1 ----

# Map & density plots figure 
(map_dens_panel <- cowplot::ggdraw() +
    draw_plot(cover.map, x = -0.2, y = -0.05, width = 0.9, height = 1) +
    draw_plot(dp.tun, x = 0.6, y = 0.5, width = 0.35, height = 0.5) +
    draw_plot(dp.sav, x = 0.6, y = 0.01, width = 0.35, height = 0.5) +
    draw_plot_label(label = c("(a)", "(b)", "(c)"), size = 38,
                    x = c(0.01, 0.6, 0.6), y = c(0.95, 0.95, 0.5)))

# Save Figure 1
ggplot2::ggsave(map_dens_panel, filename = "figures/Figure_1.png", 
                width = 80, height = 35, units = "cm")


## TRENDS PER BIOME ----

## Filtering per cover trend 
total_inc <- filter(cover.msc, Trend == "Increase")
total_dec <- filter(cover.msc, Trend == "Decrease")
total_stab <- filter(cover.msc, Trend == "Stable")


## Percentage of records per trend and biome 

# Percentage of increasing records per biome
pc_inc_tt <- (length(tundra_increase$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_inc_tt #19.56% records of the total records (savanna+tundra) are increasing tundra records

pc_inc_t <- (length(tundra_increase$Plot.ID)/length(dp_tun$Plot.ID))*100
pc_inc_t #68.05% records of tundra records are increasing tundra records

pc_inc_st <- (length(savanna_increase$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_inc_st #48.3% records of the total records (savanna+tundra) are increasing savanna records

pc_inc_s <- (length(savanna_increase$Plot.ID)/length(dp_sav$Plot.ID))*100
pc_inc_s #67.78% records of savanna records are increasing savanna records


# Percentage of decreasing records per biome
pc_dec_tt <- (length(tundra_decrease$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_dec_tt #5.96% records of the total records (savanna+tundra) are decreasing tundra records

pc_dec_t <- (length(tundra_decrease$Plot.ID)/length(dp_tun$Plot.ID))*100
pc_dec_t #20.76% records of tundra records are decreasing tundra records

pc_dec_st <- (length(savanna_decrease$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_dec_st #18.82% records of the total records (savanna+tundra) are decreasing savanna records

pc_dec_s <- (length(savanna_decrease$Plot.ID)/length(dp_sav$Plot.ID))*100
pc_dec_s #26.41% records of savanna records are decreasing savanna records


# Percentage of stable records per biome
pc_sta_tt <- (length(tundra_stable$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_sta_tt #3.21% records of the total records (savanna+tundra) are stable tundra records

pc_sta_t <- (length(tundra_stable$Plot.ID)/length(dp_tun$Plot.ID))*100
pc_sta_t #11.18% records of tundra records are stable tundra records

pc_sta_st <- (length(savanna_stable$Plot.ID)/length(cover.msc$Plot.ID))*100
pc_sta_st #4.13% records of the total records (savanna+tundra) are stable savanna records

pc_sta_s <- (length(savanna_stable$Plot.ID)/length(dp_sav$Plot.ID))*100
pc_sta_s #5.79% records of savanna records are stable savanna records


## Mean values per trend and biome

# Mean increasing value per biome
mean_inc_t <- tundra_increase %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_inc_t # 0.37% ± 0.5 is the mean increase value in the tundra 

mean_inc_s <- savanna_increase %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_inc_s # 0.63 ± 0.82 is the mean increase value in the savanna


# Mean stable value per biome
mean_stab_t <- tundra_stable %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_stab_t # 0.001% ± 0.003 is the mean stable value in the tundra 

mean_stab_s <- savanna_stable %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_stab_s # 0.0003 ± 0.005% is the mean stable value in the savanna


# Mean decreasing value per biome
mean_dec_t <- tundra_decrease %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_dec_t # -0.36% ± 0.75 is the mean decrease value in the tundra 

mean_dec_s <- savanna_decrease %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_dec_s # -0.4 ± 0.45 is the mean decrease value in the savanna


## Overall absolute cover value per biome

# Mean cover change value in the tundra (overall)
mean_tun <- dp_tun %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_tun # 0.18% ± 0.61 is the mean of cover values in the tundra 

# Mean cover change value in the savanna (overall)
mean_sav <- dp_sav %>% summarise(mean = mean(Annual.rate), sd = sd(Annual.rate))
mean_sav # 0.32% ± 0.85 is the mean of cover values in the savanna


## MODELS IN COVER CHANGE DIFFERENCES ----

# Differences in increasing cover trends between biomes
inc.means.mod <- MCMCglmm(Annual.rate ~ Biome_type, data = total_inc, 
                             nitt = 100000, burnin = 5000, thin = 30)
summary(inc.means.mod) # significantly different increase values
save(inc.means.mod, file = "models/fixed/inc.means.mod.RData")

# Differences in stable cover trends between biomes
stab.means.mod <- MCMCglmm(Annual.rate ~ Biome_type, data = total_stab, 
                             nitt = 100000, burnin = 5000, thin = 30)
summary(stab.means.mod) # not significantly different
save(stab.means.mod, file = "models/fixed/stab.means.mod.RData")

# Differences in decreasing cover trends between biomes
dec.means.mod <- MCMCglmm(Annual.rate ~ Biome_type, data = total_dec, 
                             nitt = 100000, burnin = 5000, thin = 30)
summary(dec.means.mod) # not significantly different
save(dec.means.mod, file = "models/fixed/dec.means.mod.RData")

# Differences in overall cover trends between biomes
overall.mod <- MCMCglmm(Annual.rate ~ Biome_type, data = cover.msc, 
                    nitt = 100000, burnin = 5000, thin = 30)
summary(overall.mod) # not significantly different overall values
save(overall.mod, file = "models/fixed/overall.mod.RData")



## TRENDS PER CONTINENT ----

# Percentage records per trend and continent - increases
inc.cont.t <- tundra_increase %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_inc$Plot.ID))*100)

inc.cont.s <- savanna_increase %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_inc$Plot.ID))*100)

# Percentage records per trend and continent - stables
stab.cont.t <- tundra_stable %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_stab$Plot.ID))*100)

stab.cont.s <- savanna_stable %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_stab$Plot.ID))*100)

# Percentage records per trend and continent - decreases
dec.cont.t <- tundra_decrease %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_dec$Plot.ID))*100)

dec.cont.s <- savanna_decrease %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(total_dec$Plot.ID))*100)

# Relative to the total number of records
inc.cont <- total_inc %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(cover.msc$Plot.ID))*100)

stab.cont <- total_stab %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(cover.msc$Plot.ID))*100)

dec.cont <- total_dec %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(cover.msc$Plot.ID))*100)


# Reported records in the tundra
continent.tun <- dp_tun %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(dp_tun$Plot.ID))*100)

# Reported records in the savanna
continent.sav <- dp_sav %>% group_by(Continent) %>% count() %>%
  mutate(percent = (n/length(dp_sav$Plot.ID))*100)


## Increasing records relative to each continent
relative <- cover.msc %>% group_by(Continent) %>% count() %>% rename(total = n)

# Number of increasing records per continent
inc.cont.relat <- cover.msc %>% group_by(Continent) %>%
  tally(Trend == "Increase")

# Calculate percentage
inc.relative <- merge(relative, inc.cont.relat, by = "Continent") %>% mutate(percent = (n/total)*100)
inc.relative


## NUMBER OF RECORDS PER METHOD ----
method.sum <- cover.msc %>% group_by(Method) %>% count(Method)
