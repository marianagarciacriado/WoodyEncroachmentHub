#### Woody encroachment across biomes 
#### Script 11. 'Magnitude vs magnitude' analysis
#### Mariana García Criado
#### November 2019


## LIBRARIES ----
library(dplyr)
library(broom)
library(tidyverse)
library(MCMCglmm)
library(ggpubr)


## THEME -----
clima.theme <- theme(legend.position = "right", legend.title = element_blank(), 
                     legend.text = element_text(size=26), legend.key = element_blank(), 
                     legend.spacing.x = unit(0.3, 'cm'),
                     axis.title.x = element_text(face="bold", size=21),
                     axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=21),
                     axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))


## DATA PREP ----
clima.fit <- read.csv("mastersheets/clima_fit.csv")

# Add Site.ID to the mastersheet (this function is amazing)
clima.fit.id2 <- transform(clima.fit, Site.ID = as.numeric(interaction(Latitude, Longitude, drop = TRUE)))

# create a column for each year prior to the start year (4-year window)
clima.fit.id3 <- clima.fit.id2 %>% group_by(Site.ID) %>% mutate(year.b3 = min(Start_year) - 3) %>% 
  mutate(year.b2 = min(Start_year) - 2) %>% mutate(year.b1 = min(Start_year) - 1)

# create a column for each year prior to the end year (4-year window)
clima.fit.id4 <- clima.fit.id3 %>% group_by(Site.ID) %>% mutate(year.a4 = max(End_year) - 4) %>% 
  mutate(year.a3 = max(End_year) - 3) %>% mutate(year.a2 = max(End_year) - 2) %>% mutate(year.a1 = max(End_year) - 1)

# Filter to keep only start and end climatic values
start.clim <- clima.fit.id4 %>% group_by(Site.ID) %>% 
  filter(year == year.b4 | year == year.b3 | year == year.b2 | year == year.b1 | year == Start_year)

end.clim <- clima.fit.id4 %>% group_by(Site.ID) %>% 
  filter(year == year.a4 | year == year.a3 | year == year.a2 | year == year.a1 | year == End_year)

# Calculate the mean for each group
start.clim.mean <- start.clim %>% group_by(Site.ID, variable) %>% mutate(start.mean = mean(value))

end.clim.mean <- end.clim %>% group_by(Site.ID, variable) %>% mutate(end.mean = mean(value))

# Simplify databases
start.simple <- start.clim.mean %>% select(Biome_type, Start_year, End_year, Start_cover, End_cover, Total_cover_change, 
                                           Plot.ID, year.b4, variable, Site.ID, start.mean, geo.coords, Biome_trend)

end.simple <- end.clim.mean %>% select(Biome_type, Start_year, End_year, Start_cover, End_cover, Total_cover_change, 
                                           Plot.ID, year.b4, variable, Site.ID, end.mean, geo.coords, Biome_trend)

# Join start and end databases and filter to those within the 1979 - 2013 time window
start.end.clim <- left_join(start.simple, end.simple, by = c("Plot.ID", "variable")) %>% 
  distinct() %>% filter(year.b4.x >= 1979) 

# Calculate climatic differences, retain increases only and remove NAs 
start.end.dif <- start.end.clim %>% mutate(climbaseline = 0) %>% 
  mutate(climdif = end.mean - start.mean) %>% drop_na(Start_cover.x, End_cover.x, start.mean, end.mean) %>% 
  filter(Start_cover.x !="not reported" | Start_cover.x !="not recorded" | Start_cover.x !="")

start.end.dif$End_cover.x <- as.character(start.end.dif$End_cover.x)
start.end.dif$End_cover.x <- as.numeric(start.end.dif$End_cover.x)

start.end.dif$Start_cover.x <- as.character(start.end.dif$Start_cover.x)
start.end.dif$Start_cover.x <- as.numeric(start.end.dif$Start_cover.x)

# Filter per climatic variable
mat.magnitude <- filter(start.end.dif, variable == "mat")
map.magnitude <- filter(start.end.dif, variable == "map")

# Filter per biome
mat.tun.mag <- filter(mat.magnitude, Biome_type.x == "Tundra")
mat.sav.mag <- filter(mat.magnitude, Biome_type.x == "Savanna")

map.tun.mag <- filter(map.magnitude, Biome_type.x == "Tundra")
map.sav.mag <- filter(map.magnitude, Biome_type.x == "Savanna")



## TEMPERATURE INCREASES ----

# Tundra model
mat.tun.inc.mag <- filter(mat.tun.mag, climdif > 0)

mat.tun.mag.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1):geo.coords.x, 
                                 data = mat.tun.inc.mag, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.tun.mag.mod)
# negative slope non-significant
plot(mat.tun.mag.mod$Sol, auto.layout = F)
plot(mat.tun.mag.mod$VCV)
autocorr.plot(mat.tun.mag.mod$VCV)
save(mat.tun.mag.mod, file = "models/random/mat_tun_mag_mod.RData")


# Savanna model
mat.sav.inc.mag <- filter(mat.sav.mag, climdif > 0)

mat.sav.mag.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1):geo.coords.x, 
                            data = mat.sav.inc.mag, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.sav.mag.mod)
# negative non-significant
plot(mat.sav.mag.mod$Sol, auto.layout = F)
plot(mat.sav.mag.mod$VCV)
autocorr.plot(mat.sav.mag.mod$VCV)
save(mat.sav.mag.mod, file = "models/random/mat_sav_mag_mod.RData")


# Temperature plot
(mat.mag.plot <- ggplot(mat.magnitude, x = climdif, y = End_cover.x) + 
   geom_segment(mapping = aes(x = climbaseline, y = Start_cover.x, xend = climdif, yend = End_cover.x, group = factor(Plot.ID), 
                              color=Biome_trend.x), size = 1.5, alpha = 0.6, data = mat.magnitude) + geom_vline(xintercept = 0) +
   scale_color_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                      guide = guide_legend(title = "Cover trends"), 
                      labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                 "Tundra decrease", "Tundra increase", "Tundra stable")) + ylim(0, 100) +
   ylab("Woody cover (%)\n") + xlab("\nDifference in Mean Annual Temperature(°C)") + 
   theme(legend.key = element_blank()) + clima.theme)



## PRECIPITATION INCREASES ----

# Tundra model
map.tun.inc.mag <- filter(map.tun.mag, climdif > 0)

map.tun.mag.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1):geo.coords.x, 
                            data = map.tun.inc.mag, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.tun.mag.mod)
# negative slope non-significant
plot(map.tun.mag.mod$Sol, auto.layout = F)
plot(map.tun.mag.mod$VCV)
autocorr.plot(map.tun.mag.mod$VCV)
save(map.tun.mag.mod, file = "models/random/map_tun_mag_mod.RData")


# Savanna model 
map.sav.inc.mag <- filter(map.sav.mag, climdif > 0)

map.sav.mag.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1):geo.coords.x, 
                                 data = map.sav.inc.mag, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.sav.mag.mod)
# positive significant
plot(map.sav.mag.mod$Sol, auto.layout = F)
plot(map.sav.mag.mod$VCV)
autocorr.plot(map.sav.mag.mod$VCV)
save(map.sav.mag.mod, file = "models/random/map_sav_mag_mod.RData")


# Precipitation plot
(map.mag.plot <- ggplot(map.magnitude, x = climdif, y = End_cover.x) + 
    geom_segment(mapping = aes(x = climbaseline, y = Start_cover.x, xend = climdif, yend = End_cover.x, group = factor(Plot.ID), 
                               color=Biome_trend.y), size = 1.5, alpha = 0.6, data = map.magnitude) +
    scale_color_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                       guide = guide_legend(title = "Cover trends"), 
                       labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                  "Tundra decrease", "Tundra increase", "Tundra stable")) + ylim(0, 100) +
    geom_vline(xintercept = 0) +
    ylab("Woody cover (%)\n") + xlab("\nDifference in Mean Annual Precipitation (mm)") + 
    theme(legend.key = element_blank()) + clima.theme)


## FIGURE 4 ----
(mag.panel <- ggarrange(mat.mag.plot, map.mag.plot, labels = c("(a)", "(b)"), ncol = 1, nrow = 2,
                         font.label = list(size = 26), common.legend = TRUE, legend = "right"))
ggsave(mag.panel, filename = "figures/Figure_4.png", 
       width = 40, height = 70, units = "cm")


## SUMMARY TABLE ----
dataListMag <- list(mat.tun.mag.mod, mat.sav.mag.mod, 
                    map.tun.mag.mod, map.sav.mag.mod)

# Create lists of input model names
dataListNamesMag <- list("Magnitude vs magnitude MAT Tundra", "Magnitude vs magnitude MAT Savanna",
                         "Magnitude vs magnitude MAP Tundra", "Magnitude vs magnitude MAP Savanna")

# Get model outputs and add model names
readyListMag <- mapply(cbind, lapply(dataListMag, clean.MCMC), "modelName" = dataListNamesMag, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmc.outputs.mag <- as.data.frame(do.call(rbind, readyListMag), stringsAsFactors = FALSE)

# Convert to html
stargazer(mcmc.outputs.mag, title = "Magnitude vs magnitude models", type = "html", summary = FALSE, 
          out = "figures/magnitude_mods.htm")
