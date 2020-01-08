#### Woody encroachment across biomes 
#### Script 04. Climatic space with climatologies
#### Mariana García Criado
#### February 2018

## LIBRARIES ----
library(dplyr)
library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(ggplot2)
library(MCMCglmm)
library(cowplot)

## CHELSA EXTRACTION ----
# The climatic raster files are heavy and are thus not stored online.
# In order to continue reproducing this code, please either: 
# a) download the climatic data from http://chelsa-climate.org/ or 
# b) skip the code until line 39, where the climatic data has already been extracted
# The code is left commented so it is clear it should only be run once.

# CHELSA climatologies (mean value for the whole period 1979 - 2013)
#temp <- raster("S:/Climate_Data/Chelsa/CHELSA_bio10_1.tif")
#rainfall <- raster("S:/Climate_Data/Chelsa/CHELSA_bio10_12.tif")

## Loading plant cover data
#cover.ms.clim <- read.csv("mastersheets/cover_ms_clean.csv")

## Extracting rainfall and temperature climatologies from CHELSA 
#cover.ms.clim$tempc <- raster::extract(temp, cbind(cover.ms.clim$Longitude, cover.ms.clim$Latitude))
#cover.ms.clim$tempc <- cover.ms.clim$tempc/10

#cover.ms.clim$precip <- raster::extract(rainfall, cbind(cover.ms.clim$Longitude, cover.ms.clim$Latitude))

#write.csv(cover.ms.clim, "mastersheets/cover_ms_clim.csv")


## CLIMATE SPACE (FIGURE 2) ----
cover.ms.clim <- read.csv("mastersheets/cover_ms_clim.csv")
cover.ms.clim$Absolute_change <- abs(cover.ms.clim$Annual.rate)

(clim_space <- ggplot(cover.ms.clim, aes(x=tempc, y=precip, colour=Biome_trend)) + 
    geom_point(position = position_jitter(), aes(size = Absolute_change), alpha = 0.5) + 
    scale_radius(range=c(4, 30)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    guides(colour = guide_legend("Biome trends", override.aes = list(size = 12), order = 1), 
           size = guide_legend(order = 2)) +
    scale_colour_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                        guide = guide_legend(title = "Biome trends"), 
                        labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                   "Tundra decrease", "Tundra increase", "Tundra stable")) + 
    ylab("Mean Annual Precipitation (mm)\n") + xlab("\nMean Annual Temperature (°C)") + 
    labs(size = "Cover change rate (% per year)") + 
    scale_x_continuous(lim=c(-24, 33)) + scale_y_continuous(lim=c(-5, 2600)) + 
    theme(axis.title.x = element_text(face="bold", size=26),
          axis.text.x  = element_text(vjust=0.5, size=22, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=22, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(face = "bold", size=25), legend.text = element_text(size=22), 
          legend.key = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


## MAT marginal plot
avg_mat <- plyr::ddply(cover.ms.clim, "Biome_type", summarise, grop.mean=mean(tempc))

(clim.mat <- ggplot(cover.ms.clim, aes(x = tempc, fill = Biome_type)) + geom_density(alpha = 0.6) + 
    annotate("text", x = 15.5, y = 0.12, label = "Savanna", color = "#F8766D", size = 10) +
    annotate("text", x = -10, y = 0.12, label = "Tundra", color = "#00BFC4", size = 10) +
    scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
    geom_vline(data = avg_mat, aes(xintercept = grop.mean), linetype = "dashed", color = c("salmon", "turquoise4")) + 
    geom_vline(xintercept = 0) +
    xlab("Mean annual temperature (°C)\n") + ylab("\nDensity") + scale_x_continuous(lim=c(-24, 33)) +
    theme(axis.title.x = element_text(face="bold", size=20),
          axis.text.x  = element_text(vjust=0.5, size=14, colour = "black"), 
          axis.title.y = element_text(face="bold", size=20),
          axis.text.y  = element_text(vjust=0.5, size=14, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(size=20), legend.text = element_text(size=14), legend.key = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# MAP density plot
avg_map <- plyr::ddply(cover.ms.clim, "Biome_type", summarise, grop.mean=mean(precip))

(clim.map <- ggplot(cover.ms.clim, aes(x = precip, fill = Biome_type)) + geom_density(alpha = 0.6) + 
    coord_flip() + scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
    xlab("Mean annual precipitation (mm)\n") + ylab("\nDensity") + scale_x_continuous(lim = c(-5, 2600)) + 
    geom_vline(data = avg_map, aes(xintercept = grop.mean), linetype = "dashed", color = c("salmon", "turquoise4")) + 
    geom_vline(xintercept = 0) +
    theme(axis.title.x = element_text(face="bold", size=20),
          axis.text.x  = element_text(vjust=0.5, size=14, colour = "black"), 
          axis.title.y = element_text(face="bold", size=20),
          axis.text.y  = element_text(vjust=0.5, size=14, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(size=20), legend.text = element_text(size=14), legend.key = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


# Arrange marginal plots around the climatic space plot (Figure 2)
C1 <- insert_xaxis_grob(clim_space, clim.mat, grid::unit(1, "in"), position = "top")
C2 <- insert_yaxis_grob(C1, clim.map, grid::unit(1, "in"), position = "right")
ggdraw(C2)
ggplot2::ggsave(C2, filename = "figures/Figure_2.png", 
                width = 50, height = 25, units = "cm")


## CLIMATOLOGY MODELS ----

# Include geographical grids according to coordinates - every 10 degrees of latitude and longitude
cover.ms.clim <- cover.ms.clim %>% mutate(lat_cat = floor(Latitude), long_cat = floor(Longitude))
cover.ms.clim$lat_cat <- plyr::round_any(cover.ms.clim$lat_cat, 10, f = floor)
cover.ms.clim$long_cat <- plyr::round_any(cover.ms.clim$long_cat, 10, f = floor)
cover.ms.clim <- cover.ms.clim %>% mutate(geo.coords = paste("_", lat_cat, "_", long_cat, "_"))
write.csv(cover.ms.clim, "mastersheets/cover_ms_clim_coord.csv")

tun.clim <- filter(cover.ms.clim, Biome_type == "Tundra")
sav.clim <- filter(cover.ms.clim, Biome_type == "Savanna")


## Tundra climatology model
tun.clim.mod <- MCMCglmm(Annual.rate ~ tempc + precip + tempc*precip, data = tun.clim, 
                         random = ~us(1):geo.coords, nitt = 100000, burnin = 5000, thin = 30)

summary(tun.clim.mod)
plot(tun.clim.mod$Sol, auto.layout = F)
plot(tun.clim.mod$VCV)
autocorr.plot(tun.clim.mod$VCV)
hist(mcmc(tun.clim.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
# Positive slope with temperature but not significant, negative for precip and interaction (non-significant)
save(tun.clim.mod, file = "models/random/tun.clim.mod.RData")


# Tundra manual predictions
preddata <- expand.grid(tempc = seq(from=min(tun.clim$tempc), 
                                    to = max(tun.clim$tempc), by = 1), 
                        precip = c(quantile(tun.clim$precip,0.10), mean(tun.clim$precip),
                                   quantile(tun.clim$precip,0.90)),
                        tempcXprecip = 1,
                        geo.coords = 1,
                        Annual.rate = 0)
predclim.tun <- predict.MCMCglmm(tun.clim.mod, newdata = preddata, interval = "confidence", type = "terms")
pred.raw.clim.tun <- cbind(data.frame(predclim.tun), preddata)

# Plot predictions
(pred.clim.tun.plot <- ggplot()+
    geom_point(data = tun.clim, aes(x = tempc, y= Annual.rate), size = 2) +
    geom_line(data = pred.raw.clim.tun, aes(x = tempc, y = fit, colour = factor(precip)), size=2) + 
    geom_ribbon(data = pred.raw.clim.tun, aes(x = tempc, ymin = lwr, ymax = upr, fill = factor(precip)), 
                alpha = 0.32, linetype = 0) +
    theme_bw())
# No real difference here between precipitation gradients



## Savanna climatology model
sav.clim.mod <- MCMCglmm(Annual.rate ~ tempc + precip + tempc*precip, data = sav.clim, 
                         random = ~us(1):geo.coords, nitt = 100000, burnin = 5000, thin = 30)

summary(sav.clim.mod)
plot(sav.clim.mod$Sol, auto.layout = F)
plot(sav.clim.mod$VCV)
autocorr.plot(sav.clim.mod$VCV)
hist(mcmc(sav.clim.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
# positive ns interaction between temp x precip, negative slopes for both temp and precip (non-significant)
save(sav.clim.mod, file = "models/random/sav.clim.mod.RData")


# Savanna manual predictions
preddata2 <- expand.grid(precip = seq(from=min(sav.clim$precip), 
                                      to = max(sav.clim$precip), by = 1), 
                         tempc = c(quantile(sav.clim$tempc,0.10), mean(sav.clim$tempc),
                                   quantile(sav.clim$tempc,0.90)),
                         tempcXprecip = 1,
                         geo.coords = 1,
                         Annual.rate = 0)

predclim.sav <- predict.MCMCglmm(sav.clim.mod, newdata = preddata2, interval = "confidence", type = "terms")
pred.raw.clim.sav <- cbind(data.frame(predclim.sav), preddata2)


# Plot predictions
(pred.clim.sav.plot <- ggplot()+
    geom_point(data = sav.clim, aes(x = precip, y= Annual.rate), size = 2) +
    geom_line(data = pred.raw.clim.sav, aes(x = precip, y = fit, colour = factor(tempc)), size=2) + 
    geom_ribbon(data = pred.raw.clim.sav, aes(x = precip, ymin = lwr, ymax = upr, fill = factor(tempc)), 
                alpha = 0.32, linetype = 0) +
    theme_bw())
# No difference here between gradients


## FIGURE S4 ----

# MAT vs. cover change
(clim.mat.plot <- ggplot(cover.ms.clim, aes(x=tempc, y=Annual.rate, colour=Biome_type)) + 
    geom_point(size = 5, alpha = 0.5) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    ylab("Cover change rate (% per year)\n") + xlab("\nMean Annual Temperature (°C)") + labs(color = "Biome") +
    theme(axis.title.x = element_text(face="bold", size=26),
          axis.text.x  = element_text(vjust=0.5, size=22, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=22, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(face = "bold", size=25), legend.text = element_text(size=22), 
          legend.key = element_blank(), plot.margin = unit(c(1,1,1,1), units = , "cm")))

# MAP vs. cover change
cover.ms.clim.o <- arrange(cover.ms.clim, Biome_type)

(clim.map.plot <- ggplot(cover.ms.clim.o, aes(x=precip, y=Annual.rate, colour=Biome_type)) + 
    geom_point(size = 5, alpha = 0.5) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
    ylab("Cover change rate (% per year)\n") + xlab("\nMean Annual Precipitation (mm)") + labs(color = "Biome") +
    theme(axis.title.x = element_text(face="bold", size=26),
          axis.text.x  = element_text(vjust=0.5, size=22, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=22, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(face = "bold", size=25), legend.text = element_text(size=22), 
          legend.key = element_blank(), plot.margin = unit(c(1,1,1,1), units = , "cm")))

## Figure panel
(clim.panel <- ggpubr::ggarrange(clim.mat.plot, clim.map.plot, labels = c("(a)", "(b)"),
                                  font.label = list(size = 26), ncol =2, nrow = 1, 
                                 common.legend = TRUE, legend = "top"))
ggplot2::ggsave(clim.panel, filename = "figures/Figure_S4.png", 
                width = 50, height = 25, units = "cm")



## HIGH VS LOW RAINFALL SAVANNAS ----
high.rainfall <- filter(sav.clim, precip > 650) 
#316 sites out of 776, that's 40%

low.rainfall <- filter(sav.clim, precip < 650)
#459 sites out of 776, that's 59.1%

mean.high <- high.rainfall %>% summarise(mean(Annual.rate)) #0.4186774
mean.low <- low.rainfall %>% summarise(mean(Annual.rate)) #0.25

# Are cover change rates higher in high vs. low rainfall savannas?
sav.clim$low_high[sav.clim$precip <= 650] <- "low"
sav.clim$low_high[sav.clim$precip > 650] <- "high"

sav.rainfall <- MCMCglmm(Annual.rate ~ low_high, data = sav.clim, 
                          nitt = 100000, burnin = 5000, thin = 30)
summary(sav.rainfall) #marginally higher rates of cover change in high-rainfall savannas
save(sav.rainfall, file = "models/fixed/sav.rainfall.RData")

