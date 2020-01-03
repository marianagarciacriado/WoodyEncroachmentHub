#### Woody encroachment across biomes 
#### Script 13. Gradient analyses (dry vs wet sites, hot vs cool sites)
#### Mariana Garcia
#### April 2019

## Libraries
.libPaths("C:/R_library")
library(ggplot2)
library(dplyr)
library(MCMCglmm)
library(ggpubr)

## Setting the theme ----
clima.mod.theme <- theme(legend.position = "right", legend.title = element_text(size=26), 
                     legend.text = element_text(size=26), legend.key = element_blank(), 
                     legend.spacing.x = unit(0.3, 'cm'),
                     axis.title.x = element_text(face="bold", size=26),
                     axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=26),
                     axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))

## DATA LOADING ----
clim.fit.dw <- read.csv("scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/clima_fit_sst.csv")

# Add Site.ID to the mastersheet (this function is amazing)
clim.fit.dw <- transform(clim.fit.dw, Site.ID = as.numeric(interaction(Latitude, Longitude, drop = TRUE)))

# Filter for changes in MAT
mat.dw <- filter(clim.fit.dw, variable == "mat")
mat.tun.dw <- filter(mat.dw, Biome_type == "Tundra")
mat.sav.dw <- filter(mat.dw, Biome_type == "Savanna")

# Filter for changes in MAP
map.dw <- filter(clim.fit.dw, variable == "map")
map.sav.dw <- filter(map.dw, Biome_type == "Savanna")
map.tun.dw <- filter(map.dw, Biome_type == "Tundra")

## Defining prior - parameter-expanded prior with inverse Wishart distribution
a <- 1000
prior6 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a)))



## TUNDRA MAT X PRECIP MODEL ----
mat.tun.grad.mod <- MCMCglmm(Annual.rate ~ estimate + precip + estimate*precip, 
                            random = ~us(1):geo.coords, prior = prior6,
                            data = mat.tun.dw,
                            nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(mat.tun.grad.mod) # significant interaction
plot(mat.tun.grad.mod$Sol, auto.layout = F)
plot(mat.tun.grad.mod$VCV)
autocorr.plot(mat.tun.grad.mod$VCV)
save(mat.tun.grad.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat_tun_grad_mod.RData")


## Manual predictions
pred.tg <- expand.grid(estimate = seq(from=min(mat.tun.dw$estimate), 
                                        to = max(mat.tun.dw$estimate), by = 0.05), 
                         precip = c(quantile(mat.tun.dw$precip, 0.20), 
                                    quantile(mat.tun.dw$precip, 0.50), 
                                    quantile(mat.tun.dw$precip, 0.80)),
                         estimateXprec = 1,
                         geo.coords = 1,
                         Annual.rate = 0)

predtungrad <- predict.MCMCglmm(mat.tun.grad.mod, newdata = pred.tg, interval = "confidence", type = "terms")
pred.raw.tun.grad <- cbind(data.frame(predtungrad), pred.tg)


# colour palette
mycolors.rain <- c("#eec136", "#B8B8B8", "#3182bd")

# add colours to the points according to the precipitation quantile value
mat.tun.dw$color.rain[mat.tun.dw$precip <= 164] <- "#eec136"
mat.tun.dw$color.rain[mat.tun.dw$precip > 164 & map.tun.dw$precip < 673] <- "#B8B8B8"
mat.tun.dw$color.rain[mat.tun.dw$precip >= 673] <- "#3182bd"

# alpha palette
mycolors.alpha <- c(0.6, 0.2, 0.6)

# add colours to the points according to the precipitation quantile value
mat.tun.dw$alpha.rain[mat.tun.dw$precip <= 164] <- "0.6"
mat.tun.dw$alpha.rain[mat.tun.dw$precip > 164 & map.tun.dw$precip < 673] <- "0.3"
mat.tun.dw$alpha.rain[mat.tun.dw$precip >= 673] <- "0.6"

# add colours to the points according to the precipitation quantile value
pred.raw.tun.grad$alpha.rain[pred.raw.tun.grad$precip <= 164] <- "0.6"
pred.raw.tun.grad$alpha.rain[pred.raw.tun.grad$precip > 164 & pred.raw.tun.grad$precip < 673] <- "0.3"
pred.raw.tun.grad$alpha.rain[pred.raw.tun.grad$precip >= 673] <- "0.6"


## Plotting predictions
(pred.tun.grad.plot <- ggplot()+ ylab("Woody cover change rate (% per year)\n") + xlab("\nTundra MAT change rate (°C per year)") +
    geom_point(data = mat.tun.dw, aes(x = estimate, y = Annual.rate), alpha = mat.tun.dw$alpha.rain, 
               colour = mat.tun.dw$color.rain, size = 5) +
    geom_line(data = pred.raw.tun.grad, aes(x = estimate, y = fit, colour = factor(precip)), size = 2) + 
    scale_colour_manual(values = mycolors.rain, name = "Precipitation (mm)", labels = c("Dry", "Average", "Wet")) +
    scale_fill_manual(values = mycolors.rain, name = "Precipitation (mm)", labels = c("Dry", "Average", "Wet")) +
    geom_ribbon(data = pred.raw.tun.grad, aes(x = estimate, ymin = lwr, ymax = upr, fill = factor(precip)), 
                alpha = pred.raw.tun.grad$alpha.rain) + geom_hline(yintercept=0, linetype="dashed") +
    theme(aspect.ratio = 1) + clima.mod.theme)  



## SAVANNA MAT X PRECIP MODEL ----
mat.sav.grad.mod <- MCMCglmm(Annual.rate ~ estimate + precip + estimate*precip, 
                             random = ~us(1):geo.coords, prior = prior6,
                             data = mat.sav.dw,
                             nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(mat.sav.grad.mod) #non-significant interaction
plot(mat.sav.grad.mod$Sol, auto.layout = F)
plot(mat.sav.grad.mod$VCV)
autocorr.plot(mat.sav.grad.mod$VCV)
save(mat.sav.grad.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat_sav_grad_mod.RData")




## SAVANNA PRECIP X MAP MODEL ----
sav.rain <- MCMCglmm(Annual.rate ~ precip + estimate + precip*estimate, 
                     random = ~us(1):geo.coords, prior = prior6,
                     data = map.sav.dw, 
                     nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sav.rain) #significant relationship
plot(sav.rain$Sol, auto.layout = F)
plot(sav.rain$VCV)
autocorr.plot(msav.rain$VCV)
save(sav.rain, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sav_rain_mod.RData")

# Predictions
pred.sr <- expand.grid(estimate = seq(from=min(map.sav.dw$estimate), 
                                      to = max(map.sav.dw$estimate), by = 1), 
                       precip = c(quantile(map.sav.dw$precip, 0.20), 
                                        quantile(map.sav.dw$precip, 0.50), 
                                        quantile(map.sav.dw$precip, 0.80)),
                       estimateXprecip = 1,
                       geo.coords = 1,
                       Annual.rate = 0)

predsavrain <- predict.MCMCglmm(sav.rain, newdata = pred.sr, interval = "confidence", type = "terms")
pred.raw.sav.rain <- cbind(data.frame(predsavrain), pred.sr)


# colour palette
mycolors.rain <- c("#eec136", "#B8B8B8", "#3182bd")

# add colours to the points according to the precipitation quantile value
map.sav.dw$color.rain[map.sav.dw$precip <= 248] <- "#eec136"
map.sav.dw$color.rain[map.sav.dw$precip > 248 & map.sav.dw$precip < 1001] <- "#B8B8B8"
map.sav.dw$color.rain[map.sav.dw$precip >= 1001] <- "#3182bd"

# alpha palette
mycolors.alpha <- c(0.6, 0.2, 0.6)

# add colours to the points according to the precipitation quantile value
map.sav.dw$alpha.rain[map.sav.dw$precip <= 248] <- "0.6"
map.sav.dw$alpha.rain[map.sav.dw$precip > 248 & map.sav.dw$precip < 1001] <- "0.3"
map.sav.dw$alpha.rain[map.sav.dw$precip >= 1001] <- "0.6"

# add colours to the points according to the precipitation quantile value
pred.raw.sav.rain$alpha.rain[pred.raw.sav.rain$precip <= 248] <- "0.6"
pred.raw.sav.rain$alpha.rain[pred.raw.sav.rain$precip > 248 & pred.raw.sav.rain$precip < 1001] <- "0.3"
pred.raw.sav.rain$alpha.rain[pred.raw.sav.rain$precip >= 1001] <- "0.6"


## Plotting predictions
(pred.sav.rain.plot <- ggplot()+ ylab("Woody cover change rate (% per year)\n") + xlab("\nSavanna MAP change rate (mm per year)") +
    geom_point(data = map.sav.dw, aes(x = estimate, y = Annual.rate), alpha = map.sav.dw$alpha.rain, 
               colour = map.sav.dw$color.rain, size = 5) +
    geom_line(data = pred.raw.sav.rain, aes(x = estimate, y = fit, colour = factor(precip)), size = 2) + 
    scale_colour_manual(values = mycolors.rain, name = "Precipitation (mm)", labels = c("Dry", "Average", "Wet")) +
    scale_fill_manual(values = mycolors.rain, name = "Precipitation (mm)", labels = c("Dry", "Average", "Wet")) +
    geom_ribbon(data = pred.raw.sav.rain, aes(x = estimate, ymin = lwr, ymax = upr, fill = factor(precip)), 
                alpha = pred.raw.sav.rain$alpha.rain) + geom_hline(yintercept=0, linetype="dashed") +
    theme(aspect.ratio = 1) + clima.mod.theme)  




## TUNDRA PRECIP X MAP MODEL ----
tun.rain <- MCMCglmm(Annual.rate ~ precip + estimate + precip*estimate, 
                     random = ~us(1):geo.coords, prior = prior6,
                     data = map.tun.dw, 
                     nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(tun.rain) #non-significant
plot(tun.rain$Sol)
plot(tun.rain$VCV)
autocorr.plot(tun.rain$VCV)
save(tun.rain, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/tun_rain_mod.RData")



## SAVANNA MAP X TEMP MODEL ---- 
map.sav.grad.mod <- MCMCglmm(Annual.rate ~ estimate + tempc + estimate*tempc, 
                            random = ~us(1):geo.coords, prior = prior6,
                            data = map.sav.dw, 
                            nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.sav.grad.mod) #non-significant interaction
plot(map.sav.grad.mod$Sol)
plot(map.sav.grad.mod$VCV)
autocorr.plot(map.sav.grad.mod$VCV)
save(map.sav.grad.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map_sav_grad_mod.RData")



## TUNDRA MAP X TEMP MODEL ----
map.tun.grad.mod <- MCMCglmm(Annual.rate ~ estimate + tempc + estimate*tempc, 
                             random = ~us(1):geo.coords, prior = prior6,
                             data = map.tun.dw,
                             nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.tun.grad.mod) #non-significant interaction
plot(map.tun.grad.mod$Sol)
plot(map.tun.grad.mod$VCV)
autocorr.plot(map.tun.grad.mod$VCV)
save(map.tun.grad.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map_tun_grad_mod.RData")



## TUNDRA MAT X TEMP MODEL ----
tun.mat.temp.mod <- MCMCglmm(Annual.rate ~ estimate + tempc + estimate*tempc, 
                             random = ~us(1):geo.coords, prior = prior6,
                             data = mat.tun.dw,
                             nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(tun.mat.temp.mod) #non-significant interaction
plot(tun.mat.temp.mod$Sol)
plot(tun.mat.temp.mod$VCV)
autocorr.plot(tun.mat.temp.mod$VCV)
save(tun.mat.temp.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/tun_mat_temp_mod.RData")



## SAVANNA MAT X TEMP MODEL ----
sav.mat.temp.mod <- MCMCglmm(Annual.rate ~ estimate + tempc + estimate*tempc, 
                             random = ~us(1):geo.coords, prior = prior6,
                             data = mat.sav.dw,
                             nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sav.mat.temp.mod) #non-significant interaction
plot(sav.mat.temp.mod$Sol)
plot(sav.mat.temp.mod$VCV)
autocorr.plot(sav.mat.temp.mod$VCV)
save(sav.mat.temp.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sav_mat_temp_mod.RData")




## FIGURE PANEL ----
(full.panel <- ggarrange(effect.sizes.temp.sig, effect.sizes.prec.sig, pred.tun.grad.plot, pred.sav.rain.plot, 
                         labels = c("(a)", "(b)", "(c)", "(d)"), nrow = 2, ncol = 2, font.label = list(size = 26)))

ggsave(full.panel, filename = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/full_panel.png", 
       width = 65, height = 40, units = "cm")
