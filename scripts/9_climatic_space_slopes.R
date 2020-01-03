#### Woody encroachment across biomes 
#### Script 9. Climatic space with site-specific climatic change
#### Mariana Garcia
#### February 2018

# Load packages ----
.libPaths("C:/R_library")
library(dplyr)
library(ggplot2)
library(MCMCglmm)
library(cowplot)

##### SST change - climatic space ----
clim.sst.cs <- read.csv("scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/clima_fit_sst.csv")

mat_slopes_sst <- filter(clim.sst.cs, variable == "mat")
map_slopes_sst <- filter(clim.sst.cs, variable == "map")
clima_slopes_sst <- merge(mat_slopes_sst, map_slopes_sst, by = "Plot.ID")

# convert negative values to absolute
clima_slopes_sst$Annual.rate.x <- abs(clima_slopes_sst$Annual.rate.x)

# plot change in climatic variables
(clim_space_sst <- ggplot(data = clima_slopes_sst, aes(x=estimate.x, y=estimate.y, colour=Biome_trend.x)) + 
    geom_point(position = position_jitter(), aes(size = Annual.rate.x), alpha = 0.5) + scale_radius(range=c(4, 30)) +
    guides(colour = guide_legend("Biome trends", override.aes = list(size = 12), order = 1), size = guide_legend(order = 2)) + 
    geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
    scale_colour_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray18"), 
                        guide = guide_legend(title = "Biome trends"), 
                        labels = c("Savannah decrease", "Savannah increase", "Savannah stable", 
                                   "Tundra decrease", "Tundra increase", "Tundra stable")) +
    ylab("Change in MAP (mm per year)\n") + xlab("\nChange in MAT (°C per year)") +
    labs(size = "Cover change rate (% per year)", colour = "Biome trends") +
    theme(axis.title.x = element_text(face="bold", size=26),
          axis.text.x  = element_text(vjust=0.5, size=22, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=22, colour = "black"),
          panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
          panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.title = element_text(face = "bold", size = 25), legend.text = element_text(size=22), 
          legend.key = element_blank(),
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


## MAT marginal plot ----
(dens.mat.sst <- ggplot(mat_slopes_sst, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.6) + 
   annotate("text", x = -0.05, y = 11, label = "Savanna", color = "#F8766D", size = 6) +
   annotate("text", x = 0.1, y = 11, label = "Tundra", color = "#00BFC4", size = 6) +
   scale_fill_manual(values = c("#F8766D", "#00BFC4")) + geom_vline(xintercept = 0) +
   theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   guides(fill=guide_legend(title="Biomes")) +
   ylab("Density") + xlab("Change in MAT (°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.012434, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.058117, linetype = "dashed", color = "turquoise4") +
   theme(axis.title.x = element_text(face="bold", size=16),
         axis.text.x  = element_text(vjust=0.5, size=12, colour = "black"), 
         axis.title.y = element_text(face="bold", size=16),
         axis.text.y  = element_text(vjust=0.5, size=12, colour = "black"),
         panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
         panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.title = element_text(size=20), legend.text = element_text(size=14), legend.key = element_blank(),
         plot.margin = unit(c(1,1,1,1), units = , "cm")))


## MAP marginal plot ----
(dens.map.sst <- ggplot(map_slopes_sst, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.6) + coord_flip() +
   theme(legend.position = "right", panel.background = element_blank(), axis.line = element_line(colour = "black")) +
   guides(fill=guide_legend(title="Biomes")) + 
   ylab("Density") + xlab("Change in MAP (mm per year)") + 
   scale_fill_manual(values = c("#F8766D", "#00BFC4")) + geom_vline(xintercept = 0) +
   geom_vline(xintercept = 0) + geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.43650, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.14759, linetype = "dashed", color = "turquoise4") +
   theme(axis.title.x = element_text(face="bold", size=16),
         axis.text.x  = element_text(vjust=0.5, size=12, colour = "black"), 
         axis.title.y = element_text(face="bold", size=16),
         axis.text.y  = element_text(vjust=0.5, size=12, colour = "black"),
         panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
         panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.title = element_text(size=20), legend.text = element_text(size=14), legend.key = element_blank(),
         plot.margin = unit(c(1,1,1,1), units = , "cm")))


## Complete plot with marginal plots ----
z1 <- insert_xaxis_grob(clim_space_sst, dens.mat.sst, grid::unit(1, "in"), position = "top")
z2 <- insert_yaxis_grob(z1, dens.map.sst, grid::unit(1, "in"), position = "right")
ggdraw(z2)
ggplot2::ggsave(z2, filename = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/Figure_S3.png", 
                width = 50, height = 25, units = "cm")


## Climatic space change model ----
tun_matp_slopes <- filter(clima_slopes_sst, Biome_type.x == "Tundra")
sav_matp_slopes <- filter(clima_slopes_sst, Biome_type.x == "Savanna")


## Fitting the model with grid cells as random factor and interaction between climatic factors as fixed effects

## Tundra model ----
tun.slop.mod <- MCMCglmm(Annual.rate.x ~ estimate.x + estimate.y + estimate.x * estimate.y, 
                         data = tun_matp_slopes, random = ~us(1):geo.coords.x, 
                         nitt = 100000, burnin = 5000, thin = 30)

summary(tun.slop.mod) #negative significant slope for temp change, positive non-significant for precip 
# and negative non-significantinteraction
plot(tun.slop.mod$Sol, auto.layout = F)
plot(tun.slop.mod$VCV)
autocorr.plot(tun.slop.mod$VCV)
save(tun.slop.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/tun_slop_mod.RData")


# model predictions
preddata4 <- expand.grid(estimate.x = seq(from=min(tun_matp_slopes$estimate.x), 
                                          to = max(tun_matp_slopes$estimate.x), by = 0.02), 
                         estimate.y = c(quantile(tun_matp_slopes$estimate.y,0.10), mean(tun_matp_slopes$estimate.y),
                                        quantile(tun_matp_slopes$estimate.y,0.90)),
                         estimate.xXestimate.y = 1,
                         geo.coords.x = 1,
                         Annual.rate.x = 0)

predslop.tun <- predict.MCMCglmm(tun.slop.mod, newdata = preddata4, interval = "confidence", type = "terms")
pred.raw.slop.tun <- cbind(data.frame(predslop.tun), preddata4)

# plot predictions
(pred.slop.tun.plot <- ggplot()+
    geom_point(data = tun_matp_slopes, aes(x = estimate.x, y= Annual.rate.x), size = 2) +
    geom_line(data = pred.raw.slop.tun, aes(x = estimate.x, y = fit, colour = factor(estimate.y)), size=2) + 
    geom_ribbon(data = pred.raw.slop.tun, aes(x = estimate.x, ymin = lwr, ymax = upr, fill = factor(estimate.y)), 
                alpha = 0.32, linetype = 0) +
    theme_bw())
#no major difference in predictions between gradients


## Savanna model ----
sav.slop.mod <- MCMCglmm(Annual.rate.x ~ estimate.x + estimate.y + estimate.x * estimate.y, 
                         data = sav_matp_slopes, random = ~us(1):geo.coords.x, 
                         nitt = 100000, burnin = 5000, thin = 30)
                               
summary(sav.slop.mod) # positive slopes for all, non-significant
plot(sav.slop.mod$Sol, auto.layout = F)
plot(sav.slop.mod$VCV)
autocorr.plot(sav.slop.mod$VCV)
save(sav.slop.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sav_slop_mod.RData")

# model predictions
preddata5 <- expand.grid(estimate.y = seq(from=min(sav_matp_slopes$estimate.y), 
                                          to = max(sav_matp_slopes$estimate.y), by = 1), 
                         estimate.x = c(quantile(sav_matp_slopes$estimate.x,0.10), mean(sav_matp_slopes$estimate.x),
                                        quantile(sav_matp_slopes$estimate.x,0.90)),
                         estimate.xXestimate.y = 1,
                         geo.coords.x = 1,
                         Annual.rate.x = 0)

predslop.sav <- predict.MCMCglmm(sav.slop.mod, newdata = preddata5, interval = "confidence", type = "terms")
pred.raw.slop.sav <- cbind(data.frame(predslop.sav), preddata5)

# plot predictions
(pred.slop.sav.plot <- ggplot()+
    geom_point(data = sav_matp_slopes, aes(x = estimate.y, y= Annual.rate.x), size = 2) +
    geom_line(data = pred.raw.slop.sav, aes(x = estimate.y, y = fit, colour = factor(estimate.x)), size=2) + 
    geom_ribbon(data = pred.raw.slop.sav, aes(x = estimate.y, ymin = lwr, ymax = upr, fill = factor(estimate.x)), 
                alpha = 0.32, linetype = 0) +
    theme_bw())
#no major difference in predictions between gradients