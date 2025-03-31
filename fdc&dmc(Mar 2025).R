library(readxl)
library(ggpubr)
library(scales)
library(dplyr)
library(tidyr)
library(stats)
library(gridExtra)
library(purrr)
library(viridis)
library(imputeTS)
library(lubridate)
library(data.table)
library(fuzzyjoin)
library(hrbrthemes)
library(zoo)
library(ggthemes)



dff_physio <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/codes/catch_geo.csv")


df_storage <- read.csv("~/MUSES/2nd publication/codes/df_storage_new.csv", header = TRUE, sep = ",", dec = ".")

df_storage <- df_storage %>%  mutate(station2=recode(station2, "Schistose"="Weathered layer", "Marly"="Impermeable layer",
                                                 "Permeable layer interface"="Permeable layer interface", "Aggregated"="Aggregated"))

df_storage$station2 <- factor(df_storage$station2, levels = c("Weathered layer", "Impermeable layer", "Permeable layer interface", "Aggregated"))



df_storage <- df_storage[is.na(df_storage$P)==FALSE,]

  

dff_slope <- df_storage %>%  
  group_by(station, station2) %>%  
  arrange(date) %>%  
  mutate(Q_cum = cumsum(q),
         P_cum = cumsum(P))


dff_cum.seasonal <- df_storage %>%  
  mutate(month=month(date),
         season=ifelse(month %in% c(4:9),"summer","winter")) %>%
  group_by(station, station2, season) %>%  
  arrange(date) %>%  
  mutate(Q_cum = cumsum(q),
         P_cum = cumsum(P))


dff_cum.summer <- subset(dff_cum.seasonal, season == "summer")
dff_cum.winter <- subset(dff_cum.seasonal, season == "winter")




ddf_physio <- merge(subset(dff_physio, geology=="Sandstone and conglomerates")[,-2], 
                    subset(dff_physio, geology=="Marls and claystone")[,-2], 
                    by="station") %>%
  mutate(geo_ratio = value.x - value.y)


dff_slope <- merge(dff_slope, ddf_physio[,-c(2,3)])
dff_cum.summer <- merge(dff_cum.summer, ddf_physio[,-c(2,3)])
dff_cum.winter <- merge(dff_cum.winter, ddf_physio[,-c(2,3)])



plot_dual <- ggplot(dff_slope) + 
  geom_line(aes(x=P_cum, y=Q_cum, color=geo_ratio, group=station), linewidth=0.5) +
  geom_abline(aes(intercept=0, slope=1), linetype="dashed") +
  labs(x="Aggregated P [mm]", y="Aggregated q [mm]",
       color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  theme_bw() + theme(strip.background = element_rect(color=NA, fill=NA, size=0.5),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     text=element_text(size=8)) + 
  coord_cartesian(xlim = c(0, max(dff_slope$P_cum)), ylim = c(0, max(dff_slope$P_cum)))



plot_dual.summer <- ggplot(dff_cum.summer) + 
  geom_line(aes(x=P_cum, y=Q_cum, color=geo_ratio, group=station), linewidth=0.5) +
  geom_text(aes(x = 500, y = 4500, label = "Summer"), size=3) +
  geom_abline(aes(intercept=0, slope=1), linetype="dashed") +
  labs(x="Aggregated P [mm]", y="Aggregated q [mm]",
       color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  theme_bw() + theme(strip.background = element_rect(color=NA, fill=NA, size=0.5),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     text=element_text(size=8)) + 
  coord_cartesian(xlim = c(0, max(dff_cum.summer$P_cum)), 
                  ylim = c(0, max(dff_cum.summer$P_cum)))



plot_dual.winter <- ggplot(dff_cum.winter) + 
  geom_line(aes(x=P_cum, y=Q_cum, color=geo_ratio, group=station), linewidth=0.5) +
  geom_abline(aes(intercept=0, slope=1), linetype="dashed") +
  geom_text(aes(x = 500, y = 6000, label = "Winter"), size=3) +
  labs(x="Aggregated P [mm]", y="Aggregated q [mm]",
       color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  theme_bw() + theme(strip.background = element_rect(color=NA, fill=NA, size=0.5),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     text=element_text(size=8)) + 
  coord_cartesian(xlim = c(0, max(dff_cum.winter$P_cum)), 
                  ylim = c(0, max(dff_cum.winter$P_cum)))







df_fdc <- df_storage %>% 
  group_by(station) %>% 
  mutate(rank=rank(-q),
         epf=100*rank/(max(rank)+1))


df_fdc <- merge(df_fdc, ddf_physio[,-c(2,3)])



plot_fdc <- ggplot() + 
  geom_line(data=df_fdc, aes(x=epf, y=q, color=geo_ratio, group=station), size=0.5) +
  theme_bw() + labs(y=expression(paste("q [mm ",day^{-1},"]")), x="Exceedence probability [-]", 
                    color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  scale_y_continuous(trans = log10_trans(), limits = c(10^(-2), 10^2),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(size=8)) +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))



plot <- ggarrange(plot_dual, plot_dual.winter, plot_dual.summer, plot_fdc,
                  nrow=2, ncol=2, labels=c("(a)","(b)","(c)","(d)"), 
                  common.legend = T, legend = "right",
                  font.label = list(size = 8, face = "plain"),
                  widths = c(1, 1), heights = c(1, 1))

plot








setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()



ggsave(plot, path="figures", filename = "dm&fdc.png",
       device = ragg::agg_png, dpi=350,
       width = 17, height = 14, units = "cm",
       bg="white")








