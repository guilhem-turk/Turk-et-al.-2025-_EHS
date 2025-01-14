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
library(ggrepel)


streams3 <- read.csv("~/MUSES/2nd publication/codes/streams3.csv", header = TRUE, sep = ",", dec = ".")


streams3$date <- as.Date(streams3$date)


streams3 <- streams3 %>%  mutate(station2=recode(station, "i Huewelerbach"="Permeable layer interface", "b Colpach"="Schistose",
                                                 "a Weierbach"="Schistose", "h Pall"="Permeable layer interface", "j Reichlange"="Aggregated",
                                                 "g Schwebich"="Permeable layer interface", "e Wollefsbach"="Marly",
                                                 "l Roudbach"="Aggregated","c Mierbech"="Marly", "d Bibeschbach"="Marly", 
                                                 "f Mess"="Marly", "k Useldange"="Aggregated"))

streams3 <- streams3 %>%  mutate(station2=recode(station2, "Schistose"="Weathered layer", "Marly"="Impermeable layer",
                                                 "Permeable layer interface"="Permeable layer interface", "Aggregated"="Aggregated"))

streams3$station2 <- factor(streams3$station2, levels = c("Weathered layer", "Impermeable layer", "Permeable layer interface", "Aggregated"))




stations <- streams3 %>% 
  group_by(station)  %>% 
  reframe(station2 = unique(station2))



streams3$P[is.na(streams3$P)] <- 0
streams3$q[is.na(streams3$q)] <- 0
streams3$q[streams3$q < 0] <- 0


dff <- streams3 %>% 
  filter(P > 0) %>% 
  group_by(station) %>%
  reframe(HP = quantile(P, 0.75, na.rm=T))

dff <- merge(streams3, dff, by="station")

dff$p_top <- 1
dff$p_top[dff$P<dff$HP] <- 0


dff <- dff %>% 
  group_by(station) %>%
  mutate(HQ = quantile(q, 0.75, na.rm=T))

dff$q_top <- 1
dff$q_top[dff$q<dff$HQ] <- 0



dff <- arrange(dff,date)






## compute the TTDs through ensemble hydrograph separation


setwd("C:/Users/turk/Documents/MUSES/Literature/Fnw_Ensemble hydrograph separation/EHS v1.4 R scripts and demo files")  # set this path to the folder where you have the source code EHS_v1.4 and the input file specified by input_filename



source("EHS_v1.4.R")   #replace filename as needed for future versions


# now set these options
p_thresh <- 0.5  # this is the threshold precipitation rate (in P units) below which P tracer inputs will be ignored

nttd <- 13   # number of time steps for TTDs

# these are the lag times associated with each time step
TTD_lag <- seq(1, nttd) - 0.5  # here we treat Q and Cq as being instantaneously measured
# if instead they are time-averaged, then replace -0.5 with -0.75 
# (see Kirchner, 2019, but note that here the index starts at 1 rather than 0)



ttd_comp <- function(dff) {
  
  ttd <- list()
  ttd_se <- list()
  wtd_ttd <- list()
  wtd_ttd_se <- list()
  
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Pfilter <- dff$p_top   # filter for top 20% of (above-threshold) precipitation values
  Qfilter <- dff$q_top   # filter for top 20% of discharge values
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  # estimate transit time distributions (TTDs)
  ttd <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, m=nttd-1)[1]
  
  # estimate transit time distributions (TTDs)
  ttd_se <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, m=nttd-1)[2]
  
  # estimate flow-weighted TTDs
  ttd_nr <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, robust=FALSE, m=nttd-1)[1]
  
  # estimate flow-weighted TTDs
  ttd_se_nr <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, robust=FALSE, m=nttd-1)[2]
  
  
  return(list(ttd, ttd_se, ttd_nr, ttd_se_nr))
  
}


ttd <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[1])))

ttd_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[2])))

ttd_nr <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[3])))

ttd_se_nr <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[4])))



results.ttd <- data.frame(
  lag = TTD_lag,
  ttd = unlist(ttd),
  ttd_se = unlist(ttd_se),
  ttd_nr = unlist(ttd_nr),
  ttd_se_nr = unlist(ttd_se_nr),
  station = rep(stations$station, each=13),
  station2 = rep(stations$station2, each=13),
  name = "Fnew_all")



results.ttd$ttd[results.ttd$ttd < 0] <- 0
results.ttd$ttd_se[results.ttd$ttd < 0] <- 0


results.ttd2 <- results.ttd %>%
  group_by(station, station2) %>%
  mutate(ttd = cumsum(ttd),
         ttd_se = sqrt(cumsum(ttd_se^2)))


results.ttd3 <- results.ttd2 %>%
  group_by(lag, name) %>%
  reframe(ttd = mean(ttd))







## same procedure, but for the highest 20 % of discharge


ttd_comp <- function(dff) {
  
  ttd <- list()
  ttd_se <- list()
  wtd_ttd <- list()
  wtd_ttd_se <- list()
  
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Pfilter <- dff$p_top   # filter for top 20% of (above-threshold) precipitation values
  Qfilter <- dff$q_top   # filter for top 20% of discharge values
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  # estimate transit time distributions (TTDs)
  ttd <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, Qfilter=Qfilter, m=nttd-1)[1]
  
  # estimate transit time distributions (TTDs)
  ttd_se <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, Qfilter=Qfilter, m=nttd-1)[2]
  
  # estimate flow-weighted TTDs
  ttd_nr <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, Qfilter=Qfilter, robust=FALSE, m=nttd-1)[1]
  
  # estimate flow-weighted TTDs
  ttd_se_nr <- EHS_TTD(Cp, Cq, p, q, p_threshold=p_thresh, Qfilter=Qfilter, robust=FALSE, m=nttd-1)[2]
  
  
  return(list(ttd, ttd_se, ttd_nr, ttd_se_nr))
  
}


ttd <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[1])))

ttd_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[2])))

ttd_nr <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[3])))

ttd_se_nr <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ttd_comp(.x)[4])))





results.ttd_b <- data.frame(
  lag = TTD_lag,
  ttd = unlist(ttd),
  ttd_se = unlist(ttd_se),
  ttd_nr = unlist(ttd_nr),
  ttd_se_nr = unlist(ttd_se_nr),
  station = rep(stations$station, each=13),
  station2 = rep(stations$station2, each=13),
  name = "Fnew_hq20")



results.ttd_b$ttd[results.ttd_b$ttd < 0] <- 0
results.ttd_b$ttd_se[results.ttd_b$ttd < 0] <- 0


results.ttd2_b <- results.ttd_b %>%
  group_by(station, station2) %>%
  mutate(ttd = cumsum(ttd),
         ttd_se = sqrt(cumsum(ttd_se^2)))


results.ttd3_b <- results.ttd2_b %>%
  group_by(lag, name) %>%
  reframe(ttd = mean(ttd))




ddf <- bind_rows(results.ttd2, results.ttd2_b)
ddf2 <- bind_rows(results.ttd3, results.ttd3_b)






# Create a new data frame for facet labels
facet_labels <- ddf %>%
  group_by(station) %>%
  summarise(lag = max(ddf$lag) * 0.05, ttd = 0.95)

facet_labels2 <- ddf %>%
  group_by(station, station2) %>%
  summarise(lag = max(ddf$lag) * 0.05, ttd = 0.80)



# Calculate regression coefficients
regressions <- ddf2 %>%
  group_by(name) %>%
  do(model = lm(ttd ~ sqrt(lag*2+1), data = .)) %>%
  mutate(
    intercept = coef(model)[1],
    slope = coef(model)[2],
    equation = paste0("y = ", round(intercept, 2), " + ", round(slope, 2), "*sqrt(x)")
  )








plot <- ggplot(ddf) + 
  geom_smooth(data=ddf2, aes(x=lag*2+1, y=ttd, linetype=name), method=lm, formula = y~sqrt(x), 
              fullrange=T, color="darkgrey", se=F, linewidth=0.7) +
  geom_errorbar(aes(x=lag*2+1, y=ttd, ymin=ttd-ttd_se, ymax=ttd+ttd_se, color=station2), 
                width=0, linewidth=0.4) +
  geom_point(aes(x=lag*2+1, y=ttd, color=station2, shape=name), fill="white", stroke=0.7, size=1) +
  geom_text(data = facet_labels, aes(x = lag*2+1, y = ttd, label = station), 
            hjust = 0, vjust = 1, size = 3, color = "black") + 
  geom_text(data = facet_labels2, aes(x = lag*2+1, y = ttd, label = station2, color=station2), 
            hjust = 0, vjust = 1, size = 3) + 
  scale_linetype_manual(values=c("dotted", "solid"), labels=regressions$equation) +
  scale_color_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  scale_shape_manual(values=c(21,24), labels=c("All samples", "Highest 20 % q")) +
  theme_bw() + labs(x="Time interval [weeks]", y=expression(paste(F[new]," [-]")), 
                    linetype="", shape="", color="") +
  facet_wrap(~station, ncol=3, scales = "free") + ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 27, by = 4)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        text = element_text(size=9),
        legend.text = element_text(size = 9)) +
  guides(color="none", )



plot





subset(ddf, lag == 12.5) %>%
  group_by(station2) %>%
  reframe(ttd_dev = round((max(ttd)-min(ttd))*100/2,1),
          ttd = mean(ttd)*100)
  






setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()



ggsave(plot, path="figures", filename = "TTD_all.png",
       device = ragg::agg_png, dpi=350,
       width = 17, height = 20, units = "cm",
       bg="white")

