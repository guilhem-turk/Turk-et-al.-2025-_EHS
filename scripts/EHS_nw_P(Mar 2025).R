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



streams3 <- read.csv("~/MUSES/2nd publication/codes/streams3.csv", header = TRUE, sep = ",", dec = ".")


streams3$date <- as.Date(streams3$date)


streams3 <- streams3 %>%  mutate(station2=recode(station2, "Schistose"="Weathered layer", "Marly"="Impermeable layer",
                                                 "Permeable layer interface"="Permeable layer interface", "Aggregated"="Aggregated"))

streams3$station2 <- factor(streams3$station2, levels = c("Weathered layer", "Impermeable layer", "Permeable layer interface", "Aggregated"))




stations <- streams3 %>% 
  group_by(station)  %>% 
  reframe(station2 = unique(station2))



streams3$P[is.na(streams3$P)] <- 0
streams3$q[streams3$q < 0] <- 0



dff <- streams3[is.na(streams3$q)==FALSE,]


dff2 <- dff %>% 
  filter(P > 0) %>% 
  group_by(station) %>%
  reframe(HP = quantile(P, 0.75, na.rm=T))

dff <- merge(dff, dff2, by="station")

dff$p_top <- 1
dff$p_top[dff$P<dff$HP] <- 0


dff <- dff %>% 
  group_by(station) %>%
  mutate(Hq = quantile(q, 0.05, na.rm=T))

dff$q_top <- 1
dff$q_top[dff$q<dff$Hq] <- 0



dff <- arrange(dff,date)








setwd("C:/Users/turk/Documents/MUSES/Literature/Fnw_Ensemble hydrograph separation/EHS v1.4 R scripts and demo files")  # set this path to the folder where you have the source code EHS_v1.4 and the input file specified by input_filename



source("EHS_v1.4.R")   #replace filename as needed for future versions


# now set these options
p_thresh <- 0.5  # this is the threshold precipitation rate (in P units) below which P tracer inputs will be ignored











# now calculate the Fnw over the entire timeseries in all the nested catchments

ehs_comp <- function(dff) {
  
  dff <- arrange(dff,date)
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  # estimate (robust, weighted) new water fractions for events only
  qpFnew <- try(EHS_Fnew(Cp, Cq, p, q, p_threshold=p_thresh)[1])
  
  # estimate (robust, weighted) new water fractions standard error for events only
  qpFnew_se <- try(EHS_Fnew(Cp, Cq, p, q, p_threshold=p_thresh)[2])
  
  
  # estimate (non-robust, weighted) new water fractions for events only
  nr_qpFnew <- try(EHS_Fnew(Cp, Cq, p, q, robust=FALSE, p_threshold=p_thresh)[1])
  
  # estimate (non-robust, weighted) new water fractions standard error for events only
  nr_qpFnew_se <- try(EHS_Fnew(Cp, Cq, p, q, robust=FALSE, p_threshold=p_thresh)[2])
  
  return(list(qpFnew, qpFnew_se, nr_qpFnew, nr_qpFnew_se))
  
}



qpFnew <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_comp(.x)[1])))


qpFnew_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_comp(.x)[2])))


nr_qpFnew <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_comp(.x)[3])))


nr_qpFnew_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_comp(.x)[4])))



results.fnw <- data.frame(
  qpFnew = unlist(qpFnew),
  qpFnew_se = unlist(qpFnew_se),
  nr_qpFnew = unlist(nr_qpFnew),
  nr_qpFnew_se = unlist(nr_qpFnew_se),
  station = stations$station,
  station2 = stations$station2)









# now calculate the Fnw profiles according to the flow rate in all the nested catchments

p_thresh <- 0.5


ehs_prof_comp <- function(dff) {
  
  dff <- arrange(dff,date)
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  profile <- try(data.frame(EHS_profile(Cp, Cq, p, q, p_threshold=p_thresh, crit=p,
                                        lwr = c(50, 60, 70, 80, 90),
                                        upr = c(60, 70, 80, 90, 100))))
  
  return(profile)
  
}


profile_results <- dff %>% 
  group_by(station) %>%
  group_map(~ehs_prof_comp(.x))


Fnew <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[5])))


Fnew_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[6])))


p_profile <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[1])))


fnw_profile_p <- data.frame(
  P = unlist(p_profile),
  qpFnew = unlist(Fnew),
  qpFnew_se = unlist(Fnew_se),
  station = rep(stations$station, each=5),
  station2 = rep(stations$station2, each=5))







# calculate the Fnw profiles according to the flow rate in all the nested catchments using the non-robust approach



ehs_prof_comp <- function(dff) {
  
  dff <- arrange(dff,date)
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  profile <- try(data.frame(EHS_profile(Cp, Cq, p, q, robust=FALSE, p_threshold=p_thresh, crit=p, 
                                        lwr = c(50, 60, 70, 80, 90),
                                        upr = c(60, 70, 80, 90, 100))))
  
  return(profile)
  
}


profile_results <- dff %>% 
  group_by(station) %>%
  group_map(~ehs_prof_comp(.x))


Fnew <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[5])))


Fnew_se <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[6])))


p_profile <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[1])))


fnw_profile.nr_p <- data.frame(
  P = unlist(p_profile),
  qpFnew = unlist(Fnew),
  qpFnew_se = unlist(Fnew_se),
  station = rep(stations$station, each=5),
  station2 = rep(stations$station2, each=5))





ddf_physio <- merge(subset(dff_physio, geology=="Sandstone and conglomerates")[,-2], 
                    subset(dff_physio, geology=="Marls and claystone")[,-2], 
                    by="station") %>%
  mutate(geo_ratio = value.x - value.y)


fnw_profile_p <- merge(fnw_profile_p, ddf_physio[,-c(2,3)])
fnw_profile.nr_p <- merge(fnw_profile.nr_p, ddf_physio[,-c(2,3)])





# Create a new data frame for facet labels
facet_labels <- fnw_profile.nr_p %>%
  group_by(station, station2) %>%
  summarise(P = 2.5, qpFnew = 0.5)



plot_fnw_profile <- ggplot() + 
  geom_hline(data=results.fnw, aes(yintercept=qpFnew)) +
  geom_hline(data=results.fnw, aes(yintercept=nr_qpFnew), linetype="dashed") +
  geom_errorbar(data=fnw_profile.nr_p, aes(x=P+1, y=qpFnew, ymin=qpFnew-qpFnew_se, 
                                           ymax=qpFnew+qpFnew_se, color=geo_ratio), width=0, linewidth=0.6) +
  geom_point(data=fnw_profile.nr_p, aes(x=P+1, y=qpFnew, color=geo_ratio, shape="Non-robust"), size=2) +
  geom_errorbar(data=fnw_profile_p, aes(x=P, y=qpFnew, ymin=qpFnew-qpFnew_se, 
                                        ymax=qpFnew+qpFnew_se, color=geo_ratio), width=0, linewidth=0.6) +
  geom_point(data=fnw_profile_p, aes(x=P, y=qpFnew, color=geo_ratio, shape="Robust"), fill="white", size=2, stroke=1) +
  geom_text(data = facet_labels, aes(x = P, y = qpFnew, label = station), 
            hjust = 0, vjust = 1, size = 3) + 
  theme_bw() + labs(x="P cumulated [mm] (two weeks)", y=expression(paste(F[new]," [-]")), shape="Method",
                    color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  facet_wrap(~station, ncol=3) + 
  scale_y_continuous(sec.axis = ~.) +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  scale_shape_manual(values=c(16,21)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) + 
  coord_cartesian(ylim = c(-0.05,0.6))


plot_fnw_profile










setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()


ggsave(plot_fnw_profile, path="figures", filename = "Fnw_profile_p(new).png",
       device = ragg::agg_png, dpi=350,
       width = 18, height = 14, units = "cm",
       bg="white")





