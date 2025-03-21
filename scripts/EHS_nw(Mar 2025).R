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
  qpFnew = round(unlist(qpFnew),3)*100,
  qpFnew_se = round(unlist(qpFnew_se),3)*100,
  nr_qpFnew = round(unlist(nr_qpFnew),3)*100,
  nr_qpFnew_se = round(unlist(nr_qpFnew_se),3)*100,
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
  
  
  profile <- try(data.frame(EHS_profile(Cp, Cq, p, q, p_threshold=p_thresh, crit=q,
                                        lwr = c(0,20, 40, 60, 70, 80, 90),
                                        upr = c(20, 40, 60, 70, 80, 90, 100))))
  
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


q_profile <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[1])))


fnw_profile_q <- data.frame(
  q = unlist(q_profile),
  qpFnew = unlist(Fnew),
  qpFnew_se = unlist(Fnew_se),
  station = rep(stations$station, each=7),
  station2 = rep(stations$station2, each=7))







# calculate the Fnw profiles according to the flow rate in all the nested catchments using the non-robust approach



ehs_prof_comp <- function(dff) {
  
  dff <- arrange(dff,date)
  
  p <- dff$P      #precipitation water flux
  q <- dff$q      #streamflow water flux
  
  Cp <- dff$dO.x    #precipitation tracer concentration (or del value)
  Cq <- dff$dO.y    #streamflow tracer concentration (or del value)
  
  
  profile <- try(data.frame(EHS_profile(Cp, Cq, p, q, robust=FALSE, p_threshold=p_thresh, crit=q, 
                                        lwr = c(0,20, 40, 60, 70, 80, 90),
                                        upr = c(20, 40, 60, 70, 80, 90, 100))))
  
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


q_profile <- dff %>% 
  group_by(station) %>%
  group_map(~unname(unlist(ehs_prof_comp(.x)[1])))


fnw_profile.nr_q <- data.frame(
  q = unlist(q_profile),
  qpFnew = unlist(Fnew),
  qpFnew_se = unlist(Fnew_se),
  station = rep(stations$station, each=7),
  station2 = rep(stations$station2, each=7))





ddf_physio <- merge(subset(dff_physio, geology=="Sandstone and conglomerates")[,-2], 
                    subset(dff_physio, geology=="Marls and claystone")[,-2], 
                    by="station") %>%
  mutate(geo_ratio = value.x - value.y)


fnw_profile_q <- merge(fnw_profile_q, ddf_physio[,-c(2,3)])
fnw_profile.nr_q <- merge(fnw_profile.nr_q, ddf_physio[,-c(2,3)])




# Create a new data frame for facet labels
facet_labels <- fnw_profile.nr_q %>%
  group_by(station, station2) %>%
  summarise(q = max(fnw_profile.nr_q$q) * 0.025, qpFnew = max(fnw_profile.nr_q$qpFnew) * 0.95)



plot_fnw_profile <- ggplot() + 
  geom_hline(data=results.fnw, aes(yintercept=qpFnew/100)) +
  geom_hline(data=results.fnw, aes(yintercept=nr_qpFnew/100), linetype="dashed") +
  geom_errorbar(data=fnw_profile.nr_q, aes(x=q*1.1, y=qpFnew, ymin=qpFnew-qpFnew_se, 
                                           ymax=qpFnew+qpFnew_se, color=geo_ratio), width=0, linewidth=0.6) +
  geom_point(data=fnw_profile.nr_q, aes(x=q*1.1, y=qpFnew, color=geo_ratio, shape="Non-robust"), size=2) +
  geom_errorbar(data=fnw_profile_q, aes(x=q, y=qpFnew, ymin=qpFnew-qpFnew_se, 
                                        ymax=qpFnew+qpFnew_se, color=geo_ratio), width=0, linewidth=0.6) +
  geom_point(data=fnw_profile_q, aes(x=q, y=qpFnew, color=geo_ratio, shape="Robust"), fill="white", size=2, stroke=1) +
  geom_text(data = facet_labels, aes(x = q, y = qpFnew, label = station), 
            hjust = 0, vjust = 1, size = 3) + 
  theme_bw() + labs(x=expression(paste("q [mm ",h^{-1},"]")), y=expression(paste(F[new]," [-]")), shape="Method",
                    color="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  facet_wrap(~station, ncol=3) + 
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2.5), 10^(-0.5))) +
  scale_y_continuous(sec.axis = ~.) +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  scale_shape_manual(values=c(16,21)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=9),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) + 
  coord_cartesian(ylim = c(-0.05,0.6))


plot_fnw_profile






results.fnw_mean <- results.fnw %>%
  group_by(station2) %>%
  reframe(qpFnew = round(mean(qpFnew),3)*100,
          nr_qpFnew = round(mean(nr_qpFnew),3)*100)



# Create a new data frame for facet labels
facet_labels <- fnw_profile_q %>%
  group_by(station2) %>%
  summarise(q = max(fnw_profile_q$q) * 0.008, qpFnew = max(fnw_profile_q$qpFnew) * 1.1)






plot_a <- ggplot() + 
  geom_ribbon(data=fnw_profile_q, aes(x=q, ymin=qpFnew-qpFnew_se, ymax=qpFnew+qpFnew_se, 
                                      fill=geo_ratio, group=station), alpha=0.5) +
  geom_line(data=fnw_profile_q, aes(x=q, y=qpFnew, color=geo_ratio, group=station), linewidth=0.3) +
  geom_text(data = facet_labels, aes(x = q, y = qpFnew, label = c("Weathered layer","Impermeable layer",
                                                                  "Permeable layer interface","Aggregated")), 
            hjust = 0, vjust = 1, size = 3) + 
  theme_bw() + labs(x=expression(paste("q [mm ",h^{-1},"]")), y=expression(paste(F[new]," [-]")), 
                    color="Impermeable to\npermeable bedrock\nfraction difference [%]", 
                    fill="Impermeable to\npermeable bedrock\nfraction difference [%]") +
  facet_wrap(~station2, ncol=2) + 
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3), 10^(-0.75))) +
  scale_y_continuous(sec.axis = ~.) +
  scale_color_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  scale_fill_gradientn(colours = c("#2a9d8f","darkgrey","#e9c46a")) +
  scale_shape_manual(values=c(16,21)) +
  theme(panel.grid.major = element_line(linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) + 
  coord_cartesian(ylim = c(-0.05,0.4))


plot_a









plot_c <- ggplot(fnw_profile_q) + 
  geom_point(aes(x=q, y=qpFnew, shape=station2), size=1) +
  geom_smooth(aes(x=q, y=qpFnew, linetype=station2), method = "gam", linewidth = 0.5, color="black") +
  theme_bw() + labs(x=expression(paste("q [mm ",h^{-1},"]")), y=expression(paste(F[new]," [-]")), 
                    shape="Hydro-lithological category", linetype="Hydro-lithological category") +
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-4), 10^(-0.75))) +
  scale_y_continuous(sec.axis = ~.) +
  scale_linetype_manual(values=c("solid","dotted","dashed","twodash")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        legend.position = c(0.3,0.8)) + 
  coord_cartesian(ylim = c(-0.05,0.35)) +
  guides(shape = guide_legend(override.aes = list(size = 1.5)))


plot_c










setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()


write.table(results.fnw, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\results_fnw.txt', sep = ",", quote = FALSE)
write.table(fnw_profile_q, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\fnw_profile_q.txt', sep = ",", quote = FALSE)



ggsave(plot_fnw_profile, path="figures", filename = "Fnw_q_profile.png",
       device = ragg::agg_png, dpi=350,
       width = 18, height = 14, units = "cm",
       bg="white")



ggsave(plot_a, path="figures", filename = "Fnw_q_new.png",
       device = ragg::agg_png, dpi=350,
       width = 16, height = 11, units = "cm",
       bg="white")


ggsave(plot_c, path="figures", filename = "Fnw_concept.png",
       device = ragg::agg_png, dpi=350,
       width = 10, height = 10, units = "cm",
       bg="white")


