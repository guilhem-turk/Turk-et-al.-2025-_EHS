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


df_q <- read.csv("~/MUSES/2nd publication/codes/df_q.csv", header = TRUE, sep = ",", dec = ".")
df_storage <- read.csv("~/MUSES/2nd publication/codes/df_storage_new.csv", header = TRUE, sep = ",", dec = ".")
rain.amount2 <- read.csv("~/MUSES/2nd publication/codes/stations_precip.csv", header = TRUE, sep = ",", dec = ".")
streams3 <- read.csv("~/MUSES/2nd publication/codes/streams3.csv", header = TRUE, sep = ",", dec = ".")
results.fnw <- read.csv("~/MUSES/2nd publication/codes/results_fnw.txt", header = TRUE, sep = ",", dec = ".")
fnw_profile_q <- read.csv("~/MUSES/2nd publication/codes/fnw_profile_q.txt", header = TRUE, sep = ",", dec = ".")



dff_storage <- df_storage[is.na(df_storage$P)==FALSE,]

dff_span <- dff_storage %>%  
  group_by(station) %>%   
  reframe(start = min(date),
          end = max(date))

dff <- merge(dff_storage, dff_span)

dff <- dff %>%  
  group_by(station) %>%  
  arrange(date) %>%  
  mutate(Q_cum = cumsum(q),
         P_cum = cumsum(P))

df_slope <- dff %>%  
  group_by(station) %>%   
  reframe(slope=lm(Q_cum~0+P_cum)$coefficients)




dff$date <- as.Date(dff$date) 


get_season <- function(date) {
  month = month(date)
  if (month %in% c(3, 4, 5)) {
    return("spring")
  } else if (month %in% c(6, 7, 8)) {
    return("summer")
  } else if (month %in% c(9, 10, 11)) {
    return("autumn")
  } else {
    return("winter")
  }
}

dff$season <- sapply(dff$date, get_season)
dff$year <- year(dff$date)


ddf <- subset(dff, year(date) < year(end))

ddf_span <- ddf %>%  
  group_by(station) %>%   
  reframe(start = min(date),
          end = max(date))



ddf %>% 
  group_by(station, season, year) %>% 
  reframe(P = sum(P),
          PET = sum(pet,na.rm=T)) %>% 
  group_by(season) %>% 
  reframe(P_sd = sd(P),
          P = mean(P),
          PET_sd = sd(PET),
          PET = mean(PET))


ddf %>% 
  group_by(year) %>% 
  reframe(Q = sum(q)/12) 



df_stats <- dff %>% 
  group_by(station) %>% 
  reframe(start = min(date),
          end = max(date),
          Qa=sum(q)*365/as.numeric(end-start),
          Pa=sum(P)*365/as.numeric(end-start),
          Dmax=quantile(D, probs=0.995, na.rm=T),
          slope=lm(Q_cum~0+P_cum)$coefficients)


df_stats.q <- df_q %>% 
  group_by(station) %>% 
  reframe(q99.5=quantile(q, probs=0.995))


df_statsa <- dff[is.na(dff$pet)==FALSE,] %>% 
  group_by(station) %>% 
  reframe(start = min(date),
          end = max(date),
          PET=sum(pet)*365/as.numeric(end-start))


df_statsb <- dff %>% 
  mutate(month = month(date)) %>% 
  mutate(season2 = ifelse(month %in% c(10,11, 12, 1, 2, 3), "winter", "summer")) %>% 
  group_by(station, season2) %>% 
  reframe(Q=sum(q, na.rm=T))

df_statsb <- pivot_wider(df_statsb, names_from = season2, values_from = Q)
df_statsb <- df_statsb %>% mutate(Q_ratio = summer/winter)



dff_stats <- merge(df_stats, df_stats.q)
dff_stats <- merge(dff_stats, df_statsa[,c(1,4)])
dff_stats <- merge(dff_stats, df_statsb[,c(1,4)])







df_stat_dO.y <- streams3[is.na(streams3$dO.y)==FALSE,] %>% 
  group_by(station) %>% 
  reframe(mean_dO = mean(dO.y),
          sd_dO = sd(dO.y),
          n=n())

df_stat_dO.x <- streams3[is.na(streams3$dO.x)==FALSE,] %>% 
  group_by(station3) %>% 
  reframe(mean_dO = mean(dO.x),
          sd_dO = sd(dO.x))




results.fnw%>% 
  group_by(station2) %>% 
  reframe(mean_fnew = mean(qpFnew))
