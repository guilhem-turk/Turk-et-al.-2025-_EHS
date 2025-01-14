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


rain.amount <- read.csv("~/MUSES/2nd publication/codes/catchments_precip.csv", header = TRUE, sep = ",", dec = ".")
rain.amount2 <- read.csv("~/MUSES/2nd publication/codes/stations_precip.csv", header = TRUE, sep = ",", dec = ".")


df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Weierbach-SW1 Discharge Mean (3425010).csv", 
                        sep = ",", dec = ".", skip=17)


# convert q [m3/s] into [mm/h]  (source: Pfister et al., 2019)

df_q$Value <- (60*60)/(1000^2)*1000/0.45*as.numeric(df_q$Value)
df_q$station <- "a Weierbach"

dff_q <- df_q[,-3]



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Hovelange-Huewelerbach-1 Discharge Mean (3017010).csv", 
                            sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/2.8*as.numeric(df_q$Value)
df_q$station <- "i Huewelerbach"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Useldange-Wollefsbach Discharge Mean (5891010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/4.6*as.numeric(df_q$Value)
df_q$station <- "e Wollefsbach"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Colpach-Koulbich Discharge Mean (2921010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/19.2*as.numeric(df_q$Value)
df_q$station <- "b Colpach"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Niederpallen-Pall Discharge Mean (3257010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/37.1*as.numeric(df_q$Value)
df_q$station <- "h Pall"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Reichlange-Attert Discharge Mean (18659010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/159.5*as.numeric(df_q$Value)
df_q$station <- "j Reichlange"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Useldange-Schwebich Discharge Mean (3377010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/30.2*as.numeric(df_q$Value)
df_q$station <- "g Schwebich"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Platen-Roudbach Discharge Mean (3281010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/43.3*as.numeric(df_q$Value)
df_q$station <- "l Roudbach"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Huncherange-Mierbech Discharge Mean (3041010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/6.4*as.numeric(df_q$Value)
df_q$station <- "c Mierbech"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Livange-Bibeschbach Discharge Mean (3161010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/10.4*as.numeric(df_q$Value)
df_q$station <- "d Bibeschbach"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Pontpierre-Mess Discharge Mean (3305010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/32.2*as.numeric(df_q$Value)
df_q$station <- "f Mess"

dff_q <- rbind(dff_q, df_q[,-3])



df_q <- read.csv("C:/Users/turk/Documents/MUSES/2nd publication/data/hourly_discharge/Useldange-Attert Discharge Mean (3353010).csv", 
                 sep = ",", dec = ".", skip=17)

df_q$Value <- (60*60)/(1000^2)*1000/247.5*as.numeric(df_q$Value)
df_q$station <- "k Useldange"

dff_q <- rbind(dff_q, df_q[,-3])

dff_q <- dff_q %>%
  mutate(date=as.Date(Timestamp),
         hour=substr(Timestamp, nchar(Timestamp) - 4, nchar(Timestamp)-3),
         timestamp=as.POSIXct(date, format="%Y-%m-%d %H:%M") + hours(hour))


df_helper <- dff_q %>%  
  group_by(station) %>%  
  reframe(date = seq(as.POSIXct(min(timestamp)), as.POSIXct(max(timestamp)), by = "hour"))





ddf_q <- rename(dff_q[,c(2:5)], q = Value)
 
ddf_q$q[ddf_q$q<0] <- 0


ddf_helper <- ddf_q %>%
  group_by(date, station) %>%
  reframe(q=sum(q))

length(ddf_helper$q[is.na(ddf_helper$q==T)])


ddf_q$yday <- yday(ddf_q$date)

ddf_q2 <- ddf_q %>%  
  group_by(station, yday) %>%  
  reframe(q_helper=mean(q, na.rm=T))

ddf_q <- merge(ddf_q, ddf_q2, by=c("station","yday"))
ddf_q$ratio <- ddf_q$q/ddf_q$q_helper

ddf_q3 <- ddf_q %>%  
  group_by(date) %>%  
  reframe(ratio=mean(ratio, na.rm=T))


ddf_q <- merge(ddf_q, ddf_q3, by=c("date"))
ddf_q$q_helper <- ddf_q$q_helper*ddf_q$ratio.y
ddf_q$q[is.na(ddf_q$q)] <- ddf_q$q_helper[is.na(ddf_q$q)]



write.table(ddf_q[,-c(6:8)], file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\df_q.csv', sep = ",", quote = FALSE)




ddf_q.daily <- ddf_q %>%
  group_by(date, station) %>%
  reframe(q=sum(q, na.rm=T))




rain.amount <- rain.amount %>%  mutate(station=recode(station, "f Pall"="h Pall", "d Attert-Reichlange"="j Reichlange",
                                      "g Wollefsbach"="e Wollefsbach","k Roudbach"="l Roudbach",
                                      "i Mierbech"="c Mierbech", "h Bibeschbach"="d Bibeschbach", 
                                      "c Mess"="f Mess", "e Attert-Useldange"="k Useldange",
                                      "j Schwebich"="g Schwebich", "l Huewelerbach"="i Huewelerbach"))


dff <- merge(ddf_q.daily, rain.amount, by=c("date","station"), all.x=T)



dff <- dff %>%  mutate(station2=recode(station, "i Huewelerbach"="Permeable layer interface", "b Colpach"="Schistose",
                                                           "a Weierbach"="Schistose", "h Pall"="Permeable layer interface", "j Reichlange"="Aggregated",
                                                           "g Schwebich"="Permeable layer interface", "e Wollefsbach"="Marly",
                                                           "l Roudbach"="Aggregated","c Mierbech"="Marly", "d Bibeschbach"="Marly", 
                                                           "f Mess"="Marly", "k Useldange"="Aggregated"))


dff <- dff %>%  mutate(station2=recode(station2, "Schistose"="Weathered layer", "Marly"="Impermeable layer",
                                       "Permeable layer interface"="Permeable layer interface", "Aggregated"="Aggregated"))

dff$station2 <- factor(dff$station2, levels = c("Weathered layer", "Impermeable layer", "Permeable layer interface", "Aggregated"))



ggplot(dff) + 
  geom_col(aes(x=date, y=P), color="lightblue") +
  geom_line(aes(x=date, y=100-q*2), color="darkblue") +
  scale_y_reverse(name="P [mm/day]", limits=c(100,0), 
                  sec.axis=sec_axis(~./(-2)+50,name="q [mm/day]")) + 
  scale_x_date(date_breaks="2 year", date_labels="%Y", limits = c(as.Date("2010-06-01"), as.Date("2023-06-01"))) +
  theme_bw() + labs(x="") + facet_wrap(~station, ncol=2, scales="free") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        strip.background = element_rect(color=NA, fill=NA, linewidth=1),
        strip.text.x = element_text(size = 8)) + guides(colour = "none")




ggplot(dff) + 
  geom_line(aes(x=date, y=q, color=station2)) + 
  scale_x_date(date_breaks="2 year", date_labels="%Y", limits = c(as.Date("2010-06-01"), as.Date("2023-06-01"))) +
  scale_color_manual(values=c("#001845","#01497c","#2c7da0","#89c2d9")) +
  theme_bw() + labs(x="", y="q [mm/day]") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        strip.background = element_rect(color=NA, fill=NA, linewidth=1),
        strip.text.x = element_text(size = 8)) 





dff <- dff %>%
  group_by(station) %>%
  mutate(index = row_number(),
         N = n()) %>%
  ungroup() %>%
  mutate(index2 = index/max(N)*100)

dff %>%
  group_by(station) %>%
  reframe(max_index = max(index2))





df_fdc <- dff %>% 
  group_by(station) %>% 
  mutate(rank=rank(-q),
         epf=100*rank/(max(rank)+1))


df_fdc.mean <- dff[,-2] %>% 
  mutate(rank=rank(-q),
         epf=100*rank/(max(rank)+1))


df_fdc2 <- dff %>% 
  mutate(year=year(date)) %>%
  filter(year>2010 & year<2022) %>%
  group_by(station, year) %>% 
  mutate(rank=rank(-q),
         epf=100*rank/(max(rank)+1))



df_fdc.quantiles <- df_fdc %>%
  group_by(station) %>%
  reframe(epf = epf,
          q_down = quantile(q, probs=0.25),
          q_median = quantile(q, probs=0.5),
          q_up = quantile(q, probs=0.75))






# Transformation functions for secondary y-axis
sec_y_transform <- function(x) 10^(x / 10 - 3)
sec_y_inverse <- function(x) (log10(x+0.001) + 3) * 10  

# Create a new data frame for facet labels
facet_labels <- df_fdc %>%
  group_by(station) %>%
  summarise(epf = max(df_fdc$epf) * 0.2, q = max(df_fdc$q) * 0.95)

line_labels <- subset(df_fdc2, year %in% c("2011","2013","2021")) %>%
  group_by(station, year) %>%
  summarise(epf = max(epf) * 0.7, q = quantile(q, c(0.5,0.65,0.8)))






plot_fdc <- ggplot() + 
  geom_ribbon(data=df_fdc.quantiles, aes(x = epf, ymin = q_down, ymax =q_up), fill="skyblue", alpha = 0.5) +
  geom_hline(data=df_fdc.quantiles, aes(yintercept = q_median), color="#01497c", alpha = 0.5) +
  geom_line(data=dff, aes(x=index2, y=sec_y_transform(q), color=station2), linewidth=0.3) + 
  geom_line(data=df_fdc2, aes(x=epf, y=q, group=year), color="lightgrey", linewidth=0.3) +
  geom_line(data=df_fdc.mean, aes(x=epf, y=q), color="black", linetype="dashed", linewidth=0.3) +
  geom_line(data=df_fdc, aes(x=epf, y=q), color="black", linewidth=0.3) +
  geom_line(data=subset(df_fdc2, year == "2021"), 
            aes(x=epf, y=q, group=year), color="darkblue", linewidth=0.3) +
  geom_line(data=subset(df_fdc2, year == "2011"), 
            aes(x=epf, y=q, group=year), color="red", linewidth=0.3) +
  geom_text(data = facet_labels, aes(x = epf, y = q, label = station), 
            hjust = 0, vjust = 1, size = 3, color = "black") + 
  theme_bw() + labs(y=expression(paste("q [mm ",day^{-1},"] (flow distribution curve)")),
                    x="Exceedence probability [%]", color="Bedrock geology") +
  scale_color_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  scale_y_continuous(trans = log10_trans(), 
                     limits = c(10^(-3), 10^2),
                     breaks = trans_breaks("log10", function(x) 10^x),
                     labels = trans_format("log10", math_format(10^.x)),
                     sec.axis = sec_axis(~ sec_y_inverse(.), name = expression(paste("q [mm ",day^{-1},"] (hydrograph)")), 
                                         breaks = seq(0, 50, 10))) +  
  scale_x_continuous(sec.axis = sec_axis(~ . * 12/100 + 2011, name = "", breaks = seq(2011, 2023, by = 3))) +
  theme(panel.grid.major = element_line(linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) + 
  facet_wrap(~station, ncol=3) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))



plot_fdc









df_pet <- read_excel("~/MUSES/2nd publication/data/Catchment data_P_PET_Q.xlsx", 
                     sheet = "prepared_data")


df_pet <- df_pet %>%  mutate(station=recode(station, "k Pall"="h Pall", "g Attert-Reichlange"="j Reichlange",
                                                      "e Wollefsbach"="e Wollefsbach","i Roudbach"="l Roudbach",
                                                      "c Mierbech"="c Mierbech", "d Bibeschbach"="d Bibeschbach", 
                                                      "f Mess"="f Mess", "h Attert-Useldange"="k Useldange",
                                                      "j Schwebich" = "g Schwebich"))


df_pet <- df_pet %>% 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date),
         n = days_in_month(date),
         pet = PET/n)


df_pet.avg <- df_pet %>%
  group_by(date, year, month) %>%
  reframe(pet = mean(pet, na.rm=T))
   

df_pet.a <- df_pet.avg %>% mutate(station = "b Colpach")
df_pet.b <- df_pet.avg %>% mutate(station = "i Huewelerbach")   


df_pet <- bind_rows(df_pet[,-c(3:5,8)], df_pet.a)
df_pet <- bind_rows(df_pet, df_pet.b)





ddf <- dff[,c(1:5)] %>% 
  mutate(year = year(date),
         month = month(date))


ddf <- merge(ddf, df_pet[,-2], by=c("station","year","month"), all.x=T)


#ddf$S_in <- (sum(ddf$P) - sum(ddf$Q) - sum(ddf$pet)*0.5)*365/length(ddf$P) # initial condition for storage calculation


ddf$S_in <- 200 # initial condition for storage calculation




ddf$q[ddf$station=="i Huewelerbach"] <- ddf$q[ddf$station=="i Huewelerbach"] + 0.4 # account for seepage losses in the Huewelerbach!!




trans_storage <- function(d, FC) {
  
  S <- numeric(length(d$date))  # Initialize an empty vector for S
  
  R <- d$P
  Q <- d$q
  E <- d$pet
  
  S[1] <- d$S_in  # Initial condition for S(1)
  
  for (i in 2:length(d$date)) {
    if (S[i - 1] < FC) {
      alpha <- S[i - 1] / FC
    } else {
      alpha <- 1
    }
    S[i] <- ifelse((R[i] - Q[i] - alpha * E[i]) + S[i - 1]>0,
                   (R[i] - Q[i] - alpha * E[i]) + S[i - 1],0)
  } 
  
  return(S)
}





ddf.a <- drop_na(ddf) %>%   
  arrange(date) %>%
  group_by(station) %>%
  group_map(~trans_storage(.x, 200))

ddf <- arrange(ddf, station, date)

ddf$Sa <- NA
ddf$Sa[is.na(ddf$pet)==FALSE] <- unlist(ddf.a)

ddf.b <- drop_na(ddf) %>% 
  filter(year > 2013) %>% 
  group_by(station) %>%
  reframe(S_max = quantile(Sa, probs=0.99))


ddf <- merge(ddf, ddf.b, by="station")
  
ddf <- ddf %>% mutate(D = S_max-Sa)




ddf$q[ddf$station=="i Huewelerbach"] <- ddf$q[ddf$station=="i Huewelerbach"] - 0.4 # account for seepage losses in the Huewelerbach!!






plot_storage <- ggplot(ddf) + geom_col(aes(x=date, y=P), color="lightblue") +
  geom_line(aes(x=date, y=100-q), color="darkblue", linewidth = 0.3) +
  geom_line( aes(x=date, y=D/6), color="black", linewidth = 0.3) +
  scale_y_reverse(name=expression(paste("P and (100 - q) [mm ",day^{-1},"]")), limits=c(100,0), 
                  sec.axis=sec_axis(~.*(6),name="Storage deficit [mm]")) + 
  scale_x_date(date_breaks="2 year", date_labels="%Y", limits = c(as.Date("2010-06-01"), as.Date("2023-01-01"))) +
  scale_color_manual(values=rep("lightblue", times=12)) + labs(x="") +
  theme_bw() + facet_wrap(~station, ncol=3, scales="free") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        strip.background = element_rect(color=NA, fill=NA, linewidth=1),
        strip.text.x = element_text(size = 8)) + guides(colour = "none")









ddf.annual <- ddf %>%
  filter(year>2010 & year < 2022) %>%
  group_by(station, station2, year) %>%
  reframe(P = sum(P, na.rm = T))


ddf.annual <- ddf.annual[ddf.annual$P>0,]





plot_boxplots <- ggplot(ddf) + 
  geom_hline(aes(yintercept=median(D, na.rm=T)), linetype="dashed") +
  geom_hline(data=ddf.annual, aes(yintercept=median(P, na.rm=T)), linetype="dashed") +
  geom_violin(data=ddf.annual, aes(x=station, y=P, group=station, fill=station2), alpha=0.7, scale = "width", size = 0.2) +
  geom_boxplot(data=ddf.annual, aes(x=station, y=P, group=station), width=0.1, outlier.shape = NA) +
  geom_violin(aes(x=station, y=D, group=station, fill=station2), alpha=0.7, scale = "width", size = 0.2) +
  geom_boxplot(aes(x=station, y=D, group=station), width=0.1, outlier.shape = NA) +
  geom_text(data=subset(ddf, station=="a Weierbach"), aes(x="a Weierbach", y=1200, label="(a)"), size=3, hjust=0) +
  geom_text(data=subset(ddf, station=="a Weierbach"), aes(x="a Weierbach", y=300, label="(b)"), size=3, hjust=0) +
  theme_bw() + labs(x="", y=expression(paste(P[a]," and D [mm]")), fill="Bedrock geology") +
  scale_fill_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  theme(panel.grid.major = element_line(linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(size=8)) + facet_grid(~station2,  scales ="free_x", space = "free") +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5)) 



plot_boxplots






   
   
   
setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()


ggsave(plot_fdc, path="figures", filename = "fdc_new.png",
       device = ragg::agg_png, dpi=350,
       width = 15, height = 18, units = "cm",
       bg="white")


ggsave(plot_storage, path="figures", filename = "storage_new.png",
       device = ragg::agg_png, dpi=350,
       width = 18, height = 15, units = "cm",
       bg="white")


ggsave(plot_boxplots, path="figures", filename = "precip_storage.png",
       device = ragg::agg_png, dpi=350,
       width = 15, height = 9, units = "cm",
       bg="white")







write.table(ddf, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\df_storage_new.csv', sep = ",", quote = FALSE)




