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



attert <- read_excel("C:/Users/turk/Documents/MUSES/2nd publication/data/ATTERT_biweekly.xlsx", 
                     range = "A4:W5573", col_names = FALSE,
                     sheet = "ATTERT_biweekly_tour")

attert <- attert[,c(1,2,3,20,22)]
attert <- attert %>% rename(date=1, time=2, station=3, dH=4, dO=5)

attert <- attert %>% mutate(time=round(hour(time) + (minute(time) / 60)))


attert <- attert %>% drop_na(dH)
attert$date <- as.POSIXlt(attert$date,format="%d.%m.%Y", 
                          tz = "GMT")

attert <- attert %>%                                      
  group_by(date, station) %>% 
  reframe(time = mean(time, na.rm=T),
          dH = mean(dH, na.rm=T),
          dO = mean(dO, na.rm=T))



alzette <- read_excel("C:/Users/turk/Documents/MUSES/2nd publication/data/ALZETTE_AMONT_biweekly.xlsx", 
                      range = "A4:W3110", col_names = FALSE,
                      sheet = "ALZETTE_AMONT_biweekly_tour")

alzette <- alzette[,c(1,2,3,20,22)]
alzette <- alzette %>% rename(date=1, time=2, station=3, dH=4, dO=5)

alzette <- alzette %>% mutate(time=round(hour(time) + (minute(time) / 60)))


alzette <- alzette %>% drop_na(dH)
alzette$date <- as.POSIXlt(alzette$date,format="%d.%m.%Y", 
                           tz = "GMT")



alzette <- alzette %>%                                      
  group_by(date, station) %>% 
  reframe(time = mean(time, na.rm=T),
          dH = mean(dH, na.rm=T),
          dO = mean(dO, na.rm=T))


df_all <- rbind(attert, alzette)


date_spans <- df_all %>%  
  group_by(station) %>%  
  summarise(min(date),
            max(date))


df_all$date <- as.Date(df_all$date)



df_helper <- df_all %>% 
  group_by(station) %>%
  reframe(time_avg = round(mean(time, na.rm=T)))


df_all <- merge(df_all, df_helper, by="station")


df_all$time[is.na(df_all$time)==T] <- df_all$time_avg[is.na(df_all$time)==T]

df_all <- rename(df_all, hour = time)




rain_isotopes <- subset(df_all, station == "P04" | station == "P05" | station == "P06" | 
                          station == "P07" | station == "P15" | station == "P16" | 
                          station == "P17")


rain_isotopes <- rain_isotopes %>%  
  mutate(station3=recode(station, "P04"="Northwest", "P05"="Northwest", "P06"="Northwest",
                         "P07"="Northwest","P15"="South", "P16"="South", "P17"="South"))


rain_isotopes <- rain_isotopes %>%  
  mutate(station=recode(station, "P04"="P4", "P05"="P5", "P06"="P6",
                        "P07"="P7","P15"="P15", "P16"="P16", "P17"="P17"))


rain_isotopes.elevation <- data.frame(
  station = c("P4", "P5", "P6", "P7", "P15", "P16", "P17"),
  elevation = c(250,487,281,292,283,267,400))




## Exact sampling interval uncertain - I arbitrarely take 14 days


df_rain <- rain.amount2 %>%  
  group_by(station) %>%  
  reframe(date=date,
          P=rollapply(P, width=14, sum, align="right", fill=NA))


rain_isotopes2 <- merge(rain_isotopes, df_rain, by=c("station","date"))





ggplot() +  
  geom_point(data=subset(rain_isotopes2, station3=="Northwest"),
             aes(x=date, y=dO, color=station), size=1) +
  geom_point(data=subset(rain_isotopes2, station3=="South"),
             aes(x=date, y=dO-25, color=station), size=1) +
  scale_color_manual(values=c("#143601","#538D22","#aad576","#89c2d9","#03045e","#48cae4","#0077b6"),
                     labels=c("Foetz [283 m]", "Livange [267 m]", "Budersbierg [400]", "Oberpallen [250 m]", 
                              "Roodt [487 m]", "Useldange [281 m]", "Pratz [292 m]")) +
  scale_y_continuous(breaks = c(-20, -16, -12, -8, -4, 0, 4, 8),
                     sec.axis=sec_axis(~.+25, breaks = c(-20, -16, -12, -8, -4, 0, 4, 8),
                                       name=expression(paste(delta^{18}, "O [\u2030]")))) +
  scale_x_date(date_breaks="2 year", date_labels="%Y") +
  theme_bw() + labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), color="") +
  theme(strip.background = element_rect(color=NA, fill=NA, size=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size=8)) 






## dual isotope plot to check for evaporative interferences


rain_isotopes2 <- rain_isotopes2 %>% mutate(dexcess = dH - 8*dO)


ggplot(rain_isotopes2) + 
  geom_point(aes(x=dO, y=dH), color="skyblue") +
  geom_point(data=rain_isotopes2[rain_isotopes2$dexcess<=quantile(rain_isotopes2$dexcess, probs=0.05),], 
             aes(x=dO, y=dH), color="darkgrey") +
  geom_abline(aes(intercept = 10, slope = 8), linewidth=1, linetype="dashed")  +
  labs(x=expression(paste(delta^{18}, "O [\u2030]")), 
       y=expression(paste(delta^{2}, "H [\u2030]")), color="") + 
  theme_bw() + facet_wrap(~station3) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color=NA, fill=NA, size=0.5),
        text=element_text(size=8))


#rain_isotopes2a <- rain_isotopes2[rain_isotopes2$dexcess>quantile(rain_isotopes2$dexcess, probs=0.05),]


#rain_isotopes3 <- rain_isotopes2a %>% 

rain_isotopes3 <- rain_isotopes2 %>% 
  group_by(date, station3) %>%  
  reframe(dO = weighted.mean(dO, P, na.rm=T),
          dH = weighted.mean(dH, P, na.rm=T))


rain_isotopes3 <- drop_na(rain_isotopes3)









## now prepare the stream isotope dataset



stream_isotopes <- subset(df_all, station =="S01" | station == "S02" | station == "S03" | station == "S05" |
                            station == "S06" | station == "S07" | station == "S08" | station == "S09" | 
                            station == "S10" | station == "S25" | station == "S27" | station == "S30" )


stream_isotopes <- stream_isotopes %>%  mutate(station=recode(station, "S01"="i Huewelerbach", "S02"="b Colpach",
                                                              "S03"="a Weierbach", "S05"="h Pall", "S06"="j Reichlange",
                                                              "S07"="g Schwebich", "S08"="e Wollefsbach",
                                                              "S10"="l Roudbach","S25"="c Mierbech", "S27"="d Bibeschbach", 
                                                              "S30"="f Mess", "S09"="k Useldange"))

stream_isotopes <- stream_isotopes %>%  mutate(station3=recode(station, "i Huewelerbach"="Northwest", "b Colpach"="Northwest",
                                                               "a Weierbach"="Northwest", "h Pall"="Northwest", "j Reichlange"="Northwest",
                                                               "g Schwebich"="Northwest", "e Wollefsbach"="Northwest",
                                                               "l Roudbach"="Northwest","c Mierbech"="South", "d Bibeschbach"="South", 
                                                               "f Mess"="South", "k Useldange"="Northwest"))

df_isotopes <- merge(rain_isotopes3, stream_isotopes, by=c("date", "station3"), all.y=T)




# Exact sampling interval unknown - I take the interval between one observation and the following one, unless it is greater than 21 days, then I arbitrarely take 14 days and insert an empty line.


df_helper <- df_isotopes %>% 
  group_by(station) %>% 
  mutate(diff = date - lag(date))

df_helper$diff[is.na(df_helper$diff)] <- 14
df_helper$diff[df_helper$diff>21] <- 14

df_helper$date <- df_helper$date - df_helper$diff

dff_isotopes <- merge(df_isotopes, df_helper[,c(1,5)], by=c("date", "station"), all=T)

dff_isotopes <- dff_isotopes[-c(1:9),]


df_q <- df_q %>%
  group_by(station) %>%
  mutate(q_24h = rollapply(q, width=24, mean, align="right", fill=NA))


dff_q <- df_q %>% 
  group_by(station, date) %>%
  reframe(q=mean(q, na.rm=T),
          q_24h=mean(q_24h, na.rm=T))


ggplot(dff_q) +
  geom_point(aes(x=q, y=q_24h)) + theme_bw()



df_storage2 <- df_storage %>%  
  group_by(station) %>%  
  reframe(date=date, 
          D=rollapply(D, width=7, mean, align="right", fill=NA), # take the storage of the antecedent week
          P0=rollapply(P, width=3, sum, align="right", fill=NA),
          P=rollapply(P, width=14, sum, align="right", fill=NA)) # take the discharge of the following week for delayed peaks




dff_isotopes2 <- merge(dff_isotopes, df_storage2, by=c("date", "station"), all.x=T)
dff_isotopes2 <- merge(dff_isotopes2, df_q, by=c("date", "hour", "station"), all.x=T)

dff_isotopes2 <- dff_isotopes2 %>%  mutate(station2=recode(station, "i Huewelerbach"="Permeable layer interface", "b Colpach"="Schistose",
                                                           "a Weierbach"="Schistose", "h Pall"="Permeable layer interface", "j Reichlange"="Aggregated",
                                                           "g Schwebich"="Permeable layer interface", "e Wollefsbach"="Marly",
                                                           "l Roudbach"="Aggregated","c Mierbech"="Marly", "d Bibeschbach"="Marly", 
                                                           "f Mess"="Marly", "k Useldange"="Aggregated"))

dff_isotopes2$station2 <- factor(dff_isotopes2$station2, levels = c("Schistose", "Marly", "Permeable layer interface", "Aggregated"))


# dff_isotopes2 <- dff_isotopes2[is.na(dff_isotopes2$Q)==FALSE,]




## dual isotope plot to check for evaporative interferences

dff_isotopes2 <- dff_isotopes2 %>% mutate(dexcess.x = dH.x - 8*dO.x)
dff_isotopes2 <- dff_isotopes2 %>% mutate(dexcess.y = dH.y - 8*dO.y)

# dff_isotopes2$dO.y[dff_isotopes2$dexcess.y<=min(dff_isotopes2$dexcess.x, na.rm=T)] <- NA

dff_isotopes2 <- dff_isotopes2[,-c(2,9,13)]



intra_seas_dO.x <- dff_isotopes2 %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(station, month) %>%
  reframe(dO.x_mean = mean(dO.x, na.rm=T),
          dO.x_sd = sd(dO.x, na.rm=T)) %>%
  group_by(month) %>%
  reframe(dO.x_mean = mean(dO.x_mean, na.rm=T),
          dO.x_sd = mean(dO.x_sd, na.rm=T)) 

mean(intra_seas_dO.x$dO.x_sd)




main_plot <- ggplot(rain_isotopes2) + 
  geom_point(aes(x=dO, y=dH, color=station), size=0.7, alpha=0.7) +
  geom_abline(aes(intercept = 10, slope = 8), linewidth=1, linetype="dashed")  +
  scale_color_manual(values=c("#143601","#538D22","#aad576","#89c2d9","#03045e","#48cae4","#0077b6")) +
  labs(x=expression(paste(delta^{18}, "O [\u2030]")), 
       y=expression(paste(delta^{2}, "H [\u2030]")), color="") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_rect(color=NA, fill=NA, size=0.5),
        legend.position = "none",
        plot.margin = margin(1,1,1,1),
        text=element_text(size=8))



boxplot.y <- ggplot(rain_isotopes2) + 
  geom_boxplot(aes(x=station, y=dH, fill=station), alpha=0.7) +
  theme_bw() + labs(x="", y=expression(paste(delta^{2}, "H [\u2030]")), fill="") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,1),
        text=element_text(size=8)) + 
  scale_fill_manual(values=c("#143601","#538D22","#aad576","#89c2d9","#03045e","#48cae4","#0077b6"))



boxplot.x <- ggplot(rain_isotopes2) + 
  geom_boxplot(aes(x=station, y=dO, fill=station), alpha=0.7) +
  theme_bw() + labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), fill="") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title=element_blank(),
        legend.position = "none",
        plot.margin = margin(1,1,1,1),
        text=element_text(size=8)) + 
  scale_fill_manual(values=c("#143601","#538D22","#aad576","#89c2d9","#03045e","#48cae4","#0077b6")) + rotate()





dual_plot <- ggarrange(boxplot.x , NULL, main_plot, boxplot.y,
                        align="hv", common.legend = T,  ncol = 2, nrow = 2,
                        heights=c(1,2.5), widths=c(2.5,1),
                        legend = "none")


dual_plot





iso_precip.quantiles <- dff_isotopes2 %>%
  group_by(station3) %>%
  reframe(dO.x25 = round(quantile(dO.x, probs=0.25, na.rm=T),1),
          dO.x75 = round(quantile(dO.x, probs=0.75, na.rm=T),1),
          dO.x50 = round(median(dO.x, na.rm=T),1),
          iqr = dO.x75-dO.x25,
          sd = round(sd(dO.x, na.rm=T),1))



plot_rain <- ggplot() +  
  geom_point(data=subset(rain_isotopes2, station3=="Northwest"),
             aes(x=date, y=dO, color=station), size=0.7) +
  geom_point(data=subset(rain_isotopes2, station3=="South"),
             aes(x=date, y=dO-25, color=station), size=0.7) +
  geom_hline(data=subset(iso_precip.quantiles, station3=="Northwest"),
             aes(yintercept = dO.x50), color="black", linetype="dashed") +
  geom_hline(data=subset(iso_precip.quantiles, station3=="South"),
             aes(yintercept = dO.x50-25), color="black", linetype="dashed") +
  geom_text(aes(x=as.Date("2011-06-01"), y=8, label="North"), size=3) + 
  geom_text(aes(x=as.Date("2011-06-01"), y=-38, label="South"), size=3) + 
  scale_color_manual(values=c("#143601","#538D22","#aad576","#89c2d9","#03045e","#48cae4","#0077b6"),
                     labels=c("Foetz [283 m]", "Livange [267 m]", "Budersbierg [400]", "Oberpallen [250 m]", 
                              "Roodt [487 m]", "Useldange [281 m]", "Pratz [292 m]")) +
  scale_y_continuous(breaks = c(-20, -16, -12, -8, -4, 0, 4, 8),
                     sec.axis=sec_axis(~.+25, breaks = c(-20, -16, -12, -8, -4, 0, 4, 8),
                                       name=expression(paste(delta^{18}, "O [\u2030]")))) +
  scale_x_date(date_breaks="2 year", date_labels="%Y") +
  theme_bw() + labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), color="") +
  theme(strip.background = element_rect(color=NA, fill=NA, size=0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        text = element_text(size=8)) 


plot_rain





plot_rain.comb <- ggarrange(plot_rain, dual_plot,
                            ncol=1, 
                            labels=c("a","b"), font.label = list(size = 10, face = "plain"))


plot_rain.comb










iso_precip.quantiles2 <- dff_isotopes2 %>%
  reframe(start = as.Date(min(date)),
          end = as.Date(max(date)),
          dO.x25 = round(quantile(dO.x, probs=0.25, na.rm=T),1),
          dO.x75 = round(quantile(dO.x, probs=0.75, na.rm=T),1),
          dO.x50 = round(median(dO.x, na.rm=T),1),
          iqr = dO.x75-dO.x25,
          sd = round(sd(dO.x, na.rm=T),1))


iso_stream.quantiles <- dff_isotopes2 %>%
  group_by(station2) %>%
  reframe(start = as.Date(min(date)),
          end = as.Date(max(date)),
          dO.y25 = round(quantile(dO.y, probs=0.25, na.rm=T),1),
          dO.y75 = round(quantile(dO.y, probs=0.75, na.rm=T),1),
          dO.y50 = round(median(dO.y, na.rm=T),1),
          iqr = dO.y75-dO.y25,
          sd = round(sd(dO.y, na.rm=T),1))



# Create a new data frame for facet labels
facet_labels <- dff_isotopes2 %>%
  group_by(station2) %>%
  summarise(date = min(dff_isotopes2$date), dO.y = max(dff_isotopes2$dO.y, na.rm=T) * 0.95)



plot_stream <- ggplot(subset(dff_isotopes2, P0 <= 15)) +
  geom_rect(data=iso_precip.quantiles2, aes(xmin=start, xmax=end, ymin=dO.x25, ymax=dO.x75),
            fill="lightblue", alpha = 0.6) +
  geom_rect(data=iso_stream.quantiles, aes(xmin=start, xmax=end, ymin=dO.y25, ymax=dO.y75),
            fill="darkblue", alpha = 0.6) +
  geom_point(data=dff_isotopes2[,-14], aes(x=date, y=dO.y), size=0.6, color="lightgrey", alpha=0.5) +
  geom_point(aes(x=date, y=dO.y, color=station2), size=0.6) +
  geom_point(data=subset(dff_isotopes2, P0 > 15), aes(x=date, y=dO.y), shape=21, size=0.6, alpha=0.7) +
  geom_hline(data=iso_precip.quantiles2, aes(yintercept = dO.x50), color="black", linetype="dashed") +
  geom_hline(data=iso_stream.quantiles, aes(yintercept = dO.y50), color="black", linetype="solid") +
  geom_text(data = facet_labels, aes(x=date, y=dO.y, label=c("(a)  Weathered layer","(b)  Impermeable layer",
                                                             "(c)  Permeable layer interface","(d)  Aggregated")), 
            hjust = 0, vjust = 1, size = 3) + 
  scale_x_date(date_breaks="2 year", date_labels="%Y", 
               limits = c(min(dff_isotopes2$date), max(dff_isotopes2$date))) +
  scale_color_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  theme_bw() + facet_wrap(~station2, ncol=2, scales="free_x") + 
  labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), color="") +
  theme(panel.grid.major = element_line(linetype = "dashed"), 
        panel.grid.minor = element_blank(),
        text=element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.position="none") 


plot_stream







plot_stream.bis <- ggplot(dff_isotopes2) + 
  geom_rect(data=iso_precip.quantiles2, aes(xmin=-Inf, xmax=Inf, ymin=dO.x25, ymax=dO.x75),
            fill="lightblue", alpha = 0.6) +
  geom_rect(data=iso_stream.quantiles, aes(xmin=-Inf, xmax=Inf, ymin=dO.y25, ymax=dO.y75),
            fill="darkblue", alpha = 0.6) +
  geom_hline(data=iso_precip.quantiles2, aes(yintercept = dO.x50), color="black", linetype="dashed") +
  geom_hline(data=iso_stream.quantiles, aes(yintercept = dO.y50), color="black", linetype="solid") +
  geom_violin(aes(x=station, y=dO.y, group=station, fill=station2), alpha=0.7, scale = "width") +
  geom_boxplot(aes(x=station, y=dO.y, group=station), width=0.1, outlier.shape = NA) +
  theme_bw() + labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), fill="") + 
  scale_fill_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(size=10)) + facet_grid(~station2,  scales ="free_x", space = "free") 



plot_stream.bis




plot_stream.comb <- ggarrange(plot_stream, plot_stream.bis, 
                              ncol=1, heights=c(1.5,1), labels = c("", "(e)"), 
                              font.label = list(size = 10, face = "plain"))


plot_stream.comb










setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()



ggsave(plot_rain.comb, path="figures", filename = "rain_isotopes(new).png",
       device = ragg::agg_png, dpi=350,
       width = 12, height = 17, units = "cm",
       bg="white")

ggsave(plot_stream.bis, path="figures", filename = "stream_isotopes(app).png",
       device = ragg::agg_png, dpi=350,
       width = 18, height = 10, units = "cm",
       bg="white")

ggsave(plot_stream, path="figures", filename = "stream_isotopes(new).png",
       device = ragg::agg_png, dpi=350,
       width = 16, height = 12, units = "cm",
       bg="white")


ggsave(plot_stream.comb, path="figures", filename = "stream_isotopes(comb).png",
       device = ragg::agg_png, dpi=350,
       width = 16, height = 18, units = "cm",
       bg="white")




write.table(dff_isotopes2, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\streams3.csv', sep = ",", quote = FALSE)














## Archive






ggplot(dff_isotopes2) + 
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=quantile(dO.x, probs=0.25, na.rm=T), ymax=quantile(dO.x, probs=0.75, na.rm=T)),
            fill="skyblue", alpha = 0.5) +
  geom_hline(aes(yintercept = median(dO.x, na.rm=T)), color="#01497c", linetype="dashed") +
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=quantile(dexcess.x/3, probs=0.25, na.rm=T), ymax=quantile(dexcess.x/3, probs=0.75, na.rm=T)),
            fill="skyblue", alpha = 0.5) +
  geom_hline(aes(yintercept = median(dexcess.x/3, na.rm=T)), color="#01497c", linetype="dashed") +
  geom_violin(aes(x=station, y=dO.y, group=station, fill=station2), alpha=0.7) +
  geom_violin(aes(x=station, y=dexcess.y/3, group=station, fill=station2), alpha=0.7) +
  geom_boxplot(aes(x=station, y=dO.y, group=station), width=0.1, outlier.shape = NA) +
  geom_boxplot(aes(x=station, y=dexcess.y/3, group=station), width=0.1, outlier.shape = NA) +
  scale_y_continuous(sec.axis = sec_axis(~.*3, name = "d-excess [\u2030]")) + 
  theme_bw() + labs(x="", y=expression(paste(delta^{18}, "O [\u2030]")), fill="") + 
  scale_fill_manual(values=c("#264653","#2a9d8f","darkblue","#e9c46a")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0,0.5,0,0.5),"cm"),
        text=element_text(size=10)) + facet_wrap(~station2, nrow=1, scales ="free_x")




