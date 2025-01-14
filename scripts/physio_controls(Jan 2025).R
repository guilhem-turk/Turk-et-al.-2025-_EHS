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
library(corrplot)
library(factoextra)


results.fnw <- read.csv("~/MUSES/2nd publication/codes/results_fnw.txt", header = TRUE, sep = ",", dec = ".")

fnw_profile_q <- read.csv("~/MUSES/2nd publication/codes/fnw_profile_q.txt", header = TRUE, sep = ",", dec = ".")

physio_settings <- read_excel("~/MUSES/2nd publication/data/Caractéristiques_bassins_versants.xlsx", 
                              sheet = "physio_selected")

df_storage <- read.csv("~/MUSES/2nd publication/codes/df_storage_new.csv", header = TRUE, sep = ",", dec = ".")




df_physio <- physio_settings[,c(1,4:8,10,14,16,18:22,2,3)]

df_physio[,c(2:6,10:14)] <- df_physio[,c(2:6,10:14)] %>% mutate(across(where(is.numeric), round, digits=1))
df_physio[,c(8,9)] <- df_physio[,c(8,9)] %>% mutate(across(where(is.numeric), round, digits=2))
df_physio[,c(7)] <- df_physio[,c(7)] %>% mutate(across(where(is.numeric), round, digits=0))




dff_storage <- drop_na(df_storage)

dff_span <- dff_storage %>%  
  group_by(station) %>%   
  reframe(start = min(date),
          end = max(date))

dff_storage <- subset(dff_storage, year <= year(min(dff_span$end)))

dff_storage %>%  
  group_by(station) %>%   
  reframe(start = min(date),
          end = max(date))


ddf_storage.yy <- dff_storage %>%
  group_by(station, year) %>%
  reframe(q = sum(q),
          P = sum(P),
          PET = sum(pet),
          Smax = mean(S_max))


ddf_storage<- ddf_storage.yy  %>%
  group_by(station) %>%
  reframe(q = mean(q),
          P = mean(P),
          PET = mean(PET),
          Smax = mean(Smax))




fnw_profile_q$profile <- paste0("q", rep(c(100,80,60,40,30,20,10), times=12))

df <- fnw_profile_q[fnw_profile_q$profile %in% c("q10","q20"),]

df <- df %>%
  group_by(station, station2) %>%
  reframe(q = mean(q),
          qpFnew = mean(qpFnew),
          qpFnew_se = mean(qpFnew_se),
          profile = "q20")


results.fnw$profile <- "all"


dff <- bind_rows(df[,-3], results.fnw[,-c(3,4)])



df_physio <- df_physio %>%  mutate(station=recode(station, "k Pall"="h Pall", "g Attert-Reichlange"="j Reichlange",
                                                  "e Wollefsbach"="e Wollefsbach","i Roudbach"="l Roudbach",
                                                  "c Mierbech"="c Mierbech", "d Bibeschbach"="d Bibeschbach", 
                                                  "f Mess"="f Mess", "h Attert-Useldange"="k Useldange",
                                                  "j Schwebich" = "g Schwebich", "l Huewelerbach" = "i Huewelerbach"))


ddf <- merge(dff, df_physio)
ddf <- merge(ddf, ddf_storage)




ddf <- ddf %>%  mutate(station2=recode(station2, "Schistose"="Weathered layer", "Marly"="Impermeable layer",
                                                 "Permeable layer interface"="Permeable layer interface", "Aggregated"="Aggregated"))

ddf$station2 <- factor(ddf$station2, levels = c("Weathered layer", "Impermeable layer", "Permeable layer interface", "Aggregated"))





ddf_alt <- ddf[,-c(7,12:15,19,20)]

ddf_alt <- pivot_longer(ddf_alt, !c(1:5), values_to = "value", names_to = "predictor")



ddf_alt$predictor <- factor(ddf_alt$predictor, levels = c("area", "forested", "grasslands", "agricultural", "elevation",
                                                          "sandstone", "marls", "shists","q","P","PET","Smax"))

new_labels = c("area"="Area [km2]","forested"="Forest [%]","grasslands"="Grassland [%]",
               "agricultural"="Agriculture [%]","elevation"="Elevation [m asl.]","sandstone"="Sandstone [%]",
               "marls"="Marls [%]","shists"="Schists [%]","q"="qa [mm]","P"="Pa [mm]","PET"="Ea [mm]","Smax"="Smax [mm]")




# Create a new data frame for facet labels
facet_labels <- subset(ddf_alt, profile=="q20") %>%
  group_by(predictor) %>%
  summarise(value = max(value, na.rm=T)-0.05*(max(value, na.rm=T)-min(value, na.rm=T)), qpFnew = 0.27)




plot_physio <- ggplot(subset(ddf_alt, profile=="q20")) +
  geom_point(aes(x=value, y=qpFnew*100, color=station2), size=1.5) +
  stat_cor(aes(x=value, y=qpFnew*100, group=predictor, 
               label = paste0("`", "r", "`~`=`~", ..r.., "~`,`~`p =`~", 
                              ifelse(readr::parse_number(..p.label..)<0.001, "0.001",
                                     round(readr::parse_number(..p.label..),digits=3)))),
           method = "pearson", label.y = 25, size=2.5) +
  geom_text(data=facet_labels, aes(x=value, y=qpFnew*100, 
                                   label=c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)",
                                           "(i)","(j)","(k)","(l)")), size = 3.5) + 
  scale_color_manual(values=c("#264653","#2a9d8f","#e9c46a","darkblue")) +
  facet_wrap(~predictor, scales = "free", strip.position = "bottom", 
             labeller = as_labeller(new_labels)) + 
  theme_bw() + labs(x="", y=expression(paste(F[new]," [-]")), color="Bedrock geology") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size=10),
        legend.position = "top")  

plot_physio










ddf2 <- ddf[,-c(1:5,7,12:15,19:20)]

#colnames(ddf2) <- c("A", "LU_F", "LU_G", "LU_A", "ELEV", "TWI", "IDPR", "GEO_SS", "GEO_ML", "GEO_SH")

M <- cor(ddf2, use = "complete.obs", method="pearson")

testRes <- cor.mtest(ddf2, conf.level = 0.95)

corrplot(M, p.mat = testRes$p, order="hclust", type="lower", diag=FALSE, method = 'circle', sig.level = c(0.001, 0.01, 0.05), insig = "label_sig",
         tl.col="black", tl.srt=45, pch.cex = 0.9)


plot_cor <- {corrplot(M, p.mat = testRes$p, order="hclust", diag=FALSE, method = 'circle', sig.level = c(0.001, 0.01, 0.05), insig = "label_sig",
                      tl.col="black", tl.srt=45, pch.cex = 0.9);
             corrplot(M, p.mat = testRes$p, order="hclust", type="lower", diag=FALSE, method = 'circle', sig.level = 0.005, insig = "p-value",
                      tl.col="black", tl.srt=45, tl.pos = "n", cl.pos="n", add=T)
             recordPlot()}









setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()



ggsave(plot_physio, path="figures", filename = "physio_controls(new).png",
       device = ragg::agg_png, dpi=350,
       width = 17, height = 16, units = "cm",
       bg="white")


ggsave(replayPlot(plot_cor), path="figures", filename = "corr_matrix.png",
       device = ragg::agg_png, dpi=350,
       width = 20, height = 20, units = "cm",
       bg="white")




