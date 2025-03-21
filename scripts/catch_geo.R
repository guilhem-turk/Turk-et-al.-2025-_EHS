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



physio_settings <- read_excel("~/MUSES/2nd publication/data/Caractéristiques_bassins_versants.xlsx", 
                              sheet = "physio_selected")



df_physio <- physio_settings[,c(1,18:22)]


df_physio <- df_physio %>%  mutate(station=recode(station, "k Pall"="h Pall", "g Attert-Reichlange"="j Reichlange",
                                                  "e Wollefsbach"="e Wollefsbach","i Roudbach"="l Roudbach",
                                                  "c Mierbech"="c Mierbech", "d Bibeschbach"="d Bibeschbach", 
                                                  "f Mess"="f Mess", "h Attert-Useldange"="k Useldange",
                                                  "j Schwebich" = "g Schwebich", "l Huewelerbach" = "i Huewelerbach"))



dff_physio <- pivot_longer(df_physio, cols = !station, names_to = "geology")



dff_physio <- dff_physio %>%  mutate(geology=recode(geology, "aluvials"="Surficial deposits", 
                                                  "limestone"="Limestone and dolomites",
                                                  "marls"="Marls and claystone",
                                                  "sandstone"="Sandstone and conglomerates",
                                                  "shists"="Schists and quartzites"))


dff_physio$geology <- factor(dff_physio$geology, levels = c("Surficial deposits", "Limestone and dolomites", 
                                                            "Marls and claystone", "Sandstone and conglomerates", 
                                                            "Schists and quartzites"))



plot <- ggplot(dff_physio) +
  geom_col(aes(x=station, y=value, fill=geology), width = 0.5, color="white") +
  theme_minimal() + coord_flip() + labs(x="", y="", fill="Bedrock geology") +
  scale_fill_manual(values=c("grey","#e76f51","#2a9d8f","#e9c46a","#264653")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        text=element_text(size=6)) +
  facet_wrap(~station, scales="free")



plot




ggplot(dff_physio) +
  geom_col(aes(x=station, y=value, fill=geology),  color="white") +
  theme_minimal() + coord_flip() + labs(x="", y="", fill="Bedrock geology") +
  scale_fill_manual(values=c("grey","#e76f51","#2a9d8f","#e9c46a","#264653")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        text=element_text(size=8)) +
  facet_wrap(~station, scales="free_y", ncol=1)







setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()


ggsave(plot, path="figures", filename = "catch_geo.png",
       device = ragg::agg_png, dpi=350,
       width = 15, height = 8, units = "cm",
       bg="white")





write.table(dff_physio, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\catch_geo.csv', sep = ",", quote = FALSE)
