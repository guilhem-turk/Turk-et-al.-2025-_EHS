# Load necessary libraries
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
library(gstat)  # For spatial interpolation (kriging)
library(sf)     # For spatial data handling

df_arlon <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/Arlon_Precip_DayTotal.xlsx", 
                       range = "A10:B8000", col_types = c("date", "numeric"))

df_fratin <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/Fratin_Precip_DayTotal.xlsx", 
                        range = "A10:B8000", col_types = c("date", "numeric"))

df_namoussart <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/Namoussart_Precip_DayTotal.xlsx", 
                            range = "A10:B8000", col_types = c("date", "numeric"))

pluvio_stations.north <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/pluvio_stations.xlsx", 
                                    sheet="North",
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric"))

pluvio_stations_coord.north <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/pluvio_stations_coord.xlsx", 
                                          sheet="North")

pluvio_stations.south <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/pluvio_stations.xlsx", 
                                    sheet="South",
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric"))

pluvio_stations_coord.south<- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/pluvio_stations_coord.xlsx", 
                                         sheet="South")

isotope_stations_coord <- read_excel("~/MUSES/2nd publication/data/spatial_interpolation/data/tournée_isotope_liste_points.xls", 
                                     col_types = c("skip", "text", "skip", 
                                                   "skip", "text", "skip", "skip", "numeric", 
                                                   "numeric", "skip"))

shp_path <- '~/MUSES/2nd publication/data/spatial_interpolation/scripts/Catchments.shp'
catchments <- st_read(shp_path)

catchments <- catchments[catchments$gridcode %in% c(6, 8, 9, 14, 17, 18, 20, 23, 24, 34, 36, 40), ]

isotope_stations_coord <- isotope_stations_coord[isotope_stations_coord$ID %in%
                                                   c("S01","S02","S03","S05","S06","S07",
                                                     "S08","S09","S10","S25","S27","S30",
                                                     "P4","P5","P6", "P7","P15","P16","P17"),]

df_arlon$date <- as.Date(df_arlon$`#Timestamp`)
df_fratin$date <- as.Date(df_fratin$`#Timestamp`)
df_namoussart$date <- as.Date(df_namoussart$`#Timestamp`)

df_arlon <- rename(df_arlon, belg_9695 = Value)
df_fratin <- rename(df_fratin, belg_9574 = Value)
df_namoussart <- rename(df_namoussart, belg_9698 = Value)

df_belgium <- merge(drop_na(df_arlon[,c(2,3)]), drop_na(df_fratin[,c(2,3)]), by="date", all=T)
df_belgium <- merge(df_belgium, drop_na(df_namoussart[,c(2,3)]), by="date", all=T)


pluvio_stations.north <- merge(pluvio_stations.north, df_belgium, by="date", all.x=T)


precip_ts.north <- drop_na(pivot_longer(pluvio_stations.north, cols=-"date", names_to = "ID", values_to = "P"))
precip_ts.south <- drop_na(pivot_longer(pluvio_stations.south, cols=-"date", names_to = "ID", values_to = "P"))


precip_ts.north$region <- "North"
precip_ts.south$region <- "South"

precip_ts <- rbind(precip_ts.north, precip_ts.south)

precip_ts$flag <- 0
precip_ts$flag[precip_ts$P < 0.2] <- 1


precip_ts <- precip_ts %>%
  group_by(date, region) %>%
  mutate(cum_flag = sum(flag),
         N = n())


precip_ts.residue <- subset(precip_ts,  cum_flag/N == 1 | N-cum_flag <= 1)

precip_ts.residue %>%
  group_by(ID)  %>% reframe(P = sum(P))

precip_ts.north <- subset(precip_ts, region == "North" & cum_flag/N != 1 & N-cum_flag > 1)
precip_ts.south <- subset(precip_ts, region == "South" & cum_flag/N != 1 & N-cum_flag > 1)

precip_ts <- rbind(precip_ts.north[,c(1:4)], precip_ts.south[,c(1:4)])




pluvio_stations_coord.north$region <- "North"
pluvio_stations_coord.south$region <- "South"

pluvio_stations_coord <- rbind(pluvio_stations_coord.north, pluvio_stations_coord.south)

precip_ts <- merge(precip_ts, pluvio_stations_coord, by=c("ID","region"))
precip_ts <- arrange(precip_ts, date)

pluvio_stations_coord2 <- st_as_sf(pluvio_stations_coord, coords = c("X", "Y"), crs = st_crs(catchments))
isotope_stations_coord2 <- st_as_sf(drop_na(isotope_stations_coord),
                                    coords=c("E","N"), crs = st_crs(catchments))

isotope_stations_coord2$stat_type <- substr(isotope_stations_coord2$ID, 0, 1)
isotope_stations_coord2$station <- c("l","b","a","f","d","j","g","e","k","t","u","v","w",
                                     "i","h","c","x","y","z")

lux <- st_read('~/MUSES/2nd publication/data/spatial_interpolation/scripts/LIMADM_PAYS.shp')
litho <- st_read('~/MUSES/2nd publication/data/spatial_interpolation/scripts/LITHOLOGY_Full_study_area.shp')




st_bbox(lux)
st_bbox(pluvio_stations_coord2)

plot <- ggplot() + geom_sf(data=litho, aes(fill=LITHO), color="transparent", alpha=0.7) +
  geom_sf(data=lux$geometry, fill="transparent", color="black", linewidth=0.7) +
  geom_sf(data=catchments$geometry, fill="transparent", linewidth=0.3, color="black") + 
  geom_sf(data=pluvio_stations_coord2, aes(shape=region)) +
  geom_sf(data=isotope_stations_coord2, aes(color=stat_type), shape=21, linewidth=2) +
  geom_text_repel(data=subset(isotope_stations_coord2, stat_type=="S"), 
                  aes(label=station, geometry=geometry), stat = "sf_coordinates",size=3, fontface = "bold") +
  scale_color_manual(values=c("red","black"), labels=c("Pluvio", "Stream")) + 
  scale_shape_manual(values=c(24,25)) +
  scale_fill_manual(values=c("#e76f51","#f4a261","#e9c46a","#2a9d8f","#264653"),
                    labels=c("Calcerous-Dolomite", "Alluvials","Sandstone-Sedimentary",
                             "Marls", "Shists")) +
  theme_bw() + labs(fill="Bedrock geology", shape="Pluvio stations", 
                    color="Isotope stations", x="", y="") + 
  coord_sf(xlim = c(36000,107000), ylim = c(57500,140000)) +
  theme(text=element_text(size=8),
        legend.key.size = unit(0.3, "cm"))

plot





st_bbox(catchments)

plota <- ggplot() + geom_sf(data=litho, aes(fill=LITHO), color="transparent", alpha=0.7) +
  geom_sf(data=lux$geometry, fill="transparent", color="black", linewidth=1) +
  geom_sf(data=catchments$geometry, fill="transparent", linewidth=0.3, color="black") + 
  geom_sf(data=isotope_stations_coord2, aes(color=stat_type), shape=21, linewidth=2) +
  geom_text_repel(data=subset(isotope_stations_coord2, stat_type=="S"), 
                  aes(label=station, geometry=geometry), stat = "sf_coordinates",size=3, fontface = "bold") +
  scale_color_manual(values=c("red","black"), labels=c("Pluvio", "Stream")) + 
  scale_fill_manual(values=c("#e76f51","#f4a261","#e9c46a","#2a9d8f","#264653"),
                    labels=c("Calcerous-Dolomite", "Alluvials","Sandstone-Sedimentary",
                             "Marls", "Shists")) +
  theme_bw() + labs(fill="Bedrock geology", shape="Pluvio stations", 
                    color="Isotope stations", x="", y="") + 
  coord_sf(xlim = c(45000,80000), ylim = c(64000,103000)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        legend.key.size = unit(0.3, "cm"),
        legend.position = "right", 
        legend.justification = "bottom")

plota



plotb <- ggplot() + 
  geom_sf(data=lux$geometry, fill="transparent", color="black", linewidth=0.7) +
  geom_sf(data=catchments$geometry, fill="transparent", linewidth=0.3, color="black") + 
  geom_sf(data=pluvio_stations_coord2, aes(shape=region)) +
  scale_shape_manual(values=c(24,25)) +
  theme_bw() + labs(fill="Bedrock geology", shape="Pluvio stations", 
                    color="Isotope stations", x="", y="") + 
  coord_sf(xlim = c(36000,107000), ylim = c(57500,140000)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=8),
        legend.key.size = unit(0.3, "cm"))

plotb








precip_ts2 <- precip_ts %>%
  mutate(year = year(date)) %>%
  group_by(ID, year)  %>% reframe(P = sum(P)) %>%
  group_by(ID)  %>% reframe(P = mean(P)) 

precip_ts2 <- merge(precip_ts2, pluvio_stations_coord, by=c("ID"))


ggplot(precip_ts2) + 
  stat_cor(aes(x=elevation, y=P,  
               label = paste0(..r.label.., "~`,`~`p =`~", 
                              ifelse(readr::parse_number(..p.label..)<0.001, "0.001",
                                     round(readr::parse_number(..p.label..),digits=3)))),
           method = "spearman", label.y = c(1000), size=2.5) + 
  geom_point(aes(x=elevation, y=P)) + 
  geom_text_repel(aes(x=elevation, y=P, label=location), point.padding=0.5, size=3) +
  facet_wrap(~region) + theme_bw() + labs(x="elevation [masl]", y="P [mm/a]") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=10),
        legend.position = "none",
        strip.background = element_rect(color=NA, fill=NA, size=1),
        strip.text.x = element_text(size = 10))



catchments.north <- subset(catchments, !(gridcode %in% c("17","18","9")))
catchments.south <- subset(catchments, gridcode %in% c("17","18","9"))





#### Southern region


# interpolate catchments

catchments <- catchments.south
precip_ts <- subset(precip_ts, region == "South")
isotope_stations_coord2 <- subset(isotope_stations_coord2, stat_type=="P" & station %in% c("x", "y", "z"))

#precip_ts <- subset(precip_ts, date==as.Date("2010-01-15"))

# Create an empty list to store interpolated precipitation for each time step
interpolated_precip <- list()
interpolated_dates <- list()
variogram_plots <- list()

# Loop through each unique date in the dataset
unique_times <- unique(precip_ts$date)


k <- 1

for (time in unique_times) {
  # Subset data for the current time step
  current_precip <- precip_ts[precip_ts$date == time, ]
  
  # Convert the subset to an sf object with points for precipitation data
  current_precip_sf <- st_as_sf(current_precip, coords = c("X", "Y"), crs = st_crs(catchments))
  
  # Fit the variogram model
  
  v <- try(variogram(P ~ 1, current_precip_sf))
  
  if (!inherits(v, "gstatVariogram") && !inherits(v, "variogramCloud")) {
    
    interpolated_precip[[k]] <- c(NA,NA,NA)
    variogram_plots[[k]] <- NA
    
  } else {
    
    vgm.model <- try(fit.variogram(v, vgm(c("Exp", "Ste", "Sph")), fit.kappa = TRUE))
    
    
    # Perform kriging interpolation onto catchments for the current time step
    kriged_precip <- krige(P ~ 1, locations = current_precip_sf, newdata = catchments$geometry, model = vgm.model)
    
    # Store the interpolated precipitation for the current time step
    interpolated_precip[[k]] <- kriged_precip$var1.pred
    variogram_plots[[k]] <- plot(v, vgm.model, plot.numbers = TRUE)
    
  }
  
  interpolated_dates[[k]] <- as.POSIXct(time)
  
  
  percent_complete = round(k/length(unique_times)*100, digits = 2)
  message(paste0(percent_complete, "% complete"))
  
  k=k+1
  
}


station_names <- c("c Mess","i Mierbech","h Bibeschbach")


date <- as.Date(as.POSIXct(unlist(interpolated_dates)))

interpolated_results <- data.frame(
  station = rep(station_names, every=3),
  date = rep(date, each = 3),
  P = unlist(interpolated_precip)
)




df_helper <- interpolated_results %>%  
  group_by(station) %>%  
  reframe(date = seq(as.Date(min(interpolated_results$date)), 
                     as.Date(max(interpolated_results$date)), by = "day"))


df_catchments <- merge(df_helper, interpolated_results, by=c("date","station"), all=T)

df_catchments[is.na(df_catchments)] <- 0
df_catchments$P[df_catchments$P<0] <- 0


df_catchments.south <- df_catchments


ggplot(df_catchments.south) + geom_col(aes(x=date, y=P), color="skyblue", fill="skyblue") +
  facet_wrap(~station) + theme_bw()





# interpolate precipitation stations 




# Create an empty list to store interpolated precipitation for each time step
interpolated_precip <- list()
interpolated_dates <- list()
variogram_plots <- list()

# Loop through each unique date in the dataset
unique_times <- unique(precip_ts$date)


k <- 1

for (time in unique_times) {
  # Subset data for the current time step
  current_precip <- precip_ts[precip_ts$date == time, ]
  
  # Convert the subset to an sf object with points for precipitation data
  current_precip_sf <- st_as_sf(current_precip, coords = c("X", "Y"), crs = st_crs(catchments))
  
  # Fit the variogram model
  
  v <- try(variogram(P ~ 1, current_precip_sf))
  
  if (!inherits(v, "gstatVariogram") && !inherits(v, "variogramCloud")) {
    
    interpolated_precip[[k]] <- c(NA,NA,NA)
    variogram_plots[[k]] <- NA
    
  } else {
    
    vgm.model <- try(fit.variogram(v, vgm(c("Exp", "Ste", "Sph")), fit.kappa = TRUE))
    
    
    # Perform kriging interpolation onto catchments for the current time step
    kriged_precip <- krige(P ~ 1, locations = current_precip_sf, newdata = isotope_stations_coord2$geometry, 
                           model = vgm.model)
    
    # Store the interpolated precipitation for the current time step
    interpolated_precip[[k]] <- kriged_precip$var1.pred
    variogram_plots[[k]] <- plot(v, vgm.model, plot.numbers = TRUE)
    
  }
  
  interpolated_dates[[k]] <- as.POSIXct(time)
  
  
  percent_complete = round(k/length(unique_times)*100, digits=2)
  message(paste0(percent_complete, "% complete"))
  
  
  k=k+1
  
}


station_names <- c("P15", "P16", "P17")


date <- as.Date(as.POSIXct(unlist(interpolated_dates)))

interpolated_results <- data.frame(
  station = rep(station_names, every=3),
  date = rep(date, each = 3),
  P = unlist(interpolated_precip)
)


df_helper <- interpolated_results %>%  
  group_by(station) %>%  
  reframe(date = seq(as.Date(min(interpolated_results$date)), 
                     as.Date(max(interpolated_results$date)), by = "day"))


df_stations <- merge(df_helper, interpolated_results, by=c("date","station"), all=T)

df_stations[is.na(df_stations)] <- 0
df_stations$P[df_stations$P<0] <- 0


df_stations.south <- df_stations

ggplot(df_stations.south) + geom_col(aes(x=date, y=P), color="skyblue", fill="skyblue") +
  facet_wrap(~station) + theme_bw()






#### Northern region; re-run the first part of the script before running!


# interpolate catchments

catchments <- catchments.north
precip_ts <- subset(precip_ts, region == "North")
isotope_stations_coord2 <- subset(isotope_stations_coord2, stat_type=="P" & station %in% c("t", "u", "v", "w"))

#precip_ts <- subset(precip_ts, date==as.Date("2010-01-15"))

# Create an empty list to store interpolated precipitation for each time step
interpolated_precip <- list()
interpolated_dates <- list()
variogram_plots <- list()

# Loop through each unique date in the dataset
unique_times <- unique(precip_ts$date)


k <- 1

for (time in unique_times) {
  # Subset data for the current time step
  current_precip <- precip_ts[precip_ts$date == time, ]
  
  # Convert the subset to an sf object with points for precipitation data
  current_precip_sf <- st_as_sf(current_precip, coords = c("X", "Y"), crs = st_crs(catchments))
  
  # Fit the variogram model
  
  v <- try(variogram(P ~ 1, current_precip_sf))
  
  if (!inherits(v, "gstatVariogram") && !inherits(v, "variogramCloud")) {
    
    interpolated_precip[[k]] <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA)
    variogram_plots[[k]] <- NA
    
  } else {
    
    vgm.model <- try(fit.variogram(v, vgm(c("Exp", "Ste", "Sph")), fit.kappa = TRUE))
    
    
    # Perform kriging interpolation onto catchments for the current time step
    kriged_precip <- krige(P ~ 1, locations = current_precip_sf, newdata = catchments$geometry, model = vgm.model)
    
    # Store the interpolated precipitation for the current time step
    interpolated_precip[[k]] <- kriged_precip$var1.pred
    variogram_plots[[k]] <- plot(v, vgm.model, plot.numbers = TRUE)
    
  }
  
  interpolated_dates[[k]] <- as.POSIXct(time)
  
  
  percent_complete = round(k/length(unique_times)*100, digits = 2)
  message(paste0(percent_complete, "% complete"))
  
  k=k+1
  
}


station_names <- c("l Huewelerbach","f Pall", "k Roudbach", "j Schwebich", 
                   "a Weierbach","g Wollefsbach","e Attert-Useldange","d Attert-Reichlange",
                   "b Colpach")


date <- as.Date(as.POSIXct(unlist(interpolated_dates)))

interpolated_results <- data.frame(
  station = rep(station_names, every=9),
  date = rep(date, each = 9),
  P = unlist(interpolated_precip)
)




df_helper <- interpolated_results %>%  
  group_by(station) %>%  
  reframe(date = seq(as.Date(min(interpolated_results$date)), 
                     as.Date(max(interpolated_results$date)), by = "day"))


df_catchments <- merge(df_helper, interpolated_results, by=c("date","station"), all=T)

df_catchments[is.na(df_catchments)] <- 0
df_catchments$P[df_catchments$P<0] <- 0


df_catchments.north <- df_catchments


ggplot(df_catchments.north) + geom_col(aes(x=date, y=P), color="skyblue", fill="skyblue") +
  facet_wrap(~station) + theme_bw()





# interpolate precipitation stations 




# Create an empty list to store interpolated precipitation for each time step
interpolated_precip <- list()
interpolated_dates <- list()
variogram_plots <- list()

# Loop through each unique date in the dataset
unique_times <- unique(precip_ts$date)


k <- 1

for (time in unique_times) {
  # Subset data for the current time step
  current_precip <- precip_ts[precip_ts$date == time, ]
  
  # Convert the subset to an sf object with points for precipitation data
  current_precip_sf <- st_as_sf(current_precip, coords = c("X", "Y"), crs = st_crs(catchments))
  
  # Fit the variogram model
  
  v <- try(variogram(P ~ 1, current_precip_sf))
  
  if (!inherits(v, "gstatVariogram") && !inherits(v, "variogramCloud")) {
    
    interpolated_precip[[k]] <- c(NA,NA,NA,NA)
    variogram_plots[[k]] <- NA
    
  } else {
    
    vgm.model <- try(fit.variogram(v, vgm(c("Exp", "Ste", "Sph")), fit.kappa = TRUE))
    
    
    # Perform kriging interpolation onto catchments for the current time step
    kriged_precip <- krige(P ~ 1, locations = current_precip_sf, newdata = isotope_stations_coord2$geometry, 
                           model = vgm.model)
    
    # Store the interpolated precipitation for the current time step
    interpolated_precip[[k]] <- kriged_precip$var1.pred
    variogram_plots[[k]] <- plot(v, vgm.model, plot.numbers = TRUE)
    
  }
  
  interpolated_dates[[k]] <- as.POSIXct(time)
  
  
  percent_complete = round(k/length(unique_times)*100, digits=2)
  message(paste0(percent_complete, "% complete"))
  
  
  k=k+1
  
}


station_names <- c("P4","P5","P6", "P7")


date <- as.Date(as.POSIXct(unlist(interpolated_dates)))

interpolated_results <- data.frame(
  station = rep(station_names, every=4),
  date = rep(date, each = 4),
  P = unlist(interpolated_precip)
)


df_helper <- interpolated_results %>%  
  group_by(station) %>%  
  reframe(date = seq(as.Date(min(interpolated_results$date)), 
                     as.Date(max(interpolated_results$date)), by = "day"))


df_stations <- merge(df_helper, interpolated_results, by=c("date","station"), all=T)

df_stations[is.na(df_stations)] <- 0
df_stations$P[df_stations$P<0] <- 0


df_stations.north <- df_stations

ggplot(df_stations.north) + geom_col(aes(x=date, y=P), color="skyblue", fill="skyblue") +
  facet_wrap(~station) + theme_bw()






dff_catchments <- rbind(df_catchments.north, df_catchments.south)
dff_stations <- rbind(df_stations.north, df_stations.south)


setwd("C:/Users/turk/Documents/MUSES/2nd publication")
getwd()


ggsave(plota, path="figures", filename = "study_area.png",
       device = ragg::agg_png, dpi=350,
       width = 13, height = 16, units = "cm",
       bg="white")

ggsave(plotb, path="figures", filename = "study_area2.png",
       device = ragg::agg_png, dpi=350,
       width = 13, height = 16, units = "cm",
       bg="white")



write.table(dff_catchments, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\catchments_precip.csv', sep = ",", quote = FALSE)
write.table(dff_stations, file='C:\\Users\\turk\\Documents\\MUSES\\2nd publication\\codes\\stations_precip.csv', sep = ",", quote = FALSE)


