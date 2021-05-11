library(watlastools)
library(gganimate) 
library(gifski)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(cowplot)
library(RMySQL)

# receiver detections

con <- dbConnect(MySQL(),
                 user="atlaskama", password="watlasrulez:)",
                 dbname="atlas2019", host="abtdb1.nioz.nl")


options(scipen=999)

##### Get Detections for test tag throughout testing. 
as.numeric(as.POSIXct("2019-08-13 19:45"))*1000
as.numeric(as.POSIXct("2019-08-13 19:55"))*1000

dets <- dbGetQuery(con, "SELECT * from atlas2019.DETECTIONS where TAG = 31001000795 AND TIME > 1565718300000 AND TIME < 1565718900000 LIMIT 250000;")
dbDisconnect(con)

#########
dets$datetime <- as.POSIXct(dets$TIME/1000, origin = "1970-01-01") # Time is given in milliseconds so must /1000 before converting
rec_bleeps <- merge(dets, receivers, by.x = "BS", by.y="BSid")



setwd("C:/Users/cbeardsworth/OneDrive - NIOZ") # work PC
setwd("~/GitHub/watlas_validation") #work github folder

bath <- read_sf("data/DutchWaddenBathymetry-144_LAT.shp")
coast_NL <- read_sf("data/WaddenCoast.gpkg")#%>% #https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon 
coast <- read_sf("data/DutchWaddenBathymetry100.shp")
receivers <- read_sf("data/2020receivers.gpkg")

# Figure 1 - Wadden sea and receiver overview -----

griend <- st_as_sfc(st_bbox(c(xmin = 645464, xmax = 656867, ymin = 5900456, ymax = 5908406), crs=32631)) #for box on fig 1
box <- st_as_sfc(st_bbox(c(xmin = 665000, xmax = 670000, ymin = 5912923, ymax = 5922923), crs=32631)) #for box on fig 1

bird_data <- wat_get_data(tag = 31001000795,
                            tracking_time_start = "2019-08-13 19:45",
                            tracking_time_end = "2019-08-13 20:05",
                            host= "145.1.124.30", 
                            #host="abtdb1.nioz.nl",
                            database = "atlas2019",
                            username = "atlaskama",
                            password = "watlasrulez:)") %>%
  mutate(TIME=TIME/1000, x=X,y=Y, datetime=as.POSIXct(TIME, origin = "1970-01-01"))%>%
  st_as_sf(coords = c("x","y"))%>%
  st_set_geometry("geometry")%>%
  st_set_crs(32631)

observer <- c(649754.80,5902434.69)

base <- ggplot() +
  geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) +
  geom_sf(data = coast,aes(fill="Griend (Land)"), col="grey40", lwd=0.1) +
  geom_sf(data=receivers, fill="red",aes(shape="Receivers"), size =3)+
  coord_sf(xlim= c(646864,654867), ylim=c(5901056,5905406))+ #griend only
  #coord_sf(xlim= c(min(bird_data$X)-100,max(bird_data$X)+100), ylim=c(min(bird_data$Y)-100,max(bird_data$Y)+100))+ #movement area only
  annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                         style=north_arrow_orienteering(text_size=8))+
  annotation_scale(location="br", width_hint=0.2)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
        panel.border= element_rect(colour="black", fill="NA"),
        legend.key=element_blank(), 
        #legend.position = c(0.15,0.85),
        #legend.box.background = element_rect(colour="black"),
        legend.spacing.y = unit(-0.1, "cm")) +
        scale_fill_manual(values = c("Griend (Land)" = "grey65", "Mudflat" = "papayawhip"), 
                    name = NULL) +
       scale_shape_manual(values = c("Receivers" = 24), 
                     name = NULL) +
        scale_color_manual(name=NULL,
                     labels=c("Boat","Walking"),
                     values=c("#440154FF","#1F968BFF"))


bird_move <- base +
  geom_point(data=bird_data, aes(x=X,y=Y),col="purple")+
  geom_point(data=rec_bleeps, aes(x=BS_x, y=BS_y), fill = "white", size= 3, shape = 24)+
  transition_time(datetime)+
  geom_point(aes(x= 649754.80,y=5902434.69))+
  labs(title = "Time: {frame_time}")
  #shadow_wake(wake_length = 0.05, alpha = FALSE)+
  #shadow_mark(fill="black")

animate(bird_move)

base <- ggplot() +
  geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) +
  geom_sf(data = coast,aes(fill="Griend (Land)"), col="grey40", lwd=0.1) +
  geom_sf(data=receivers, fill="red",aes(shape="Receivers"), size =3)+
  #coord_sf(xlim= c(646864,654867), ylim=c(5901056,5905406))+ #griend only
  coord_sf(xlim= c(min(bird_data$X)-100,max(bird_data$X)+100), ylim=c(min(bird_data$Y)-100,max(bird_data$Y)+100))+ #movement area only
  annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                         style=north_arrow_orienteering(text_size=8))+
  annotation_scale(location="br", width_hint=0.2)+
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
        panel.border= element_rect(colour="black", fill="NA"),
        legend.key=element_blank(), 
        #legend.position = c(0.15,0.85),
        #legend.box.background = element_rect(colour="black"),
        legend.spacing.y = unit(-0.1, "cm")) +
  scale_fill_manual(values = c("Griend (Land)" = "grey65", "Mudflat" = "papayawhip"), 
                    name = NULL) +
  scale_shape_manual(values = c("Receivers" = 24), 
                     name = NULL) +
  scale_color_manual(name=NULL,
                     labels=c("Boat","Walking"),
                     values=c("#440154FF","#1F968BFF"))


bird_move <- base +
  geom_point(data=bird_data, aes(x=X,y=Y),col="purple")+
  transition_time(datetime)+
  geom_point(aes(x= 649754.80,y=5902434.69))+
  shadow_wake(wake_length = 0.1, alpha = FALSE)


ani.options(interval = 60)
animate(bird_move, "test.gif", title_frame = TRUE)


plot(prelim_data$X, prelim_data$Y)
plot(649754.80, 5902434.69,  col="red")
