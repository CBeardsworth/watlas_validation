library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(dplyr)

# Get data --------------------------------------------------------------------------------------------------------------------

#atlas data

stat <- read.csv("data/stat_raw.csv")%>%
    mutate(atlas_x="X", atlas_y ="Y")%>%
    st_as_sf(coords=c("X","Y"), crs = 32631)

stat_filt <- read.csv("data/stat_filt.csv")%>%
    mutate(atlas_x="X", atlas_y ="Y")%>%
    st_as_sf(coords=c("X","Y"), crs = 32631)

movingLine<- read.csv("data/moving_raw.csv")%>%
    mutate(atlas_x="X", atlas_y ="Y")%>%
    st_as_sf(coords=c("X","Y"), crs = 32631)%>%
    group_by(date, transport)%>%
    summarize(do_union=F)%>%
    st_cast("LINESTRING")

movingLine_filt <- read.csv("data/moving_filt.csv")%>%
    mutate(atlas_x="X", atlas_y ="Y")%>%
    st_as_sf(coords=c("X","Y"), crs = 32631)%>%
    group_by(date, transport)%>%
    summarize(do_union=F)%>%
    st_cast("LINESTRING")

#gps data

gps_stat <- read_sf("data/gps_stat.gpkg", as_tibble = F)

gps_movingLine <- read_sf("data/gps_moving.gpkg")%>%
                  group_by(date, transport)%>%
                  summarize(do_union=F)%>%
                  st_cast("LINESTRING")

# detections

atlas_dets_geom <- read.csv("data/detections_atlas.csv")%>%
    st_as_sf(coords=c("atlas_x", "atlas_y"), crs = 32631)



#### Figures ####-------------------------------------------------------------------------------------------------------------

bath <- read_sf("D:/OneDrive - NIOZ/2_Reference Data/Basemaps/utm31 Bathy/DutchWaddenBathymetry-144_LAT.shp")
coast_NL <- read_sf("D:/OneDrive - NIOZ/2_Reference Data/Basemaps/WaddenCoast.gpkg")#%>% #https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon 
coast <- read_sf("D:/OneDrive - NIOZ/2_Reference Data/Basemaps/utm31 Bathy/DutchWaddenBathymetry100.shp")
receivers <- read_sf("D:/OneDrive - NIOZ/2_Reference Data/Receiver Locations/2020receivers.gpkg")

# Figure 1 - Wadden sea and receiver overview -----

griend <- st_as_sfc(st_bbox(c(xmin = 645464, xmax = 656867, ymin = 5900456, ymax = 5908406), crs=32631)) #for box on fig 1
box <- st_as_sfc(st_bbox(c(xmin = 665000, xmax = 670000, ymin = 5912923, ymax = 5922923), crs=32631)) #for box on fig 1

wadden <- ggplot() +  
    geom_sf(data = bath, aes(fill="Mudflat"), col="grey50",lwd=0.1) + 
    geom_sf(data = coast_NL, aes(fill="Land"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, fill="red", aes(shape="Receivers"), size = 3)+
    geom_sf(data=griend, col="blue", fill=NA)+
    coord_sf(xlim= c(605107,670000), ylim=c(5860000,5922923))+
    annotate(geom = "text", x = 615200, y = 5915100, label = "North Sea", 
             fontface = "italic", color = "grey22", size = 4) +
    annotate(geom = "text", x = 640000, y = 5880200, label = "Wadden Sea", 
             fontface = "italic", color = "grey22", size = 4) +
    geom_sf_text(data=griend, label="Griend Mudflat", size=2.5, nudge_x=-2000, nudge_y=3200, col="blue")+
    
    xlab("Longitude")+
    ylab("Latitude")+
    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                           style=north_arrow_orienteering(text_size=8))+
    annotation_scale(location="br", width_hint=0.2)+
    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"), 
                      name = NULL) +
    scale_shape_manual(values = c("Receivers" = 24), 
                       name = NULL) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())
#wadden

pdf(file = "figs/Validation_Fig1_wadden.pdf",
    width = 9, height = 8)
wadden
dev.off()

# Figure 2 - experiment -----

track <- ggplot() +  
    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) +
    geom_sf(data = coast,aes(fill="Griend (Land)"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, fill="red",aes(shape="Receivers"), size =3)+
    geom_sf(data=gps_movingLine, aes(col=transport),linetype="dashed", lwd=1)+
    geom_sf_label(data=gps_stat, label=gps_stat$ID, size=3.5, label.padding = unit(0.1,"lines"))+
    xlab("Longitude")+
    ylab("Latitude")+
    #coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
    coord_sf(xlim= c(646864,654867), ylim=c(5900956,5906406))+ #griend only
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
#track

pdf(file = "figs/Validation_Fig2_experiment.pdf",
    width = 9, height = 5.5)
track
dev.off()

##### Figure 3 - Moving Raw and Filtered vs GPS ####

raw <- ggplot() + 
    ggtitle("Raw ATLAS data")+
    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
    geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, fill="red",aes(shape="Receivers"), size =3)+
    geom_sf(data=movingLine, aes(col="ATLAS",linetype="ATLAS"), size=1)+
    geom_sf(data=gps_movingLine, aes(col="GPS", linetype="GPS"), lwd=0.5)+
    xlab("Longitude")+
    ylab("Latitude")+
    #coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
    coord_sf(xlim= c(646864,654867), ylim=c(5900956,5906406))+ #griend only
    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                           style=north_arrow_orienteering(text_size=8))+
    annotation_scale(location="br", width_hint=0.2)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
          panel.border= element_rect(colour="black", fill="NA"),
          legend.key=element_blank(), 
          legend.position = "none")+
    #legend.box.background = element_rect(colour="black"),
    #legend.spacing.y = unit(-0.2, "cm")) +
    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                      name = NULL) +
    scale_shape_manual(values = c("Receivers" = 24),
                       name = NULL) +
    scale_color_manual(name=NULL,
                       values=c("GPS" = "black", "ATLAS" = "darkorchid3"))+
    scale_linetype_manual(values = c("GPS" = "solid", "ATLAS" = "solid"),
                          name = NULL) 
#raw

filt <- ggplot() + 
    ggtitle("Filter-Smoothed ATLAS data")+
    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
    geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, fill="red",aes(shape="Receivers"), size =3)+
    geom_sf(data=movingLine_filt, aes(col="ATLAS",linetype="ATLAS"), size=1)+
    geom_sf(data=gps_movingLine, aes(col="GPS", linetype="GPS"), lwd=0.5)+
    xlab("Longitude")+
    ylab("Latitude")+
    #coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
    coord_sf(xlim= c(646864,654867), ylim=c(5900956,5906406))+ #griend only
    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                           style=north_arrow_orienteering(text_size=8))+
    annotation_scale(location="br", width_hint=0.2)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
          panel.border= element_rect(colour="black", fill="NA"),
          legend.key=element_blank(), 
          legend.position = "none")+
    #legend.box.background = element_rect(colour="black"),
    #legend.spacing.y = unit(-0.2, "cm")) +
    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                      name = NULL) +
    scale_shape_manual(values = c("Receivers" = 24),
                       name = NULL) +
    scale_color_manual(name=NULL,
                       values=c("GPS" = "black", "ATLAS" = "darkorchid3"))+
    scale_linetype_manual(values = c("GPS" = "solid", "ATLAS" = "solid"),
                          name = NULL) 
#filt  
leg <- get_legend(raw + theme(legend.position="bottom"))

pdf(file = "figs/Validation_Fig3_Raw_v_Filt.pdf",
    width = 9, height = 12)
p <- plot_grid(raw, filt,leg, labels=c("A","B", ""), ncol=1, nrow=3, rel_heights = c(1,1,0.1))
p
dev.off()

##### Figure 4 - Static Raw and Filtered vs GPS ####
#plot(gps_stat)
raw <- ggplot() + 
    ggtitle("Raw ATLAS data")+
    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
    geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, aes(shape="Receivers", size= "Receivers"), fill="red")+
    geom_sf(data=stat, aes(shape = "ATLAS Location estimate", size= "ATLAS Location estimate"), col="darkorchid3")+
    geom_sf(data=gps_stat, aes(shape = "GPS Location Estimate (mean)", size = "GPS Location Estimate (mean)"), col="black")+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(xlim= c(646864,654867), ylim=c(5900956,5908006))+ #griend only
    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                           style=north_arrow_orienteering(text_size=8))+
    annotation_scale(location="br", width_hint=0.2)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
          panel.border= element_rect(colour="black", fill="NA"),
          legend.key=element_blank(),
          legend.spacing.y = unit(-0.12, "cm"), 
          legend.position = "none") +
    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                      name = NULL) +
    scale_shape_manual(values = c("Receivers" = 24, "GPS Location Estimate (mean)" = 4, "ATLAS Location estimate"=4),
                       name = NULL)+
    scale_size_manual(values = c("Receivers" = 3, "GPS Location Estimate (mean)" = 5, "ATLAS Location estimate"=1),
                      name = NULL)+
    guides(size = guide_legend(override.aes = list(size=c(2,5,5), col=c("darkorchid3", "black","black"))))

#raw

filt <- ggplot() + 
    ggtitle("Filter-Smoothed ATLAS data")+
    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
    geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
    geom_sf(data=receivers, aes(shape="Receivers", size= "Receivers"), fill="red")+
    geom_sf(data=stat_filt, aes(shape = "ATLAS Location estimate", size= "ATLAS Location estimate"), col="darkorchid3")+
    geom_sf(data=gps_stat, aes(shape = "GPS Location Estimate (mean)", size = "GPS Location Estimate (mean)"), col="black")+
    xlab("Longitude")+
    ylab("Latitude")+
    coord_sf(xlim= c(646864,654867), ylim=c(5900956,5908006))+ #griend only
    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                           style=north_arrow_orienteering(text_size=8))+
    annotation_scale(location="br", width_hint=0.2)+
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
          panel.border= element_rect(colour="black", fill="NA"),
          legend.key=element_blank(),
          legend.spacing.y = unit(-0.12, "cm"), 
          legend.position = "none") +
    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                      name = NULL) +
    scale_shape_manual(values = c("Receivers" = 24, "GPS Location Estimate (mean)" = 4, "ATLAS Location estimate"=4),
                       name = NULL)+
    scale_size_manual(values = c("Receivers" = 3, "GPS Location Estimate (mean)" = 5, "ATLAS Location estimate"=1),
                      name = NULL)+
    guides(size = guide_legend(override.aes = list(size=c(2,5,5), col=c("darkorchid3", "black","black"))))

#filt

leg <- get_legend(raw + theme(legend.position="bottom"))

#quantile(stat$error_dist, 0.95)

raw_hist <- ggplot()+
    geom_histogram(data=stat[stat$error_dist<50,], aes(x=error_dist), col="black", fill = "grey40", bins=50) + # 95 percentil = 45.4 therefore plot all errors below 50
    scale_x_continuous(expand=c(0,0), breaks=seq(0,50,5))+
    scale_y_continuous(expand=c(0,0), limits= c(0,1000)) +
    xlab("Error (distance from GPS (m))")+
    ylab("Frequency")+
    theme_classic()


filt_hist <- ggplot()+
    geom_histogram(data=stat_filt[stat_filt$error_dist<50,], aes(x=error_dist), col="black", fill = "grey40", bins=50) + # 95 percentil = 45.4 therefore plot all errors below 50
    scale_x_continuous(expand=c(0,0), breaks=seq(0,50,5))+
    scale_y_continuous(expand=c(0,0), limits= c(0,1000)) +
    xlab("Error (distance from GPS (m))")+
    ylab("Frequency")+
    theme_classic()
#quantile(stat_filt$error_dist, 0.95) # 20.0


maps <- plot_grid(raw,filt, labels = c("A", "B"))
hist <- plot_grid(raw_hist, filt_hist, labels = c("C", "D"))

pdf(file = "figs/Validation_Fig4_Raw_v_Filt_Static.pdf",
    width = 9, height = 7)
p <- plot_grid(maps,leg,  hist, ncol=1, nrow=3,rel_heights = c(1,0.08,0.7))
p
dev.off()


# Figure 5 - Detections for each receiver -----

plot_dets <- function(i){
    box <- st_as_sfc(st_bbox(c(xmin = 634264, xmax = 647264, ymin = 5910223, ymax = 5916423), crs=32631)) #for box on fig 1
    
    ggplot() + 
            geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
            geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
            geom_sf(data=gps_movingLine, col="grey50", linetype="dashed")+
            geom_sf(data=box, col="black", fill="white")+
            geom_sf(data=receivers[receivers$BSid==i,], aes(shape="Receivers"), fill="red", size = 5)+
            annotate("text", label = paste("Height =", receivers[receivers$BSid==i,]$Height),x= 634564, y =5911506, hjust=0, size=3)+
            annotate("text", label = paste("% Detections =", receivers[receivers$BSid==i,]$pc_dets, "%"),x= 634564, y =5913506, hjust=0, size=3)+
            annotate("text", label = paste("Furthest Detection =", round(receivers[receivers$BSid==i,]$furthest_det,0), "m"),x= 634564, y =5912506, hjust=0, size=3)+
            
            geom_sf(data=atlas_dets_geom[atlas_dets_geom$BS==i,], aes(shape = "ATLAS Location estimate"), col="darkorchid3", size=0.1)+
            xlab("Longitude")+
            ylab("Latitude")+
            coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
            annotation_north_arrow(location="tl",height= unit(0.5,"cm"), width= unit(0.4, "cm"), 
                                   style=north_arrow_orienteering(text_size=6))+
            annotation_scale(location="tl", width_hint=0.2,pad_x=unit(1.1, "cm"),pad_y=unit(0.3, "cm"),height= unit(0.3,"cm"), text_cex=0.9)+
            theme(panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
                  panel.border= element_rect(colour="black", fill="NA"),
                  legend.key=element_blank(),
                  legend.spacing.y = unit(-0.12, "cm"), 
                  legend.position = "none") +
            scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                              name = NULL) +
            scale_shape_manual(values = c("Receivers" = 24,"ATLAS Location estimate" = 4),
                               name = NULL)}

my_plots <- lapply(unique(atlas_dets_geom$BS), FUN=plot_dets)

pdf(file = "figs/Validation_Fig5_Dets.pdf",
    width = 9, height = 18)
p <- cowplot::plot_grid(plotlist = my_plots, ncol=2, nrow=5)
p
dev.off()

