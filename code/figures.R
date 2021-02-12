library(sf)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(dplyr)
library(atlastools)

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

bath <- read_sf("data/DutchWaddenBathymetry-144_LAT.shp")
coast_NL <- read_sf("data/WaddenCoast.gpkg")#%>% #https://www.eea.europa.eu/data-and-maps/data/eea-coastline-for-analysis-2/gis-data/eea-coastline-polygon 
coast <- read_sf("data/DutchWaddenBathymetry100.shp")
receivers <- read_sf("data/2020receivers.gpkg")

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

png(file = "figs/Validation_Fig1_wadden.png", unit="in", res= 600,
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

png(file = "figs/Validation_Fig2_experiment.png",unit="in", res=600,
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

png(file = "figs/Validation_Fig3_Raw_v_Filt.png",unit="in", res=600,
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

png(file = "figs/Validation_Fig4_Raw_v_Filt_Static.png",unit="in", res=600,
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

png(file = "figs/Validation_Fig5_Dets.png", unit="in", res=600,
    width = 9, height = 18)
p <- cowplot::plot_grid(plotlist = my_plots, ncol=2, nrow=5)
p
dev.off()

png(file = "figs/Validation_Fig5_Dets.png", unit="in", res=600,
    width = 18, height = 9)
p <- cowplot::plot_grid(plotlist = my_plots, ncol=4, nrow=3)
p
dev.off()

# ESM S1 -----
library(data.table)
library(lubridate)
all_var_stat = NULL
all_var_moving = NULL

#VARXY Filter figures
for (var in seq(10,5000,10)){
    #stationary test
    sum_stat_filt <- read.csv("data/stat_raw.csv")%>%
        filter(VARX < var & VARY< var)%>%
        group_by(ID)%>%
        summarize(n = length(error_dist), fix_rate= (n/300)*100, mean_NBS = mean(NBS),sd_NBS = sd(NBS), mean_error = mean(error_dist), sd= sd(error_dist), q2.5 = quantile(error_dist, 0.025), q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
    
    var_stat <- data.frame(var=var, sum_stat_filt)
    all_var_stat <- rbind(all_var_stat,var_stat)
    
    #moving test
    sum_moving_filt <- read.csv("data/moving_raw.csv")%>%
        filter(VARX < var & VARY< var)%>%
        filter(time_diff < 2)%>%
        group_by(NBS) %>%
        summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
    
    
    var_moving <- data.frame(var=var, sum_moving_filt)
    all_var_moving <- rbind(all_var_moving,var_moving)}

#smoothing window
all_win_stat = NULL
all_win_moving = NULL

gps_stat <- read_sf("data/gps_stat.gpkg", as_tibble = F)

match_gps_moving <- read_sf("data/gps_moving.gpkg", as_tibble = F)%>%
    select(time, gps_x, gps_y)
setDT(match_gps_moving)
match_gps_moving[,gps_time_match:=time]
setkey(match_gps_moving,gps_time_match)

for (win in c(0,seq(3,60,2))){
    
    if(win==0){
        sum_stat_filt <- read.csv("data/stat_raw.csv") %>%
            filter(VARX <2000, VARY < 2000)  %>%
            group_by(ID)%>%
            summarize(n = length(error_dist), fix_rate= (n/300)*100, mean_NBS = mean(NBS),sd_NBS = sd(NBS), mean_error = mean(error_dist), sd= sd(error_dist), q2.5 = quantile(error_dist, 0.025), q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
        
        sum_moving_filt <- read.csv("data/moving_raw.csv") %>%
            filter(time_diff < 2, VARX <2000, VARY < 2000)%>%
            group_by(NBS)%>%
            summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
    }
    
    else{
        
        sum_stat_filt <- read.csv("data/stat_raw.csv") %>%
            mutate(Timestamp = as.POSIXct(Timestamp), TIME = as.numeric(TIME)/1000) %>%
            filter(VARX <2000, VARY < 2000) %>%
            group_by(ID)%>%
            atl_median_smooth(moving_window=win)%>%
            mutate(error_dist = sqrt(((gps_mean_x - X)^2) + ((gps_mean_y -Y)^2))) %>%
            group_by(ID)%>%
            summarize(n = length(error_dist), fix_rate= (n/300)*100, mean_NBS = mean(NBS),sd_NBS = sd(NBS), mean_error = mean(error_dist), sd= sd(error_dist), q2.5 = quantile(error_dist, 0.025), q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
        
        
        sum_moving_filt <- read.csv("data/moving_raw.csv") %>%
            mutate(Timestamp = as.POSIXct(Timestamp), time = as.POSIXct(time), TIME = as.numeric(TIME)/1000, raw_x = X, raw_y = Y) %>%
            filter(VARX <2000, VARY < 2000)%>%
            atl_median_smooth(moving_window=win)%>%
            mutate(error_dist = sqrt(((gps_x - X)^2) + ((gps_y -Y)^2)))%>%
            filter(time_diff < 2)%>%
            group_by(NBS)%>%
            summarize(n = length(error_dist), mean_error = mean(error_dist), sd= sd(error_dist), median_error = median(error_dist), q2.5 = quantile(error_dist, 0.025),q50 = quantile(error_dist, 0.5),q97.5 = quantile(error_dist, 0.975))
        
    }
    
    win_stat <- data.frame(win=win, sum_stat_filt)
    all_win_stat <- rbind(all_win_stat,win_stat)
    
    win_moving <- data.frame(win=win, sum_moving_filt)
    all_win_moving <- rbind(all_win_moving,win_moving)
    }
all_win_stat <- all_win_stat[all_win_stat$n>all_win_stat$win,] # ensure that when median window is more than the window it is removed. 

# ESM1 - plots ------    
#Stationary plots

all_var_stat$ID <- as.factor(all_var_stat$ID)
stat1 <- ggplot(all_var_stat[all_var_stat$var <=5000 ,], aes(x=var, y = mean_error, group = as.factor(ID), col=ID))+
    geom_line(size=1)+
    scale_y_continuous(expand=c(0,0), limits=c(0,100), breaks = seq(0,100,20))+
    scale_x_reverse(expand=c(0,0), limits=c(5000, -100))+
    scale_color_manual(values=rainbow(n=16))+
    ylab("mean distance to GPS (m)")+
    xlab("VARX & VARY filtering values")+
    geom_vline(xintercept=2000,linetype="dashed")+
    theme(legend.position="none",
          panel.background = element_rect(fill = "white"),
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())



stat2 <- ggplot(all_var_stat[all_var_stat$var <=5000 ,], aes(x=var, y = fix_rate, group = as.factor(ID), col=ID))+
    geom_line(size=1)+
    scale_y_continuous(expand=c(0,0), limits=c(0,105), breaks=seq(0,100,10))+
    scale_x_reverse(expand=c(0,0), limits=c(5000, -100))+
    geom_vline(xintercept=2000,linetype="dashed")+
    ylab("% of expected localisations remaining")+
    xlab("VARX & VARY filtering values")+
    scale_color_manual(values=rainbow(n=16))+
    theme(legend.position="none",
          panel.background = element_rect(fill = "white"),
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())

#smoothing
all_win_stat$ID <- as.factor(all_win_stat$ID)
stat3 <- ggplot(all_win_stat, aes(x=win, y = mean_error, group = as.factor(ID), col=ID))+
    geom_line(size=1)+
    scale_y_continuous(expand=c(0,0), limits = c(0,50), breaks=seq(0,50,10))+
    scale_x_continuous(expand=c(0,0), limits=c(0,60), breaks=seq(3,60,6))+
    ylab("mean distance to GPS (m)")+
    xlab("median smooth")+
    scale_color_manual(values=rainbow(n=16))+
    theme(panel.background = element_rect(fill = "white"),
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())

#moving plots
all_var_moving$NBS <- as.factor(all_var_moving$NBS)

move1 <- ggplot(all_var_moving[all_var_moving$var <=5000,], aes(x=var, y = mean_error, group = as.factor(NBS), col=NBS))+
    geom_line(size=1)+
    scale_y_continuous(expand=c(0,0), limits=c(0,25), breaks=seq(0,25,5))+
    scale_x_reverse(expand=c(0,0), limits=c(5000, -100))+
    scale_color_viridis_d()+
    geom_vline(xintercept=2000,linetype="dashed")+
    ylab("mean distance to GPS (m)")+
    xlab("VARX & VARY filtering values")+
    theme(legend.position="none",
          panel.background = element_rect(fill = "white"),
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())


move2 <- ggplot(all_var_moving[all_var_moving$var <=5000,], aes(x=var, y = n, group = as.factor(NBS), col=NBS))+
    geom_line(size=1)+
    scale_y_continuous(expand=c(0,0), limits=c(0,10000), breaks = c(100,seq(1000,10000,2000)))+
    scale_x_reverse(expand=c(0,0), limits=c(5000,-100))+
    geom_vline(xintercept=2000,linetype="dashed")+
    scale_color_viridis_d()+
    ylab("number of localisations")+
    xlab("VARX & VARY filtering values")+
    theme(panel.background = element_rect(fill = "white"),
          legend.position="none",
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())

#smoothing
all_win_moving$NBS <- as.factor(all_win_moving$NBS)
move3 <- ggplot(all_win_moving, aes(x=win, y = mean_error, group = as.factor(NBS), col=NBS))+
    geom_line(size=1)+
    scale_color_viridis_d()+
    scale_y_continuous(expand=c(0,0), limits=c(0,25), breaks=seq(0,25,5))+
    scale_x_continuous(expand=c(0,0), limits=c(0,60), breaks=seq(3,60,6))+
    ylab("mean distance to GPS (m)")+
    xlab("median smooth")+
    labs(col="Num. \nReceivers")+
    theme(panel.background = element_rect(fill = "white"),
          panel.border= element_rect(colour="black", fill=NA),
          legend.spacing.y = unit(-0.15, "cm"),
          legend.key=element_blank())

stat_plots <- plot_grid(stat1, stat2,stat3, ncol=3, rel_widths = c(0.8,0.8,1),labels="auto")
move_plots <- plot_grid(move1, move2,move3, ncol=3, rel_widths = c(0.8,0.8,1),labels=c("d","e","f"))

# now add the title
title_stat <- ggdraw() + 
    draw_label(
        "Stationary test data",
        fontface = 'bold',
        x = 0.5, hjust=0.65
    ) 

title_move <- ggdraw() + 
    draw_label(
        "Moving test data",
        fontface = 'bold',
        x = 0.5, hjust=0.65
    )

pdf(file = "figs/Validation_Supplementary1_FilterSmoothing.pdf",
    width = 9, height = 10)
p <- plot_grid(title_stat, stat_plots,title_move,move_plots, ncol=1, nrow=4, rel_heights = c(0.1,1,0.1,1))
p
dev.off()

png(file = "figs/Validation_Supplementary1_FilterSmoothing.png",unit="in", res=600,
    width = 9, height = 10)
p <- plot_grid(title_stat, stat_plots,title_move,move_plots, ncol=1, nrow=4, rel_heights = c(0.1,1,0.1,1))
p
dev.off()

