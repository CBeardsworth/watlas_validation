# Bird example

library(watlastools)
library(atlastools)
library(dplyr)
library(ggplot2)
library(ggspatial)

setwd("C:/Users/cbeardsworth/OneDrive - NIOZ")

birds <- read.csv("2_Reference Data/Bird Metadata/tags_watlas_all.csv") %>%
    filter(captivity == "no", species == "islandica", season == "2020")%>%
    mutate(ATLAS = as.numeric(ifelse(nchar(tag)==3, paste0(31001000,tag), paste0(3100100,tag))))

tides <- read.csv("2_Reference Data/Water/Wide/allYears-tidalPattern-WestTerschelling_Wide-UTC.csv", stringsAsFactors = F)%>%
    mutate(high_start_time = as.POSIXct(high_start_time),
           high_end_time = as.POSIXct(high_end_time),
           low_time = as.POSIXct(low_time))%>%
    filter(high_start_time > as.POSIXct("2020-07-01 00:00:01") & high_start_time < as.POSIXct("2020-11-01 00:00:01"))
for(j in 1:nrow(tides)){ #one summary row per bird per tide
    all_p_dat <- NULL
    all_lines <- NULL
for(i in 1:nrow(birds)){
    
    tag <- birds$ATLAS[i]
    id <- birds$tag[i]
    release <- as.POSIXct(birds[birds$tag ==id, "release_ts"], format = "%d/%m/%Y %H:%M:%S")
    
        
        # start <- tides$low_time[j] - (3600*3 + 1800) # activity of birds is concentrated 3.5hr before low tide and 2 hours after low tide
        start <- tides$high_start_time[j]
        start_CET <- start
        attr(start_CET, "tzone") <- "CET"
        
        if(start > release){
            #end <- tides$low_time[j] + (3600*2)
            end <- tides$high_end_time[j]
            end_CET <- end
            attr(end_CET, "tzone") <- "CET"
            
            prelim_data <- wat_get_data(tag = tag,
                                        tracking_time_start = as.character(start_CET),
                                        tracking_time_end = as.character(end_CET),
                                        host= "145.1.124.30", 
                                        #host="abtdb1.nioz.nl",
                                        database = "atlas2020",
                                        username = "atlaskama",
                                        password = "watlasrulez:)")
            
            if(nrow(prelim_data) > 1000){
                prelim_data$speed_in <- atl_get_speed(data=prelim_data, time="TIME", x="X",y="Y", type = "in")
                
                prelim_data <-prelim_data %>%
                    mutate(TIME = TIME/1000, 
                           id=TAG,
                           timestamp = as.POSIXct(TIME, origin = "1970-01-01", tz="UTC"), 
                           time=timestamp,
                           x = X, 
                           y= Y)
                
                lines <- prelim_data %>%
                    #group_by(patch_id) %>%
                    st_as_sf(coords=c("x","y")) %>%
                    summarise(do_union=F) %>%
                    st_cast("LINESTRING")%>%
                    st_set_crs(32631)
                
                p_dat <- prelim_data%>%
                    st_as_sf(coords=c("x","y")) %>%
                    st_set_crs(32631)
                
                clean_data <- atl_filter_covariates(data=prelim_data,
                                                    filters = c("VARX <2000", 
                                                                "speed_in < 150",
                                                                "VARY <2000"))  
                clean_data <- atl_median_smooth(clean_data, x="X", y = "Y", time = "TIME", moving_window = 3)
                
                lines_clean <- clean_data %>%
                    #group_by(patch_id) %>%
                    st_as_sf(coords=c("x","y")) %>%
                    summarise(do_union=F) %>%
                    st_cast("LINESTRING")%>%
                    st_set_crs(32631)%>%
                    mutate(id=factor(id))
                
                all_lines <- rbind(all_lines, lines_clean)
                
                p_dat_clean <- clean_data%>%
                    st_as_sf(coords=c("x","y")) %>%
                    st_set_crs(32631)%>%
                    mutate(id=factor(id))
                
                all_p_dat <- rbind(all_p_dat,p_dat_clean)
                
            }}}
                if(is.null(all_p_dat) ==F) {               
                bird_plot1 <- ggplot()+
                    #ggtitle(paste("Raw data // Bird:", id, sep=" "))+    
                    geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
                    geom_sf(data=receivers, shape = 24, fill="red", size = 5)+
                    geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
                    #geom_sf(data=patch_summary_speed, aes(col = factor(patch)))+
                    geom_sf(data=all_lines, col="grey", size = 0.5)+
                    geom_sf(data=all_p_dat, size = 1, aes(col=id))+
                    xlab("Longitude") +
                    ylab("Latitude") +
                    #coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
                    #coord_sf(xlim= c(646564,654867), ylim=c(5900756,5906406))+ #griend only
                    coord_sf(xlim = c(min(all_p_dat$X)-500, max(all_p_dat$X)+500), ylim = c(min(all_p_dat$Y)-500, max(all_p_dat$Y)+500)) +
                    scale_color_brewer(palette = "Dark2")+
                    
                    annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                                           style=north_arrow_orienteering(text_size=8))+
                    annotation_scale(location="br", width_hint=0.2)+
                    theme(panel.background = element_rect(fill = "white"),
                          panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
                          panel.border= element_rect(colour="black", fill="NA"),
                          legend.key=element_blank(), 
                          legend.position = "none") +
                    scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                                      name = NULL) 
                bird_plot1
                # 
                # bird_plot2 <- ggplot()+
                #     ggtitle(paste("Filtered and smoothed // Bird:", id, sep=" "))+    
                #     geom_sf(data = bath, aes(fill="Mudflat"),col="grey50", lwd=0.1) + 
                #     geom_sf(data=receivers, shape = 24, fill="red", size = 5)+
                #     geom_sf(data = coast,aes(fill="Land"), col="grey40", lwd=0.1) +
                #     #geom_sf(data=patch_summary_speed, aes(col = factor(patch)))+
                #     geom_sf(data=lines_clean, col="grey", size = 0.5)+
                #     geom_sf(data=p_dat_clean, size = 1, col="purple")+
                #     xlab("Longitude") +
                #     ylab("Latitude") +
                #     #coord_sf(xlim= c(635264,657167), ylim=c(5894956,5915506))+ #griend region
                #     #coord_sf(xlim= c(646564,654867), ylim=c(5900756,5906406))+ #griend only
                #     coord_sf(xlim = c(min(p_dat$X)-500, max(p_dat$X)+500), ylim = c(min(p_dat$Y)-500, max(p_dat$Y)+500)) +
                #     #scale_color_brewer(palette = "Dark2")+
                #     
                #     annotation_north_arrow(location="br",height= unit(0.8,"cm"), width= unit(0.6, "cm"), pad_y=unit(0.8, "cm"),
                #                            style=north_arrow_orienteering(text_size=8))+
                #     annotation_scale(location="br", width_hint=0.2)+
                #     theme(panel.background = element_rect(fill = "white"),
                #           panel.grid.major = element_line(linetype="dashed", colour = "grey90"), 
                #           panel.border= element_rect(colour="black", fill="NA"),
                #           legend.key=element_blank(), 
                #           legend.position = "none") +
                #     scale_fill_manual(values = c("Land" = "grey65", "Mudflat" = "papayawhip"),
                #                       name = NULL) 
                # plot_grid(bird_plot1, bird_plot2, ncol = 2)
                ggsave(paste0("4_Projects/2_WATLAS Validation/Figures/Bird examples/tides/", tides[j,"tideID"],"_",".png"), width=21, height= 12, unit = "in")
                
            }}
