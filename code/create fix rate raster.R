library(sf)
library(sp)
library(stars)
library(raster)

griend_respatches <- read_sf("data/griend_respatches2020.gpkg")%>%
    filter(duration > 120) %>%#only use residence patches of over 2 min. 
    mutate(fix_rate = fix_rate * 100)

sp_griend <- as(griend_respatches[,c("fix_rate", "geom")], "Spatial") #change to spatial polygons
template <- raster(xmn = 644800, xmx = 656600, ymn = 5900200, ymx = 5908000, res = 50,crs="+init=epsg:32631") #100m resolution
raster <- rasterize(sp_griend, template, field = "fix_rate", fun = "mean")
stars_griend <- st_as_stars(raster) #change to stars
names(stars_griend)<- "Fix rate"
write_stars(stars_griend, "data/griend_respatches2020_50m.tif")


fix_rates <- griend_respatches 
st_geometry(fix_rates) <- NULL
write.csv(fix_rates[,c("patch", "id","tideID", "time_start", "time_end", "nfixes","duration","dist_in_patch","fix_rate","start_location")], "data/griend_respatches2020_fixrates.csv", row.names=F)
