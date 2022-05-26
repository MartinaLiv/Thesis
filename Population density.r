#Population density data
library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)

pop_density <- raster("POP_DENS_2020_1km_Aggregated.tif")
pop_density <- crop(pop_density, extent(c(xmin=-30, xmax=54, ymin=25, ymax=74))) #crop on Europe

#create a mask 
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation')


#change the resolution till 1 degree
r1= aggregate(pop_density, fact= 12, fun=mean, expand=TRUE)
r2= aggregate(r1, fact= 10, fun= mean, expand=T)

r2masked <- raster::mask(r2, Europe)

#standardize the values of pixels dividing every value for the max value
r2m.st <- r2masked$POP_DENS_2020_1km_Aggregated / maxValue(r2masked)

#upload species data
d <- read.csv(file = "C:/Users/marti/OneDrive/Desktop/progetto tesi/d.csv", header = T)

lonlat <- d %>%
  dplyr::select(d.Longitude, d.Latitude) #subset coordinates

coordinates(lonlat) <- ~d.Longitude+d.Latitude
crs(lonlat) <- "+proj=longlat +ellps=WGS84"
 
writeRaster(r2.mst, filename = "D:/progetto tesi/pop_ds.tif", format= "GTiff", overwrite= T)
