#Population density data

pop_density <- raster("POP_DENS_2020_1km_Aggregated.tif")
pop_density <- crop(pop_density, extent(c(xmin=-30, xmax=54, ymin=25, ymax=74))) #crop on Europe

#change the resolution till 1 degree
r1= aggregate(pop_density, fact= 12, fun=mean, expand=TRUE)
r2= aggregate(r1, fact= 10, fun= mean, expand=T)

#standardize the values of pixels dividing every value for the max value
r2d <- r2$POP_DENS_2020_1km_Aggregated / maxValue(r2)

#upload species data
d <- read.csv(file = "C:/Users/marti/OneDrive/Desktop/progetto tesi/d.csv", header = T)

lonlat <- d %>%
  dplyr::select(d.Longitude, d.Latitude) #subset coordinates

coordinates(lonlat) <- ~d.Longitude+d.Latitude

PopDe <- extract(r2d, lonlat, df= TRUE) #extract Population Density data for the coordinates of occurrences


