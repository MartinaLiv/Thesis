#how many plots are inside Natura2000 areas

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(dplyr)
library(raster)
library(sf)
library(sp)

y1<- read.csv("D:/progetto tesi/y1.csv")
y1 <- y1 %>%
  select(d.Longitude, d.Latitude, id) %>%
  unique()

Nat.sp <- shapefile("C:/Users/marti/Downloads/Natura2000_end2021_Shapefile/Natura2000_end2021_epsg3035.shp")
crs(Nat.sp)<- "+proj=longlat +ellps=WGS84"

y1sp <- SpatialPointsDataFrame(coords = y1[, 1:2], proj4string = CRS("+proj=longlat +ellps=WGS84"), data = y1)

ov2 <- over(y1sp, Nat.sp)
ov2$id <- y1[,3]
ov2$In_Out<- ifelse(is.na(ov2$SITECODE), 0, 1) #new variable with values 1 or 0 --> if the plot is inside a Nat2000 area is 1

mydf <- merge(ov2, y1, by= "id", all= F)

#raster layer representing plots inside Nat2000 areas
nplotNat <- raster(ext = extent(c(-30, 54, 25, 74)), crs = crs(Nat.sp), res = 0.25)
cells <- cellFromXY(nplotNat, mydf[, 9:10])
nplotNat[cells]<- mydf[, 8]

#I want to know how many plots are inside a Nat2000 areas per grid
mydf2<- mydf %>%
  group_by(id) %>%
  summarise(In_Out, count=n()) %>%
  unique() #count how many 0 or 1 are there for each grid

mydf2 <- mydf2 %>%
  filter(In_Out %in% 1) %>%
  ungroup() # only 1

mydf3 <- left_join(mydf, mydf2, by= "id") #add the new "variable" to my dataframe

#raster
nplotNat2 <-  raster(ext = extent(c(-30, 54, 25, 74)), crs = crs(Nat.sp), res = 0.25)
cells2 <- cellFromXY(nplotNat2, mydf3[, 9:10])
nplotNat2[cells2]<- mydf3[, 12]

#standardize raster's values
nplotNat2st <- nplotNat2$layer/maxValue(nplotNat2)

#write
writeRaster(nplotNat2st, filename = "D:/progetto tesi/nPlotNatura2000.tif", format= "GTiff")

