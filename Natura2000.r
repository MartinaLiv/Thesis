#how many plots are inside Natura2000 areas

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(dplyr)
library(raster)
library(sf)
library(sp)
library(phyloregion)

d<- read.csv("D:/progetto tesi/d.csv")
Nat.sp <- shapefile("C:/Users/marti/Downloads/Natura2000_end2021_Shapefile/Natura2000_end2021_epsg3035.shp")
crs(Nat.sp)<- "+proj=longlat +ellps=WGS84"

#grids 0.25
world <- ne_countries(scale = "medium", returnclass = "sf")
r<-raster::extent(c(xmin=-30, xmax=54, ymin=25, ymax=74))
r.sp <- as(r, "SpatialPolygons")
m <- sp::SpatialPolygonsDataFrame(r.sp, data.frame(sp = "x"))
m1 <- fishnet(mask = m, res = 0.25)
crs(m1) <- crs(world)
m1$id=1:nrow(m1)
m1=m1[,-1] #tolgo oggetto grids da m1

coordinates(d)= ~d.Longitude+d.Latitude #trasforma oggetto in spatial dataframe df 
crs(d)=crs(m1)
ov3 <- over(d[,2], m1)

y025 <- cbind(as.data.frame(d[, c(1,2,3,4,5,6)]), ov3)
y025<- y025[complete.cases(y025),]

y025 <- y025 %>%
  select(d.Longitude, d.Latitude, id)%>%
  unique()
ysp <- SpatialPointsDataFrame(coords = y025[, 1:2], proj4string = CRS("+proj=longlat +ellps=WGS84"), data = y025)

ovNat <- over(ysp, Nat.sp)
ovNat$id <- y025[, 3]
ovNat$In_Out <- ifelse(is.na(ovNat$SITECODE), 0, 1)

df <- merge(ovNat, y025, by= "id", all= F)

#raster to see how plots in Natura2000 areas are distributed
Nat.r <-  raster(ext = extent(c(-30, 54, 25, 74)), crs = crs(Nat.sp), res = 0.25)
cells <- cellFromXY(Nat.r, df[, 9:10])
Nat.r[cells]<- df[, 8]

#count how many plots are inside areas
df2 <- df %>%
  group_by(id) %>%
  summarise(In_Out, count= n()) %>%
  unique()

df2 <- df2 %>%
  filter(In_Out %in% 1) %>%
  ungroup()

df3 <- left_join(df, df2, by= "id")

#rasterize
nNat <- raster(ext = extent(c(-30, 54, 25, 74)), crs = crs(Nat.sp), res = 0.25)
cells3 <- cellFromXY(nNat, df3[, 9:10])
nNat[cells3]<- df3[, 12]

#standardize
nNatst <- nNat$layer/maxValue(nNat)
