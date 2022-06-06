#BAM with pouplation density and kernel density estimation of European roads at resolution of 0.25 degrees 

library(ggplot2)
library(dismo)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(dplyr)
library(raster)

#create a mask for our data
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe")
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation')
europe_grid <- europe_grid <- raster(ext=extent(c(xmin=-30, xmax=54, ymin=25, ymax=74)), crs = "+proj=longlat +ellps=WGS84", res = 0.25)
mask <- rasterize(Europe, europe_grid)
crs(mask)<- "+proj=longlat +ellps=WGS84"

#read our presence data and select unique coordinates
d <- read.csv("D:/progetto tesi/d.csv")
p <- d %>%
  select(d.Longitude, d.Latitude) %>%
  unique()
  
#5000 background points
set.seed(1234)
b<- randomPoints(mask= mask, 5000 , p= p, lonlatCorrection = T) 
b <- SpatialPoints(b, proj4string = CRS("+proj=longlat +ellps=WGS84")) #as Spatial

#population density data
popd<- raster("D:/progetto tesi/pd_025.tif")
popd <- popd$pd_025/maxValue(popd) #standardized

pdp <- extract(popd, p, df= T) #values for presences
pdb <- extract(popd, b, df= T) #values for bg points

#kd roads
kd <- raster("D:/progetto tesi/kd25.tif")

kdp <- extract(kd, p, df= T) #values for presences
kdb <- extract(kd, b, df= T) #values for bg points

#create two dfs, one for presences one for bg points
df1 <- merge(pdp, kdp, by= "ID") #presences
df2 <-  merge(pdb, kdb, by= "ID") #background

#remove NA values in df1
df1 <- na.omit(df1)

#sample 5000 points from presences
set.seed(1234)
random <- runif(nrow(df1))
df1r<- df1[order(random),]
sub.df1 <- df1r[1:5000,]

#set the column names
colnames(df2) <- c("ID", "Pop_density", "Kd_roads")
colnames(sub.df1) <- colnames(df2)

#theme for ggplot
myTheme<-theme(panel.background= element_rect(color="black", fill="white"),
               panel.grid.major = element_blank(),
               plot.title = element_text(size=15,face = 'bold',hjust = 0.6),
               legend.title=element_text(size=15),
               legend.text = element_text(size=7),
               axis.title.x = element_text(size = 15),
               axis.text.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               axis.text.y = element_text(size = 15),
               plot.title.position ='plot')
               
#ggplot
ggplot()+
  geom_point(data= df2, aes(Pop_density, Kd_roads),
             size=2, shape=16, col= "black", position = "jitter" )+ #bg points
  geom_point(data= sub.df1, aes(Pop_density, Kd_roads),
              size=2, shape= 16, col= "red", position = "jitter")+ #presence points
  myTheme

