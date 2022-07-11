#population density
library(raster)
library(sf)
library(dplyr)
library(rnaturalearth)

pop_density <- raster("POP_DENS_2020_1km_Aggregated.tif")
pop_density <- crop(pop_density, extent(c(xmin=-30, xmax=54, ymin=25, ymax=74))) #crop on Europe

#create a mask 
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe") # sf Europe
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation') #remove Russian Federation
  

#change the resolution till 1 degree
r1= aggregate(pop_density, fact= 12, fun=mean, expand=TRUE)
r2= aggregate(r1, fact= 10, fun= mean, expand=T)

#we then decided to change the resolution to 0.25°
pop025 <- raster::disaggregate(r2, fact= 4, fun= mean, expand= T)
pop025 <- raster::mask(pop025, Europe) #mask raster

#kernel density estimation of roads
library(SpatialEco)  #compute spatial kernel density 
library(shp2graph) #create nodes of roads

# worked on the shapefile of European roads on qGis in order to remove not usefull areas for the analysis
r <- shapefile("D:/progetto tesi/tuo/gigi.shp") #shapefile modified on qGis

n <- readshpnw(r) #creates a list
n_coord <- Nodes.coordinates(n[[2]]) #creates nodes form a SpatialLineDataFrame
n.sp <- SpatialPoints(n_coord, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")) #transformed nodes in Spatial Points

#base layer to compute spatial kd
europe_grid <- raster(ext=extent(c(xmin=-30, xmax=54, ymin=25, ymax=74)),crs ="+proj=longlat +datum=WGS84 +no_defs" , res = 0.25) 
europe_grid[]<- rep(1,ncell(europe_grid)) #assign values to the base raster

kd.s <- sp.kde(n.sp, standardize = T, newdata = europe_grid) #raster with standardized kernel density estimation of european roads

writeRaster(kd.s, filename = "D:/progetto tesi/KernelDensity.tiff", format= "GTiff") #saved

#values in UK too high and influence the analysis--> removed

# shapefile transformed in sf
rsf <- st_as_sf(r) 

#get an sf object of UK
UK <- ne_countries(country = "united kingdom", returnclass = "sf") #sf UK
UK <- st_set_crs(UK, st_crs(rsf)) #set the same crs for roads and uk box
UK<- st_cast(UK, "MULTILINESTRING") #transformed in multilinestring as the sf of european roads

#get a boundingbox of UK
bboxUK <- st_bbox(UK) 
pUK <- st_as_sfc(bboxUK) # transformed in sf object

#remove UK from shapefile
noUK <- st_difference(rsf, pUK) #shapefile senza UK
noUK<- as(noUK, "Spatial") # back to sp

#kd without UK
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe") #sf of Europe

m <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!= 'Russian Federation') %>%
  filter(name_long!= "England") %>%
  filter(name_long!="Wales")%>%
  filter(name_long!= "Scotland")  #create a new mask without United Kingdom

m <- raster::rasterize(m , europe_grid)

n1 <- readshpnw(noUK) #create list with nodes
n_coord1 <- Nodes.coordinates(n1[[2]]) #get nodes as matrix
nsp1 <- SpatialPoints(n_coord1, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")) # nodes in spatial points

noUK.kd <- sp.kde(nsp1, standardize = T, newdata = m) #compute kde roads without UK
writeRaster(noUK.kd, filename = "D:/progetto tesi/KernelDensity2.tif", format= "GTiff") #saved

#BAM
library(dismo) #background points
library(rgdal)

#create a mask for our data
Europe <- ne_countries(scale="medium", type="map_units", returnclass="sf", continent="Europe") #Europe sf
Europe <- Europe %>%
  dplyr::select(geometry,name_long)  %>%    
  filter(name_long!='Russian Federation') # remove Russian Federation
europe_grid <- raster(ext=extent(c(xmin=-30, xmax=54, ymin=25, ymax=74)), crs = "+proj=longlat +ellps=WGS84", res = 0.25) #base raster
mask <- rasterize(Europe, europe_grid) #mask for background points 
crs(mask)<- "+proj=longlat +ellps=WGS84" #set crs WGS84

#read our presence data and select unique coordinates
d2 <- read.csv("D:/progetto tesi/d2.csv") # read data.frame 
p2 <- d2 %>%
  select(d.Longitude, d.Latitude) %>%
  unique() #select coordinates which are unique
  
#5000 background points
set.seed(999)
b<- randomPoints(mask= mask, 5000 , p= p2, lonlatCorrection = T)  #random points on European extent excluding presence points
b <- SpatialPoints(b, proj4string = CRS("+proj=longlat +ellps=WGS84")) #as Spatial

#population density data
popd<- raster("D:/progetto tesi/pd_025.tif") #upload raster population density 
popd <- popd$pd_025/maxValue(popd) #standardized

pdp <- extract(popd, p2, df= T) #values for presences
pdb <- extract(popd, b, df= T) #values for bg points

#kd roads
kd <- raster("D:/progetto tesi/kd25.tif") #upload raster KDE european roads

kdp <- extract(kd, p2, df= T) #values for presences
kdb <- extract(kd, b, df= T) #values for bg points

#create two dfs, one for presences one for bg points
df1 <- merge(pdp, kdp, by= "ID") #presences
df2 <-  merge(pdb, kdb, by= "ID") #background

#remove NA values in df1 (England?)
df1<- df1[complete.cases(df1), ]

#sample 5000 points from presences
set.seed(999)

v <- sample(1:16749,5000, replace = FALSE) #da 1 a n righe che hai in df1
sub.df1 <- df1[v,] #subseting to get random sample of our presence data


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


# other variables: elevation and temporal uncertainty -- > we need to extract these two new variables from the original dataframe

require(bigreadr)
require(tidyverse)
require(CoordinateCleaner)
require(gridExtra)
require(countrycode)
library(data.table)
library(sf)
library(rnaturalearth)
library(raster)
library(RStoolbox)
library(rasterVis)
library(viridis)
library(RColorBrewer)
library(phyloregion)

DT2.oa$Species <- gsub(DT2.oa$Species,pattern = ' ',replacement = '_')
species_level <- DT2.oa[grepl("_", DT2.oa$Species),]
NOspecies_level <- DT2.oa[!grepl("_", DT2.oa$Species),] #tutte le osservazioni che non possono essere considerate
View(table(NOspecies_level$Species))

#Unione delle 2 tabelle necessarie per le analisi e selezione delle variabili
d <- merge(species_level, header.oa, by='PlotObservationID')
d2<- data.frame(d$PlotObservationID, d$Species, d$Original_abundance, d$Abundance_scale, d$Continent, d$Country, d$Date_of_recording, d$Latitude, d$Longitude, d$Elevation, d$Relative_cover)

d2 <- d2[d2$d.Continent=='Europe',] #only Europe
d2 <- d2[d2$d.Abundance_scale != 'x_SC',] #set abundance 

d2 <- d2[complete.cases(d2),] #remove NA

#I want to see the distribution of dates of recording in the new dataframe
breaks <- pretty(range(d2$d.Date_of_recording),
                 n= nclass.Sturges(d2$d.Date_of_recording),
                 min.n = 1) #breaks for histogram

ggplot(d2, aes(x = d.Date_of_recording)) + 
  geom_histogram(color = 1, fill = "white",
                 breaks = breaks) #histogram with dates

write.csv(d2, "D:/progetto tesi/d2.csv")#save the new dataframe

# temporal uncertainty
d <- read.csv("D:/progetto tesi/d2.csv") #new dataframe with elevation and date of recording

d$Year <- substr(d$d.Date_of_recording, 1,4) #get a string?
d$Year <- as.numeric(d$Year) #transformed in numeric
d$YearDif <- 2014-d$Year #compute the difference in years
yearDifneg <- data.frame(YearDif= - d$YearDif) #negative
yearDifneg <- yearDifneg %>% filter(!YearDif==-0) 
YearDif <- data.frame(YearDif=d$YearDif) # transformed in dataframe 
year <- data.frame(rbind(YearDif, yearDifneg)) # bind 
sd <- sd(year$YearDif) #standard deviation 


tI <- function(x, sd) (1/(sd*sqrt(2*pi)))*exp(-0.5*(x^2)/(sd^2)) #function to get the p of that record to be uncertain (?)
y <-tI(YearDif$YearDif, sd)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

s <- range01(y)
temp <- data.frame(cbind(p=s, year=YearDif$YearDif)) #dataframe with temporal uncertainty

# new variable: how many plots are inside Natura2000 areas per grid of 0.25°

Nat<-  read_sf("D:/progetto tesi/Natura2000_end2021_epsg3035.shp")
Nat<- st_transform(Nat, crs = st_crs(kdr)) # set the correct crs
Nat <- as(Nat, "Spatial") #as spatial 

#create grids 0.25
world <- ne_countries(scale = "medium", returnclass = "sf") #sf of the world
r<-raster::extent(c(xmin=-30, xmax=54, ymin=25, ymax=74)) #raster with extent of Europe
r.sp <- as(r, "SpatialPolygons") #as Polygon
m <- sp::SpatialPolygonsDataFrame(r.sp, data.frame(sp = "x")) # as SpatialPolygonDataFrame  
m1 <- fishnet(mask = m, res = 0.25) #grid res 0.25
crs(m1) <- crs(world) #set grid crs
m1$id=1:nrow(m1) # set grid ids
m1=m1[,-1] #rempove grid column

coordinates(d)= ~d.Longitude+d.Latitude #trasforma oggetto in spatial dataframe df 
crs(d)=crs(m1)
ov3 <- over(d[,3], m1) #intersect with species

d2 <- cbind(as.data.frame(d, ov3)) #bind to have gridded plots
y025 <- d2 %>%
  select(d.Longitude, d.Latitude, id) #select variable of interest

ysp <- SpatialPointsDataFrame(coords = y025[, 1:2], proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"), data = y025) #as spatial
ovNat <- over(ysp, Nat) #plots intersection with Natura2000
ovNat$id <- y025[, 3] #retain grid ids
ovNat$In_Out <- ifelse(is.na(ovNat$SITECODE), 0, 1) #set a condition: plots inside Natura2000 areas= 1, otherwise=0

df <- cbind(ovNat, y025[, 1:2]) #bind coordinates

df2 <- df %>%
  group_by(id) %>%
  summarise(In_Out, count= n()) #count how many plots inside Natura2000 areas are present per grid
  
df2 <- df2 %>%
  filter(In_Out %in% 1) %>%
  ungroup() #retain only those present in Natura2000 areas (with values of 1)

df3 <- df%>% dplyr::select(id, d.Longitude, d.Latitude) #coordinates

df4 <- merge(df3, df2, by= "id")# final dataframe 

# PCA
#we change dataframe so we need to extract the values of population density and kernel density for the new one

p <- d %>%
  select(d.Longitude, d.Latitude) #select coordinates of new presence points

#kernel density roads
kdr <- raster("D:/progetto tesi/kd25.tif")
kd_roads <- extract(kdr, p, df=T)

#pop density
popd <- raster("D:/progetto tesi/pd_025.tif")
popd <- popd$pd_025/maxValue(popd)

pd <- extract(popd, p, df=T)

#create dataframe for pca
df4 <- read.csv("D:/progetto tesi/df4.csv")

mydf<- data.frame(inc_temp= temp$p, kd_road= kd_roads$kd25, popd= pd$pd_025, elev= d2$d.Elevation, Nat2000= df4$count) #all variable computed till now

write.csv(mydf, file = "D:/progetto tesi/ df_pca.csv")

mydf<- mydf[complete.cases(mydf),] #NA values in kd and pop d because we removed England


library(ade4)
pca=dudi.pca(mydf,center=T,scale=T, scannf = FALSE, nf = 2) #pca
perc.eig=100*pca$eig/sum(pca$eig) #compute eigenvalues

plot(pca$li$Axis1,pca$li$Axis2,xlab="PC1",ylab="PC2",type="n")
text(pca$li[,1],pca$li[,2],labels= abbreviate(row.names(mydf)),cex=0.7)
 
#perc.eig 
# 25.94305 23.61674 20.20191 16.99067 13.24762

s.corcircle(pca$c1,xax=1,yax=2) #Loadings PCA






