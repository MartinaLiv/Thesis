#kernel density estimation of European Roads
library(raster)
library(sf)
library(rgeos)
library(shp2graph) #create nodes of roads
library(spatialEco) #compute spatial kd

# worked on the shapefile of European roads on qGis in order to remove not usefull areas for the analysis

r <- shapefile("D:/progetto tesi/tuo/gigi.shp") #shapefile modified on qGis

n <- readshpnw(r)
n_coord <- Nodes.coordinates(n[[2]]) #creates nodes form a SpatialLineDataFrame
n.sp <- SpatialPoints(n_coord, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs")) #transformed nodes in Spatial Points

#base layer to compute spatial kd
europe_grid <- raster(ext=extent(c(xmin=-30, xmax=54, ymin=25, ymax=74)),crs ="+proj=longlat +datum=WGS84 +no_defs" , res = 1) 
europe_grid[]<- rep(1,ncell(europe_grid))

kd.s <- sp.kde(n.sp, standardize = T, newdata = europe_grid) #raster with standardized kernel density estimation of european roads

writeRaster(kd.s, filename = "D:/progetto tesi/KernelDensity.tiff", format= "GTiff")

#values in UK too high and influence the analysis--> removed

# shapefile transformed in sf
rsf <- st_as_sf(r) 

#get an sf object of UK
UK <- ne_countries(country = "united kingdom", returnclass = "sf") #sf UK
UK <- st_set_crs(UK, st_crs(rsf))
UK<- st_cast(UK, "MULTILINESTRING")

#get a boundingbox of UK
bboxUK <- st_bbox(UK)
pUK <- st_as_sfc(bboxUK)

#remove UK from shapefile
noUK <- st_difference(rsf, pUK) #shapefile senza UK
noUK<- as(noUK, "Spatial") # back to sp

#kd without UK
n1 <- readshpnw(noUK)
n_coord1 <- Nodes.coordinates(n1[[2]])
nsp1 <- SpatialPoints(n_coord1, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

noUK.kd <- sp.kde(nsp1, standardize = T, newdata = europe_grid)

writeRaster(noUK.kd, filename = "D:/progetto tesi/KernelDensity2.tif", format= "GTiff")
