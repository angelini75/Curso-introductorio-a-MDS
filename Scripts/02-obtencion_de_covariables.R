rm(list = ls())

library(raster)
library(sp)
library(mapview)
library(sf)
library(dplyr)

srtm <- getData("SRTM", lat = 4, lon = -79, path = "Datos/")
# tmin <- getData("worldclim", lat = 4, lon = -79, res = 0.5, 
#                 var = 'tmin')
# tmax <- getData("worldclim", lat = 4, lon = -79, res = 0.5, 
#                 var = 'tmax', path = "Datos/")
# prec <- getData("worldclim", lat = 4, lon = -79, res = 0.5, 
#                 var = 'prec', path = "Datos/")
bio <- getData("worldclim", lat = 4, lon = -79, res = 0.5, 
               var = 'bio', path = "Datos/")

# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

# descargamos los departamentos
distritos <- getData("GADM", level = 1, country = "COL", path = "Datos/")
# los convertimos en sfg
d <- st_as_sf(distritos)
# visualizamos
mapview(d, zcol = "NAME_1")
# Seleccionamos Valle el Cauca
d <- filter(d, NAME_1 == "Valle del Cauca")
d <- d %>% st_cast("POLYGON") # de MULTIPOLYGON a POLYGON# 
d <- d[rownames(d) == "1.26",]

bio <- crop(bio, d)
dem <- crop(srtm, d)
# mapview(x = bio,
#         na.color = "transparent",
#         col.regions = mapviewPalette("mapviewTopoColors"),
#         query.type = "mousemove",
#         query.digits = 1) 
mapview(dem) + mapview(d)


# https://spatialreference.org/ref/epsg/3115/
crs(dem) <- crs("+init=epsg:4326")
MAGNA_SIRGAS <- crs("+proj=tmerc +lat_0=4.596200416666666 +lon_0=-77.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" )
dem <- projectRaster(dem, crs=MAGNA_SIRGAS)
crs(dem)

# writeRaster(x = dem, filename = "Datos/DEM/dem", format = "SAGA",
#             overwrite=TRUE)

############################################################
# Trabajar desde SAGA para crear los atributos del terreno #
############################################################

## cargamos los atributos el terreno
# listamos los archivos
files <- list.files(path = "Datos/DEM/", pattern = ".sdat")

dem <- stack(paste0("Datos/DEM/",files))
crs(dem) <- MAGNA_SIRGAS
dem <- projectRaster(dem, crs="+init=epsg:4326")

mapview(x = dem,
        na.color = "transparent",
        col.regions = mapviewPalette("mapviewTopoColors"),
        query.type = "mousemove",
        query.digits = 1) 

as.data.frame(names(dem))
dem <- dem[[c(-1,-4, -8)]]
names(bio)

res(dem)
res(bio)

dem <- raster::resample(x = dem, y = bio, method = "bilinear")
covs <- stack(dem, bio)

# cargamos los puntos, le damos las coordenadas y CRS
data <- read.csv("resultados/Splines_ph.csv")
coordinates(data) <- ~X+Y
proj4string(data) <- CRS("+init=epsg:4326")

data@data <- cbind(data@data, extract(x = covs, y = data))

rm(list = ls()[-2:-4])
save.image("resultados/puntos+covs.RData")

# Si llegaste hasta aquí, puedes llevarte este regalo
# https://spacetimewithr.org/book
