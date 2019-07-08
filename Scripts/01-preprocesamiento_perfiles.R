rm(list = ls())

library(sf) # data.frames espaciales
library(sp) # predecesor de sf
library(dplyr) # manejo de datos
library(mapview) # visualizador interactivo de datos espaciales
library(raster) # para descargar departamentos

# el símbolo %>% significa *a este objeto "le aplicamos" esto*

# datos de SISLAC http://54.229.242.119/sislac/es
# requiere crear una cuenta y loguearse para descargar perfiles
perfiles <- read.csv("Datos/perfiles_01-jul-19-10.csv")
names(perfiles) # nombres de columnas
coordinates(perfiles) <- ~ longitude + latitude # podría ser ~X+Y
p <- st_as_sf(perfiles) %>% # convertimos a objeto sf
  group_by(profile_identifier) %>% # agrupamos las filas por perfil
  mutate(n = n()) # calculamos cuantas filas (horizontes o capas) tiene cada perfil 
                  # y agregamos el valor en una columna llamada n


# agregamos el sistema de coordenadas a alos puntos (WGS84)
# https://cengel.github.io/rspatial/2_spDataTypes.nb.html#the-sf-package
# st_crs(p) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
st_crs(p) <- 4326
st_crs(p) # así queda configurado el sistema de coordenadas

# ahora visualizamos los perfiles
mapview(p %>%  # pero sólo un punto por perfil
          group_by(profile_identifier) %>% # agrupamos por perfile
          filter(top == 0), zcol = "n") # filtramos las filas que tienen top = 0

# descargamos los departamentos
distritos <- getData("GADM", level = 1, country = "COL", path = "Datos/")
# los convertimos en sfg
d <- st_as_sf(distritos)
# visualizamos
mapview(d, zcol = "NAME_1")
# lista de departamentos
unique(d$NAME_1)

# Seleccionamos Valle el Cauca
d <- filter(d, NAME_1 == "Valle del Cauca")
mapview(d)
# El departamento incluye varias islas pero nosotros
# sólo queremos quedarnos con el poligono continental
d # antes
d <- d %>% st_cast("POLYGON") # de MULTIPOLYGON a POLYGON
d # ahora
mapview(d) # Queremos Feature ID 1.26
mapview(d[rownames(d) == "1.26",])
# 
d <- d[rownames(d) == "1.26",]

# Ahora extraemos sólo los perfiles que intersectan d
p <- st_intersection(p, d) 
# ops! hay un error. d y p no tienen el mismo sistema de coordenadas
st_crs(p); st_crs(d)
# se trata sólo de un error en la definición del crs (coordinate referent system)
st_crs(d) <- 4326
p <- st_intersection(p, d)
mapview(p) + mapview(d)

# Seleccionamos las variables que queremos mapear: MO, pH y Arcilla
names(p)
s <- p %>% select_("profile_identifier", "top", "bottom", 
                   "organic_carbon", "ph", "clay")
names(s)[1] <- "id"

# Observemos algunas estadísticas por perfil
z <- s %>% group_by(id) %>% # agrupamos por perfil
  summarise(arc = mean(clay, rm.na = TRUE), # aplicamos estadísticas por perfil
            arc.sd = sd(clay), 
            rel = arc.sd/arc,
            arc0 = first(clay),
            top = first(top),
            bottom = first(bottom))

mapview(z, zcol = "arc")

# estadísticas entre perfiles
summary(z)
GGally::scatmat(z)

# Ok, cómo extraer el valor de una propiedad en un rango determinado: SPLINE

t <- as.data.frame(cbind(s, st_coordinates(s)))

t <- t[,-9] # removemos la columna geometry


# t[cID] <- as.factor(as.character(t[,cID]))
# ndata=length(unique(t[,cID]))
# IDnames <- as.character(unique.default(t[,cID]))
# t[,cID]<-as.factor(t[,cID])
# levels(t[,cID]) <- seq_along(levels(t[,cID]))
# dat_x <- cbind(t[,cID], as.numeric(t[,cLS]), as.numeric(t[,cLI]), t[,cAttrib])  
# dat_m<- as.matrix(dat_x)


# A partir de los nombres completamos los valores de las variables ID, cLS, cLI, cAttrib abajo:
cID=1 					 #Columna  con el Identificador único
cX =7          ## Columna con la coordenada X
cY =8           ## Columna con la coordenada Y
cLS=2			  	 #Columna con el l??mite superior de la capa (desde)
cLI=3			  	 #Columna con el l??mite inferior de la capa (hasta)
cAttrib=5 			 #Columna con atributo a mapea
# cDate=2
# cSource=8

VarName ="ph" 			 #Nombre de la variable (se usa para las columnas, y el archivo de salida)

## Definimos los parametros de los splines

lam <- 0.1  			   #Valor de lambda
mxd <- 100 					 # Profundidad maxima
d<- t(c(0,30)) 			 #GlobalSoilMap.Net specifications or user defined depths



##########################################

##### Parametros de los splines ##################################################################
dat2 <- t[order(t[cID],t[cLS]), ]  ##Tambien ordenar por LS


###########################################################################################################
# Inputs
dat2[,cLS]<-as.numeric(as.character(dat2[,cLS]))
dat2[,cLI]<-as.numeric(as.character(dat2[,cLI]))
dat2[,cAttrib]<-as.numeric(as.character(dat2[,cAttrib]))
dat2$id <- as.factor(dat2$id)
###  Prueba de integridad de los datos ####
### Vamos a ver una serie de warnings en estos pasos, al final tendremos un resumen
dat2$rechazado <- 0
for(i in 1:length(levels(dat2[,cID]))) {
  ssb <- subset(dat2, dat2[, 1] == levels(dat2[,cID])[i])
  dat2[dat2[,cID]==levels(dat2[,cID])[i],ncol(dat2)]<- !((min(ssb[cX])-max(ssb[cX])) + (min(ssb[cY])-max(ssb[cY]))) == 0
}
dat2[which(dat2[,cLS]-dat2[,cLI]==0),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cID])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cLS])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cLI])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cAttrib])),ncol(dat2)]<-1
dat2[which(dat2[,cLS] > dat2[,cLI]),ncol(dat2)]<-1


## Este es el resumen de las pruebas de integridad:
table(dat2$rechazado==0) ## FALSE corresponde a datos rechazados


## Si hubiesen datos rechazados, vamos a guardarlos en una tabla para corroborarlos
#write.csv(subset(dat2, dat2$rechazado==1), "rechazados.csv", row.names=F)

## Nos quedamos con los aceptados
dat2 <- subset(dat2, dat2$rechazado==0)

###
dat2[cID] <- as.factor(as.character(dat2[,cID]))
ndata=length(unique(dat2[,cID]))
IDnames <- as.character(unique.default(dat2[,cID]))
dat2[,cID]<-as.factor(dat2[,cID])
levels(dat2[,cID]) <- c(1:length(levels(dat2[,cID])))
dat_x<-cbind(dat2[,cID], as.numeric(dat2[,cLS]), as.numeric(dat2[,cLI]), dat2[,cAttrib])  
dat_m<- as.matrix(dat_x)

############################################################################## 
## Generamos los horizontes sintéticos mediantes ea plines
#install.packages("splines")
library(splines)

# un error en la proxima linea, es porque el archivo spline_functions.RData no esta en la carpeta de trabajo! 
load("Datos/spline_functions.RData") # Malone 2009. Tiene que estar en la carpeta 

##Error en solve.default(z, t(y)) : 
##  sistema es computacionalmente singular: número de condición rec??proco = 7.66064e-17
## Debemos variar el lamdba
#p<- NA

dat <- dat_m
int_s<-ea_spline(dat_x,ndata,lam,d)

nyfit<-int_s[[1]]
spfit<-int_s[[2]]


## Extraemos los resultados y limpiamos los valores negativos.
nyfit[nyfit<0] <- NA
spfit[spfit<0] <- NA


#### Graficos  #####
st=sample(1:ndata, 1) ##Elegimos un perfil al azar

for (st in st) {
  #  png(paste("perfil", st, ".png"))
  subs <- subset(dat_m, dat_m[, 1] == st)
  plot(rep(subs[,4],each=2), sort(c(subs[,2], subs[,3])), type="s", xlab=VarName, ylab="Profundidad",
       ylim = rev(range(c(0,subs[,3]))), main=paste("Perfil: ", st,  " / ", IDnames[st]), xlim=c(min(subs[,4])*0.8,max(subs[,4])*1.2))
  lines(spfit[,st], 0:mxd,col="red", lty=1, ylim = rev(range(c(0,subs[,3]))))
  abline(h=as.vector(d), lty=2, col="grey60")
  newhor <- vector()
  for(nwh in 1:length(d)-1){newhor[nwh] <- (d[nwh]+d[nwh+1])/2}
  points(nyfit[st,1:length(d)-1],newhor, col="blue", pch=19,ylim = rev(range(c(0,subs[,3]))))
  legend(inset=.02, x="bottomright", legend= c("medido", paste("spline (lambda ", lam), "horiz. sintéticos", "muestreo"),
         col=c("black", "red", "grey60", "blue"),text.col = "black", lty = c(1, 1, 2, 0), pch = c(NA, NA, NA, 20), cex=0.8)
  #  dev.off( )
}

#### Exportar ####
cNames <- c("ID", "X", "Y", paste(VarName, paste(d[,1:length(d)-1], "-", d[,2:length(d)], sep=""), sep="_"), "MaxDepth")

OutFileName <- paste("Splines_", VarName, ".csv", sep="")
levels(dat2[,cID])<-IDnames
XY <- unique(merge(IDnames, dat2[,c(cID,cX,cY)], by.x=1, by.y=1))

result <- cbind(IDnames, XY[,2:3],nyfit)
names(result) <- cNames
summary(result)

write.table(result, paste0("resultados/",OutFileName), row.names=FALSE, sep=",", dec=".")







