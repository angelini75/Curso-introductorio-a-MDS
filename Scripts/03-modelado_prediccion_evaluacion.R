# Este script está basado en un desarrollo de Guillermo Federico Olmedo (2019)

rm(list = ls())

library(raster)
library(sp)
library(caret)
library(randomForest)
library(mapview)
library(quantregForest)

load("resultados/puntos+covs.RData")

# For its use on R we need to define a model formula

data@data <- data@data[,c("ph_0.30", names(data)[4:34])]

data <- data[complete.cases(data@data),]


## Se trabaja solo con el 75% de los datos y se utilizan 25% para validar
data@data <- cbind(data@data, coordinates(data))

set.seed(2345) # repetible
inTrain <- createDataPartition(y = data@data$ph_0.30, p = .75, list = FALSE)
training <- data@data[ inTrain,]
testing <- data@data[-inTrain,]

# saveRDS(testing, "validation_Dataset2001.rds")

fm <- as.formula(paste("ph_0.30 ~", paste0(names(data)[c(-1,-33:-34)],
                                         collapse = "+")))



#### Calibrar el modelo y preparar los datos de validacion
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)

(rfmodel <- rfe(x=training[,2:32], y=training[,1], sizes=c(1:6), rfeControl=control2))

predictors(rfmodel)
plot(rfmodel)
plot(rfmodel, type = c("g", "o"), metric = "Rsquared")


testing$residuals <- testing$ph_0.30 - predict(rfmodel, testing)
stopCluster(cl = cl)

## Mapa de valores mas probable
beginCluster()


(predRFE <- clusterR(covs, predict, args=list(model=rfmodel)))


endCluster()
# writeRaster(predRFE, file="OCSKGM2015.tif", 
#             overwrite=TRUE)

mapview(mask(predRFE, d)) + mapview(data, zcol = "ph_0.30")


##############################################################################
# Mapa de incertidumbre #####

beginCluster(6,type="SOCK")

model <- quantregForest(y=testing$residuals, x=testing[,2:32], ntree=500, keep.inbag=TRUE)


#Estimate model uncertainty

unc <- clusterR(covs[[names(testing[,2:32])]], predict, args=list(model=model,what=sd))

endCluster()
#salvar el mapa de incertidumbre

mapview(mask(unc, d))

#####################################################################
step(lm(formula = fm, data = training[,1:32]), direction = "both")
model <- lm(formula = ph_0.30 ~ Aspect + Channel_Network_Base_Level + 
              srtm_21_12 + LS_Factor + Topographic_Wetness_Index + Valley_Depth + 
              Vertical_Distance_to_Channel_Network + bio6_23 + bio9_23 + 
              bio13_23 + bio14_23 + bio15_23 + bio16_23 + bio18_23 + bio19_23 + 
              bio8_23, data = training[, 1:32])
summary(model)

testing$pred.lm <- predict(model, testing)

RMSE(pred = testing$pred.lm, obs = testing$ph_0.30)

library(gstat)
training$residuals.lm <- training$ph_0.30 - predict(model, training)

coordinates(training) <- ~ X + Y
dependend_var.v <- variogram(object = residuals.lm ~ 1, data = training)

dependend_var.ovgm <- fit.variogram(
  dependend_var.v,vgm(nugget=0,model = "Exp", 
                      range=sqrt(diff(training@bbox[1,])^2 + 
                                   diff(training@bbox[2,])^2)/4,
                      psill=var(training$residuals.lm)))

plot(dependend_var.v, dependend_var.ovgm, plot.nu=T, main="Residuos") ## 350*260
dependend_var.ovgm

coordinates(testing) <- ~X+Y

testing$residuals.lm <- testing$ph_0.30 - predict(model, testing)
predRK <- krige(formula = residuals.lm ~1, locations = training, testing, dependend_var.ovgm)[1]

testing$pred.rk <- testing$pred.lm + predRK$var1.pred

RMSE(pred = testing$pred.rk, obs = testing$ph_0.30)

rfmodel

#mapview(krige(residuals~1, dat,dem  , dependend_var.ovgm)[1])

l <- raster::predict(object = covs, model)
grid <- as(covs[[1]], "SpatialGridDataFrame")
proj4string(training) <- CRS("+init=epsg:4326")

k <- raster(krige(formula = residuals.lm ~1, training, grid, dependend_var.ovgm))

mapview(mask(l, d))
mapview(mask(l+k, d))

