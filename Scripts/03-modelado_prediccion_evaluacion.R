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

inTrain <- createDataPartition(y = data@data$ph_0.30, p = .75, list = FALSE)
training <- data@data[ inTrain,]
testing <- data@data[-inTrain,]

# saveRDS(testing, "validation_Dataset2001.rds")

fm <- as.formula(paste("ph_0.30 ~", paste0(names(data)[-1],
                                         collapse = "+")))



#### Calibrar el modelo y preparar los datos de validacion
library(doParallel)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)

(rfmodel <- rfe(x=training[,2:32], y=training[,1], sizes=c(1:6), rfeControl=control2))


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



model <- quantregForest(y=testing$residuals, x=testing[,2:32], ntree=500, keep.inbag=TRUE)

beginCluster(6,type="SOCK")

#Estimate model uncertainty

unc <- clusterR(covs[[names(testing[,2:32])]], predict, args=list(model=model,what=sd))

endCluster()
#salvar el mapa de incertidumbre

mapview(mask(unc, d))

