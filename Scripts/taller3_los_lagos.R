#Script predicción espacial utilizando método de inverso de la distancia
#Modelos de regresión lineal simple/múltiples y regresión basado en las coordenadas
## 01. Cargar librerias
#install.packages('gstat')
library(gstat)
library(abind)
library(stars)
library(raster)
library(terra)
library(sf)
library(tmap)
library(dplyr)


##00. Cargar datos de precipitaciones mensuales
pre_mayo <- readRDS('Data/Data_processed/data_prec_mayo.rds')
pre_jun <- readRDS('Data/Data_processed/data_prec_junio.rds')
pre_jul <- readRDS('Data/Data_processed/data_prec_julio.rds')

##Cargar estaciones y geometria para visualizacion
lagos_est <- readRDS('Data/Data_processed/lagos_est.rds')
geometria <- readRDS('Data/Data_processed/area_estudio.rds')
plot(geometria)
plot(lagos_est$geometry)


##00.1 Cargar predictores raster
preds <- rast('Data/Data_processed/Predictores.tif')
names(preds) <-names(pre_mayo)[3:7]
plot(preds)

## 00.2 Carga de límites de area de estudio
lim <- readRDS('Data/Data_processed/area_estudio.rds')
plot(lim)

## 02. Crear grilla
grilla <- preds[[1]]
values(grilla) <- 0


## 03. Transformacion a star para utilizar krige
grilla <- st_as_stars(grilla) |>
  st_crop(lim)
plot(grilla)


## 04. Convertir a sf
pre_may <- pre_mayo |> ungroup() |>
  st_as_sf() |>
  na.omit()

pre_jun <- pre_jun |> ungroup() |>
  st_as_sf() |>
  na.omit()

pre_jul <- pre_jul |> ungroup() |>
  st_as_sf() |>
  na.omit()


##05.Krige precipitación (inverso de la distancia), no entrega error

#Predicción espacial de precipitacion en mayo usando inverso a la distancia a resoluciónd de 1km 
out_idw_may <- krige(pre_mayo~1, pre_may, grilla)
plot(out_idw_may, col = topo.colors(20), main ='Predicción espacial de precipitación Mayo -IDW')
##Comentario: el patron de precipitacion aumenta hacia la zona sur. En el norte de la region se obtienen los menores valores de la prediccion

#Predicción espacial de precipitacion en mayo usando inverso a la distancia a resoluciónd de 1km 
out_idw_jun <- krige(pre_junio~1, pre_jun, grilla)
plot(out_idw_jun, col = topo.colors(20), main ='Predicción espacial de precipitación Junio -IDW')
##Comentario: el patron de precipitacion muestra dos nucleos de baja precipitacion en el extremo norte y suroeste de la region, mientras que hacia el este los valores estimados de precipitacion son mayores.


#Predicción espacial de precipitacion en mayo usando inverso a la distancia a resoluciónd de 1km 
out_idw_jul <- krige(pre_julio~1, pre_jul, grilla)
plot(out_idw_jul, col = topo.colors(20), main ='Predicción espacial de precipitación Julio -IDW')
##Comentario: similar a junio, el patron de precipitacion muestra dos nucleos de baja precipitaciones pero en el norte en los limites de bajas precipitaciones estimadas aumentan hasta valores entre 200 y 300 mm.


##Combinar graficos
par(mfrow = c(1,3))
out_idw_may_r <- rast(out_idw_may)
out_idw_jun_r <- rast(out_idw_jun)
out_idw_jul_r <- rast(out_idw_jul)
stack_idw <- plot(out_idw_may_r[[1]], main ='Predicción espacial de precipitación Mayo -IDW'); plot(out_idw_jun_r[[1]],  main ='Predicción espacial de precipitación Junio -IDW'); plot(out_idw_jul_r[[1]], main ='Predicción espacial de precipitación Julio -IDW')
plot(stack_idw)


## 06.  Modelos de regresión lineal simple
##06.1 Modelo de regresión de la precipitación de mayo a partir del NDVI de mayo
mod1_ndvi_may <- lm(pre_mayo~ndvi_mayo, pre_may)
summary(mod1_ndvi_may)
#el r^2 es bajo, solamente explica un 6% de la variabilidad

##06.2 Modelo de regresión de la precipitación de mayo a con respecto del DEM 
mod2_dem_may <- lm(pre_mayo~dem, pre_may)
summary(mod2_dem_may)
#Cuando realizamos el modelo pero con respecto al dem el R^2 aumenta explicando un 9% de la variabilidad 


##07. cargar y ordenar predictores
##Mayo
preds
preds_ndviMay <- st_as_stars(preds[[3]])
names(preds_ndviMay) <- 'ndvi_mayo'

#Junio
preds
preds_ndviJun <- st_as_stars(preds[[4]])
names(preds_ndviJun) <- 'ndvi_junio'


#Julio
preds
preds_ndviJul <- st_as_stars(preds[[5]])
names(preds_ndviJul) <- 'ndvi_julio'


#08. Predicciones espaciales utilizando variables predictoras
##08.1 Predicción espacial de precipitacion en mayo con respecto NDVI mayo 
out_lm_may<- krige(pre_mayo~ndvi_mayo, pre_may, preds_ndviMay)
out_lm_may <- rast(out_lm_may)
plot(out_lm_may, main=c('Predicciones espaciales', 'Varianza'), col = topo.colors(20)) #izquierda predicción, derecha varianza
#Calculo del error de la predicción espacial en Mayo
error_lm_may <- (out_lm_may)[[2]] |> sqrt() 
par(mfrow = c(1, 3))
plot(error_lm_may, main='Error de predicción')

#Combinar mapas
plot(c(out_lm_may, error_lm_may), main=c('Predicciones espaciales', 'Varianza', 'Error'))
  


##08.2 Predicción espacial de precipitacion en junio con respecto NDVI junio
out_lm_jun<- krige(pre_junio~ndvi_junio, pre_jun, preds_ndviJun)
out_lm_jun <- rast(out_lm_jun)
plot(out_lm_jun, main=c('Predicciones espaciales Junio', 'Varianza Junio'), col = topo.colors(20))
error_lm_jun <- (out_lm_jun)[[2]] |> sqrt() 
par(mfrow = c(1, 3))
plot(error_lm_jun, main='Error de predicción Junio')
#Combinar mapas
plot(c(out_lm_jun, error_lm_jun), main=c('Predicciones espaciales', 'Varianza', 'Error Junio'))


##08.3 Predicción espacial de precipitacion en julio con respecto NDVI julio
out_lm_jul<- krige(pre_julio~ndvi_julio, pre_jul, preds_ndviJul)
out_lm_jul <- rast(out_lm_jul)
plot(out_lm_jul,main=c('Predicciones espaciales Julio', 'Varianza Julio'), col = topo.colors(20))
error_lm_jul <- (out_lm_jul)[[2]] |> sqrt() 
par(mfrow = c(1, 3))
plot(error_lm_jul, main='Error de predicción Julio')
#Combinar mapas
plot(c(out_lm_jul, error_lm_jul), main=c('Predicciones espaciales', 'Varianza', 'Error Julio'))


## 09. Juntar todos los resultados en una única visualización
res <- c(out_lm_may, out_lm_jun, out_lm_jul)
res <- res[[c(1,3,5,2,4,6)]]

names(res) <- c('preds_may', 'preds_jun', 'preds_jul',
             'var_may', 'var_jun', 'var_jul')
plot(res) #arriba predicciones, abajo errores (var)



##10. Visualizar con {tmap}
tmap_mode('view')
tm_shape(res) +
  tm_raster(title = 'Precipitación (mm)', style = 'cont') +
  tm_facets (ncol = 3, nrow = 2, free.scales = TRUE) +
  tm_shape(pre_may) + 
  tm_dots ()
#tmap_arrange(ncol=2, nrow=3)





##11. Regresión en las coordenadas - Interpolación con coordenadas de las estaciones
##11.1  Mayo
library(sf)
st_coordinates(pre_may)
pre_may <- cbind(pre_may, st_coordinates(pre_may))
pre_may
names(pre_may) <- tolower(names(pre_may))

mod.lm1 <- lm(pre_mayo ~ x + y , data =pre_may)
summary(mod.lm1)
#R^2 de 0.38 
out_cor1_may <- krige(pre_mayo ~ x + y , pre_may, grilla)
plot(out_cor1_may, col = topo.colors (20), main ='Predicción espacial en las coordenadas Mayo')
     

out_cor2_may <- krige(pre_mayo ~ x + y + I(x^2) + I(y^2) + I(x*y), pre_may, grilla)
plot(out_cor2_may, col = topo.colors(20), main ='Predicción espacial en las coordenadas Método 2 Mayo')


##11.2  Junio
st_coordinates(pre_jun)
pre_jun <- cbind(pre_jun, st_coordinates(pre_jun))
pre_jun
names(pre_jun) <- tolower(names(pre_jun))

mod.lm2 <- lm(pre_junio ~ x + y , data =pre_jun)
summary(mod.lm2)
#R^2 de 0.24

out_cor1_jun <- krige(pre_junio ~ x + y , pre_jun, grilla)
plot(out_cor1_jun, col = topo.colors (20), main ='Predicción espacial en las coordenadas Junio')

out_cor2_jun <- krige(pre_junio ~ x + y + I(x^2) + I(y^2) + I(x*y), pre_jun, grilla)
plot(out_cor2_jun, col = topo.colors(20), main ='Predicción espacial en las coordenadas Método 2 Junio')


##11.3  Julio
st_coordinates(pre_jul)
pre_jul <- cbind(pre_jul, st_coordinates(pre_jul))
pre_jul
names(pre_jul) <- tolower(names(pre_jul))

mod.lm3 <- lm(pre_julio ~ x + y , data =pre_jul)
summary(mod.lm3)
#R^2 0.09
out_cor1_jul <- krige(pre_julio ~ x + y , pre_jul, grilla)
plot(out_cor1_jul, col = topo.colors (20), main ='Predicción espacial en las coordenadas Julio')

out_cor2_jul <- krige(pre_julio ~ x + y + I(x^2) + I(y^2) + I(x*y), pre_jul, grilla)
plot(out_cor2_jul, col = topo.colors(20), main ='Predicción espacial en las coordenadas Método 2 Julio')


## 12. Modelos de regresion lineal multiples

##12.1 pasar preds a star
preds_stars <- st_as_stars(preds)
preds_stars <- split(preds_stars, 'band')
names(preds_stars) <- names(preds)


#Mayo
mod1mul_may <- lm(pre_mayo~. ,st_drop_geometry(pre_may)[-1])
summary(mod1mul_may)
##se obtiene r cuadrado de 0.5 al utilizar todas las variables predictoras, explica el 50% de variabilidad en la precipitacion, hasta ahora ha sido de los valores mas altos

#Aplicando la prediccion espacial
preds_stars
out_mul_may <- krige(pre_mayo~dem+distancia_costa+ndvi_mayo+ndvi_junio+ndvi_julio, pre_may[, -c(1,8:9)], preds_stars)
plot(rast(out_mul_may),  main=c('Predicciones espaciales Mayo', 'Varianza Mayo'), col = topo.colors(20))
##Error en mayo
rast(out_mul_may)[[2]] |> sqrt() |> plot() #los errores mas grandes son el la cordillera, donde hay menos estaciones

#Junio
out_mul_jun <- krige(pre_junio~dem+distancia_costa+ndvi_mayo+ndvi_junio+ndvi_julio, pre_jun[, -c(1,8:9)], preds_stars)
plot(rast(out_mul_jun), main=c('Predicciones espaciales Junio', 'Varianza Junio'), col = topo.colors(20)) #estimación en izquierda y error en derecha
rast(out_mul_jun)[[2]] |> sqrt() |> plot()

#Junio
out_mul_jul <- krige(pre_julio~dem+distancia_costa+ndvi_mayo+ndvi_junio+ndvi_julio, pre_jul[, -c(1,8:9)], preds_stars)
plot(rast(out_mul_jul),  main=c('Predicciones espaciales Julio', 'Varianza Julio'), col = topo.colors(20)) 
rast(out_mul_jul)[[2]] |> sqrt() |> plot()


##El modelo de regresion lineal multiple fue aquel que dio mejores resultados con r^2 cercanos a 0.5. Por el contrario, el peor resultado se obtuvo con el modelo de regresion lineal simple 

