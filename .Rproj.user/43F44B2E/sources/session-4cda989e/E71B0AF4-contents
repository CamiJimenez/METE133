#Librerias 
library(gstat)
library(sp)
library(sf)
library(terra)
library(tmap)
library(ggplot2)
library(stars)
library(abind)
library(tidyverse)

#Cargar datos
pre_may <- readRDS('Data/Data_processed/data_prec_mayo.rds')
pre_may <-pre_may |>
  ungroup()|>
  st_as_sf()|>
  na.omit()

pre_jun <- readRDS('Data/Data_processed/data_prec_junio.rds')
pre_jun <-pre_jun |>
  ungroup()|>
  st_as_sf()|>
  na.omit()

pre_jul <- readRDS('Data/Data_processed/data_prec_julio.rds')
pre_jul <-pre_jul |>
  ungroup()|>
  st_as_sf()|>
  na.omit()

#Area de estudio 
lim <- readRDS('Data/Data_processed/area_estudio.rds')   


#Cargar predictores
preds <- rast('Data/Data_processed/Predictores.tif')
names(preds) <-names(pre_may)[4:8]
plot(preds)


##Crear grilla
grilla <- preds
#values(grilla) <- 0
grilla <- st_as_stars(grilla) |>
  split('band')|>
  st_crop(lim)
names(grilla) <- c('dem','distancia_costa','ndvi_mayo','ndvi_junio','ndvi_julio')
names(grilla) <- tolower(names(grilla))
plot(grilla)


##Leer archivos modelos de variogramas ajustados
#Variogramas simples
fit_mva_may <- readRDS('Outputs/Out_4/fit_may_s.rds')
fit_mva_jun <- readRDS('Outputs/Out_4/fit_jun_s.rds')
fit_mva_jul <- readRDS('Outputs/Out_4/fit_jul_s.rds')


#Variogramas multiples
fit_mvar_may <- readRDS('Outputs/Out_4/fit_may_m.rds')
fit_mvar_jun <- readRDS('Outputs/Out_4/fit_jun_m.rds')
fit_mvar_jul <- readRDS('Outputs/Out_4/fit_jul_m.rds')


res_ndvi_mayo <- readRDS('Outputs/Out_4/fit_mvexr_may.rds')

###kriging ordinario
#PRUEBA
ok_may_res <- krige(pre_mayo~1,pre_may,grilla,model = res_ndvi_mayo)
plot(ok_may_res)
ok_may_2_res <- rast(ok_may_res)
tmap_mode('view')
tm_shape(ok_may_2_res, title = ' Kriging')+
  tm_raster(title = 'Precipitaciones [mm]',style = 'cont', palette="RdYlBu")+
  tm_facets(free.scales = TRUE)+
  tm_shape(pre_may)+
  tm_dots(col = 'pre_mayo',palette = 'RdYlBu', style = 'cont',size = 0.1)+
  tm_layout(title ="Precipitaciones Mayo ",legend.outside = TRUE)

#tmap_save(ok_may_2_res,"Outputs/out_5/ok_may_2_res.png", dpi = 300)

##Cuando se realiza el kriging con el modelo variografico multiple los resultados solamente dan un valor de precipitacion y no se ha logrado estimar la interpolacion espacial

ok_may <- krige(pre_mayo~1,pre_may,grilla,model = fit_mvar_may)
plot(ok_may)
ok_may_2 <- rast(ok_may)
tmap_mode('view')
tm_shape(ok_may_2)+
  tm_raster(title = 'Precipitaciones [mm]',style = 'cont', palette="RdYlBu")+
  tm_facets(free.scales = TRUE)+
  tm_shape(pre_may)+
  tm_dots(col = 'pre_mayo',palette = 'RdYlBu', style = 'cont',size = 0.1)+
  tm_layout(title ="Precipitaciones Mayo - ok",legend.outside = TRUE)

#Al utilizar el modelo variografico de la precipitacion en mayo respecto del NDVI de mayo se obtienen los mejores resultados de kriging. El dominio espacial muestra variabilidad en torno a las estaciones pero no a la region por lo que no se representa el patron de precipitacion regional. 


#Validacion cruzada Mayo
ok_may_cv <- krige.cv(pre_mayo~1,as(pre_may,'Spatial'),grilla, model = fit_mvar_may,nfold = 10)
mean(ok_may_cv$residual)
mar2_cv <- 1 - sum(ok_may_cv$residual^2)/sum((ok_may_cv$observed-mean(ok_may_cv$observed))^2)
mar2_cv
#Calculo del promedio de los residuos

ok_may_oo <- krige.cv(pre_mayo~1,as(pre_may,'Spatial'),grilla, model = fit_mvar_may)
mean(ok_may_oo$residual)
mar2_oo <- 1 - sum(ok_may_oo$residual^2)/sum((ok_may_oo$observed-mean(ok_may_oo$observed))^2)
mar2_oo
##El r ^2 da muy bajo < 1 por lo que el modelo no explica la variabilidad espacial. 


#Mayo fue el unico mes con autocorrelacion espacial, tanto junio como julio no tuvieron valores p-value significantes pero para hacer un analisis exploratorio se realizaron de igual forma los modelos de kriging 

#Junio
ok_jun <- krige(pre_junio~1,pre_jun,grilla,model = fit_mvar_jun)
plot(ok_jun)
ok_jun_2 <- rast(ok_jun)
plot(ok_jun_2)
tmap_mode('view')
tm_shape(ok_jun_2)+
  tm_raster(title = 'Precipitaciones [mm]',style = 'cont', palette="RdYlBu")+
  tm_facets(free.scales = TRUE)+
  tm_shape(pre_jun)+
  tm_dots(col = 'pre_junio',palette = 'RdYlBu', style = 'cont',size = 0.1)+
  tm_layout(title ="Precipitaciones Junio - rm",legend.outside = TRUE)
ok_jun[[2]] |> global('mean', na.em = TRUE) |> sqrt()
ok_jun_2[[2]] |> global('mean', na.em = TRUE) |> sqrt()

#La estacion 138 (segmento norte) tiene valores de precipitacion muy bajos con respecto a las demas (2.5(mm)), ese valor puede influir en el promedio de la precipitacion, ademas hay dos estaciones que registran los valores mas altos(555, 195). Durante ese mes ocurrieron eventos de precipitaciones extremas que podrian sesgar el estudio espacial. 

#En la zona cordillerana el modelo no estaria representado la variabilidad espacial de la precipitaciones porque no hay datos de estaciones alli

#De la interpolacion con el metodo de kriging tenemos en el mapa de la derecha 

ok_jun_cv <- krige.cv(pre_junio~1,as(pre_jun,'Spatial'),grilla, model = fit_mvar_jun,nfold = 10)
mean(ok_jun_cv$residual)
jun2_cv <- 1 - sum(ok_jun_cv$residual^2)/sum((ok_jun_cv$observed-mean(ok_jun_cv$observed))^2)
jun2_cv

ok_jun_oo <- krige.cv(pre_junio~1,as(pre_jun,'Spatial'),grilla, model = fit_mvar_jun)
mean(ok_jun_oo$residual)
jun2_oo <- 1 - sum(ok_jun_oo$residual^2)/sum((ok_jun_oo$observed-mean(ok_jun_oo$observed))^2)
jun2_oo
#El r^2 de junio es muy bajo < 1 por lo que no estaria explicando la distribucion espacial de la precipitacion. 

#Julio
ok_jul <- krige(pre_julio~1,pre_jul,grilla,model = fit_mvar_jul)
plot(ok_jul)
ok_jul_2 <- rast(ok_jul)
plot(ok_jul_2)
tmap_mode('view')
tm_shape(ok_jul_2)+
  tm_raster(title = 'Precipitaciones [mm]',style = 'cont', palette="RdYlBu")+
  tm_facets(free.scales = TRUE)+
  tm_shape(pre_jul)+
  tm_dots(col = 'pre_julio',palette = 'RdYlBu', style = 'cont',size = 0.1)+
  tm_layout(title ="Precipitaciones Julio - rm",legend.outside = TRUE)

ok_jul_cv <- krige.cv(pre_julio~1,as(pre_jul,'Spatial'),grilla, model = fit_mvar_jul,nfold = 10)
mean(ok_jul_cv$residual)
jul2_cv <- 1 - sum(ok_jul_cv$residual^2)/sum((ok_jul_cv$observed-mean(ok_jul_cv$observed))^2)
jul2_cv

ok_jul_oo <- krige.cv(pre_julio~1,as(pre_jul,'Spatial'),grilla, model = fit_mvar_jul)
mean(ok_jul_oo$residual)
jul2_oo <- 1 - sum(ok_jul_oo$residual^2)/sum((ok_jul_oo$observed-mean(ok_jul_oo$observed))^2)
jul2_oo



#variograma de los residuos#

rk <- krige(pre_mayo~ndvi_mayo+ndvi_junio+ndvi_julio+distancia_costa+dem,pre_may,grilla,fit_mvar_may)
rk_may <-rast(rk)
tmap_mode('view')
tm_shape(rk_may[[1]])+
  tm_raster(title = 'Precipitaciones [mm]',style = 'cont', palette="RdYlBu")+
  tm_facets(free.scales = TRUE)+
  tm_shape(pre_may)+
  tm_dots(col = 'pre_mayo',palette = 'RdYlBu', style = 'cont',size = 0.1)+
  tm_layout(title ="Precipitaciones Mayo - rk",legend.outside = TRUE)

#validación cruzada

rk_may_cv <- krige.cv(pre_mayo~ndvi_mayo,as(pre_may,'Spatial'),grilla, model = fit_mvar_may,nfold = 10)
rmse_mar <-sqrt(mean(rk_may_cv$residual^2))
rmse_mar
mar2_cv <- 1 - sum(rk_may_cv$residual^2)/sum((rk_may_cv$observed-mean(rk_may_cv$observed))^2)
mar2_cv

rk_may_oo <- krige.cv(pre_mayo~ndvi_mayo,as(pre_may,'Spatial'),grilla, model = fit_mvar_may)
rmse2_mar <-sqrt(mean(rk_may_cv$residual^2))
rmse2_mar
mar2_oo <- 1 - sum(rk_may_oo$residual^2)/sum((rk_may_oo$observed-mean(rk_may_oo$observed))^2)
mar2_oo


##En los talleres previos se han calculado estimadores lineales ponderados asignados a la cercanía de la posicion estimada como el metodo del vecino mas cercano que le atribuye toda la ponderacion al dato mas cercano a estimar, esto se realiza cuando  se encuentra dentro del poligono de influencia. Otro estimador utilizado fue el de inverso de la distancia, que atribuye a cada dato una ponderacion proporcional al inverso de su distancia la sitio a estimar.

#Esos estimadores dependen solamente de criterios de cercanias, para analizar otras variables se utilizan los metodos de kriging que introducen la rebundancia entre datos, continuidad espacial y anisotropia a traves del uso del variograma.

#Como el kriging considera los aspectos variograficos y los modelos de ajustes de los variogramas tenian mucha fluctuacion de datos, posiblemente ese sea una variable de error dentro del modelo, como tambien la baja cantidad de estaciones consideradas en el estudio.

#El modelo variografico que mejor representa las variables fue el de precipitaciones de mayo con respecto al ndvi de mayo.

##Para junio y julio, estos meses no tienen autocorrelacion espacial por lo que el modelo de kriging no seria el estimador indicado para el analisis. Cuando la distribucion de los datos estan sesgadas por valores extremos eso influye en el variograma, una tecnica geoestadistica podria hacer una trasformacion logaritmica de los datos y desde alli analizar su distribucion espacial. 

















