##Taller 2.
getwd()
library(terra)
#install.packages('geodata')
library(geodata)
library(sf)
library(terra)
## Cargar librerias para visualizacion
library(tmap)
library(spData)
#install.packages('spDataLarge')
library(systemfonts)
library(grid)
#install.packages('geodata')
#install.packages('spDataLarge')
#install.packages('basemaps')
library(basemaps)
library(dplyr)
library(dplyr)

##00. Cargar datos

pre_may <- readRDS('Data/Data_processed/data_prec_mayo.rds')
pre_jun <- readRDS('Data/Data_processed/data_prec_junio.rds')
pre_jul <- readRDS('Data/Data_processed/data_prec_julio.rds')

geometria <- readRDS('Data/Data_processed/area_estudio.rds')
preds <- rast('Data/Data_processed/Predictores.tif')
lagos_reg <-readRDS('Data/Data_processed/area_estudio.rds')
lagos_est <- readRDS('Data/Data_processed/lagos_est.rds')


##Geometria de chile
chllvl2 <- geodata::gadm('chl', level = 2, path= 'Data/Data_raw')
chllvl2 <- st_as_sf(chllvl2)
map_ch_noisla <- chllvl2 %>% select(GID_2) %>% filter(GID_2!="CHL.16.1_1")
#bajar peso de map_ch_noisla
#object_size(map_ch_noislaap) 
map_ch_noisla <- st_simplify(map_ch_noisla, preserveTopology = FALSE, dTolerance = 1000)
#Geometria de chile
chl <- geodata::gadm('chl', level = 1, path= 'Data/Data_raw')
chl <- st_as_sf(chl)
chl$NAME_1
plot(chl$geom)
plot(lagos_reg)
lagos_sf <- st_as_sf(lagos_reg)
plot(lagos_sf)


##00.1 Cargar predictores
preds <- rast('Data/Data_processed/Predictores.tif')
names(preds)
preds_dem <- (preds[[1]])
plot(preds_dem)



##descarga de basemap

get_maptypes()
bm <- basemap_raster(lagos_sf, map_service = 'osm', map_type = 'topographic')
bmstreets <- basemap_raster(lagos_sf, map_service = 'osm', map_type = 'streets')
bmworld <- basemap_raster(lagos_sf, map_service = 'esri', map_type = 'world_imagery')

#tmap jobs
tmap_mode("plot")


##Chile inset
chl_map <- tm_shape(map_ch_noisla) +
  tm_polygons()+
  tm_shape(lagos_sf)+
  tm_polygons(col = "red")+
  tm_fill(col="white", alpha=0.5, lwd=0.5)
chl_map

##Mapas de  elevacion


# for (i in 1:5) {
#   print(preds[[i]])
#   dem_map <-  tm_shape(bm)+
#     tm_rgb()+
#     tm_shape(preds[[i]]) +
#     tm_raster(title = 'Predictores',
#               style = 'quantile') + 
#     tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
#     tm_layout(title = 'Elevación Los Lagos',
#               main.title.size = 1,
#               fontfamily = 'serif',
#               legend.outside = TRUE) +
#     tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

# plot con mapa
# dem_map
# print(chl_map, vp = viewport(x=0.57, y=0.45, width = 0.5, height = 0.5))
#   titulo =  c("map_predictor_",i,"_.png")
#   titulo = paste("map_predictor",i,".png", sep = "_")

#   print(titulo)
#   tmap_save(dem_map, titulo,insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
#                         width = unit( 2, "inches"), 
#                         height = unit(2, "inches")
#                                 ) , dpi = 300)

#manual Evelacion 

dem_map <-  tm_shape(bm)+
  tm_rgb()+
  tm_shape(preds[[1]]) +
  tm_raster(title = 'Elevación Los Lagos',
            style = 'quantile',
            palette='YlOrBr') + 
  tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
  tm_layout( main.title.size = 1,
             fontfamily = 'serif',
             legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

tmap_save(dem_map, "Outputs/out_2/Elevación_Los_Lagos.png",insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
                                                                                                      width = unit( 2, "inches"), 
                                                                                                      height = unit(2, "inches")
) , dpi = 300)

#manual Distancia_Costa 

dem_map <-  tm_shape(bm)+
  tm_rgb()+
  tm_shape(preds[[2]]) +
  tm_raster(title = 'Distancia a la costa',
            style = 'quantile') + 
  tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
  tm_layout(main.title.size = 1,
            fontfamily = 'serif',
            legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

tmap_save(dem_map, "Outputs/out_2/Distancia_Costa.png",insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
                                                                                                  width = unit( 2, "inches"), 
                                                                                                  height = unit(2, "inches")
) , dpi = 300)

#manual NDVI_2022-05-01 

dem_map <-  tm_shape(bm)+
  tm_rgb()+
  tm_shape(preds[[3]]) +
  tm_raster(title = 'NDVI 2022-05',
            style = 'quantile') + 
  tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
  tm_layout(main.title.size = 1,
            fontfamily = 'serif',
            legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

tmap_save(dem_map, "Outputs/out_2/NDVI_2022-05.png",insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
                                                                                               width = unit( 2, "inches"), 
                                                                                               height = unit(2, "inches")
) , dpi = 300)

#manual NDVI_2022-06-01

dem_map <-  tm_shape(bm)+
  tm_rgb()+
  tm_shape(preds[[4]]) +
  tm_raster(title = 'NDVI 2022-06',
            style = 'quantile') + 
  tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
  tm_layout(main.title.size = 1,
            fontfamily = 'serif',
            legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

tmap_save(dem_map, "Outputs/out_2/NDVI_2022-06.png",insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
                                                                                               width = unit( 2, "inches"), 
                                                                                               height = unit(2, "inches")
) , dpi = 300)

#manual NDVI_2022-07-01

dem_map <-  tm_shape(bm)+
  tm_rgb()+
  tm_shape(preds[[5]]) +
  tm_raster(title = 'NDVI 2022-07',
            style = 'quantile') + 
  tm_shape(geometria) + tm_borders() + tm_shape(lagos_est)+tm_dots() +
  tm_layout(main.title.size = 1,
            fontfamily = 'serif',
            legend.outside = TRUE) +
  tm_compass(type = "arrow", position = c("left", "top")) +tm_scale_bar()+tm_graticules()

tmap_save(dem_map, "Outputs/out_2/NDVI_2022-07.png",insets_tm = chl_map,  insets_vp = viewport(x=0.75, y=0.45,
                                                                                               width = unit( 2, "inches"), 
                                                                                               height = unit(2, "inches")
) , dpi = 300)
dem_map



#view mode
tmap_mode("view")
##Operando datos pre


pre_may <- st_as_sf(pre_may)
pre_jun <- st_as_sf(pre_jun)
pre_jul <- st_as_sf(pre_jul)
##
dem_map <- tm_shape(bm,name = 'Topographic' )+tm_rgb()+
  tm_shape(bmstreets ,name = 'Streets' )+tm_rgb()+
  tm_shape(bmworld ,name = 'World imagery' )+tm_rgb()+
  tm_shape(preds[[5]],name = 'NDVI 2022-07') +
  tm_raster(title = 'NDVI 2022-07',
            style = 'quantile') + 
  tm_shape(preds[[4]],name = 'NDVI 2022-06') +
  tm_raster(title = 'NDVI 2022-06',
            style = 'quantile') +
  tm_shape(preds[[3]],name = 'NDVI 2022-05') +
  tm_raster(title = 'NDVI 2022-05',
            style = 'quantile') +
  tm_shape(preds[[2]],name = 'Distancia a la costa') +
  tm_raster(title = 'Distancia a la costa',
            style = 'quantile') +
  tm_shape(preds[[1]],name = 'Elevación Los Lagos') +
  tm_raster(title = 'Elevación Los Lagos',
            style = 'quantile',
            palette='YlOrBr') +
  tm_shape(geometria,name = 'Limite') + tm_borders() + 
  
  tm_shape(pre_may,name = 'Precipitación Mayo')+
  tm_symbols(size='pre_mayo', col="blue")+
  
  tm_shape(pre_jun,name = 'Precipitación Junio')+
  tm_symbols(size='pre_junio', col="red")+
  
  tm_shape(pre_jul,name = 'Precipitación Julio')+
  tm_symbols(size='pre_julio', col="pink")+
  tm_layout(main.title.size = 1,
            fontfamily = 'serif',
            legend.outside = TRUE)+
  tm_scale_bar()+tm_graticules()


dem_map




tmap_save(dem_map, 'Outputs/out_2/map_interactivo.html')