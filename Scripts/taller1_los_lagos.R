## 00. Visualizar ruta de trabajo
getwd()


##01. instalar paquete geodata
#install.packages("geodata")
library(geodata)


##02. instalar paquete terra
#install.packages("terra")
library(terra)


##03. Acceder a libreria sf
library(sf)


## 04. Definir area de estudio
chl <- geodata::gadm('chl', level = 1, path= 'Data/Data_raw')
chl <- st_as_sf(chl)
chl$NAME_1
plot(chl$geom)


## 04.1 acceder a la geometria de Los Lagos por indexacion
plot(chl[9,]$geometry)
lagos_reg <- chl[9,]$geometry
lagos_reg
plot(lagos_reg)
#guardar geometria como sf
lagos_sf <- st_as_sf(lagos_reg) 

## 0.5 Carga de archivos de estaciones y climaticos
data_estaciones <- read.csv2('Data/Data_raw/estaciones_agromet.csv')
data_climatica <- read.csv2('Data/Data_raw/data_climatica_RAN_2022.csv')


## 05.1 leer encabezado de los archivos cargados
head(data_estaciones)
class(data_estaciones)
head(data_climatica)
class(data_climatica)


##06. Transformar a estaciones a objeto sf y definir coordenadas
est_sf <- st_as_sf(data_estaciones, 
                   coords = c('longitud', 'latitud'),
                   crs = 4326)


##06.1 Plotear estaciones
plot(est_sf$geometry)


## 07. conocer cuales estaciones intersectan con el limite regional de los lagos
lagos_reg <- st_union(lagos_reg)
saveRDS(lagos_reg, 'Data/Data_processed/area_estudio.rds')

est_lagos <- st_intersection(est_sf, lagos_reg)
plot(est_lagos$geometry)
plot(lagos_reg, add = TRUE)


## 08. Descarga datos MODIS NDVI para mayo, junio y julio
#
#install.packages(c("leaflet", "shiny","shinydashboard","shinyFiles",
# "shinyalert", "rappdirs","shinyjs",
#"leafem", "mapedit", "magrittr"))

ext <- as.numeric(st_bbox(lagos_reg))


library(MODIStsp)
MODIStsp(gui             = FALSE,
         out_folder      = 'Data/data_raw/MODIS/',
         out_folder_mod  = 'Data/data_raw/MODIS/',
         selprod         = 'Vegetation_Indexes_Monthly_1Km (M*D13A3)',
         bandsel         = 'NDVI', 
         sensor          = 'Terra',
         user            = 'danipasten' , # your username for NASA http server
         password        = '3P8knglt9',  # your password for NASA http server
         start_date      = '2022.05.01', 
         end_date        = '2022.07.31', 
         verbose         = TRUE,
         bbox            = ext, 
         spatmeth        = 'bbox',
         out_format      = 'GTiff',
         compress        = 'LZW',
         out_projsel     = 'User Defined',
         output_proj     = '+proj=longlat +datum=WGS84 +no_defs +type=crs',
         delete_hdf      = TRUE,
         parallel        = TRUE
)


## 09. leer datos MODIS (Primer predictor)
library(terra)
files_NDVI <- list.files('Data/Data_raw/MODIS/VI_Monthly_1Km_v6/NDVI', full.names = TRUE)


## 09.1 crear raster de los archivos NDVI
NDVI <- rast(list.files('Data/Data_raw/MODIS/VI_Monthly_1Km_v6/NDVI', full.names = TRUE))
plot(NDVI)
NDVI


##09.2 cortar y enmascarar raster NDVI
NDVI_crop <- crop(NDVI, vect(lagos_reg))
NDVI_mask <- mask(NDVI_crop, vect(lagos_reg))
plot(NDVI_mask)


## 09.3 cambiar nombres
dates <- as.Date(gsub('_', '', substr(names(NDVI_mask), 14, 32)), format = "%Y%j")
names(NDVI_mask) <- paste0('NDVI_', dates)
plot(NDVI_mask)
res(NDVI)


##10. Descarga de DEM de resolucion 1 km (Segundo Predictor)
DEM <- geodata::elevation_30s('chl', path  = 'Data/data_raw')
names(DEM) <- 'Elevación_Los_Lagos'
DEM <- crop(DEM, vect(lagos_reg))
DEM <- mask(DEM, vect(lagos_reg))
plot(DEM)


## 11. Generar raster de distancia a la costa (Tercer Predictor)
install.packages("rnaturalearthdata")
coastline <- rnaturalearth::ne_coastline('medium', returnclass = 'sf')
plot(coastline$geometry)
lagos_coast <- st_cast(lagos_reg, 'MULTILINESTRING')

coast_lagos <- st_intersection(coastline, st_as_sfc(st_bbox(lagos_coast)))
plot(coast_lagos$geometry)
plot(lagos_coast, col='red', add = TRUE)


## 12. Poligono de distancia a la costa
dist_cost <- DEM 
values(dist_cost) <- 1:ncell(DEM)
plot(dist_cost)
dis_cost_poly <- terra::as.polygons(dist_cost)
dis_cost_poly <- st_as_sf(dis_cost_poly)


## 13. Calculo de los centroides
centro <- st_centroid(dis_cost_poly)


## 14. Calculo de la distancia de centroides a la linea de costa
distancia <- st_distance(centro, coast_lagos)
values(dist_cost) <- as.numeric(distancia)
names(dist_cost) <- 'Distancia_Costa'
dist_cost <- crop(dist_cost, vect(lagos_reg))
dist_cost <- mask(dist_cost, vect(lagos_reg))
plot(dist_cost)


## 15. Raster de predictores
plot(DEM)
plot(dist_cost)
plot(NDVI_mask)

ext(DEM)
ext(NDVI_mask)
ext(dist_cost)


##15.1 Resampleo de NDVI a la resolucion del DEM
NDVI2 <- resample(NDVI_mask, DEM)


##15.2 plotear los predictores
plot(c(DEM, dist_cost, NDVI2))


##16. Union de los predictores de Elevacion, Distancia a la costa y NDVI (Raster multicapa)
predictors <- c(DEM, dist_cost, NDVI2)
predictors


##16.1 Guardar los SpatRaster  de los predictores
writeRaster(predictors, 'Data/Data_processed/Predictores.tif')


##17 cargar predictores
library(terra)
preds <- rast('Data/Data_processed/Predictores.tif')


##17.1 Verificar CRS
crs(preds)
st_crs(est_lagos)


##18. Extraer los valores de los predictores en la ubicacion de las estaciones
data_ex <- extract(preds, vect(est_lagos))
data_ex2 <- cbind(data_ex, est_lagos)
plot(data_ex2$geometry)
names(data_ex2)


##18.1 filtrar el Data Frame de data_ex2 para obtener las columnas necesarias
data_ex2 <- data_ex2[c(8,2:6,14)]


##19. Agregar datos de precipitacion a las estaciones
library(tibble)
library(lubridate)
library(dplyr)
library(tidyr)
data_climatica


##21. Agregacion utilizando R Base
data_prec <- as_tibble(data_climatica)
data_prec <- data_prec[, c(2:3,5)]
data_prec$fecha_hora <- as.POSIXct(data_prec$fecha_hora)
data_prec$mes <- format(data_prec$fecha_hora, '%m')
data_prec$group <- paste(data_prec$station_id, data_prec$mes, sep= '-')
prec_acum <- tapply(data_prec$precipitacion_horaria, data_prec$group, sum, na.rm= TRUE)

df_prec_acu <- tibble(grupo = attr(prec_acum, 'names'), prec_acum = as.numeric(prec_acum))
df_prec_acu$mes <- sapply(strsplit(df_prec_acu$grupo, '-'), `[[`,2)
df_prec_acu$ema <- sapply(strsplit(df_prec_acu$grupo, '-'), `[[`,1) 

df_prec_acu <- df_prec_acu[df_prec_acu$mes %in% c('05', '06', '07'),]
df_prec_acu <- df_prec_acu[df_prec_acu$ema %in% est_lagos$ema,]

df_prec_acu
unique(df_prec_acu$ema) |> length()



##20. Filtrado de estaciones y transformacion de datos as.tibble usando Tidyverse
data_unida <- data_climatica |> 
  dplyr::filter(station_id %in% est_lagos$ema) |>
  as.tibble() |>
  select(c(2:3,5)) |>
  mutate(fecha_hora = ymd_hms(fecha_hora)) |>
  group_by(station_id, mes = floor_date(fecha_hora, '1 month')) |>
  na.omit() |>
  summarize(Pcum = sum(precipitacion_horaria)) |>
  dplyr::filter(mes %in% as_datetime(c("2022-05-01", "2022-06-01", "2022-07-01"))) |>
  pivot_wider(names_from = mes, values_from = Pcum) |>
  right_join(data_ex2, by =c('station_id' = 'ema'))
names(data_unida)[2:4] <- c('Pre_Mayo', 'Pre_Junio', 'Pre_Julio')
names(data_unida) <- tolower(names(data_unida))
names(data_unida)[5] <- 'dem'
names(data_unida)[7:9] <- c('ndvi_mayo', 'ndvi_junio', 'ndvi_julio')
names(data_unida)
data_unida


##20.1 Separar y guardar data unida de precipitacion mayo, junio y julio
#Mayo
saveRDS(data_unida[, -(3:4)],'Data/Data_processed/data_prec_mayo.rds')
#Junio
saveRDS(data_unida[, -c(2,4)],'Data/Data_processed/data_prec_junio.rds')
#Julio
saveRDS(data_unida[, -c(2:3)],'Data/Data_processed/data_prec_julio.rds')

##Analisis estadisticos

##21. Resumen estadistico
saveRDS(summary(data_unida), 'Data/Data_processed/est_data_unida.rds') 
estadistica <- readRDS('Data/Data_processed/est_data_unida.rds')
summary(data_unida$pre_mayo)
summary(data_unida$pre_junio)
summary(data_unida$pre_julio)
summary(data_unida$dem)
summary(data_unida$distancia_costa)
summary(data_unida$ndvi_mayo)
summary(data_unida$ndvi_junio)
summary(data_unida$ndvi_julio)


##22. Exploracion de los datos
##22.1 Histogramas Utilizando {ggplot2}
library(ggplot2)

##Histograma de Precipitacion en mayo
hist_mayo <- ggplot(data = data_unida,
                    mapping =aes(x=data_unida$pre_mayo))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_mayo<-hist_mayo +
  labs(title = 'Histograma de Precipitación en Mayo', 
       caption = "Datos obtenidos de estaciones agrometeorológicas del Ministerio de Agricultura (AGROMET)",
       x =' Precipitacion acumulada Mayo',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_mayo <- hist_mayo + theme_bw()
plot(hist_mayo)



##Histograma de Precipitacionn en Junio
hist_junio <- ggplot(data = data_unida,
                     mapping =aes(x=data_unida$pre_junio))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_junio<-hist_junio +
  labs(title = 'Histograma de Precipitación en Junio', 
       x =' Precipitación acumulada Junio',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_junio <- hist_junio + theme_bw()
plot(hist_junio)



##Histograma de PrecipitaciÃ³n en Julio
hist_julio <- ggplot(data = data_unida,
                     mapping =aes(x=data_unida$pre_julio))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_julio<-hist_julio +
  labs(title = 'Histograma de Precipitación en Julio', 
       x =' PrecipitaciÃ³n acumulada Julio',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_julio <- hist_julio + theme_bw()
plot(hist_julio)


##Histograma de elevaciones
hist_dem<- ggplot(data = data_unida,
                    mapping =aes(x=data_unida$dem))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_dem<-hist_dem +
  labs(title = 'Distribución Elevaciones', 
       x =' Elevación (m)',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_dem <- hist_dem + theme_bw()
plot(hist_dem)


##Histograma de Distancia a la costa
hist_dist_cost <- ggplot(data = data_unida,
                  mapping =aes(x=data_unida$distancia_costa))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_dist_cost<-hist_dist_cost +
  labs(title = 'Distribución Distancia a la costa', 
       x =' Distancia (m)',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_dist_cost <- hist_dist_cost + theme_bw()
plot(hist_dist_cost)


##Histograma de NDVI Mayo
hist_ndvi_mayo <- ggplot(data = data_unida,
                         mapping =aes(x=data_unida$ndvi_mayo))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_ndvi_mayo<-hist_ndvi_mayo +
  labs(title = 'Distribución NDVI mayo', 
       x =' NDVI',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_ndvi_mayo <- hist_ndvi_mayo + theme_bw()
plot(hist_ndvi_mayo)


##Histograma de NDVI Junio
hist_ndvi_junio <- ggplot(data = data_unida,
                         mapping =aes(x=data_unida$ndvi_junio))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_ndvi_junio<-hist_ndvi_junio +
  labs(title = 'Distribución NDVI Junio', 
       x =' NDVI',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_ndvi_junio<- hist_ndvi_junio + theme_bw()
plot(hist_ndvi_junio)


##Histograma de NDVI Julio
hist_ndvi_julio <- ggplot(data = data_unida,
                          mapping =aes(x=data_unida$ndvi_julio))+
  geom_histogram(bins=10, fill='grey', colour='black')

hist_ndvi_julio<-hist_ndvi_julio +
  labs(title = 'Distribución NDVI Julio', 
       x =' NDVI',
       y = 'Frecuencia')+
  theme(plot.title = element_text(hjust = 0.5))
hist_ndvi_julio<- hist_ndvi_julio + theme_bw()
plot(hist_ndvi_julio)

##Diagramas de densidad
par(mfrow = c(1, 3))
den_mayo <- ggplot(data_unida)+
  geom_density(aes(pre_mayo))+
  labs(title='Curva de densidad Precipitación Mayo (mm)', x= 'Precipitación (mm', y ='Densidad')
  theme_classic()


  den_jun <- ggplot(data_unida)+
    geom_density(aes(pre_junio))+
    labs(title='Curva de densidad Precipitación Junio (mm)', x= 'Precipitación (mm', y ='Densidad')
  theme_classic()
  
  
  den_jul <- ggplot(data_unida)+
    geom_density(aes(pre_julio))+
    labs(title='Curva de densidad Precipitación Julio (mm)', x= 'Precipitación (mm', y ='Densidad')
  theme_classic()


##22.1 Cuantiles, rango y media
quantile(c(data_unida$pre_mayo, NA), na.rm = TRUE)
quantile(c(data_unida$pre_junio, NA), na.rm = TRUE)
quantile(c(data_unida$pre_julio, NA), na.rm = TRUE)
quantile(c(data_unida$dem, NA), na.rm = TRUE)
quantile(c(data_unida$distancia_costa, NA), na.rm = TRUE)
quantile(c(data_unida$ndvi_mayo,NA), na.rm = TRUE)
quantile(c(data_unida$ndvi_juni,NA), na.rm = TRUE)
quantile(c(data_unida$ndvi_juli,NA), na.rm = TRUE)



##22.2 Cuantiles, rango y media
range(c(data_unida$pre_mayo, NA), na.rm = TRUE)
range(c(data_unida$pre_junio, NA), na.rm = TRUE)
range(c(data_unida$pre_julio, NA), na.rm = TRUE)
range(c(data_unida$dem, NA), na.rm = TRUE)
range(c(data_unida$distancia_costa, NA), na.rm = TRUE)
range(c(data_unida$ndvi_mayo,NA), na.rm = TRUE)
range(c(data_unida$ndvi_junio,NA), na.rm = TRUE)
range(c(data_unida$ndvi_julio,NA), na.rm = TRUE)


##22.2 Cuantiles, rango y media
mean(c(data_unida$pre_mayo, NA), na.rm = TRUE)
mean(c(data_unida$pre_junio, NA), na.rm = TRUE)
mean(c(data_unida$pre_julio, NA), na.rm = TRUE)
mean(c(data_unida$dem, NA), na.rm = TRUE)
mean(c(data_unida$distancia_costa, NA), na.rm = TRUE)
mean(c(data_unida$ndvi_mayo,NA), na.rm = TRUE)
mean(c(data_unida$ndvi_junio,NA), na.rm = TRUE)
mean(c(data_unida$ndvi_julio,NA), na.rm = TRUE)



##Graficos base de R
par(mfrow=c(2,2))
hist(data_unida$pre_mayo, main = 'Histograma Precipitación Mayo')
hist(data_unida$pre_junio, main = 'Histograma Precipitación Junio')
hist(data_unida$pre_julio, main = 'Histograma Precipitación Julio')



##Graficos base de R
par(mfrow=c(2,2))
boxplot(data_unida$pre_mayo, main = 'boxplot Precipitación Mayo')
boxplot(data_unida$pre_junio, main = 'boxplot Precipitación Junio')
boxplot(data_unida$pre_julio, main = 'boxplot Precipitación Julio')



##aplicación de modelos de regresion lineal: estimacion de la precipitacion con respecto al modelo de elevación
mod1 <- lm(pre_mayo~dem, data = data_unida)
mod2 <- lm(pre_junio~dem, data = data_unida)
mod3 <- lm(pre_julio~dem, data = data_unida)
n1 <- length(mod1)
n2 <- length(mod2)
n3 <- length(mod3)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod1)$coefficients
summary(mod2)$coefficients
summary(mod3)$coefficients

##residuos
resid(mod1)
resid(mod2)
resid(mod3)

##des.vest residuos
sqrt(sum(resid(mod1)^2)/(n1-2))
sqrt(sum(resid(mod2)^2)/(n2-2))    
sqrt(sum(resid(mod3)^2)/(n3-2))


##Regresión de precipitacion con respecto a la distancia a la costa
mod1_dist <- lm(pre_mayo~distancia_costa, data = data_unida)
mod2_dist <- lm(pre_junio~distancia_costa, data = data_unida)
mod3_dist <- lm(pre_julio~distancia_costa, data = data_unida)
c1 <- length(mod1_dist)
c2 <- length(mod2_dist)
c3 <- length(mod3_dist)
summary(mod1_dist)
summary(mod2_dist)
summary(mod3_dist)
summary(mod1_dist)$coefficients
summary(mod2_dist)$coefficients
summary(mod3_dist)$coefficients

##residuos
resid(mod1_dist)
resid(mod2_dist)
resid(mod3_dist)

##des.vest residuos
sqrt(sum(resid(mod1_dist)^2)/(c1-2))
sqrt(sum(resid(mod2_dist)^2)/(c2-2))    
sqrt(sum(resid(mod3_dist)^2)/(c3-2))


##Regresion de precipitacion con respecto a la distancia a NDVI
mod1_ndvi <- lm(pre_mayo~ndvi_mayo, data = data_unida)
mod2_ndvi <- lm(pre_junio~ndvi_junio, data = data_unida)
mod3_ndvi <- lm(pre_julio~ndvi_julio, data = data_unida)
nd1 <- length(mod1_ndvi)
nd2 <- length(mod2_ndvi)
nd3 <- length(mod3_ndvi)
summary(mod1_ndvi)
summary(mod2_ndvi)
summary(mod3_ndvi)
summary(mod1_ndvi)$coefficients
summary(mod2_ndvi)$coefficients
summary(mod3_ndvi)$coefficients

##residuos
resid(mod1_ndvi)
resid(mod2_ndvi)
resid(mod3_ndvi)

##des.vest residuos
sqrt(sum(resid(mod1_ndvi)^2)/(nd1-2))
sqrt(sum(resid(mod2_ndvi)^2)/(nd2-2))    
sqrt(sum(resid(mod3_ndvi)^2)/(nd3-2))


##Regresion de precipitación utilizando todas las variables
mod1_unido <- lm(pre_mayo~., data = data_unida[-c(1,3,4,10)])
mod2_unido <- lm(pre_junio~., data = data_unida[-c(1,2,4,10)])
mod3_unido<- lm(pre_julio~., data = data_unida[-c(1,2,3,10)])
m1 <- length(mod1_unido)
m2 <- length(mod2_unido)
m3 <- length(mod3_unido)
summary(mod1_unido)
summary(mod2_unido)
summary(mod3_unido)
summary(mod1_unido)$coefficients
summary(mod2_unido)$coefficients
summary(mod3_unido)$coefficients

##residuos
resid(mod1_unido)
resid(mod2_unido)
resid(mod3_unido)

##des.vest residuos
sqrt(sum(resid(mod1_unido)^2)/(m1-2))
sqrt(sum(resid(mod2_unido)^2)/(m2-2))    
sqrt(sum(resid(mod3_unido)^2)/(m3-2))


library(ggplot2)
##raficas modelos de regresion multiples
ggplot(mod1, aes(pre_mayo,dem)) +
  geom_point() +
  geom_smooth(method ='lm', formula=y~x)+
  theme_light()+
  annotate("text", x=20, y=10, label="R^2==0.009", parse = TRUE)
