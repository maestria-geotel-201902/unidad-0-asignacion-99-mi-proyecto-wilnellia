library(sf)
library(tidyverse)
library(spdep)
library(lmtest)
library(tmap)
library(RColorBrewer)
library(ggplot2)
library(gstat)

# Datos que utilizaremos para el análisis  y desarrollo de nuestro trabajo:

rutaprep <- 'data/onamet_prec_anual_sf.gpkg'
rutadiv <- 'data/divisionRD.gpkg'
st_layers(rutaprep)
prep <- st_read(rutaprep)
prep

st_layers(rutadiv)
prov <- st_read(rutadiv, layer = 'PROVCenso2010')
prov
prov <- st_read(dsn = 'data/divisionRD.gpkg', layer = 'PROVCenso2010')
plot(prov['PROV'])

# Verificamos el sistema de coordenadas que poseen los datos:

st_crs(prep)
crsdestino <- 32619
preputm <- prep %>% st_transform(crs = crsdestino)
preputm

# Realizamos el Análisis Exploratorios de los Datos para el "Año 1994"

nrow(preputm)
summary(preputm$a1994)
hist(preputm$a1994)
hist(log(preputm$a1994))
shapiro.test(preputm$a1994)
shapiro.test(log(prep$a1994))

# Los datos se pueden visualizar como una distribución normal.

prep1994 <- na.omit(preputm[,c('Estación', 'a1994')])
prep1994$a1994log <- log(prep1994$a1994)
prep1994

ggplot() +
  geom_sf(data = prov, fill = 'white') +
  geom_sf(data = prep1994, aes(col = a1994log), size = 6) +
  scale_colour_gradient(low="#deebf7", high="#3182bd") +
  geom_sf_text(data = prov, aes(label=TOPONIMIA), check_overlap = T, size = 2) +
  geom_sf_text(data = prep1994, aes(label=Estación), check_overlap = T, size = 1.5) +
  theme_bw()

## Variograma muestral

v1994 <- variogram(a1994log~1, prep1994)
v1994
plot(v1994, plot.numbers = T)

# Variograma Modelo

# Generamos tres variograma modelo, y elegimos el que se utilizará en la interpolación por Kriging. 

v1994_m <- fit.variogram(v1994, vgm(model = "Sph", range = 50000))
v1994_m
plot(v1994, v1994_m, plot.numbers = T)

v1994_m2 <- fit.variogram(v1994, vgm(model = "Exp", range = 50000))
v1994_m2
plot(v1994, v1994_m2, plot.numbers = T)

v1994_m3 <- fit.variogram(v1994, vgm(model = "Gau", range = 50000))
v1994_m3
plot(v1994, v1994_m3, plot.numbers = T)

attr(v1994_m, 'SSErr') 
attr(v1994_m2, 'SSErr')
attr(v1994_m3, 'SSErr')#Elegimos este

grd <- st_bbox(prov) %>%
  st_as_stars(dx = 3000) %>% # 3000 metros=3km de resolución espacial
  st_set_crs(crsdestino) %>%
  st_crop(prov)
grd
plot(grd)

k <- krige(formula = a1994log~1, locations = prep1994, newdata = grd, model = v1994_m2)
k
plot(k)

ggplot() +
  geom_stars(data = k, aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  geom_sf(data = st_cast(prov, "MULTILINESTRING")) +
  geom_sf(data = prep1994) +
  geom_sf_text(data = prov, aes(label=TOPONIMIA), check_overlap = T, size = 2) +
  theme_bw()

# Para representar los valores de precipitación, tenemos:

ggplot() +
  geom_stars(data = exp(k), aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_gradient(low="#deebf7", high="#3182bd", trans = 'log10') +
  geom_sf(data = st_cast(prov, "MULTILINESTRING")) +
  geom_sf(data = prep1994) +
  geom_sf_text(data = prov, aes(label=TOPONIMIA), check_overlap = T, size = 2) +
  theme_bw()