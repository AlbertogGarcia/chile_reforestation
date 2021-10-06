library(tidyverse)
library(sf)
library(ggplot2)

#setwd("C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/EROSION")

file_list <- list.files("C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/EROSION/EROSION_ACTUAL", pattern = "*shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

erosion_actual <- sf::st_as_sf(bind_rows(shapefile_list))%>%
  filter(clas_rs %in% c("EROSION MODERADA", "EROSION SEVERA", "EROSION MUY SEVERA"))%>%
  st_make_valid()

st_write(erosion_actual, "erosion_actual.shp", driver = "ESRI Shapefile")

# intersectos <- read_sf("C:/Users/garci/Documents/chile_reforestation/geoms.shp") %>%
#   st_make_valid()%>%
#   mutate(area_prop_ha = as.numeric(units::set_units(st_area(.), ha)))%>%
#   st_intersection(st_make_valid(erosion_actual))%>%
#   mutate(proportion_highrisk = as.numeric(units::set_units(st_area(.), ha))/area_prop_ha)

file_list <- list.files("C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/EROSION/RIESGO_EROSIÓN_ACTUAL", pattern = "*shp", full.names = TRUE)
shapefile_list <- lapply(file_list, read_sf)

riesgo_erosion_actual <- sf::st_as_sf(bind_rows(shapefile_list))%>%
  filter(clase %in% c("SEVERA", "MUY SEVERA", "MODERADA"))

st_write(riesgo_erosion_actual, "riesgo_erosion_actual.shp", driver = "ESRI Shapefile")

# read nc polygon data and transform to UTM 
nc <- intersectos %>%
  st_transform(32617)

# random sample of 5 points
# pts <- st_sample(nc, size = 5) %>% st_sf

# create 1km grid - here you can substitute 200 for 50000
grid_1km <- st_make_grid(nc, cellsize = c(250, 250)) %>% 
  st_sf(grid_id = 1:length(.))

# create labels for each grid_id
grid_lab <- st_centroid(grid_1km) %>% cbind(st_coordinates(.))

# view the sampled points, polygons and grid
ggplot() +
  geom_sf(data = nc, fill = 'white', lwd = 0.05) +
  #geom_sf(data = pts, color = 'red', size = 1.7) + 
  geom_sf(data = grid_1km, fill = 'transparent', lwd = 0.3) +
  geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) +
  coord_sf(datum = NA)  +
  labs(x = "") +
  labs(y = "")