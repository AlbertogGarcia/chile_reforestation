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
