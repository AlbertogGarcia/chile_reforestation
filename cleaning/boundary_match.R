library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(stringdist)
source(here::here("cleaning", "crs_clean_fcn.R"))

setwd("C:/Users/garci/Dropbox/chile_reforestation")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx("external_data/concurso_conaf/program/proyecto.xlsx")

#properties and coordinates
predio_df <- read_xlsx("external_data/concurso_conaf/program/predio.xlsx")
#owners
propietario_df <- read_xlsx("external_data/concurso_conaf/program/propietario.xlsx")
#stands
rodal_df <- read_xlsx("external_data/concurso_conaf/program/rodal.xlsx")
#activities
actividades_df <- read_xlsx("external_data/concurso_conaf/program/actividad.xlsx")

coordinadas_df <- read_xlsx("external_data/concurso_conaf/program/coordinadas_predio.xlsx")

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props")
  )%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))

df_2019_smallholder <- property_df %>% filter(rptpro_ano == 2019 & rptpro_tipo_concurso != "Otros Interesados" )
hist(df_2019_smallholder$rptpre_superficie_predial)
length(unique(df_2019_smallholder$rptpre_id))
length(unique(subset(df_2019_smallholder, between(rptpre_superficie_predial, 10, 25 ))$rptpre_id))



property_rols <- property_df %>% select(ROL, rptpro_id, rptpre_id, rptpre_comuna, rptpre_superficie_predial, rptpro_ano)%>%
  rename(comuna = rptpre_comuna,
         pre_area = rptpre_superficie_predial)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in shapefiles from prop_rural and CIREN
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create list of all files with .shp extension in PROPIEDADES_RURALES folder
file_list <- list.files("external_data/ciren_simef/PROPIEDADES_RURALES", pattern = "*shp", full.names = TRUE)
# read in all files in the list using read_sf
shapefile_list <- lapply(file_list, st_read)

# bind rows into df then to sf object
propiedadesrurales <- st_make_valid(sf::st_as_sf(bind_rows(shapefile_list))) %>%
  separate(rol, into = c("rol1", "rol2"))%>%
  mutate(polyarea = as.numeric(units::set_units(st_area(.), ha)),
         source = "ciren_simef",
         src_prty = 1,
         ROL = paste0(rol1, "-", rol2))%>%
  select(desccomu, ROL, polyarea, source, src_prty)

prop_rural <- st_make_valid(st_read(
  "data/prop_rural.shp"
))%>%
  separate(ROL, into = c("rol1", "rol2"))%>%
  rename(desccomu = DESCCOMU)%>%
  mutate(polyarea = as.numeric(units::set_units(st_area(.), ha)),
         source = "prop_rural",
         src_prty = 0,
         ROL = paste0(rol1, "-", rol2))%>%
  st_transform(crs = st_crs(propiedadesrurales))%>%
  select(colnames(propiedadesrurales))

all_rural_props <- rbind(propiedadesrurales, prop_rural)%>%
  mutate(desccomu = tolower(accents(desccomu)))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### join enrollees with property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_area_difference = 100

property_match_rol <- all_rural_props %>%
  inner_join(property_rols, by = "ROL")%>%
  mutate(area_diff = abs(polyarea - pre_area),
         match_mthd = "ROL",
         match_prty = 1) %>%
  filter(stringdist(accents(tolower(comuna)), desccomu, method = "dl") <= 1 & area_diff <= max_area_difference)%>%
  group_by(rptpre_id, ROL, comuna)%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  slice_head()

length(unique(property_match_rol$rptpro_id)) / length(unique(property_rols$rptpro_id)) 

st_write(property_match_rol, "data/analysis_lc/matched_properties/property_match_rol.shp",
         driver = "ESRI Shapefile")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### spatial matches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_coordinadas <- property_rols %>%
  left_join(coordinadas_df, by = "rptpre_id")%>%
  left_join(rodal_df, by = "rptpre_id")%>%
  mutate(datum = ifelse(is.na(rptro_datum), rptub_datum, rptro_datum),
         easting = ifelse(is.na(rptro_este), rptub_este, rptro_este),
         northing = ifelse(is.na(rptro_norte), rptub_norte, rptro_norte),
         huso = ifelse(is.na(rptro_huso), rptub_huso, rptro_huso))%>%
  crs_clean_fcn(.)%>%
  st_transform(crs = st_crs(propiedadesrurales))%>%
  select(ROL, rptpro_id, rptpre_id, comuna, pre_area, rptpro_ano)

property_match_spatial <- st_make_valid(all_rural_props) %>%
  rename(ROL_poly = ROL)%>%
  st_join(enrolled_coordinadas)%>%
  mutate(area_diff = abs(polyarea-pre_area),
         match_mthd = "spatial",
         match_prty = 0
  )%>%
  drop_na(rptpro_id)%>%
  filter(stringdist(accents(tolower(comuna)), desccomu, method = "dl") <= 1 & area_diff <= max_area_difference)%>%
  group_by(rptpre_id, ROL, comuna)%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  slice_head()

st_write(property_match_spatial, "data/analysis_lc/matched_properties/property_match_spatial.shp")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### combining spatial and rol matches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boundary_match <- rbind(property_match_spatial , property_match_rol %>% mutate(ROL_poly = NA) ) 

property_map <- boundary_match %>%
  group_by(rptpre_id, ROL, comuna)%>%
  filter(area_diff == min(area_diff) | match_prty == 1)%>%
  select(rptpro_ano)

st_write(property_map, "data/analysis_lc/matched_properties/property_map.shp")
