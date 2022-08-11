
### loads packages
library(latex2exp)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(stringi)
library(data.table)
library(kableExtra)
library(stringdist)
library(GADMTools)
library(lwgeom)
library(units)
library(fuzzyjoin)
library(Metrics)
library(DataCombine)
library(Hmisc)

source(here::here("crs_clean_fcn.R"))

#########################################################################################
########### read in native forest law dataframe
#########################################################################################

#setting working directory
setwd("C:/Users/garci/Dropbox/chile_collab")
#setwd("/Users/tiffanyhsu/Dropbox/chile_collab")

library(readxl)

# xls files downloaded from CONAF
#projects
NFL_df <- readRDS("input_files/NFL_df.rds")

library(readxl)
coordinadas_df <- read_xlsx("concurso_conaf/coordinadas_predio.xlsx")

#stands
rodal_df <- read_xlsx("concurso_conaf/rodal.xlsx")


#########################################################################################
###########
#########################################################################################
setwd("C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef")
# create list of all files with .shp extension in PROPIEDADES_RURALES folder
file_list <- list.files("PROPIEDADES_RURALES", pattern = "*shp", full.names = TRUE)
# read in all files in the list using read_sf
shapefile_list <- lapply(file_list, read_sf)

# bind rows into df then to sf object
propiedadesrurales <- st_make_valid(sf::st_as_sf(bind_rows(shapefile_list))) %>%
  rename(rptpre_rol = rol)%>%
  mutate(area_ha = as.numeric(units::set_units(st_area(.), ha)),
         source = "ciren_simef")%>%
  select(desccomu, rptpre_rol, area_ha, source)

#########################################################################################
###########
#########################################################################################
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

setwd("C:/Users/garci/Dropbox/chile_reforestation/data")

prop_rural <- st_make_valid(st_read(
  "prop_rural.shp"
))%>%
  rename(rptpre_rol = ROL, desccomu = DESCCOMU)%>%
  #st_transform(crs = st_crs(propiedadesrurales))%>%
  mutate(area_ha = as.numeric(units::set_units(st_area(.), ha)),
         desccomu = tolower(accents(desccomu)),
         source = "prop_rural")%>%
  st_transform(crs = st_crs(propiedadesrurales))

#########################################################################################
########### combine both sets of property boundaries
#########################################################################################

all_rural_props <- prop_rural %>%  
  select(desccomu, rptpre_rol, area_ha, PROPIETARI, NOM_PREDIO, source) %>%
  bind_rows(propiedadesrurales)%>%
  mutate(desccomu = tolower(accents(desccomu)))

#########################################################################################
########### perform spatial match
#########################################################################################

enrolled_coordinadas <- NFL_df %>%
  left_join(coordinadas_df, by = "rptpre_id")%>%
  left_join(rodal_df, by = "rptpre_id")%>%
  mutate(datum = ifelse(is.na(rptro_datum), rptub_datum, rptro_datum),
         easting = ifelse(is.na(rptro_este), rptub_este, rptro_este),
         northing = ifelse(is.na(rptro_norte), rptub_norte, rptro_norte),
         huso = ifelse(is.na(rptro_huso), rptub_huso, rptro_huso))%>%
  crs_clean_fcn(.)%>%
  st_transform(crs = st_crs(all_rural_props))

# generate buffer around point coordinates

enrolled_buffer <- st_buffer(enrolled_coordinadas, 250)

# spatial match based on buffer  

spatial_match_buffer <- st_make_valid(all_rural_props) %>%
  st_join(enrolled_buffer)%>%
  mutate(area_diff = abs(area_ha - rptpre_superficie_predial)
  )%>%
  drop_na(rptpro_id)

# spatial match based on point coordinates  

spatial_match <- all_rural_props %>%
  st_join(enrolled_coordinadas)%>%
  mutate(area_diff = abs(area_ha-rptpre_superficie_predial)
  )%>%
  drop_na(rptpro_id)

# get distinct property boundaries for each project

spatial_unique <- spatial_match_buffer %>%
  select(rptpro_id, rptpre_rol.x, rptpre_rol.y, area_diff, area_ha, rptpre_superficie_predial, PROPIETARI, rptprop_nombre, NOM_PREDIO, rptpre_nombre, source) %>%
  #distinct(rptpro_id, NOM_PREDIO, area_ha, .keep_all=TRUE)
  unique(by = c("rptpro_id", "NOM_PREDIO", "area_ha", "source"))

# sorting by ID & area differences, then taking top 5 closest properties
spatial_cleaned <- spatial_unique %>% 
  arrange(rptpro_id, area_diff) %>% 
  group_by(rptpro_id, source) %>% 
  slice(1:5)%>%
  ungroup()%>%
  group_by(rptpro_id)%>%
  mutate(rol_verified = ifelse(rptpre_rol.x == rptpre_rol.y, 1, 0),
         min_area_diff = min(area_diff)
         ) %>%
  ungroup() %>%
  mutate(id = 1:nrow(.),
         priority_rank = dense_rank(desc(min_area_diff)))

spatial_names <- spatial_cleaned %>%
  drop_na(NOM_PREDIO)%>%
  filter(is.na(match_verified))%>%
  select(id, priority_rank, area_diff, PROPIETARI, rptprop_nombre, NOM_PREDIO, rptpre_nombre, match_verified)%>%
  arrange(priority_rank)
spatial_names$geometry <- NULL

#write.csv(spatial_names, "spatial_names.csv")

### read in spatial names with verified matches
setwd("C:/Users/garci/Dropbox/chile_collab")
spatial_names_updated <- read.csv("verification/spatial_names.csv") %>%
  select(match_verified, id)

spatial_df <- spatial_cleaned %>%
  mutate(min_area_diff_verified = ifelse(min_area_diff == area_diff, 1, 0))%>%
  left_join(spatial_names_updated, by = "id") %>%
  filter(rol_verified == 1 | match_verified == 1 | min_area_diff_verified == 1) %>%
  mutate(rol_points = ifelse(rol_verified == 1, 1, 0),
         match_points = ifelse(match_verified == 1, 1, ifelse(match_verified == "P", 0.1, 0)),
         area_points = ifelse(min_area_diff_verified == 1, 1, 0),
         source_points = ifelse(source == "ciren_simef", 1, 0),
         match_type = "spatial")%>%
  mutate_at(vars(rol_points, match_points, area_points), ~replace(., is.na(.), 0))%>%
  mutate(ovr_match_points = 1.5*rol_points + 1*area_points + 1.25*match_points + 1*source_points) %>%
  group_by(rptpro_id)%>%
  filter(ovr_match_points == max(ovr_match_points))%>%
  slice(1:1)%>%
  select(PROPIETARI, NOM_PREDIO, rptpre_nombre, rptprop_nombre, rptpro_id, rptpre_rol.x, area_ha, area_diff, match_type)%>%
  rename(rptpre_rol = rptpre_rol.x)



rol_match_df <- all_rural_props %>%
  inner_join(NFL_df, by = "rptpre_rol")%>%
  mutate(comuna_stringd = stringdist(tolower(rptpre_comuna), desccomu, method = "dl")) %>%
  filter(comuna_stringd <= 1) %>%
  mutate(match_type = "rol",
         source_points = ifelse(source == "ciren_simef", 1, 0),
         area_diff = abs(area_ha-rptpre_superficie_predial)) %>%
  group_by(rptpro_id)%>%
  filter(area_diff == max(area_diff))%>%
  filter(source_points == max(source_points))%>%
  slice(1:1)%>%
  select(PROPIETARI, NOM_PREDIO, rptpre_nombre, rptprop_nombre, rptpro_id, rptpre_rol, area_ha, area_diff, match_type)

proyecto_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/proyecto.xlsx")

flora_df <- rbind(rol_match_df, spatial_df)%>%
  mutate(preferred_match_type = ifelse(match_type == "rol", 1, 0))%>%
  group_by(rptpro_id)%>%
  filter(preferred_match_type == max(preferred_match_type))%>%
  slice(1:1)%>%
  select(-c(preferred_match_type))%>%
  inner_join(proyecto_df, by = "rptpro_id")
library(rio)
st_write(flora_df, "flora_df.shp")

median(flora_df$area_diff)
  




library(rio)

export(spatial_df, "spatial_df.rds")
export(rol_match_df, "rol_match_df.rds")

rol_match_geoms <- rol_match_df %>%
select(rptpro_id)

spatial_match_geoms <- spatial_df %>%
  select(rptpro_id)

st_write(rol_match_geoms, dsn = "rol_match_geoms.shp", driver = "ESRI Shapefile")

st_write(spatial_match_geoms, dsn = "spatial_match_geoms.shp", driver = "ESRI Shapefile")