library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(stringdist)
library(rio)
source(here::here("data_prep", "crs_clean_fcn.R"))

my_data_dir <- here::here("remote")
output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

select <- dplyr::select

# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in property_df (output from admin_combine-1.R)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
property_df <- readRDS(paste0(output_dir, "/property_df.rds"))

property_rols <- property_df %>% select(ROL, rptpro_id, rptpre_id, rptpre_comuna, rptpre_superficie_predial, rptpro_ano)%>%
  rename(pre_comuna = rptpre_comuna,
         pre_area = rptpre_superficie_predial)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in shapefiles from prop_rural and CIREN
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create list of all files with .shp extension in PROPIEDADES_RURALES folder
file_list <- list.files(paste0(my_data_dir, "/external_data/ciren_simef/PROPIEDADES_RURALES"), 
                        pattern = "*shp", full.names = TRUE)
# read in all files in the list using read_sf
shapefile_list <- lapply(file_list, st_read)

# bind rows into df, then to sf object
propiedadesrurales <- st_make_valid(
  sf::st_as_sf(bind_rows(shapefile_list))
  ) %>%
  separate(rol, into = c("rol1", "rol2"))%>%
  mutate(polyarea = as.numeric(units::set_units(st_area(.), ha)),
         source = "ciren_simef",
         src_prty = 1,
         ROL = paste0(rol1, "-", rol2))%>%
  select(desccomu, ROL, polyarea, source, src_prty)

prop_rural <- st_make_valid(st_read(
  paste0(my_data_dir, "/data/prop_rural.shp")
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
#### join enrollees with property boundaries via rol IDs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_match_rol <- all_rural_props %>%
  inner_join(property_rols, by = "ROL")%>%
  mutate(area_diff = abs(polyarea - pre_area),
         match_mthd = "ROL",
         match_prty = 1) %>%
  filter(stringdist(accents(tolower(pre_comuna)), desccomu, method = "dl") <= 1 )%>%
  group_by(rptpre_id, ROL, pre_comuna)%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  slice_head()

length(unique(property_match_rol$rptpro_id)) / length(unique(property_rols$rptpro_id)) 

st_write(property_match_rol, 
         paste0(output_dir, "/property_match_rol.shp"),
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
  select(ROL, rptpro_id, rptpre_id, pre_comuna, pre_area, rptpro_ano)

property_match_spatial <- st_make_valid(all_rural_props) %>%
  rename(ROL_poly = ROL)%>%
  st_join(enrolled_coordinadas)%>%
  mutate(area_diff = abs(polyarea-pre_area),
         match_mthd = "spatial",
         match_prty = 0
  )%>%
  drop_na(rptpro_id)%>%
  filter(stringdist(accents(tolower(pre_comuna)), desccomu, method = "dl") <= 1 )%>%
  group_by(rptpre_id, ROL, pre_comuna)%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  slice_head()

st_write(property_match_spatial, 
         paste0(output_dir, "/property_match_spatial.shp"),
         driver = "ESRI Shapefile")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### combining spatial and rol matches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boundary_match <- rbind(property_match_spatial , property_match_rol %>% mutate(ROL_poly = NA) ) 

property_match <- boundary_match %>%
  group_by(rptpre_id, ROL, pre_comuna)%>%
  filter(area_diff == min(area_diff) | match_prty == 1)%>%
  filter(match_prty == max(match_prty))%>%
  filter(area_diff == min(area_diff))%>%
  slice_head()%>%
  filter(area_diff <= 200)

st_write(property_match, 
         paste0(output_dir, "/property_match.shp"),
         driver = "ESRI Shapefile")
  