
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

source('crs_crean_fcn.R')

#########################################################################################
########### read in native forest law dataframe
#########################################################################################

#setting working directory

setwd("C:/Users/garci/Dropbox/chile_collab/input_files")

library(readxl)

# xls files downloaded from CONAF
#projects
NFL_df <- readRDS("NFL_df.rds")



#########################################################################################
###########
#########################################################################################
setwd("C:/Users/garci/Dropbox/chile_collab")

# create list of all files with .shp extension in PROPIEDADES_RURALES folder
file_list <- list.files("PROPIEDADES_RURALES", pattern = "*shp", full.names = TRUE)
# read in all files in the list using read_sf
shapefile_list <- lapply(file_list, read_sf)

# bind rows into df then to sf object
propiedadesrurales <- sf::st_as_sf(bind_rows(shapefile_list)) %>%
  rename(rptpre_rol = rol)%>%
  mutate(area_ha = as.numeric(units::set_units(st_area(.), ha)))%>%
  select(desccomu, rptpre_rol, area_ha)

#########################################################################################
###########
#########################################################################################

prop_rural <- st_read(
  "C:/Users/garci/Dropbox/chile_collab/prop_rural.shp"
)%>%
  rename(rptpre_rol = ROL, desccomu = DESCCOMU)%>%
  st_transform(crs = st_crs(propiedadesrurales))%>%
  mutate(area_ha = as.numeric(units::set_units(st_area(.), ha)),
         desccomu = tolower(accents(desccomu)))

#########################################################################################
########### combine both sets of property boundaries
#########################################################################################


all_rural_props <- prop_rural %>%  
  select(desccomu, rptpre_rol, area_ha) %>%
  rbind(propiedadesrurales)%>%
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

spatial_match <- all_rural_props %>%
  st_join(enrolled_coordinadas)%>%
  mutate(area_diff = abs(area_ha-rptpre_superficie_predial),
         area_prop = area_diff/rptpre_superficie_predial,
         treat = 1
  )%>%
  rename(rptpre_rol = rptpre_rol.y)%>%
  select(colnames(match_by_rol))%>%
  drop_na(rptpro_id)



