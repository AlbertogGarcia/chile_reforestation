library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(stringdist)
library(rio)
source(here::here("data_prep", "crs_clean_fcn.R"))

my_data_dir <- here::here("remote")
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")


CONAF_dir <- here::here("CONAF")

select <- dplyr::select

# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in property_df 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
property_df <- readRDS(paste0(clean_data_dir, "/property_df.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### read in shapefiles from prop_rural and CIREN
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create list of all files with .shp extension in PROPIEDADES_RURALES folder
file_list <- list.files(paste0(my_data_dir, "/external_data/ciren_simef/PROPIEDADES_RURALES"), 
                        pattern = "*shp", full.names = TRUE)
# read in all files in the list using read_sf
shapefile_list <- lapply(file_list, st_read)

# bind rows into df, then to sf object
propiedadesrurales_ciren <- st_make_valid(
  sf::st_as_sf(bind_rows(shapefile_list))
) %>%
  mutate(polyarea = as.numeric(units::set_units(st_area(.), ha)),
         source = "ciren_simef",
         src_prty = 1)%>%
  select(desccomu, rol, polyarea, source, src_prty)

propiedadesrurales_modROL <- propiedadesrurales_ciren %>%
  separate(rol, into = c("rol1", "rol2"))%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         rol_prty_ciren = 0)%>%
  select(-c(rol1, rol2))

propiedadesrurales <- propiedadesrurales_ciren %>%
  rename(ROL = rol)%>%
  mutate(rol_prty_ciren = 1)%>%
  rbind(propiedadesrurales_modROL)


prop_rural_ciren <- st_make_valid(st_read(
  paste0(my_data_dir, "/data/prop_rural.shp")
))%>%
  rename(desccomu = DESCCOMU)%>%
  mutate(polyarea = as.numeric(units::set_units(st_area(.), ha)),
         source = "prop_rural",
         src_prty = 0)

prop_rural_modROL <- prop_rural_ciren %>%
  separate(ROL, into = c("rol1", "rol2"))%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         rol_prty_ciren = 0)%>%
  select(-c(rol1, rol2))

prop_rural <- prop_rural_ciren %>%
  mutate(rol_prty_ciren = 1)%>%
  rbind(prop_rural_modROL)%>%
  st_transform(crs = st_crs(propiedadesrurales))%>%
  select(colnames(propiedadesrurales))

all_rural_props <- rbind(propiedadesrurales, prop_rural)%>%
  mutate(desccomu = tolower(accents(desccomu)))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### join enrollees with property boundaries via rol IDs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_match_ROL <- all_rural_props %>%
  inner_join(property_df %>% select(ROL, rptpro_id, rptpre_id, rptpre_comuna, rptpre_superficie_predial, rptpro_ano)
             , by = "ROL")%>%
  mutate(rol_prty = 0)

enrolled_match_rptpre_rol <- all_rural_props %>%
  inner_join(property_df %>% select(rptpre_rol, rptpro_id, rptpre_id, rptpre_comuna, rptpre_superficie_predial, rptpro_ano) %>% rename(ROL = rptpre_rol) 
             , by = "ROL")%>%
  mutate(rol_prty = 1)
  
enrolled_match_rol <- rbind(enrolled_match_ROL,enrolled_match_rptpre_rol) %>%
  mutate(area_diff = abs(polyarea - rptpre_superficie_predial),
         match_mthd = "ROL",
         match_prty = 1) %>%
  filter(stringdist(accents(tolower(rptpre_comuna)), desccomu, method = "dl") <= 1 )%>%
  group_by(rptpre_id, ROL, rptpre_comuna)%>%
  filter(stringdist(accents(tolower(rptpre_comuna)), desccomu, method = "dl") == min(stringdist(accents(tolower(rptpre_comuna)), desccomu, method = "dl")))%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  filter((rol_prty + rol_prty_ciren) == max((rol_prty + rol_prty_ciren)))%>%
  slice_head()


length(unique(enrolled_match_rol$rptpro_id)) / length(unique(property_df$rptpro_id)) 



st_write(enrolled_match_rol %>% rename(pre_comuna = rptpre_comuna) %>% select(ROL, rptpro_id, rptpre_id, pre_comuna, rptpro_ano, area_diff), 
         paste0(clean_data_dir, "/enrolled_match_rol.shp"),
         driver = "ESRI Shapefile",
         append=FALSE)



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### spatial matches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
coordinadas_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/coordinadas_predio.xlsx"))



enrolled_coordinadas <- property_df %>%
  left_join(coordinadas_df, by = "rptpre_id")%>%
  left_join(rodal_df, by = "rptpre_id")%>%
  mutate(datum = ifelse(is.na(rptro_datum), rptub_datum, rptro_datum),
         easting = ifelse(is.na(rptro_este), rptub_este, rptro_este),
         northing = ifelse(is.na(rptro_norte), rptub_norte, rptro_norte),
         huso = ifelse(is.na(rptro_huso), rptub_huso, rptro_huso))%>%
  crs_clean_fcn(.)%>%
  st_transform(crs = st_crs(propiedadesrurales))%>%
  select(ROL, rptpro_id, rptpre_id, rptpre_comuna, rptpre_superficie_predial, rptpro_ano)

enrolled_match_spatial <- st_make_valid(
  all_rural_props %>% filter(rol_prty_ciren == 1)
) %>%
  rename(ROL_poly = ROL)%>%
  st_join(enrolled_coordinadas)%>%
  mutate(area_diff = abs(polyarea-rptpre_superficie_predial),
         match_mthd = "spatial",
         match_prty = 0
  )%>%
  drop_na(rptpro_id)%>%
  filter(stringdist(accents(tolower(rptpre_comuna)), desccomu, method = "dl") <= 1 )%>%
  group_by(rptpre_id, ROL, rptpre_comuna)%>%
  filter(area_diff == min(area_diff))%>%
  filter(src_prty == max(src_prty))%>%
  slice_head()

st_write(enrolled_match_spatial %>% rename(pre_comuna = rptpre_comuna) %>% select(ROL, rptpro_id, rptpre_id, pre_comuna, rptpro_ano, area_diff, ROL_poly), 
         paste0(clean_data_dir, "/enrolled_match_spatial.shp"),
         driver = "ESRI Shapefile",
         append = FALSE)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### combining spatial and rol matches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bindnames <- c("ROL", "rptpro_id", "rptpre_id", "rptpre_comuna", "rptpro_ano", "area_diff", "match_prty", "polyarea")

boundary_match <- rbind(enrolled_match_spatial %>% select(bindnames), 
                        enrolled_match_rol %>% select(bindnames)
) 

enrolled_match <- boundary_match %>%
  group_by(rptpre_id, ROL, rptpre_comuna)%>%
  filter(area_diff == min(area_diff) | match_prty == 1)%>%
  filter(match_prty == max(match_prty))%>%
  filter(area_diff == min(area_diff))%>%
  slice_head()%>%
  filter(area_diff <= 200)

length(unique(enrolled_match$rptpro_id)) / length(unique(property_df$rptpro_id)) 
