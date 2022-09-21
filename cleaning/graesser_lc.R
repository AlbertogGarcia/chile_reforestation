library(terra)
library(parallel)
library(tidyverse)
library(here)
library(sf)
library(sjmisc)
library(rlist)
library(data.table)
setwd("C:/Users/garci/Dropbox/chile_reforestation/")

graesser_annual <- terra::rast("data/graesser_lc/Chile_MOSAIC_99-19.tif")
                                  
lu2001_rast <- terra::rast("data/lu_2001/lu_2001.tif")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_list <- list.files("external_data/ciren_simef/PROPIEDADES_RURALES", pattern = "*shp", full.names = TRUE)

roi <- c("ARAUCANÍA", "MAULE", "LOS_RÍOS", "LOS_LAGOS", "O_HIGGINS", "BIOBÍO")
file_list_roi <- list.filter(file_list, str_contains(., roi, logic = "OR"))

sf_list <- lapply(file_list_roi, st_read) # read in all files as sf objects
sf.obj <- sf_list[[1]]
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Graesser et al., 2022 land uses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
year_cols <- paste0("clas_", seq(from = 1999, to = 2018))

graesser_extract_fcn <- function(sf.obj, id_name){
  
 sf.obj <- sf.obj[1:100,]
  
  terra::extract(
    graesser_annual,
    terra::vect(
      sf.obj %>% st_transform(terra::crs(graesser_annual))
    )
    ,
    na.rm=TRUE,
    weights = T
  )%>%
    set_names(c("ID", year_cols, "weight"))%>% 
    pivot_longer(clas_1999:clas_2018) %>%
    group_by(ID, name, value)%>%
    summarize(class_coverage = sum(weight, na.rm = T)) %>%
    ungroup()%>%
    pivot_wider(names_from = 3, values_from = class_coverage)%>%
    inner_join(
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(id_name, ID)
      , by = "ID")

}

extracted_lc <- purrr::map(sf_list, ~graesser_extract_fcn(., "objectid"))%>%
  rbindlist(idcol = "list_ID", fill = TRUE)

lc_annual <- extracted_lc %>%
  replace(is.na(.), 0) %>%
  rename(forest = "144",
         crop = "1",
         grassland  = "176",
         shrub  = "152",
         wetland  = "195",
         water = "111",
         bare = "131",
         development = "124") %>%
  dplyr::select(list_ID, objectid, name, everything())%>%
  mutate(polyarea = rowSums(.[, 5:ncol(extracted_lc)]))

library(rio)
export(lc_annual, "data/analysis_lc/cleaned_properties/annual_lc_neverenrolled.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Running for all enrolled properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_match_list <- list("data/analysis_lc/matched_properties/property_match_rol.shp",
                            "data/analysis_lc/matched_properties/property_match_spatial.shp")
sf_list_match <- lapply(property_match_list, st_read)

id_cols <- c("ROL", "rptpre_id", "rptpre_comuna")

extracted_lc_match <- purrr::map(sf_list, ~graesser_extract_fcn(., id_cols))%>%
  rbindlist(idcol = "list_ID", fill = TRUE)

lc_annual_match <- extracted_lc %>%
  replace(is.na(.), 0) %>%
  rename(forest = "144",
         crop = "1",
         grassland  = "176",
         shrub  = "152",
         wetland  = "195",
         water = "111",
         bare = "131",
         development = "124") %>%
  dplyr::select(list_ID, id_cols, name, everything())%>%
  mutate(polyarea = rowSums(.[, (length(id_cols)+3):ncol(extracted_lc)]))

export(lc_annual_match, "data/analysis_lc/cleaned_properties/lc_annual_enrolled.rds")
