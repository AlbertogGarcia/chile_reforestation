library(terra)
library(parallel)
library(tidyverse)
library(raster)
library(exactextractr)
library(here)
library(sf)
library(rlist)
library(sjmisc)
library(units)
setwd("C:/Users/garci/Dropbox/chile_reforestation/")

selsct <- dplyr::select

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
##### lu2001 extraction function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(readr)
lu2001_fcn <- function(sf.obj, id_cols){
  
  sf.obj <- sf.obj[1:1000,]
  
  terra::extract(
    lu2001_rast,
    terra::vect(
      sf.obj %>% st_transform(terra::crs(lu2001_rast))
    ),
    na.rm=TRUE,
    weights = T
  )%>%
    group_by(ID, clas) %>%
    summarize(class_coverage = sum(weight, na.rm = T)) %>%
    pivot_wider(names_from = clas, values_from = class_coverage)%>%
    ungroup()%>%
    inner_join(
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(id_cols, ID)
               , by = "ID")
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in covariates: dist to road, industry, slope, elev, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elevation <- getData('alt', country='CHL')[[1]]
terr <- terrain(elevation, opt=c('slope'), unit='degrees')
roads.sf <- st_read(
  "data/CHL_rds/CHL_roads.shp"
) %>%
  st_transform(terra::crs(lu2001_rast))%>%
  filter(RTT_DESCRI == "Primary Route" | RTT_DESCRI == "Secondary Route")

industry.sf <- st_read(
  "external_data/ciren_simef/INDUSTRIA_FORESTAL/Catastro_Industria_Forestal_Primaria_Infor_2019.shp"
) %>%
  st_transform(terra::crs(lu2001_rast))

native_especies <- c("Roble", "Tepa", "Raulí", "nativas", "Olivillo", "Mañío", "Ulmo", "Lingue", "Lenga", "Coihue", "Tineo", "Canelo", "Alerce", "Álamo")
pattern <- paste0(native_especies, collapse = "|")

native_industry.sf <- industry.sf %>%
  filter(grepl(pattern, especies, ignore.case = TRUE))

# erosion_actual.sf <- st_read(
#   "input_data/erosion_actual.shp"
# ) %>%
#   st_transform(st_crs(lu2001_rast))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### extracting, calculating covariate values by polygon function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_covs_fcn <- function(sf.obj, id_cols){
  
  sf.obj <- sf.obj[1:1000,]
  
  return_covs <- st_make_valid(sf.obj) %>%
    dplyr::select(id_cols)%>%
    st_transform(terra::crs(lu2001_rast))%>%
    mutate(elev = unlist(exact_extract(elevation, ., 'mean')),
           slope = unlist(exact_extract(terr, ., 'mean'))
    )%>%
    st_centroid()%>%
    mutate(road_dist = drop_units(st_distance(., roads.sf[st_nearest_feature(., roads.sf), ])),
           natin_dist = drop_units(st_distance(., native_industry.sf[st_nearest_feature(., native_industry.sf), ])),
           ind_dist = drop_units(st_distance(., industry.sf[st_nearest_feature(., industry.sf), ]))
    )
    
  # 
  # return_erosion <- sf.object %>%
  #   st_make_valid()%>%
  #   st_intersection(st_make_valid(erosion_actual.sf))%>%
  #   mutate(mod_to_severe_erosion_area = as.numeric(units::set_units(st_area(.), ha)))%>%  
  #   st_drop_geometry()%>%
  #   group_by(objectid, rol, desccomu)%>%
  #   mutate(mod_to_severe_erosion_area = ifelse(is.na(mod_to_severe_erosion_area), 0, mod_to_severe_erosion_area),
  #          mod_to_severe_erosion_area = sum(mod_to_severe_erosion_area))%>%
  #   distinct(objectid, rol, desccomu, .keep_all = TRUE)%>%
  #   dplyr::select(objectid, rol, desccomu, mod_to_severe_erosion_area) # only select columns needed to merge
  # 
  return_covs <- return_covs %>%
    mutate(lat = unlist(map(return_covs$geometry,1)),
           long = unlist(map(return_covs$geometry,2)))
  # %>%
  #   left_join(return_erosion, by = c("objectid", "rol", "desccomu"))%>%
  #   mutate(mod_to_severe_erosion_area = ifelse(is.na(mod_to_severe_erosion_area), 0, mod_to_severe_erosion_area), 
  #          proportion_erosion = mod_to_severe_erosion_area / property_area_ha
  #   )
  
  return(return_covs)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Running for all CIREN properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# library(furrr)
# future::plan(multisession, workers = 6)

extracted_lu2001 <- map_dfr(sf_list, ~lu2001_fcn(., "objectid"), .id = "list_ID")%>%
  replace(is.na(.), 0)%>%
  dplyr::select(list_ID, ID, objectid, everything())%>%
  mutate(polypixels = rowSums(.[, 3:ncol(.)]))
  

covariates <- purrr::map_dfr(sf_list, return_covs_fcn, .id = "list_ID")%>%
  left_join(extracted_lu2001, by = c("list_ID", "objectid"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Running for all enrolled properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sf.obj <- st_read("data/analysis_lc/matched_properties/property_match_rol.shp")
property_match_list <- list("data/analysis_lc/matched_properties/property_match_rol.shp",
                            "data/analysis_lc/matched_properties/property_match_spatial.shp")
sf_list_match <- lapply(property_match_list, st_read)

id_cols <- c("ROL", "rptpre_id", "comuna")

extracted_lu2001_match <- map_dfr(sf_list_match, ~lu2001_fcn(., id_cols), .id = "list_ID")%>%
  replace(is.na(.), 0)%>%
  dplyr::select(list_ID, ID, id_cols, everything())%>%
  mutate(polypixels = rowSums(.[, (length(id_cols) + 3):ncol(.)]))

covariates_enrolled <- purrr::map_dfr(sf_list_match, ~return_covs_fcn(., id_cols), .id = "list_ID")%>%
  left_join(extracted_lu2001_match, by = c("list_ID", id_cols))

covariates_neverenrolled <- covariates[!lengths(st_intersects(covariates, covariates_enrolled)), ]
