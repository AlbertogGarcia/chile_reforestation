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

select <- dplyr::select


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### lu2001 extraction function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lu2001_rast <- terra::rast("data/lu_2001/lu_2001.tif")

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
##### Graesser et al., 2022 land use function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
graesser_annual <- terra::rast("data/graesser_lc/Chile_MOSAIC_99-19.tif")

year_cols <- seq(from = 1999, to = 2018)

class_number <- c(1, 111, 124, 131, 138, 144, 152, 176, 195)
class_name <- c("Crop", "Water", "Development", "Bare", "Plantations", "Trees", "Shrubs", "Grassland", "Wetland") 

graesser_extract_fcn <- function(sf.obj, id_cols){
  tic()
  sf.obj <- sf.obj[1:1000,]
  
  temp <- terra::extract(
    graesser_annual,
    terra::vect(
      sf.obj %>% st_transform(terra::crs(graesser_annual))
    )
    ,
    na.rm=TRUE,
    weights = T
  )%>%
    set_names(c("ID", year_cols, "weight"))%>%
    pivot_longer("1999":"2018", names_to = "year", values_to = "class") 
  
  return <- temp %>%
    group_by(ID, year, class)%>%
    summarize(class_coverage = sum(weight, na.rm = T)) %>%
    ungroup()%>%
    mutate(class = c(class_name, class)[match(class, c(class_number, class))])%>%
    pivot_wider(names_from = c("class", "year"), values_from = class_coverage) %>%
    inner_join(
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(id_cols, ID)
      , by = "ID")
  
  return$poly_pixarea = rowSums(dplyr::select(return, ends_with("2009")), na.rm = T)
  
  toc()
  return(return)
  
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
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### unenrolled properties
file_list <- list.files("external_data/ciren_simef/PROPIEDADES_RURALES", pattern = "*shp", full.names = TRUE)

roi <- c("ARAUCANÍA", "MAULE", "LOS_RÍOS", "LOS_LAGOS", "O_HIGGINS", "BIOBÍO")
file_list_roi <- list.filter(file_list, str_contains(., roi, logic = "OR"))
sf_list <- lapply(file_list_roi, st_read) # read in all files as sf objects

### enrolled properties
property_match_list <- list("data/analysis_lc/matched_properties/property_match_rol.shp",
                            "data/analysis_lc/matched_properties/property_match_spatial.shp")
sf_list_match <- lapply(property_match_list, st_read)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Running 3 functions for all CIREN properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# library(furrr)
# future::plan(multisession, workers = 6)

id_cols_ciren <- c("objectid", "regidere")

### extracting 2001 land uses
extracted_lu2001 <- map_dfr(sf_list, ~lu2001_fcn(., id_cols_ciren), .id = "list_ID")%>%
  replace(is.na(.), 0)%>%
  dplyr::select(list_ID, ID, id_cols_ciren, everything())%>%
  mutate(polypixels = rowSums(.[, (length(id_cols_ciren) + 3):ncol(.)]))

### extracting Graesser 2022 land uses
extracted_graesser <- purrr::map_dfr(sf_list, ~graesser_extract_fcn(., id_cols_ciren), .id = "list_ID")%>%
  replace(is.na(.), 0)

### getting other covariates
covariates <- purrr::map_dfr(sf_list, ~return_covs_fcn(., id_cols_ciren), .id = "list_ID")%>%
  left_join(extracted_lu2001, by = c("list_ID", id_cols_ciren))%>%
  left_join(extracted_graesser, by = c("list_ID", id_cols_ciren))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Running for all enrolled properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

id_cols_ciren <- c("ROL", "rptpre_id", "comuna")

### extracting 2001 land uses
extracted_lu2001_match <- map_dfr(sf_list_match, ~lu2001_fcn(., id_cols_ciren), .id = "list_ID")%>%
  replace(is.na(.), 0)%>%
  dplyr::select(list_ID, ID, id_cols_ciren, everything())%>%
  mutate(polypixels = rowSums(.[, (length(id_cols_ciren) + 3):ncol(.)]))

### extracting Graesser 2022 land uses
extracted_graesser_match <- purrr::map_dfr(sf_list_match, ~graesser_extract_fcn(., id_cols), .id = "list_ID")%>%
  replace(is.na(.), 0)

### getting other covariates
covariates_enrolled <- purrr::map_dfr(sf_list_match, ~return_covs_fcn(., id_cols_ciren), .id = "list_ID")%>%
  left_join(extracted_lu2001_match, by = c("list_ID", id_cols_ciren))%>%
  left_join(extracted_graesser_match, by = c("list_ID", id_cols_ciren))

covariates_neverenrolled <- covariates[!lengths(st_intersects(covariates, covariates_enrolled)), ]

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Reading in all EVI data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_regions <- data.frame("files" = file_list_roi)%>%
  rowid_to_column("list_ID")%>%
  separate(files, sep = "/", into = c(rep(NA, 3), "list_region"))%>%
  mutate(list_region = substr(list_region, 1, nchar(list_region) - 15))


# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/losrios_evi", pattern="*.csv", full.names =  T)
losrios_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "LOS_RÍOS")

# load los lagos evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/loslagos_evi", pattern="*.csv", full.names =  T)
loslagos_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "LOS_LAGOS")

# load BioBio evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/biobio_evi", pattern="*.csv", full.names =  T)
biobio_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "BIOBÍO")

# load O'Higgins evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/ohiggins_evi", pattern="*.csv", full.names =  T)
ohiggins_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "LIBERTADOR_GENERAL_BERNARDO_O_HIGGINS")

# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/maule_evi", pattern="*.csv", full.names =  T)
maule_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "MAULE")

# load ARAUCANÍA evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/araucania_evi", pattern="*.csv", full.names =  T)
araucania_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "ARAUCANÍA")

# load Ñuble evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/nuble_evi", pattern="*.csv", full.names =  T)
nuble_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(list_region = "BIOBÍO")

ciren_evi <- rbind(losrios_evi, loslagos_evi, ohiggins_evi, biobio_evi, araucania_evi, maule_evi, nuble_evi)%>%
  inner_join(list_regions, by = "list_region")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Read in EVI for enrolled properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
match_evi <- read_csv("data/property_ndvi/new_CIREN_ndvi/match_evi/match_evi")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Combine EVI data with covariates and land classification data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_neverenrolled <- covariates_neverenrolled %>%
  mutate_at(vars(list_ID), as.integer)%>%
  left_join(ciren_evi, by = c("list_ID", "objectid"))

data_enrolled <- covariates_enrolled %>%
  mutate_at(vars(list_ID), as.integer)%>%
  left_join(match_evi, by = c("list_ID", id_cols))
