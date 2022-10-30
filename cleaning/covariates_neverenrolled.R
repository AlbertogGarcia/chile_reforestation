library(terra)
library(parallel)
library(tidyverse)
library(here)
library(sf)
library(sjmisc)
library(rlist)
library(data.table)
library(tictoc)
library(exactextractr)
library(raster)
library(units)
library(nngeo)
setwd("C:/Users/garci/Dropbox/chile_reforestation/")

select <- dplyr::select

# graesser_annual <- terra::rast("data/graesser_lc/Chile_MOSAIC_99-19.tif")
unzip("data/graesser_lc/AnnualMosaics.zip", list=TRUE)
tif_file_list <- list.files("data/graesser_lc/AnnualMosaics", pattern = "*tif", full.names = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciren_path <- "external_data/ciren_simef/PROPIEDADES_RURALES"
file_list <- list.files(ciren_path, pattern = "*shp", full.names = TRUE)

roi <- c("ARAUCANÍA", "MAULE", "LOS_RÍOS", "LOS_LAGOS", "O_HIGGINS", "BIOBÍO")
file_list_roi <- list.filter(file_list, str_contains(., roi, logic = "OR"))

my_read <- function(x){
  temp <- st_read(x) %>% 
    st_make_valid()%>%
    mutate(ciren_region = substr(x, nchar(ciren_path)+2 , nchar(x) - 15),
           polyarea = st_area(.))
  
  return <- temp[!lengths(st_intersects(temp, enrolled_sf)), ]
  return(return)
}

enrolled_sf <- rbind(st_read("data/analysis_lc/matched_properties/property_match_spatial.shp"),
                     st_read("data/analysis_lc/matched_properties/property_match_rol.shp") %>% mutate(ROL_poly = NA)
)

sf_list <- lapply(file_list_roi, my_read)

# roi_sf <- sf::st_as_sf(bind_rows(sf_list)) # read in all files as sf objects and bind together

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Graesser et al., 2022 land uses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

annual_mosaic_list <- lapply(tif_file_list, terra::rast) #read in mosaic tifs as list of terra spatrast files

class_number <- c(1, 111, 124, 131, 138, 144, 152, 176, 195)
class_name <- c("Crop", "Water", "Development", "Bare", "Plantations", "Trees", "Shrubs", "Grassland", "Wetland") 

graesser_extract_fcn <- function(this_mosaic, sf.obj){
 
# sf.obj <- sf.obj[1:100,]
 
  temp <- terra::extract(
    this_mosaic,
    terra::vect(
      sf.obj %>% st_transform(terra::crs(this_mosaic))
    )
    ,
    na.rm=TRUE,
    weights = T
  )
  
  old_colname <- colnames(temp %>% select(2))
  year <- substr(old_colname, nchar(old_colname) - 3, nchar(old_colname))
  
  return <- temp %>%
    rename(class = old_colname)%>%
    group_by(ID, class)%>%
    summarize(class_coverage = sum(weight, na.rm = T)) %>%
    mutate(class = c(class_name, class)[match(class, c(class_number, class))],
           class = paste0(class, "_", year))%>%
    pivot_wider(names_from = class, values_from = class_coverage)%>%
    ungroup()
  
  if(year == 1999){
    
    return$pixels_count = rowSums(dplyr::select(return, ends_with("1999")), na.rm = T)
    
  } else{
    return <- return %>% dplyr::select(-ID)
  }
    
  return(return)

}


graesser_extract_helper <- function(sf.obj, annual_mosaic_list, id_cols){
  pb$tick()$print()
  
  purrr::map_dfc(annual_mosaic_list, ~graesser_extract_fcn(. , sf.obj))%>%
    inner_join(
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(ID, polyarea, id_cols)
    , by = "ID")
  
}

id_cols_ciren <- c("objectid", "regidere", "ciren_region")

pb <- progress_estimated(length(sf_list))
options(dplyr.summarise.inform = FALSE)

extracted_graesser <- purrr::map_dfr(sf_list, ~graesser_extract_helper(. , annual_mosaic_list, id_cols = id_cols_ciren))%>%
  replace(is.na(.), 0)%>%
  dplyr::select(ID, pixels_count, polyarea, id_cols_ciren, everything())


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### lu2001 extraction function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lu2001_rast <- terra::rast("data/lu_2001/lu_2001.tif")

library(readr)
lu2001_extract_fcn <- function(sf.obj, id_cols){
  pb$tick()$print()
  
#  sf.obj <- sf.obj[1:100,]
  
  return <- terra::extract(
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
      sf.obj %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(ID, id_cols)
      , by = "ID")
  
  return(return)
}
pb <- progress_estimated(length(sf_list))
extracted_lu2001 <- purrr::map_dfr(sf_list, ~lu2001_extract_fcn(., id_cols_ciren))%>%
  dplyr::select(ID, id_cols_ciren, everything())

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in covariates: dist to road, industry, slope, elev, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elevation <- getData('alt', country='CHL')[[1]]
terr <- terrain(elevation, opt=c('slope'), unit='degrees')

industry.sf <- st_read(
  "external_data/ciren_simef/INDUSTRIA_FORESTAL/Catastro_Industria_Forestal_Primaria_Infor_2019.shp"
) %>%
  st_transform(terra::crs(lu2001_rast)) %>%
  st_cast("POINT")

native_especies <- c("Roble", "Tepa", "Raulí", "nativas", "Olivillo", "Mañío", "Ulmo", "Lingue", "Lenga", "Coihue", "Tineo", "Canelo", "Alerce", "Álamo")
pattern <- paste0(native_especies, collapse = "|")

native_industry.sf <- industry.sf %>%
  filter(grepl(pattern, especies, ignore.case = TRUE))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### extracting, calculating covariate values by polygon function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_covs_fcn <- function(sf.obj, id_cols, cores){
  #pb$tick()$print()
   #sf.obj <- sf.obj[1:100,]
  
  return_covs <- sf.obj %>%
    dplyr::select(id_cols)%>%
    st_transform(terra::crs(lu2001_rast))%>%
    mutate(elev = unlist(exact_extract(elevation, ., 'mean')),
           slope = unlist(exact_extract(terr, ., 'mean'))
    )%>%
    st_centroid() %>%
   mutate(natin_dist = st_nn(., native_industry.sf, k = 1, returnDist = T, parallel = cores)[[2]],
           ind_dist = st_nn(., industry.sf, k = 1, returnDist = T, parallel = cores)[[2]])
  
  
  return_covs <- return_covs %>%
    mutate(lat = unlist(map(return_covs$geometry,1)),
           long = unlist(map(return_covs$geometry,2)))
  
  return(return_covs)
  
}


# library(furrr)
# future::plan(multisession, workers = 6)
pb <- progress_estimated(length(sf_list))
### getting other covariates
return_covs_neverenrolled <- purrr::map_dfr(sf_list, ~return_covs_fcn(., id_cols_ciren, cores = 1))
# 
"C:\Users\garci\Dropbox\chile_reforestation\data\analysis_lc\cleaned_properties\neverenrolled\extracted_graesser.rds"
library(rio)
export(return_covs_neverenrolled, "data/analysis_lc/cleaned_properties/neverenrolled/return_covs_neverenrolled.rds")

covariates_neverenrolled <- readRDS("data/analysis_lc/cleaned_properties/neverenrolled/extracted_lu2001.rds")%>%
  replace(is.na(.), 0)%>%
  right_join(readRDS("data/analysis_lc/cleaned_properties/neverenrolled/return_covs_neverenrolled.rds") , by = c(id_cols_ciren))%>%
  left_join(readRDS("data/analysis_lc/cleaned_properties/neverenrolled/extracted_graesser.rds"), by = c("ID", id_cols_ciren))


export(covariates_neverenrolled, "data/analysis_lc/cleaned_properties/neverenrolled/covariates_neverenrolled.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Reading in all EVI data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/losrios_evi", pattern="*.csv", full.names =  T)
losrios_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "LOS_RÍOS")

# load los lagos evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/loslagos_evi", pattern="*.csv", full.names =  T)
loslagos_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "LOS_LAGOS")

# load BioBio evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/biobio_evi", pattern="*.csv", full.names =  T)
biobio_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "BIOBÍO")

# load O'Higgins evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/ohiggins_evi", pattern="*.csv", full.names =  T)
ohiggins_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "LIBERTADOR_GENERAL_BERNARDO_O_HIGGINS")

# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/maule_evi", pattern="*.csv", full.names =  T)
maule_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "MAULE")

# load ARAUCANÍA evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/araucania_evi", pattern="*.csv", full.names =  T)
araucania_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "ARAUCANÍA")

# load Ñuble evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/nuble_evi", pattern="*.csv", full.names =  T)
nuble_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(ciren_region = "BIOBÍO")

ciren_evi <- rbind(losrios_evi, loslagos_evi, ohiggins_evi, biobio_evi, araucania_evi, maule_evi, nuble_evi)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Combine EVI with other covariates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

data_neverenrolled <- covariates_neverenrolled %>%
  left_join(ciren_evi, by = c("ciren_region", "objectid"))

comuna_neverenrolled <- bind_rows(sf_list) %>%
  select(id_cols_ciren, desccomu) %>%
  st_drop_geometry()

data_neverenrolled1 <- data_neverenrolled %>%
  left_join(comuna_neverenrolled, by = id_cols_ciren)

library(rio)
export(data_neverenrolled1, "data/analysis_lc/cleaned_properties/neverenrolled/data_neverenrolled.rds")


