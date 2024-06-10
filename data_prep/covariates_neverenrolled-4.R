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

my_data_dir <- here::here("remote")
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
output_dir <- clean_data_dir

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ciren_path <- paste0(my_data_dir, "/external_data/ciren_simef/PROPIEDADES_RURALES")
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

enrolled_sf <- rbind(st_read(paste0(clean_data_dir, "/enrolled_match_rol.shp")),
                     st_read(paste0(clean_data_dir, "/enrolled_match_spatial.shp")) %>% select(- ROL_poly)
                     )

sf_list <- lapply(file_list_roi, my_read)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Graesser et al., 2022 land uses
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tif_file_list <- list.files(paste0(my_data_dir, "/data/graesser_lc/AnnualMosaics"), 
                            pattern = "*tif", 
                            full.names = TRUE)

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
  
  purrr::map_dfc(annual_mosaic_list, ~graesser_extract_fcn(. , sf.obj),
                 .progress = TRUE
                 )%>%
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

library(rio)
export(extracted_graesser, paste0(output_dir, "/extracted_graesser_neverenrolled.rds"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### lu2001 extraction function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lu2001_rast <- terra::rast(
  paste0(my_data_dir, "/data/lu_2001/lu_2001.tif")
)

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

export(extracted_lu2001, paste0(output_dir, "/extracted_lu2001_neverenrolled.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in covariates: dist to road, industry, slope, elev, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elevation <- geodata::elevation_30s("Chile", path = tempdir())
population <- geodata::population(year = 2005, path = tempdir())
prec_monthly <- geodata::worldclim_country("Chile", var = "prec", path = tempdir())
prec <- app(prec_monthly, mean)
industry.sf <- st_read(
  paste0(my_data_dir, "/external_data/ciren_simef/INDUSTRIA_FORESTAL/Catastro_Industria_Forestal_Primaria_Infor_2019.shp")
) %>%
  st_transform(terra::crs(lu2001_rast))%>%
  st_cast("POINT")

native_especies <- c("Roble", "Tepa", "Raulí", "nativas", "Olivillo", "Mañío", "Ulmo", "Lingue", "Lenga", "Coihue", "Tineo", "Canelo", "Alerce", "Álamo")
pattern <- paste0(native_especies, collapse = "|")

native_industry.sf <- industry.sf %>%
  filter(grepl(pattern, especies, ignore.case = TRUE))

cities.sf <- st_read(
  paste0(my_data_dir, "/external_data/CHL_FUrbanA/CHL_core.shp")
)%>%
  st_transform(terra::crs(lu2001_rast))%>%
  st_zm()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### extracting, calculating covariate values by polygon function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

return_covs_fcn <- function(sf.obj, id_cols, cores){
  #pb$tick()$print()
   #sf.obj <- sf.obj[1:100,]
  
  return_covs <- st_make_valid(sf.obj) %>%
    dplyr::select(id_cols)%>%
    st_transform(terra::crs(lu2001_rast))%>%
    mutate(elev = unlist(exact_extract(elevation, ., 'mean')),
           pop = unlist(exact_extract(population, ., 'mean')),
           precip = unlist(exact_extract(prec, ., 'mean'))
    )%>%
    st_centroid()%>%
    mutate(natin_dist = st_nn(., native_industry.sf, k = 1, returnDist = T, parallel = cores)[[2]],
           ind_dist = st_nn(., industry.sf, k = 1, returnDist = T, parallel = cores)[[2]],
           city_dist = st_nn(., cities.sf, k = 1, returnDist = T, parallel = cores)[[2]])
  
  
  return_covs <- return_covs %>%
    mutate(lat = unlist(map(return_covs$geometry,1)),
           long = unlist(map(return_covs$geometry,2)))
  
  return(return_covs)
  
}


### getting other covariates
return_covs_neverenrolled <- purrr::map_dfr(sf_list, ~return_covs_fcn(., c(id_cols_ciren, "desccomu"), cores = 1)
                                            , .progress= T
                                            )

export(return_covs_neverenrolled, paste0(output_dir, "/return_covs_neverenrolled.rds"))


covariates_neverenrolled <- readRDS(paste0(output_dir, "/extracted_lu2001_neverenrolled.rds"))%>%
  replace(is.na(.), 0)%>%
  right_join(readRDS(paste0(output_dir, "/return_covs_neverenrolled.rds")), by = c(id_cols_ciren))%>%
  left_join(readRDS(paste0(output_dir, "/extracted_graesser_neverenrolled.rds")), by = c("ID", id_cols_ciren))

export(covariates_neverenrolled, paste0(output_dir, "/covariates_neverenrolled.rds"))













# 
"C:\Users\garci\Dropbox\chile_reforestation\data\analysis_lc\cleaned_properties\neverenrolled\extracted_graesser.rds"
library(rio)
export(return_covs_neverenrolled, "data/analysis_lc/cleaned_properties/neverenrolled/return_covs_neverenrolled.rds")

covariates_neverenrolled <- readRDS("data/analysis_lc/cleaned_properties/neverenrolled/extracted_lu2001.rds")%>%
  replace(is.na(.), 0)%>%
  right_join(readRDS("data/analysis_lc/cleaned_properties/neverenrolled/return_covs_neverenrolled.rds") , by = c(id_cols_ciren))%>%
  left_join(readRDS("data/analysis_lc/cleaned_properties/neverenrolled/extracted_graesser.rds"), by = c("ID", id_cols_ciren))


export(covariates_neverenrolled, "data/analysis_lc/cleaned_properties/neverenrolled/covariates_neverenrolled.rds")
