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

output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_properties <- st_read("data/analysis_lc/matched_properties/property_match.shp")

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


id_cols_enrolled <- c("rptpre_id", "ROL", "comuna")

options(dplyr.summarise.inform = FALSE)

extracted_graesser <- purrr::map_dfc(annual_mosaic_list, ~graesser_extract_fcn(. , enrolled_properties))

extracted_graesser <- extracted_graesser %>%
    right_join(
      enrolled_properties %>% rowid_to_column("ID") %>% st_drop_geometry() %>% dplyr::select(ID, polyarea, id_cols_enrolled)
      , by = "ID")%>%
  replace(is.na(.), 0)%>%
  dplyr::select(ID, pixels_count, polyarea, id_cols_enrolled, everything())

library(rio)
export(extracted_graesser, paste0(output_dir, "/extracted_graesser_enrolled.rds"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### lu2001 extraction function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lu2001_rast <- terra::rast(
  paste0(my_data_dir, "/data/lu_2001/lu_2001.tif")
  )

library(readr)
lu2001_extract_fcn <- function(sf.obj, id_cols){
  #pb$tick()$print()
  
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

extracted_lu2001 <- lu2001_extract_fcn(enrolled_properties, id_cols_enrolled)%>%
  dplyr::select(ID, id_cols_enrolled, everything())

library(rio)
export(extracted_lu2001, paste0(output_dir, "/extracted_lu2001_enrolled.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in covariates: dist to road, industry, slope, elev, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elevation <- getData('alt', country='CHL')[[1]]
terr <- terrain(elevation, opt=c('slope'), unit='degrees')%>%
  projectRaster(crs(lu2001_rast))

industry.sf <- st_read(
  paste0(my_data_dir, "/external_data/ciren_simef/INDUSTRIA_FORESTAL/Catastro_Industria_Forestal_Primaria_Infor_2019.shp")
) %>%
  st_transform(terra::crs(lu2001_rast))%>%
  st_cast("POINT")

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

return_covs_fcn <- function(sf.obj, id_cols, cores){
  #pb$tick()$print()
  #  sf.obj <- sf.obj[1:100,]
  
  return_covs <- st_make_valid(sf.obj) %>%
    dplyr::select(id_cols)%>%
    st_transform(terra::crs(lu2001_rast))%>%
    mutate(elev = unlist(exact_extract(elevation, ., 'mean')),
           slope = unlist(exact_extract(terr, ., 'mean'))
    )%>%
    st_centroid()%>%
    mutate(natin_dist = st_nn(., native_industry.sf, k = 1, returnDist = T, parallel = cores)[[2]],
           ind_dist = st_nn(., industry.sf, k = 1, returnDist = T, parallel = cores)[[2]])
  # mutate(road_dist = drop_units(st_distance(., roads.sf[st_nearest_feature(., roads.sf), ])),
  #        natin_dist = drop_units(st_distance(., native_industry.sf[st_nearest_feature(., native_industry.sf), ])),
  #        ind_dist = drop_units(st_distance(., industry.sf[st_nearest_feature(., industry.sf), ]))
  # )
  
  return_covs <- return_covs %>%
    mutate(lat = unlist(map(return_covs$geometry,1)),
           long = unlist(map(return_covs$geometry,2)))
  
  return(return_covs)
  
}



# library(furrr)
# future::plan(multisession, workers = 6)

### getting other covariates
return_covs_enrolled <- return_covs_fcn(enrolled_properties, id_cols_enrolled, cores = 1)


export(return_covs_enrolled, paste0(output_dir, "/return_covs_enrolled.rds"))


covariates_enrolled <- readRDS(paste0(output_dir, "/extracted_lu2001_enrolled.rds"))%>%
  replace(is.na(.), 0)%>%
  right_join(readRDS(paste0(output_dir, "/return_covs_enrolled.rds")), by = c(id_cols_enrolled))%>%
  left_join(readRDS(paste0(output_dir, "/extracted_graesser_enrolled.rds")), by = c("ID", id_cols_enrolled))

export(covariates_enrolled, paste0(output_dir, "/covariates_enrolled.rds"))

















#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Reading in EVI data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_match_evi <- read.csv("data/analysis_lc/matched_properties/enrolled_evi/property_match_evi_roi.csv") %>%
  select(2:4)%>%
  mutate(year = paste0("evi_", year))%>%
  pivot_wider(names_from = year, values_from = mean)

property_match_ids <- readRDS("data/analysis_lc/matched_properties/property_match_ids.rds")

data_enrolled <- property_match_evi %>%
  full_join(property_match_ids, by = "prop_ID")%>%
  left_join(covariates_enrolled, by = id_cols_enrolled)

export(data_enrolled, "data/analysis_lc/cleaned_properties/enrolled/data_enrolled.rds")
