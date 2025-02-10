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
#my_data_dir <- here::here("chile_reforestation")

output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

landcover_2021_dir <- "C:\\Users\\AG2964\\Dropbox (YSE)\\chile_landcover"
landcover_2021_dir <- here::here("chile_landcover")

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in shapefile of property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load matched properies (created in ciren_matches-2.R)
enrolled_properties <- st_read(paste0(output_dir, "/enrolled_match_rol.shp"))

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
  
  # Convert sf.obj to spatvector and extract land cover
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
    summarize(class_coverage = sum(weight, na.rm = T)) %>% #get total number of pixels from each class
    mutate(class = c(class_name, class)[match(class, c(class_number, class))],
           class = paste0(class, "_", year))%>% # paste year onto name of land cover class
    pivot_wider(names_from = class, values_from = class_coverage)%>% 
    ungroup()
  
  if(year == 1999){
    
    return$pixels_count = rowSums(dplyr::select(return, ends_with("1999")), na.rm = T)
    
  } else{
    return <- return %>% dplyr::select(-ID)
  }
  
  return(return)
  
}

options(dplyr.summarise.inform = FALSE)

# run function using list of annual mosaics and enrolled property .shp
extracted_graesser <- purrr::map_dfc(annual_mosaic_list, ~graesser_extract_fcn(. , enrolled_properties),
                                     .progress = TRUE)

extracted_graesser <- extracted_graesser %>%
    right_join(
      enrolled_properties %>% rowid_to_column("ID") %>% st_drop_geometry()
      , by = "ID")%>%
  replace(is.na(.), 0)

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

id_cols_enrolled <- c("rptpro_id", "rptpre_id")

extracted_lu2001 <- lu2001_extract_fcn(enrolled_properties, id_cols_enrolled)%>%
  dplyr::select(ID, id_cols_enrolled, everything())


export(extracted_lu2001, paste0(output_dir, "/extracted_lu2001_enrolled.rds"))

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
  #  sf.obj <- sf.obj[1:100,]
  
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

my_cores = 1

### getting other covariates
return_covs_enrolled <- return_covs_fcn(enrolled_properties, id_cols_enrolled, cores = my_cores)


export(return_covs_enrolled, paste0(output_dir, "/return_covs_enrolled.rds"))


covariates_enrolled <- readRDS(paste0(output_dir, "/extracted_lu2001_enrolled.rds"))%>%
  replace(is.na(.), 0)%>%
  right_join(readRDS(paste0(output_dir, "/return_covs_enrolled.rds")), by = c(id_cols_enrolled))%>%
  left_join(readRDS(paste0(output_dir, "/extracted_graesser_enrolled.rds")), by = c("ID", id_cols_enrolled))

export(covariates_enrolled, paste0(output_dir, "/covariates_enrolled.rds"))
























#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in 2021 landcover raster
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
landcover_2021_rast <- terra::rast(paste0(landcover_2021_dir, "/2021_chile_mosaic-001.tif"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in enrolled property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_properties.vec <- enrolled_properties %>%
  terra::vect()
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Get Native Forest polygons within enrolled properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# limit landcover raster to native forest
nativeforest_rast <- ifel(landcover_2021_rast == 6, 6, NA)%>%
  terra::trim() # remove NAs

# get only part of raster that intersects with property boundaries
nativeforest_enrolled <- property_match %>%
  terra::intersect(nativeforest_rast %>% project(terra::crs(property_match)))

nativeforest2021_enrolled.sf <- nativeforest_enrolled %>%
  st_as_sf()%>%
  group_by(rptpre_id, pre_comuna, ROL)%>%
  mutate(nf_poly_id = cur_group_id())%>%
  ungroup %>%
  mutate(nf_poly_area = st_area(.))

st_write(nativeforest2021_enrolled.sf, 
         paste0(output_dir, "/nativeforest2021_enrolled.shp"),
         driver = "ESRI Shapefile")




