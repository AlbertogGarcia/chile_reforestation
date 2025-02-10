library(dplyr)
library(terra)
library(sf)


my_data_dir <- here::here("remote")

output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

landcover_2021_dir <- "C:\\Users\\AG2964\\Dropbox (YSE)\\chile_landcover"

select <- dplyr::select
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in 2021 landcover raster
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
landcover_2021_rast <- terra::rast(paste0(landcover_2021_dir, "/2021_chile_mosaic-001.tif"))

# limit landcover raster to native forest
nativeforest_rast <- ifel(landcover_2021_rast == 6, 6, NA)%>%
  terra::trim() # remove NAs
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in enrolled property boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

property_match <- sf::read_sf(paste0(output_dir, "/property_match.shp"))[, c("rptpre_id", "pre_comuna", "ROL")] 

property_match_test <- terra::vect(property_match[1,])
plot(property_match_test)

#crop raster to property
nativeforest_rast_prop <- nativeforest_rast %>% 
  project(terra::crs(property_match)) %>%
  terra::crop(property_match_test)
plot(nativeforest_rast_prop)

nativeforest_poly <- as.polygons(nativeforest_rast == 6)%>%
  st_as_sf()
plot(nativeforest_poly)

# get only part of raster that intersects with property boundaries
nativeforest_enrolled <- property_match_test %>%
  terra::mask(nativeforest_rast %>% project(terra::crs(property_match)))
plot(nativeforest_enrolled)
# nativeforest_vec <- as.polygons(nativeforest_enrolled)

nativeforest2021_enrolled.sf <- nativeforest_enrolled %>%
  st_as_sf()%>%
  group_by(rptpre_id, pre_comuna, ROL)%>%
  
  mutate(nf_poly_id = cur_group_id())%>%
  ungroup %>%
  mutate(nf_poly_area = st_area(.))

# st_write(nativeforest2021_enrolled.sf, 
#          paste0(output_dir, "/nativeforest2021_enrolled.shp"),
#          driver = "ESRI Shapefile")



