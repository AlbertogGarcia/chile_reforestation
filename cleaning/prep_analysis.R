

setwd("C:/Users/garci/Dropbox/chile_reforestation/")

covariates_neverenrolled <- st_read("")

lc_annual <- readRDS("")

roi <- c("ARAUCANÍA", "MAULE", "LOS_RÍOS", "LOS_LAGOS", "O_HIGGINS", "BIOBÍO")
list_regions <- data.frame(
  "Region" = c(roi, "Ñuble"),
  "list_ID" = c(seq(from = 1, to = 6), 6)
)

neverenrolled <- left_join(covariates_neverenrolled, lc_annual, by = c("objectid", "ID", "list_ID"))%>%
  left_join(list_regions, by = "list_ID")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Load EVI data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/losrios_evi", pattern="*.csv", full.names =  T)
losrios_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "LOS_RÍOS")

# load los lagos evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/loslagos_evi", pattern="*.csv", full.names =  T)
loslagos_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "LOS_LAGOS")

# load BioBio evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/biobio_evi", pattern="*.csv", full.names =  T)
biobio_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "BIOBÍO")

# load O'Higgins evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/ohiggins_evi", pattern="*.csv", full.names =  T)
ohiggins_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "O_HIGGINS")

# load los rios evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/maule_evi", pattern="*.csv", full.names =  T)
maule_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "MAULE")

# load ARAUCANÍA evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/araucania_evi", pattern="*.csv", full.names =  T)
araucania_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "ARAUCANÍA")

# load Ñuble evi time series for all ciren properties
temp = list.files("data/property_ndvi/new_CIREN_ndvi/match_evi/nuble_evi", pattern="*.csv", full.names =  T)
nuble_evi = bind_rows(lapply(temp, read_csv))%>%
  dplyr::select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean") %>%# cast evi into wide format
  mutate(Region = "Ñuble")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciren_evi <- rbind(losrios_evi, loslagos_evi, ohiggins_evi, biobio_evi, araucania_evi, maule_evi, nuble_evi)%>%
  inner_join(list_regions, by = "Region")

neverenrolled_panel <- neverenrolled %>%
  inner_join(ciren_evi, by = c("list_ID", "objectid"))
