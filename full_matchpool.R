
### loads packages
library(latex2exp)
library(tidyverse)
library(ggplot2)
library(sf)
library(stringi)
library(data.table)
library(fuzzyjoin)
library(stringdist)
library(rio)

forestlaw_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/NFL_df.rds")
spatial_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/output_files/spatial_df.rds")
rol_match_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/output_files/rol_match_df.rds")
my_rol_covars <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/my_rol_match_covars.rds"))
my_spatial_covars <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/my_spatial_match_covars.rds"))

exclude_match <- rbind(
  st_read("C:/Users/garci/Dropbox/chile_collab/rol_match_geoms.shp"), 
  st_read("C:/Users/garci/Dropbox/chile_collab/spatial_match_geoms.shp")
  )



setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/EVI/treat_rol")

# load evi time series for all matched properties
temp = list.files(pattern="*.csv")
my_rol_evi = bind_rows(lapply(temp, read_csv))%>%
  select(rptpro_id, year, mean) %>%
  distinct(rptpro_id, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast into wide format

my_rol_match <- my_rol_covars %>%
  inner_join(rol_match_df, by = "rptpro_id")%>%
  inner_join(my_rol_evi, by = "rptpro_id")


setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/EVI/treat_spatial")

# load evi time series for all matched properties
temp = list.files(pattern="*.csv")
my_spatial_evi = bind_rows(lapply(temp, read_csv))%>%
  select(rptpro_id, year, mean) %>%
  distinct(rptpro_id, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast into wide format

my_spatial_match <- my_rol_covars %>%
  inner_join(spatial_df, by = "rptpro_id")%>%
  inner_join(my_spatial_evi, by = "rptpro_id")


#########################################################################################################
######## Los Rios control properties
#########################################################################################################

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/losrios_evi")

# load los rios ndvi time series for all ciren properties
temp = list.files(pattern="*.csv")
losrios_evi = bind_rows(lapply(temp, read_csv))%>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format

# to make sure I'm not selecting treated properties
# disjoint join los Rios CIREN properties with property df based

losrios_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/LOS_RÍOS_CIREN_2018.shp"
)%>%
  st_make_valid()


exclude_match <- exclude_match %>%
  st_transform(st_crs(losrios_ciren))

losrios_matchpool <- losrios_ciren[!lengths(st_intersects(losrios_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/losrios_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(losrios_evi, by = "objectid")

#########################################################################################################
######## Bio-Bio, Nuble control properties
#########################################################################################################

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/biobio_evi")

# load biobio ndvi time series for all ciren properties
temp = list.files( pattern="*.csv")
biobio_evi = bind_rows(lapply(temp, read_csv))%>%
  #filter(objectid == 59515) %>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/nuble_evi")

# load biobio evi time series for all ciren properties
temp = list.files( pattern="*.csv")
biobionuble_evi = bind_rows(lapply(temp, read_csv))%>%
  #filter(objectid == 59515) %>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")%>%
  bind_rows(biobio_evi)


# to make sure I'm not selecting treated properties
# join los Rios CIREN properties with property df based on rol

biobionuble_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/BIOBÍO_CIREN_2016.shp"
) %>%
  st_make_valid()
biobionuble_matchpool <- biobionuble_ciren[!lengths(st_intersects(biobionuble_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/biobionuble_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(biobionuble_evi, by = "objectid")

#########################################################################################################
######## Maule control properties
#########################################################################################################

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/maule_evi")
# load maule ndvi time series for all ciren properties
temp = list.files( pattern="*.csv")
maule_evi = bind_rows(lapply(temp, read_csv))%>%
  #filter(objectid == 59515) %>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format

# to make sure I'm not selecting treated properties
maule_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/MAULE_CIREN_2014.shp"
)  %>%
  st_make_valid()
maule_matchpool <- maule_ciren[!lengths(st_intersects(maule_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/maule_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(maule_evi, by = "objectid")


#########################################################################################################
######## Araucania control properties
#########################################################################################################
setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/araucania_evi")
# load araucania ndvi time series for all ciren properties
temp = list.files( pattern="*.csv")
araucania_evi = bind_rows(lapply(temp, read_csv))%>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format

# to make sure I'm not selecting treated properties
# to make sure I'm not selecting treated properties
araucania_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/ARAUCANÍA_CIREN_2013.shp"
)  %>%
  st_make_valid()

araucania_matchpool <- araucania_ciren[!lengths(st_intersects(araucania_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/araucania_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(araucania_evi, by = "objectid")

#########################################################################################################
######## Los Lagos control properties
#########################################################################################################

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/loslagos_evi")
# load araucania ndvi time series for all ciren properties
temp = list.files( pattern="*.csv")
loslagos_evi = bind_rows(lapply(temp, read_csv))%>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format


# to make sure I'm not selecting treated properties
# to make sure I'm not selecting treated properties
loslagos_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/LOS_LAGOS_CIREN_2016.shp"
)  %>%
  st_make_valid()
loslagos_matchpool <- loslagos_ciren[!lengths(st_intersects(loslagos_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/loslagos_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(loslagos_evi, by = "objectid")


#########################################################################################################
######## O'Higgins control properties
#########################################################################################################

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/property_ndvi/new_CIREN_ndvi/match_evi/ohiggins_evi")
# load araucania ndvi time series for all ciren properties
temp = list.files( pattern="*.csv")
ohiggins_evi = bind_rows(lapply(temp, read_csv))%>%
  select(objectid, year, mean) %>%
  distinct(objectid, year, .keep_all = TRUE) %>%
  mutate(year = paste0("evi_", year))%>%
  spread(key = "year", value = "mean")# cast ndvi into wide format

# to make sure I'm not selecting treated properties
# to make sure I'm not selecting treated properties
ohiggins_ciren <-  st_read(
  "C:/Users/garci/Dropbox/chile_reforestation/external_data/ciren_simef/PROPIEDADES_RURALES/LIBERTADOR_GENERAL_BERNARDO_O_HIGGINS_CIREN_2013.shp"
)  %>%
  st_make_valid()
ohiggins_matchpool <- ohiggins_ciren[!lengths(st_intersects(ohiggins_ciren, st_make_valid(exclude_match))), ] %>%
  #st_join(mgmtplan_df, join = st_disjoint)%>%
  inner_join(data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/property_covariates/ohiggins_CIREN_covars.rds")), by = c("objectid", "rol", "desccomu"))%>%
  inner_join(ohiggins_evi, by = "objectid")
