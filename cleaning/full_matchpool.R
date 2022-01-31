
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

spatial_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/output_files/spatial_df.rds")%>%
  select(PROPIETARI, NOM_PREDIO, rptpre_nombre, rptprop_nombre, rptpro_id, rptpre_rol.x, rptpre_rol.y, area_ha, area_diff)

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

my_spatial_match <- my_spatial_covars %>%
  inner_join(spatial_df, by = "rptpro_id")%>%
  inner_join(my_spatial_evi, by = "rptpro_id")

setwd("C:/Users/garci/Dropbox/chile_reforestation/data/analysis")

export(my_spatial_match, "my_spatial_match.rds")
export(my_rol_match, "my_rol_match.rds")

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



#########################################################################################################
######## Combining all control regions
#########################################################################################################

control_fcn <- function(df){
  return_df <- df %>%
    mutate(treat=0,
           first.treat = 0)%>%
    select(objectid, geometry.x, treat, first.treat, property_area_ha, road_dist, lat, long, slope, elev, c_1:c_NA, proportion_erosion, mod_to_severe_erosion_area, industry_dist, native_industry_dist, evi_2005:evi_2020)
  
  return(return_df)
}

all_matchpool <- control_fcn(losrios_matchpool) %>%
  bind_rows(control_fcn(maule_matchpool))%>%
  bind_rows(control_fcn(araucania_matchpool))%>%
  bind_rows(control_fcn(biobionuble_matchpool))%>%
  bind_rows(control_fcn(loslagos_matchpool))%>%
  bind_rows(control_fcn(ohiggins_matchpool))%>%
  mutate(controlid = 1:nrow(.))%>%
  rename(geometry=geometry.x,
         area_ha = property_area_ha)
setwd("C:/Users/garci/Dropbox/chile_reforestation/data/analysis")
export(all_matchpool, "control_matchpool.rds")

##################################################################################################
##########  creating full matchpool dataset
#### This will be used in analysis_matching.R to match control group 
#################################################################################################

library(ggplot2)
library(sf)
library(stringi)
library(data.table)

library(tidyverse)

#property_df <- readRDS("property_df.rds")
control_matchpool <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/control_matchpool.rds"))%>%
  mutate(treat = 0)
my_rol_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

native_forest_law <- readRDS("C:/Users/garci/Dropbox/chile_collab/input_files/NFL_df.rds") %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")%>%
  mutate(submitted_mp = ifelse( rptpro_tiene_plan_saff=="Si" | rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         treat = 1,
         first.treat = rptpro_ano)

length(unique(native_forest_law$rptpro_id))


rol_priority <- native_forest_law %>%
  filter(submitted_mp == 1)%>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  ungroup()%>%
  distinct(rptpro_id, .keep_all = TRUE)

rol_compliance <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  ungroup()%>%
  distinct(rptpro_id, .keep_all = TRUE)



# area_priority <- native_forest_law %>%
#   filter(submitted_mp == 1)%>%
#   group_by(rptpro_id)%>%
#   mutate(priority1 = ifelse(area_diff == min(area_diff), 1, 0),
#          priority2 = ifelse(match_type != "spatial", 1, 0),
#          max_priority = max(priority1 + priority2))%>%
#   filter((priority1 + priority2) == max_priority)%>%
#   ungroup()%>%
#   distinct(rptpro_id, .keep_all = TRUE)

pool_wide <- bind_rows(rol_priority, control_matchpool)%>%
  mutate(evi_trend1918 = evi_2019 - evi_2018,
         evi_trend1817 = evi_2018 - evi_2017,
         evi_trend1716 = evi_2017 - evi_2016,
         evi_trend1615 = evi_2016 - evi_2015,
         evi_trend1514 = evi_2015 - evi_2014,
         evi_trend1413 = evi_2014 - evi_2013,
         evi_trend1312 = evi_2013 - evi_2012,
         evi_trend1211 = evi_2012 - evi_2011,
         evi_trend1110 = evi_2011 - evi_2010,
         evi_trend1009 = evi_2010 - evi_2009,
         evi_trend0908 = evi_2009 - evi_2008,
         evi_trend0807 = evi_2008 - evi_2007,
         evi_trend0706 = evi_2007 - evi_2006,
         evi_trend0605 = evi_2006 - evi_2005,
         forest = c_1,
         plantation = c_3,
         pasture = c_9,
         urban = c_16,
         water = c_15,
         baresoil = c_12,
         shrub = c_5,
         snow = c_11,
         road_dist = unlist(road_dist),
         industry_dist = unlist(industry_dist),
         native_industry_dist = unlist(native_industry_dist),
         area = ifelse(is.na(rptpre_superficie_predial), area_ha, rptpre_superficie_predial)
  )%>%
  mutate(id = 1:nrow(.))

library(rio)
export(pool_wide, "pool_wide_rol.rds")
