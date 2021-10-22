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
         first.treat = rptpro_ano)%>%
  filter(submitted_mp == 1)

length(unique(native_forest_law$rptpro_id))

# spatial_priority <- native_forest_law %>%
#   group_by(rptpro_id)%>%
#   mutate(priority = ifelse(match_type == "spatial", 1, 0),
#          max_priority = max(priority))%>%
#   filter(priority == max_priority)%>%
#   filter(area_diff == min(area_diff))%>%
#   ungroup()%>%
#   distinct(rptpro_id, .keep_all = TRUE)
  
rol_priority <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  ungroup()%>%
  distinct(rptpro_id, .keep_all = TRUE)

# area_priority <- native_forest_law %>%
#   group_by(rptpro_id)%>%
#   mutate(priority = ifelse(area_diff == min(area_diff), 1, 0),
#          max_priority = max(priority))%>%
#   filter(priority == max_priority)%>%
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

library(MatchIt)
pool_wide <- pool_wide %>%
  drop_na(elev, slope, baresoil)

#love.plot(matched_2009, thresholds = c(m = .25))

r=2

my_match_method = "nearest"
my_distance = "glm"
#my_distance = "mahalanobis"

matched_2009 <- matchit(treat ~ evi_trend0807 + evi_trend0706 + evi_trend0605 
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2009),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2010 <- matchit(treat ~ evi_trend0908 + evi_trend0807 + evi_trend0706  + evi_trend0605
                        +evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2010),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2011 <- matchit(treat ~ evi_trend1009 + evi_trend0908 + evi_trend0807 + evi_trend0706 #+ evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2011),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2012 <- matchit(treat ~ evi_trend1110 + evi_trend1009 + evi_trend0908 + evi_trend0807#+ evi_trend0706 +evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2012),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2013 <- matchit(treat ~ evi_trend1211 + evi_trend1110 + evi_trend1009 + evi_trend0908 #+ evi_trend0807+ evi_trend0706 + evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub +proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2013),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2014 <- matchit(treat ~ evi_trend1312 + evi_trend1211 + evi_trend1110 + evi_trend1009 #+ evi_trend0908+ evi_trend0807+ evi_trend0706 + evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub +proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2014),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2015 <- matchit(treat ~ evi_trend1413 + evi_trend1312 + evi_trend1211 + evi_trend1110 #+ evi_trend1009 + evi_trend0908+ evi_trend0807+ evi_trend0706 + evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub +proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2015),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2016 <- matchit(treat ~ evi_trend1514 + evi_trend1413 + evi_trend1312 + evi_trend1211 #+ evi_trend1110+ evi_trend1009 + evi_trend0908+ evi_trend0807+ evi_trend0706 + evi_trend0605
                        +  evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2016),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2017 <- matchit(treat ~ evi_trend1615 + evi_trend1514 + evi_trend1413 + evi_trend1312 #+ evi_trend1211+ evi_trend1110+ evi_trend1009 + evi_trend0908+ evi_trend0807+ evi_trend0706 + evi_trend0605
                        +  evi_2008
                        + forest + plantation + baresoil + pasture + shrub +proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2017),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2018 <- matchit(treat ~ evi_trend1716 + evi_trend1615 + evi_trend1514 + evi_trend1413 #+ evi_trend1312 + evi_trend1211+ evi_trend1110+ evi_trend1009 + evi_trend0908+evi_trend0807+ evi_trend0706 +evi_trend0605
                        +  evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2018),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2019 <- matchit(treat ~ evi_trend1817 + evi_trend1716 + evi_trend1615 + evi_trend1514 #+ evi_trend1413 + evi_trend1312 + evi_trend1211+ evi_trend1110+ evi_trend1009 + evi_trend0908+evi_trend0807+ evi_trend0706 + evi_trend0605
                        + evi_2008
                        + forest + plantation + baresoil + pasture + shrub + proportion_erosion+ slope + elev + lat  + area + road_dist + industry_dist + native_industry_dist,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2019),
                        method = my_match_method, ratio = r, distance = my_distance)


map_covars <- function(df){
  # return_df <- within(df, quartile <- as.integer(cut(rptpro_puntaje, quantile(rptpro_puntaje, probs=0:4/4, na.rm = TRUE), include.lowest=TRUE)))
  
  return_df <- df %>%
    group_by(subclass)%>%
    mutate(
      control_contest = rptpro_tipo_concurso,
      control_year = max(first.treat)
    )%>%
    ungroup()
  
  return(return_df)
}


# group by matched pairs
matched_wide <- map_covars(match.data(matched_2009, subclass = "subclass"))%>%
  bind_rows(map_covars(match.data(matched_2010, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2011, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2012, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2013, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2014, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2015, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2016, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2017, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2018, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2019, subclass = "subclass")))%>%
  distinct(id, .keep_all = TRUE)


matched_long <- gather(matched_wide, "Year", "EVI2", evi_2005:evi_2020)%>%
  separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))

data <- matched_long %>%
  mutate(G = first.treat,
         period = Year,
         Y = EVI2,
         id = as.numeric(id))

data <- subset(data, first.treat == 2009 | treat == 0)
data_2 <- subset(data, control_year == 2009)

library(did)
did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ road_dist + proportion_erosion + industry_dist +native_industry_dist + water +urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev  
              ,
              weightsname = "area", 
              data=data, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic")
did.ovr <- aggte(did, type="simple")
ggdid(did.es)
