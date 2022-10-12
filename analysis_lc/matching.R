library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)
library(rio)

setwd("C:/Users/garci/Dropbox/chile_reforestation/")
all_property_wide <- readRDS("data/analysis_lc/analysis_ready/all_property_wide.rds")%>%
  filter(pixels_count != 0
         )%>%
  mutate(ind_dist = unlist(ind_dist),
         natin_dist = unlist(natin_dist),
         treat = as.factor(treat))



match_vars <- c("pretrend_Trees", 
                "pretrend_Grassland", "pretrend_Crop", 
                "pretrend_evi",
                "Forest", "Plantation", 
                "Trees_2005", 
                "Grassland_baseline", "Crop_baseline", "Shrubs_baseline", "Development_baseline", "Water_baseline",
                "slope", "elev", "lat",
                "pixels_count",
                "ind_dist", "natin_dist"
)



pool_wide <- all_property_wide %>%
  mutate(Trees_baseline = Trees_2005,
         Crop_baseline = Crop_2005,
         Shrubs_baseline = Shrubs_2005,
         Grassland_baseline = Grassland_2005,
         Development_baseline = Development_2005,
         Water_baseline = Water_2005)

pool_wide_pctg <- pool_wide %>%
  mutate(lu_pixels_count = Forest + Plantation + Urban + Water + `Snow/ice` + `No data` + `Pasture and ag` + Shrub + `Permanent bare soil` + `Temporary bare soil`)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  mutate_at(vars(Forest, Plantation), ~ . / lu_pixels_count)%>%
  mutate(pretrend_Trees = Trees_2008 - Trees_2005,
         pretrend_Grassland = Trees_2008 - Trees_2005,
         pretrend_Crop = Trees_2008 - Trees_2005,
         pretrend_evi = evi_2008 - evi_2005
  )

enrolled_wide_pctg <- pool_wide_pctg %>%
  filter(treat != 0)%>%
  drop_na(match_vars)%>%
  mutate(first.treat = ifelse(submitted_management_plan != 1, 0, first.treat),
         treat = ifelse(first.treat == 0, 0, 1))

export(enrolled_wide_pctg, "data/analysis_lc/main/enrolled_wide_pctg.rds")



complier_wide_pctg <- pool_wide_pctg %>%
  filter(treat == 0 | submitted_management_plan == 1)%>%
  drop_na(match_vars)

noncomplier_wide_pctg <- pool_wide_pctg %>%
  filter(treat == 0 | submitted_management_plan != 1)%>%
  drop_na(match_vars)


r=2

my_match_method = "nearest"
my_distance = "glm"
#my_distance = "mahalanobis"

matchit_compliers_pctg <- matchit(reformulate(match_vars, "treat")
                                  ,
                                  data = complier_wide_pctg %>% select(treat, match_vars, property_ID),
                                  method = my_match_method , ratio = r, distance = my_distance)

matched_compliers_pctg <- match.data(matchit_compliers_pctg, subclass = "subclass")%>%
  select(property_ID, treat)%>%
  left_join(complier_wide_pctg, by = c("property_ID", "treat"))

export(matched_compliers_pctg, "data/analysis_lc/main/matched_compliers_pctg.rds")

matchit_noncompliers_pctg <- matchit(reformulate(match_vars, "treat")
                                     ,
                                     data = noncomplier_wide_pctg %>% select(treat, match_vars, property_ID),
                                     method = my_match_method , ratio = r, distance = my_distance)

matched_noncompliers_pctg <- match.data(matchit_noncompliers_pctg, subclass = "subclass")%>%
  select(property_ID, treat)%>%
  left_join(noncomplier_wide_pctg, by = c("property_ID", "treat"))

export(matched_noncompliers_pctg, "data/analysis_lc/main/matched_noncompliers_pctg.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Matching diagnostics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_diag_df <- matched_compliers %>% select(match_vars, treat)%>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))
unmatched_diag_df <- complier_wide %>% select(match_vars, treat) %>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))

matched_balance <- bal.tab(matched_diag_df, treat = matched_diag_df$treat, thresholds = c(m = .25))$Balance %>% as.data.frame() %>%
  dplyr::rename(matched = 2,
                matched_threshold = 3)%>%
  select(2, 3)%>%
  rownames_to_column("variable")

unmatched_balance <- bal.tab(unmatched_diag_df, treat = unmatched_diag_df$treat, thresholds = c(m = .25))$Balance %>% as.data.frame() %>%
  dplyr::rename(unmatched = 2,
                unmatched_threshold = 3)%>%
  select(2, 3)

varnames <- c("Native forest", "Plantation forest", "Baresoil", "Pasture", "Shrub", "Mod.-to-severe erosion", "Slope", "Elevation", "Lattitude", "Area (ha)", "Dist. to road", "Dist. to industry", "Dist. to native specific industry"
              ,"Urban" , "Water")

covar_balance <- cbind(#data.frame("covariate" = varnames),
                       matched_balance, unmatched_balance)%>%
  select( - variable)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Noncomplier vs. complier diagnostics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
diag_df <- enrolled_wide_pctg %>% select(match_vars, treat)%>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))

enrolled_balance <- bal.tab(diag_df, treat = diag_df$treat, thresholds = c(m = .25))$Balance %>% as.data.frame() %>%
  dplyr::rename(matched = 2,
                matched_threshold = 3)%>%
  select(2, 3)%>%
  rownames_to_column("variable")

varnames <- c("Native forest", "Plantation forest", "Baresoil", "Pasture", "Shrub", "Mod.-to-severe erosion", "Slope", "Elevation", "Lattitude", "Area (ha)", "Dist. to road", "Dist. to industry", "Dist. to native specific industry"
              ,"Urban" , "Water")

covar_balance <- enrolled_balance %>%
  select( - variable)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Matching by cohort
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

complier_2009 <- matchit(reformulate(match_vars, "treat")
                     ,
                     data = complier_wide_pctg %>% filter(first.treat == 2009 | treat == 0),
                     method = my_match_method , ratio = r, distance = my_distance)

complier_2010 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2010 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2011 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2011 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2012 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2012 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2013 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2013 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2014 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2014 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2015 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2015 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2016 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2016 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2017 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2017 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)
  
complier_2018 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2018 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)

complier_2019 <- matchit(reformulate(match_vars, "treat")
                         ,
                         data = complier_wide_pctg %>% filter(first.treat == 2019 | treat == 0),
                         method = my_match_method , ratio = r, distance = my_distance)


map_covars <- function(df){
  
  subclass_contest <- df %>%
    select(subclass, rptpro_tipo_concurso, rptpro_ano)%>%
    drop_na(rptpro_tipo_concurso)%>%
    dplyr::rename(control_contest = rptpro_tipo_concurso,
                  control_year = rptpro_ano)%>%
    select(subclass, control_contest, control_year)
  
  return_df <- df %>%
    left_join(subclass_contest, by = "subclass")
  
  return(return_df)
}


# group by matched pairs
matched_compliercohorts_wide <- map_covars(match.data(complier_2009, subclass = "subclass"))%>%
  bind_rows(map_covars(match.data(complier_2010, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2011, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2012, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2013, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2014, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2015, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2016, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2017, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2018, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(complier_2019, subclass = "subclass")))%>%
  distinct(id, .keep_all = TRUE)
