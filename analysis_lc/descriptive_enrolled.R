library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)

setwd("C:/Users/garci/Dropbox/chile_reforestation")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx("external_data/concurso_conaf/program/proyecto.xlsx")

#properties and coordinates
predio_df <- read_xlsx("external_data/concurso_conaf/program/predio.xlsx")
#owners
propietario_df <- read_xlsx("external_data/concurso_conaf/program/propietario.xlsx")
#stands
rodal_df <- read_xlsx("external_data/concurso_conaf/program/rodal.xlsx")
#activities
actividades_df <- read_xlsx("external_data/concurso_conaf/program/actividad.xlsx")

coordinadas_df <- read_xlsx("external_data/concurso_conaf/program/coordinadas_predio.xlsx")

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1)),
         timber = as.numeric(ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0)),
         proportion_subsidized = ifelse(rptpre_superficie_predial != 0, rptpro_superficie / rptpre_superficie_predial,NA),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         `Received payment` = ifelse(received_bonus == 1, "Yes", "No")
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props")
  )%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))%>%
  filter(rptpre_superficie_predial < 5000)%>%
  group_by(rptpre_comuna, rptpre_id, ROL)%>%
  slice_head()

covariates_enrolled <- readRDS("data/analysis_lc/cleaned_properties/enrolled/covariates_enrolled.rds")%>%
  mutate(ind_dist = unlist(ind_dist),
         natin_dist = unlist(natin_dist))


comunal_pov <- read.csv("data/analysis_lc/comunal_poverty.csv", encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))


regrowth <- covariates_enrolled %>%
  inner_join(property_df, by = c("rptpre_id", "ROL"))%>%
  group_by(rptpre_id, rptpro_id, ROL)%>%
  slice_head()%>%
  ungroup()%>%
  mutate(comuna = tolower(accents(comuna)))%>%
  left_join(comunal_pov, by = "comuna")%>%
  mutate(Trees_baseline = Trees_2000,
         Grassland_baseline = Grassland_2000,
         Crop_baseline = Crop_2000,
         Shrubs_baseline = Shrubs_2000)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Now preparing to compare enrolled groups
# Smallholders vs. Other interested
# Compliers vs. Non-compliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ttest_df <- regrowth %>%
  filter(pixels_count != 0)%>%
  mutate(pre_Crop = Crop_2002/pixels_count,
         pre_Shrubs = Shrubs_2002/pixels_count,
         pre_Grassland = Grassland_2002/pixels_count,
         pre_Plantation = Plantation/pixels_count,
         pre_Forest = Forest/pixels_count,
         pre_Trees = Trees_2002/pixels_count,
         pre_Bare = Bare_2002/pixels_count)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Compliers vs. non-compliers

# variables needing matched set
elev <- t.test(elev ~ received_bonus, data = ttest_df)
slope <- t.test(slope ~ received_bonus, data = ttest_df)
industry <- t.test(ind_dist ~ received_bonus, data = ttest_df)
native_ind <- t.test(natin_dist ~ received_bonus, data = ttest_df)
crop <- t.test(pre_Crop ~ received_bonus, data = ttest_df)
shrub <- t.test(pre_Shrubs ~ received_bonus, data = ttest_df)
grassland <- t.test(pre_Grassland ~ received_bonus, data = ttest_df)
plantation <- t.test(pre_Plantation ~ received_bonus, data = ttest_df)
forest <- t.test(pre_Forest ~ received_bonus, data = ttest_df)
trees <- t.test(pre_Trees ~ received_bonus, data = ttest_df)
bare <- t.test(pre_Bare ~ received_bonus, data = ttest_df)
timber <- t.test(timber ~ received_bonus, data = ttest_df)

# other variables - using full set of applicants
area_prop <- t.test(rptpre_superficie_predial ~ received_bonus, data = property_df)
area_bono <- t.test(rptpro_superficie ~ received_bonus, data = property_df)
payment <- t.test(rptpro_monto_total ~ received_bonus, data = property_df)


covar <- c("Award (UTM)", "Bonus area (ha)", "Property area (ha)","Timber production objective" ,  "Proportion native forest", "Proportion plantation", "Proportion grassland", "Proportion shrub", "Proportion crop", "Proportion bareground", "Slope", "Elevation", "Dist. to native timber industry (km)", "Dist. to any timber industry (km)")
paid <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], grassland$estimate[2], shrub$estimate[2], crop$estimate[2], bare$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2]/1000, industry$estimate[2]/1000)
unpaid <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], grassland$estimate[1], shrub$estimate[1], crop$estimate[1], bare$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1])

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, timber$p.value, forest$p.value, plantation$p.value, grassland$p.value, shrub$p.value, crop$p.value, bare$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value)

compliance_ttest <- data.frame(covar, paid, unpaid, p_value) %>%
  mutate_at(vars(paid, unpaid, p_value), ~ round(., digits = 3))

library(rio)
export(compliance_ttest, "paper/results/compliance_ttest_table.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Contest types


# variables needing matched set
elev <- t.test(elev ~ `Contest type`, data = ttest_df)
slope <- t.test(slope ~ `Contest type`, data = ttest_df)
industry <- t.test(ind_dist ~ `Contest type`, data = ttest_df)
native_ind <- t.test(natin_dist ~ `Contest type`, data = ttest_df)
crop <- t.test(pre_Crop ~ `Contest type`, data = ttest_df)
shrub <- t.test(pre_Shrubs ~ `Contest type`, data = ttest_df)
grassland <- t.test(pre_Grassland ~ `Contest type`, data = ttest_df)
plantation <- t.test(pre_Plantation ~ `Contest type`, data = ttest_df)
forest <- t.test(pre_Forest ~ `Contest type`, data = ttest_df)
trees <- t.test(pre_Trees ~ `Contest type`, data = ttest_df)
bare <- t.test(pre_Bare ~ `Contest type`, data = ttest_df)
timber <- t.test(timber ~ `Contest type`, data = ttest_df)

# other variables - using full set of applicants
area_prop <- t.test(rptpre_superficie_predial ~ `Contest type`, data = property_df)
area_bono <- t.test(rptpro_superficie ~ `Contest type`, data = property_df)
payment <- t.test(rptpro_monto_total ~ `Contest type`, data = property_df)


covar <- c("Award (UTM)", "Bonus area (ha)", "Property area (ha)","Timber production objective" ,  "Proportion native forest", "Proportion plantation", "Proportion grassland", "Proportion shrub", "Proportion crop", "Proportion bareground", "Slope", "Elevation", "Dist. to native timber industry (km)", "Dist. to any timber industry (km)")
smallholder <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], grassland$estimate[2], shrub$estimate[2], crop$estimate[2], bare$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2]/1000, industry$estimate[2]/1000)
other_interested <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], grassland$estimate[1], shrub$estimate[1], crop$estimate[1], bare$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1])
other_df <- ttest_df %>% filter(`Contest type` == "Other interested")
other_interested_med <- c(median())

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, timber$p.value, forest$p.value, plantation$p.value, grassland$p.value, shrub$p.value, crop$p.value, bare$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value)

contest_ttest <- data.frame(covar, smallholder, other_interested, p_value) %>%
  mutate_at(vars(smallholder, other_interested, p_value), ~ round(., digits = 3))

library(rio)
export(contest_ttest, "paper/results/contest_ttest_table.rds")



library(vtable)

labs <- data.frame(name1 = c("rptpre_superficie_predial", "rptpro_superficie", "proportion_subsidized", "rptpro_monto_total", "received_bonus", "timber", "extensionista"),
                   name2 = c("Property size (ha)", "Subsidized surface (ha)", "Proportion of property subsidized", "Bonus amount (UTM)", "Complied", "Timber production objective", "Received extensionist support"))

sumtable(property_df, digits = 2,
         vars = labs$name1,
         summ = c('mean(x)', 'median(x)', 'sd(x)'),
         labels = labs)

st(property_df, digits = 2, group = "Contest type", group.test = TRUE,
   vars = labs$name1,
   summ = c('mean(x)', 'median(x)', 'sd(x)'),
   labels = labs)

st(property_df, digits = 2, group = "Received payment", group.test = TRUE,
   vars = labs$name1,
   summ = c('mean(x)', 'median(x)', 'sd(x)'),
   labels = labs)

