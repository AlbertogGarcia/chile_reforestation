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
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props")
  )%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))

covariates_enrolled <- readRDS("data/analysis_lc/cleaned_properties/enrolled/covariates_enrolled.rds")


comunal_pov <- read.csv("data/analysis_lc/comunal_poverty.csv", encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))


regrowth <- covariates_enrolled %>%
  inner_join(property_df, by = c("rptpre_id", "ROL"))%>%
  group_by(rptpre_id, rptpro_id, ROL)%>%
  slice_head()%>%
  ungroup()%>%
  mutate(comuna = tolower(accents(comuna)))%>%
  left_join(comunal_pov, by = "comuna")%>%
  mutate(Trees_baseline = Trees_1999,
         Grassland_baseline = Grassland_1999,
         Crop_baseline = Crop_1999,
         Shrubs_baseline = Shrubs_1999,
         cpov_index_2008 = as.numeric(cpov_index_2008),
         median_pov_index = median(cpov_index_2008, na.rm = T),
         above_medpov = ifelse(cpov_index_2008 > median_pov_index, 1, 0),
         below_medpov = ifelse(cpov_index_2008 <= median_pov_index, 1, 0))

regrowth_longer <- regrowth %>%
  group_by(rptpre_id, rptpro_id, ROL, rptpro_ano)%>%
  slice_head()%>%
  ungroup()%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  pivot_wider(names_from = "class", values_from = "value", names_repair = "unique")%>%
  mutate(pixels_count = ifelse(pixels_count==0, NA, pixels_count),
         Trees_pct = Trees/pixels_count,
         Trees_change = Trees_pct - (Trees_baseline/pixels_count),
         Grassland_pct = Grassland/pixels_count,
         Grassland_change = Grassland_pct - (Grassland_baseline/pixels_count),
         Crop_pct = Crop/pixels_count,
         Crop_change = Crop_pct - (Crop_baseline/pixels_count),
         Shrubs_pct = Shrubs/pixels_count,
         Shrubs_change = Shrubs_pct - (Shrubs_baseline/pixels_count)
         )

plot_cpov <- regrowth_longer %>%
  group_by(year, above_medpov)%>%
  summarise(Trees = mean(Trees, na.rm = T),
            Trees_pct = mean(Trees_pct, na.rm = T),
            Trees_change = mean(Trees_change, na.rm = T),
            Grassland = mean(Grassland, na.rm = T),
            Grassland_pct = mean(Grassland_pct, na.rm = T),
            Grassland_change = mean(Grassland_change, na.rm = T),
            Crop = mean(Crop, na.rm = T),
            Crop_pct = mean(Crop_pct, na.rm = T),
            Crop_change = mean(Crop_change, na.rm = T),
            Shrubs = mean(Shrubs, na.rm = T),
            Shrubs_pct = mean(Shrubs_pct, na.rm = T),
            Shrubs_change = mean(Shrubs_change, na.rm = T))%>%
  drop_na(above_medpov, Trees)%>%
  mutate_at(vars(year), as.numeric)

plot_cpov_notyet <- regrowth_longer %>%
  filter(year <= rptpro_ano)%>%
  group_by(year, above_medpov)%>%
  summarise(Trees = mean(Trees, na.rm = T),
            Trees_pct = mean(Trees_pct, na.rm = T),
            Trees_change = mean(Trees_change, na.rm = T),
            Grassland = mean(Grassland, na.rm = T),
            Grassland_pct = mean(Grassland_pct, na.rm = T),
            Grassland_change = mean(Grassland_change, na.rm = T),
            Crop = mean(Crop, na.rm = T),
            Crop_pct = mean(Crop_pct, na.rm = T),
            Crop_change = mean(Crop_change, na.rm = T),
            Shrubs = mean(Shrubs, na.rm = T),
            Shrubs_pct = mean(Shrubs_pct, na.rm = T),
            Shrubs_change = mean(Shrubs_change, na.rm = T))%>%
  drop_na(above_medpov, Trees)%>%
  mutate_at(vars(year), as.numeric)


ggplot(data = plot_cpov %>% filter(year <= 2009), aes(x = year, y = Trees_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()

ggplot(data = plot_cpov %>% filter(year <= 2009), aes(x = year, y = Grassland_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()

ggplot(data = plot_cpov %>% filter(year <= 2009), aes(x = year, y = Crop_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()



ggplot(data = plot_cpov_notyet, aes(x = year, y = Trees_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()

ggplot(data = plot_cpov_notyet, aes(x = year, y = Grassland_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()

ggplot(data = plot_cpov_notyet, aes(x = year, y = Crop_change, color = as.character(above_medpov)))+
  theme_minimal()+geom_line()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

points_regrowth <- regrowth_longer %>%
  mutate(indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0))

small_df <- points_regrowth %>%
  filter(rptpro_tipo_concurso != "Otros Interesados")%>%
  mutate(VPI = ifelse(indigenous == 1, 100, 50),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[1], sizecut_points[2]), 100, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[2], sizecut_points[3]), 75, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[3], sizecut_points[4]), 50, 25),
         VI = TP*.5 + .5*VPI,
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[1], bonuscut_points[2]), 100, 15),
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[2], bonuscut_points[3]), 75, 15),
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[3], bonuscut_points[4]), 50, 15),
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[4], bonuscut_points[5]), 25, 15),
         VP = .1*VMBS,
         VPS = ifelse(indigenous == 1, 100, 0),
         adjusted_puntaje = rptpro_puntaje - .2*VI - .35*VP - 0.05*VPS,
         social_puntaje = .2*VI + .35*VP + 0.05*VPS,
         smallholder = 1,
         median_social = median(social_puntaje),
         above_medsocial = ifelse(social_puntaje > median_social, 1, 0)) 

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- points_regrowth %>%
  filter(rptpro_tipo_concurso == "Otros Interesados")%>%
  mutate(VPI = ifelse(indigenous == 1, 100, 50),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[1], sizecut_points[2]), 100, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[2], sizecut_points[3]), 75, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points[3], sizecut_points[4]), 50, 25),
         VI = TP*.5 + .5*VPI,
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[1], bonuscut_points[2]), 100, 25),
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[2], bonuscut_points[3]), 75, 25),
         VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[3], bonuscut_points[4]), 50, 25),
         VP = .1*VMBS,
         VPS = ifelse(indigenous == 1, 100, 0),
         adjusted_puntaje = rptpro_puntaje - .25*VI - .25*VP - 0.05*VPS,
         social_puntaje = .25*VI + .25*VP + 0.05*VPS,
         smallholder = 0,
         median_social = median(social_puntaje),
         above_medsocial = ifelse(social_puntaje > median_social, 1, 0)
  )

targeting_priority_df <- rbind(other_df, small_df)

plot_socialscore <- targeting_priority_df %>%
 # filter(smallholder ==1)%>%
  group_by(year, above_medsocial)%>%
  summarise(Trees = mean(Trees, na.rm = T),
            Trees_pct = mean(Trees_pct, na.rm = T),
            Trees_change = mean(Trees_change, na.rm = T),
            Grassland = mean(Grassland, na.rm = T),
            Grassland_pct = mean(Grassland_pct, na.rm = T),
            Grassland_change = mean(Grassland_change, na.rm = T),
            Crop = mean(Crop, na.rm = T),
            Crop_pct = mean(Crop_pct, na.rm = T),
            Crop_change = mean(Crop_change, na.rm = T),
            Shrubs = mean(Shrubs, na.rm = T),
            Shrubs_pct = mean(Shrubs_pct, na.rm = T),
            Shrubs_change = mean(Shrubs_change, na.rm = T))%>%
  drop_na(above_medsocial, Trees)%>%
  mutate_at(vars(year), as.numeric)

plot_socialscore_notyet <- targeting_priority_df %>%
  filter(year <= rptpro_ano)%>%
  group_by(year, above_medsocial)%>%
  summarise(Trees = mean(Trees, na.rm = T),
            Trees_pct = mean(Trees_pct, na.rm = T),
            Trees_change = mean(Trees_change, na.rm = T),
            Grassland = mean(Grassland, na.rm = T),
            Grassland_pct = mean(Grassland_pct, na.rm = T),
            Grassland_change = mean(Grassland_change, na.rm = T),
            Crop = mean(Crop, na.rm = T),
            Crop_pct = mean(Crop_pct, na.rm = T),
            Crop_change = mean(Crop_change, na.rm = T),
            Shrubs = mean(Shrubs, na.rm = T),
            Shrubs_pct = mean(Shrubs_pct, na.rm = T),
            Shrubs_change = mean(Shrubs_change, na.rm = T))%>%
  drop_na(above_medsocial, Trees)%>%
  mutate_at(vars(year), as.numeric)

ggplot(data = plot_socialscore %>% filter(year <= 2009), aes(x = year, y = Trees_change, 
                                                             color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()

ggplot(data = plot_socialscore %>% filter(year <= 2009), aes(x = year, y = Grassland_change, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()

ggplot(data = plot_socialscore %>% filter(year <= 2009), aes(x = year, y = Crop_change, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()



ggplot(data = plot_socialscore_notyet, aes(x = year, y = Trees_change, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()

ggplot(data = plot_socialscore_notyet, aes(x = year, y = Grassland_change, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()

ggplot(data = plot_socialscore_notyet, aes(x = year, y = Crop_change, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line()
