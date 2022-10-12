library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(RColorBrewer)
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

covariates_enrolled <- readRDS("data/analysis_lc/cleaned_properties/enrolled/covariates_enrolled.rds")


comunal_pov <- read.csv("data/analysis_lc/comunal_poverty.csv", encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))


regrowth_enrolled <- covariates_enrolled %>%
  inner_join(property_df, by = c("rptpre_id", "ROL"))%>%
  group_by(rptpre_id, rptpro_id, ROL)%>%
  slice_head()%>%
  ungroup()%>%
  mutate(comuna = tolower(accents(comuna)))%>%
  left_join(comunal_pov, by = "comuna")



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

points_regrowth <- regrowth_enrolled %>%
  mutate(cpov_index_2008 = as.numeric(cpov_index_2008),
         median_pov_index = median(cpov_index_2008, na.rm = T),
         above_medpov = ifelse(cpov_index_2008 >= median_pov_index, 1, 0),
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
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

regrowth_scored <- rbind(other_df, small_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Turning into panel for plotting
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enrolled_trends <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  summarise_at(vars(Trees_1999:NA_2018), mean, na.rm = T)%>%
  mutate(Trees_baseline = Trees_2000,
         Crop_baseline = Crop_2000,
         Grassland_baseline = Grassland_2000)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  mutate(pct = ifelse( class == "Trees", value / Trees_baseline - 1, 
                       ifelse(class == "Crop", value / Crop_baseline - 1, value / Grassland_baseline -1)),
         level = ifelse( class == "Trees", value - Trees_baseline, 
                       ifelse(class == "Crop", value - Crop_baseline, value - Grassland_baseline))
  )

ggplot(data = enrolled_trends %>% filter(class == "Trees", between(year, 2000, 2008)), 
       aes(x = as.numeric(year), y = pct))+
  theme_minimal()+geom_line(size = 1.2)+scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover (2000-2008)") + xlab("Year") + ylab("Percent change in tree cover")+
  labs(color='Application social score')

enrolled_trends_byscore <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  group_by(above_medsocial)%>%
  summarise_at(vars(Trees_1999:NA_2018), mean, na.rm = T)%>%
  mutate(Trees_baseline = Trees_2000,
         Crop_baseline = Crop_2000,
         Grassland_baseline = Grassland_2000)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  mutate(pct = ifelse( class == "Trees", value / Trees_baseline - 1, 
                       ifelse(class == "Crop", value / Crop_baseline - 1, value / Grassland_baseline -1)),
         level = ifelse( class == "Trees", value - Trees_baseline, 
                         ifelse(class == "Crop", value - Crop_baseline, value - Grassland_baseline)),
         above_medsocial = ifelse(above_medsocial == 1, "High social priority", "Low social priority")
  )

ggplot(data = enrolled_trends_byscore %>% filter(class == "Trees", between(year, 2000, 2008)), 
       aes(x = as.numeric(year), y = pct, color = as.character(above_medsocial)))+
  theme_minimal()+geom_line(size = 1.5)+scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover (2000-2008)") + xlab("Year") + ylab("Percent change in tree cover")+
  labs(color='Application social score')+
  scale_color_brewer(palette="Dark2")
