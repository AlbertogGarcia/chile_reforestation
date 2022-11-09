library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(RColorBrewer)

file_dir <- "C:/Users/garci/Dropbox/chile_reforestation/"
results_dir <- "paper/results/"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/proyecto.xlsx"))

#properties and coordinates
predio_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/predio.xlsx"))
#owners
propietario_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/propietario.xlsx"))
#stands
rodal_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/actividad.xlsx"))

coordinadas_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/coordinadas_predio.xlsx"))

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

covariates_enrolled <- readRDS(paste0(file_dir, "data/analysis_lc/cleaned_properties/enrolled/covariates_enrolled.rds"))


comunal_pov <- read.csv(paste0(file_dir, "data/analysis_lc/comunal_poverty.csv"), encoding = "Latin-1")%>%
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
  mutate(cpov_pct_2007 = as.numeric(cpov_pct_2007),
         above_medpov = ifelse(cpov_pct_2007 > median(cpov_pct_2007, na.rm = T), 1, 0),
         cpov_categ = ifelse(
           above_medpov == 1, "Above median",
           "Below median"
         ),
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

year_lead = 10

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### By contest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_trends_bycontest <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  mutate(relative_year = as.numeric(year) - as.numeric(rptpro_ano))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  group_by(relative_year, class, `Contest type`)%>%
  summarise_at(vars(value), funs(mean, se=sd(.)/sqrt(n())))%>%
  dplyr::rename(value = mean)%>%
  ungroup %>%
  dplyr::group_by(class, `Contest type`)%>%
  dplyr::mutate(laggedval = lag(value, n = 1, default = NA),
                rate_of_change = (value - laggedval)/laggedval
  )
library(rio)
export(enrolled_trends_bycontest, paste0(results_dir, "enrolled_trends_bycontest.rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### By score
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_trends_byscore <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  mutate(relative_year = as.numeric(year) - as.numeric(rptpro_ano))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  group_by(relative_year, class, above_medsocial)%>%
  summarise_at(vars(value), funs(mean, se=sd(.)/sqrt(n())))%>%
  dplyr::rename(value = mean)%>%
  ungroup %>%
  dplyr::group_by(class, above_medsocial)%>%
  dplyr::mutate(laggedval = lag(value, n = 1, default = NA),
                rate_of_change = (value - laggedval)/laggedval,
                above_medsocial = ifelse(above_medsocial == 1, "High social priority", "Low social priority")
  )
library(rio)
export(enrolled_trends_byscore, paste0(results_dir, "enrolled_trends_byscore.rds"))

ggplot(data = enrolled_trends_byscore %>% filter(class == "Trees"), 
       aes(x = as.numeric(relative_year), y = rate_of_change, color = above_medsocial))+
  theme_minimal()+
  geom_line(size = 1.2)+
  scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover") + 
  xlab("Years prior to enrollment") +
  labs(color='Application social score')+
  scale_x_continuous(breaks = c(-10, -5, -1),
                     labels=c("10", "5", "1"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By contest and score

enrolled_trends_scorebycontest <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  mutate(relative_year = as.numeric(year) - as.numeric(rptpro_ano))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  group_by(relative_year, class, above_medsocial, `Contest type`)%>%
  summarise_at(vars(value), funs(mean, se=sd(.)/sqrt(n())))%>%
  dplyr::rename(value = mean)%>%
  group_by(class, `Contest type`, above_medsocial)%>%
  dplyr::mutate(laggedval = lag(value, n = 1, default = NA),
                rate_of_change = (value - laggedval)/laggedval,
                above_medsocial = ifelse(above_medsocial == 1, "High social priority", "Low social priority")
  )

export(enrolled_trends_scorebycontest, paste0(results_dir, "enrolled_trends_scorebycontest.rds"))

ggplot(data = enrolled_trends_scorebycontest %>% filter(class == "Trees"), 
       aes(x = as.numeric(relative_year), y = rate_of_change, color = above_medsocial, linetype = `Contest type`))+
  theme_minimal()+geom_line(size = 1.2)+scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover") + xlab("Years prior to enrollment") + ylab("Change in share of property with tree cover")+
  labs(color='Application social score')+
  scale_x_continuous(breaks = c(-10, -5, -1),
                     labels=c("10", "5", "1"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By cpov 

enrolled_trends_cpov <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  mutate(relative_year = as.numeric(year) - as.numeric(rptpro_ano))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  group_by(relative_year, class, cpov_categ)%>%
  summarise_at(vars(value), funs(mean, se=sd(.)/sqrt(n())))%>%
  dplyr::rename(value = mean)%>%
  group_by(class, cpov_categ)%>%
  dplyr::mutate(laggedval = lag(value, n = 1, default = NA),
                rate_of_change = (value - laggedval)/laggedval
  )%>%
  drop_na(cpov_categ)

export(enrolled_trends_cpov, paste0(results_dir, "enrolled_trends_cpov.rds"))


ggplot(data = enrolled_trends_cpov %>% filter(class == "Trees"), 
       aes(x = as.numeric(relative_year), y = rate_of_change, color = cpov_categ))+
  theme_minimal()+geom_line(size = 1.2)+scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover") + xlab("Years prior to enrollment") + ylab("Change in share of property with tree cover")+
  labs(color='Comuna-level poverty')+
  scale_x_continuous(breaks = c(-10,-5, -1),
                     labels=c("10", "5", "1"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By cpov and contest

enrolled_trends_cpovbycontest <- regrowth_scored %>%
  filter(pixels_count != 0)%>%
  mutate_at(vars(Trees_1999:NA_2018), ~ . / pixels_count)%>%
  pivot_longer(Trees_1999:NA_2018)%>%
  separate(name, into = c("class", "year"))%>%
  mutate(relative_year = as.numeric(year) - as.numeric(rptpro_ano))%>%
  filter(class %in% c("Trees", "Crop", "Grassland"))%>%
  group_by(relative_year, class, cpov_categ, `Contest type`)%>%
  summarise_at(vars(value), funs(mean, se=sd(.)/sqrt(n())))%>%
  dplyr::rename(value = mean)%>%
  group_by(class, cpov_categ, `Contest type`)%>%
  dplyr::mutate(laggedval = lag(value, n = 1, default = NA),
                rate_of_change = (value - laggedval)/laggedval
                )%>%
  drop_na(cpov_categ)

export(enrolled_trends_cpovbycontest, paste0(results_dir, "enrolled_trends_cpovbycontest.rds"))


ggplot(data = enrolled_trends_cpovbycontest %>% filter(class == "Trees"), 
       aes(x = as.numeric(relative_year), y = rate_of_change, color = cpov_categ, linetype = `Contest type`))+
  theme_minimal()+geom_line(size = 1.2)+scale_y_continuous(labels = scales::percent)+
  ggtitle("Enrollees' change in Tree cover") + xlab("Years prior to enrollment") + ylab("Change in share of property with tree cover")+
  labs(color='Comuna-level poverty')+
  scale_x_continuous(breaks = c(-10,-5, -1),
                     labels=c("10", "5", "1"))

