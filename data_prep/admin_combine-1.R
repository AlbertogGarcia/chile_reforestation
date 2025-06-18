library(readxl)
library(tidyverse)
library(ggplot2)
library(rio)
library(stringi)

#my_data_dir <- here::here("remote")
output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

select <- dplyr::select
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in and combine Native Forest Law admin data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/proyecto.xlsx"))

#properties and coordinates
predio_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/predio.xlsx"))
#owners
propietario_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/propietario.xlsx"))
#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/actividad.xlsx"))

coordinadas_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/coordinadas_predio.xlsx"))

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props"), remove = F)%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))

export(property_df, paste0(output_dir, "/property_df.rds"))


admin_df <- property_df %>%
  mutate(indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0)
  )

comunal_pov <- readxl::read_excel(paste0(my_data_dir,"/data/analysis_lc/comunal_poverty.xlsx")
)%>%
  mutate(comuna = tolower(accents(comuna)))#%>%
#  mutate_at(vars(cpov_pct_2007), ~as.numeric(as.character(.)))


sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_df <- admin_df %>%
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
         VPS = ifelse(is.na(rptprop_razon_social), 0, 100),
         adjusted_puntaje = rptpro_puntaje - .2*VI - .35*VP - 0.05*VPS,
         social_puntaje = .2*VI + .35*VP + 0.05*VPS,
         smallholder = 1) %>%
  mutate(comuna = tolower(accents(rptpre_comuna)))%>%
  left_join(comunal_pov, by = "comuna")

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- admin_df %>%
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
         VPS = ifelse(is.na(rptprop_razon_social), 0, 100),
         adjusted_puntaje = rptpro_puntaje - .25*VI - .25*VP - 0.05*VPS,
         social_puntaje = .25*VI + .25*VP + 0.05*VPS,
         smallholder = 0
  )%>%
  mutate(comuna = tolower(accents(rptpre_comuna)))%>%
  left_join(comunal_pov, by = "comuna")
#fuzzyjoin::stringdist_left_join(comunal_pov, by = "comuna", max_dist = 2, distance_col = "sdist_comuna")

admin_df <- rbind(other_df, small_df) %>%
  distinct(rptpre_id, rptpro_id, ROL, rptpre_comuna, .keep_all = T)

export(admin_df, paste0(output_dir, "/admin_df.rds"))

