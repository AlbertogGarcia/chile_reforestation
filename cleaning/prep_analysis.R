library(tidyverse)
library(stringi)
library(readxl)
setwd("C:/Users/garci/Dropbox/chile_reforestation/")


data_enrolled <- readRDS("data/analysis_lc/cleaned_properties/enrolled/data_enrolled.rds")%>%
  mutate(treat = 1)

accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

data_neverenrolled <- readRDS("data/analysis_lc/cleaned_properties/neverenrolled/data_neverenrolled.rds")%>%
  mutate(treat = 0,
         first.treat = 0)%>% 
  mutate_at("polyarea", as.double)%>%
  mutate(comuna = tolower(accents(desccomu)))

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
  mutate(comuna = accents(tolower(rptpre_comuna)),
         `Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1)),
         timber = as.numeric(ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0)),
         proportion_subsidized = ifelse(rptpre_superficie_predial != 0, rptpro_superficie / rptpre_superficie_predial,NA),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0)
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props")
  )%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1))%>%
  group_by(comuna, rptpre_id, ROL)%>%
  slice_head()

activity_df <- predio_df %>%
  # full_join(predio_df, by = "rptpro_id") %>%
  full_join(rodal_df, by = "rptpre_id") %>%
  full_join(actividades_df, by = "rptro_id") %>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props")
  )%>%
  mutate(comuna = accents(tolower(rptpre_comuna)),
         ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1),
         blank = 1)%>%
  select(rptac_tipo, ROL, rptpre_id, comuna, blank)%>%
  pivot_wider(names_from = rptac_tipo, values_from = blank, values_fn = length)%>%
  mutate_at(4:ncol(.), ~ ifelse(is.na(.), 0, 1))

admin_df <- property_df %>%
  group_by(rptpre_id, comuna, ROL)%>%
  mutate(first.treat = min(rptpro_ano))%>%
  filter(rptpro_ano == min(rptpro_ano))%>%
  slice_head()%>%
  ungroup %>%
  left_join(activity_df, by = c("rptpre_id", "comuna", "ROL"))

comunal_pov <- read.csv("data/analysis_lc/comunal_poverty.csv", encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))

all_property_wide <- data_enrolled %>%
  mutate(comuna = accents(tolower(comuna)))%>%
  inner_join(admin_df, by = c("rptpre_id", "comuna", "ROL"))%>%
  bind_rows(data_neverenrolled)%>%
  group_by(rptpre_id, comuna, ROL, objectid, treat, ciren_region)%>%
  dplyr::mutate(property_ID = cur_group_id())%>%
  ungroup %>%
  left_join(comunal_pov, by = "comuna")%>%
  mutate_at(vars(cpov_index_2007, cpov_pct_2007), as.numeric)
  

table(all_property_wide$first.treat)
summary(all_property_wide$cpov_index_2007)


library(rio)
export(all_property_wide, "data/analysis_lc/analysis_ready/all_property_wide.rds")






  