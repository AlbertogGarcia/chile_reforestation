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
##### By contest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_trends_bycontest <- regrowth_enrolled %>%
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
#export(enrolled_trends_bycontest, paste0(results_dir, "enrolled_trends_bycontest.rds"))

dyn.es_smallholder <- readRDS(paste0(results_dir, "dyn.es_smallholder.rds"))%>%
  mutate(`Contest type` = "Smallholder")
dyn.es_other <- readRDS(paste0(results_dir, "dyn.es_other.rds"))%>%
  mutate(`Contest type` = "Other interested")
dyn.es_bycontest <- rbind(dyn.es_other, dyn.es_smallholder)

treatment_trends <- enrolled_trends_bycontest %>%
  filter(class == "Trees")%>%
  ungroup()%>%
  rename(e = relative_year,
         observed = value)%>%
  select(observed, e, `Contest type`)%>%
  left_join(dyn.es_bycontest, by = c("e", "Contest type"))%>%
  mutate(counterfactual = observed - att)%>%
  filter(e >= -6)%>%
  pivot_longer(c("counterfactual", "observed"), names_to = "pot_outcome")%>%
  mutate(se = ifelse(pot_outcome == "observed", 0, se))

treatment_trees_plot <- ggplot(data = treatment_trends , 
                        aes(x = as.numeric(e), y = value, color = `Contest type`, linetype = pot_outcome))+
  theme_minimal()+
  geom_vline(xintercept = -0.75, linetype = "dashed")+
  geom_line()+
  geom_point(size = 1)+
  #geom_ribbon(aes(ymin=(value-crit.val*se), ymax=(value+crit.val*se)), alpha = 0.25)+
  xlab("Years prior to enrollment") + ylab("Change in share of property with tree cover")+
  scale_y_continuous(labels = scales::percent)+#, limits = c(ymin, ymax))+
  scale_color_manual(values = c(palette$dark, palette$blue))
treatment_trees_plot

treatment_trends_small <- treatment_trends %>% filter(`Contest type` == "Smallholder")
treatment_trends_other <- treatment_trends %>% filter(`Contest type` != "Smallholder")

ggplot()+
  theme_minimal()+
  geom_vline(xintercept = -0.75, linetype = "dashed")+
  geom_line(data = treatment_trends_small , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$blue)+
  geom_line(data = treatment_trends_other , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$dark)+
  geom_point(data = treatment_trends_small , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$blue)+
  geom_point(data = treatment_trends_other , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$dark)+
  geom_ribbon(data = treatment_trends_small %>% filter(se > 0), aes(x = as.numeric(e),ymin=(value-crit.val*se), ymax=(value+crit.val*se)), color = NA, alpha = 0.15)+
  geom_ribbon(data = treatment_trends_other %>% filter(se > 0), aes(x = as.numeric(e),ymin=(value-crit.val*se), ymax=(value+crit.val*se)), color = NA, alpha = 0.15)+
  xlab("event time") + ylab("Change in share of property with tree cover")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values=c("dashed", "solid"))+
  labs(linetype = "Potential outcome")


initial_small <- subset(treatment_trends_small, e == -6)$value
initial_other <-   subset(treatment_trends_other, e == -6)$value

treatment_even_small <- treatment_trends_small %>% mutate(value = value - initial_small)
treatment_even_other <- treatment_trends_other %>% mutate(value = value - initial_other)

ggplot()+
  theme_minimal()+
  geom_vline(xintercept = -0.75, linetype = "dashed")+
  geom_line(data = treatment_even_small , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$blue)+
  geom_line(data = treatment_even_other , 
            aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$dark)+
  geom_point(data = treatment_even_small , 
             aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$blue)+
  geom_point(data = treatment_even_other , 
             aes(x = as.numeric(e), y = value, linetype = pot_outcome), color = palette$dark)+
  geom_ribbon(data = treatment_even_small %>% filter(se > 0), aes(x = as.numeric(e),ymin=(value-crit.val*se), ymax=(value+crit.val*se)), color = NA, alpha = 0.2)+
  geom_ribbon(data = treatment_even_other %>% filter(se > 0), aes(x = as.numeric(e),ymin=(value-crit.val*se), ymax=(value+crit.val*se)), color = NA, alpha = 0.2)+
  xlab("Event time") + ylab("Change in share of property with tree cover")+
  scale_y_continuous(labels = scales::percent)+
  scale_linetype_manual(values=c("dashed", "solid"))
