library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(stringi)
# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/proyecto.xlsx")

#properties and coordinates
predio_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/predio.xlsx")
#owners
propietario_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/propietario.xlsx")
#stands
rodal_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/rodal.xlsx")
#activities
actividades_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/actividad.xlsx")

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"))

activity_df <- property_df %>%
  left_join(rodal_df, by = "rptpre_id") %>%
  left_join(actividades_df, by = "rptro_id") %>%
  dcast(rptpro_id
        + rptpro_ano
        ~ rptac_tipo, 
        fun.aggregate = function(x){as.integer(length(x) > 0)})


accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

NFL_df <- property_df %>%
  filter(rptpro_ano <= 2019)%>%
  full_join(activity_df, by = c("rptpro_id", "rptpro_ano")) %>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff), 0, 1)),
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0)%>%
  distinct(rptpro_id, rptpre_id, received_bonus, .keep_all = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Property size Histogram
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ggplot(property_df, aes(x=rptpre_superficie_predial, fill = `Contest type`)) + 
  #geom_histogram(binwidth = 5) +
  geom_density(alpha = .5)+
  xlab("Property size (ha)")+
  theme_minimal()+
  #geom_vline(xintercept = 50, linetype = "dashed")+
  scale_x_continuous(breaks = c(0, 20, 50, 100, 200, 400, 600), limits = c(0, 700))+
  ggtitle("Distribution of applicant property sizes")


ggsave(width = 8, height = 5, path = "paper/figs", filename = "psize_density.png", dpi = 500)

smallholder_sizes <- (property_df %>% filter(`Contest type` == "Smallholder"))$rptpre_superficie_predial

pct_smallholders_above_100ha <- length(smallholder_sizes[smallholder_sizes >= 100]) / length(smallholder_sizes)
pct_smallholders_above_50ha <- length(smallholder_sizes[smallholder_sizes >= 50]) / length(smallholder_sizes)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_df <- NFL_df %>%
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
         smallholder = 1) 

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- NFL_df %>%
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
  )

NFL_df <- rbind(other_df, small_df)
library(rio)
#export(NFL_df, "compliance_df.rds")

compliance_mod1 <- lm(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                      + reforest  + as.factor(rptpre_region)
                      , data = NFL_df)
compliance_mod2 <- lm(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) + extensionista 
                      + reforest + as.factor(rptpre_region)
                      , data = NFL_df)
compliance_mod3 <- lm(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) + extensionista + smallholder + smallholder:log(social_puntaje + 1)
                      + reforest  + as.factor(rptpre_region)
                      , data = NFL_df)
compliance_mod4 <- lm(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) + extensionista + smallholder + smallholder:log(social_puntaje + 1)
                      + reforest  
                      , data = NFL_df)
save(compliance_mod1, compliance_mod2, compliance_mod3, compliance_mod4, file="paper/results/compliance_models.RData")


ext_mod1 <- lm(extensionista  ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
               + reforest +  as.factor(rptpre_region) 
               , data = NFL_df)
ext_mod2 <- lm(extensionista  ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) + smallholder*log(social_puntaje + 1) 
               + reforest +  as.factor(rptpre_region)
               , data = NFL_df)
ext_mod3 <- lm(extensionista  ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1)
               + reforest +  as.factor(rptpre_region) 
               , data = small_df)
ext_mod4 <- lm(extensionista  ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1)
               + reforest 
               , data = small_df)

save(ext_mod1, ext_mod2, ext_mod3, ext_mod4, file="paper/results/extension_models.RData")
