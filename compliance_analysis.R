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
  full_join(propietario_df, by = "rptpre_id")

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

score_plot <- ggplot(data = NFL_df, aes(x = rptpro_puntaje, y = received_bonus, color = rptpro_tipo_concurso))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm") +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  xlab("project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by project score") +
  scale_color_discrete("contest")+
  theme_minimal()

ggsave(path = "figs", filename = "compliance_score_plot.png", width = 7, height = 5)

mod <- lm(received_bonus ~  rptpro_tipo_concurso*rptpro_puntaje,
           data = NFL_df)
summary(mod)
#### factors that predict compliance


mod2 <- lm(received_bonus ~ rptpro_tipo_concurso*rptpro_puntaje + extensionista +reforest +timber + nontimber + rptpro_superficie + rptpro_monto_total + rptpre_superficie_predial + rptpro_superficie,
          data = NFL_df)
summary(mod2)

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
         social_puntaje = .2*VI + .35*VP + 0.05*VPS) 

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
         social_puntaje = .25*VI + .25*VP + 0.05*VPS
  )

NFL_df <- rbind(other_df, small_df)

socialscore_plot <- ggplot(data = NFL_df, aes(x = social_puntaje, y = received_bonus, color = rptpro_tipo_concurso))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm", show.legend = FALSE) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  xlab("social component of project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by social score") +
  #scale_color_discrete("contest")+
  theme_minimal()

ggsave(path = "figs", filename = "compliance_socialscore_plot.png", width = 7, height = 5)

adjustedscore_plot <- ggplot(data = NFL_df, aes(x = adjusted_puntaje, y = received_bonus, color = rptpro_tipo_concurso))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm") +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  xlab("adjusted project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by adjusted score") +
  scale_color_discrete("contest")+
  theme_minimal()

ggsave(path = "figs", filename = "compliance_adjscore_plot.png", width = 7, height = 5)



#############################################################################################
#### matched set to examine heterogeneity in land use characteristics
#############################################################################################

my_rol_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

native_forest_law <- readRDS("C:/Users/garci/Dropbox/chile_collab/input_files/NFL_df.rds") %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")%>%
  mutate(submitted_mp = ifelse( rptpro_tiene_plan_saff=="Si" | rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         received_bonus = ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         treat = 1,
         first.treat = rptpro_ano)

length(unique(native_forest_law$rptpro_id))

rol_compliance <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  ungroup()%>%
  distinct(rptpro_id, .keep_all = TRUE)

compliance_matches <- rol_compliance %>%
  mutate(forest = c_1,
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
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0
         )%>%
  filter(rptpro_ano <= 2018)
  

evi <- t.test(evi_2007 ~ received_bonus, data = compliance_matches)
elev <- t.test(elev ~ received_bonus, data = compliance_matches)
slope <- t.test(slope ~ received_bonus, data = compliance_matches)
industry <- t.test(industry_dist ~ received_bonus, data = compliance_matches)
native_ind <- t.test(native_industry_dist ~ received_bonus, data = compliance_matches)
road_dist <- t.test(road_dist ~ received_bonus, data = compliance_matches)
shrub <- t.test(shrub ~ received_bonus, data = compliance_matches)
baresoil <- t.test(baresoil ~ received_bonus, data = compliance_matches)
water <- t.test(water ~ received_bonus, data = compliance_matches)
urban <- t.test(urban ~ received_bonus, data = compliance_matches)
pasture <- t.test(pasture ~ received_bonus, data = compliance_matches)
plantation <- t.test(plantation ~ received_bonus, data = compliance_matches)
forest <- t.test(forest ~ received_bonus, data = compliance_matches)
area_prop <- t.test(rptpre_superficie_predial ~ received_bonus, data = compliance_matches)
area_bono <- t.test(rptpro_superficie ~ received_bonus, data = compliance_matches)
payment <- t.test(rptpro_monto_total ~ received_bonus, data = compliance_matches)
timber <- t.test(timber ~ received_bonus, data = compliance_matches)
extensionista <- t.test(extensionista ~ received_bonus, data = compliance_matches)
erosion <- t.test(proportion_erosion ~ received_bonus, data = compliance_matches)

covar <- c("award (UTM)", "bonus area (ha)", "property area (ha)", "prob. had extensionist", "prob. timber production objective" ,  "proportion forest", "proportion plantation", "proportion pasture", "proportion shrub", "mod-to-severe erosion", "slope", "elevation", "dist. to native timber processing", "dist. to any timber processing", "dist. to road" )
paid <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], extensionista$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], pasture$estimate[2], shrub$estimate[2], erosion$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2], road_dist$estimate[2])
unpaid <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], extensionista$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], pasture$estimate[1], shrub$estimate[1], erosion$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1], road_dist$estimate[1])

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, extensionista$p.value, timber$p.value, forest$p.value, plantation$p.value, pasture$p.value, shrub$p.value, erosion$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value, road_dist$p.value)

compliance_ttest <- data.frame(covar, paid, unpaid, p_value) %>%
  mutate(p_value = round(p_value, digits = 5))

library(rio)
export(compliance_ttest, "compliance_ttest.rds")
