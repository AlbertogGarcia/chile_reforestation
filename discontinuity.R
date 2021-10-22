library(tidyverse)
library(ggplot2)

regions_5tolosrios <- c(5,6,7,8,9, 10,14)

NFL_df <- readRDS("C:/Users/agarcia/Dropbox/chile_collab/input_files/NFL_df.rds")

discontinuity_main <- NFL_df %>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         sub_contest = ifelse( rptpro_tipo_presenta == "Extensionista", paste0(rptpro_tipo_concurso, " w/ extentionist"), paste0(rptpro_tipo_concurso, " w/o extentionist"))
         )%>%
  rename(property_size = rptpre_superficie_predial)%>%
  filter(property_size <= 500)%>%
  drop_na(received_bonus)%>%
  filter(rptpro_numero_region %in% regions_5tolosrios)%>%
  filter(
    (rptpro_tipo_concurso == "Otros Interesados" & property_size > 200) |
      (rptpro_tipo_concurso != "Otros Interesados" & property_size <= 200)
  )%>%
  filter(sub_contest != "Otros Interesados w/ extentionist")

ggplot(data = discontinuity_main, aes(x = property_size, y= received_bonus, color = rptpro_tipo_concurso))+
  geom_point()+
  geom_vline(xintercept = 205, size = 1.25, linetype = "dashed")+
  #stat_summary_bin(fun = "mean", geom = "point")+
  stat_summary_bin(fun = "mean", geom = "line", size = 1.25)+
  theme_minimal()+
  ylab ("received bonus") + xlab("property size (ha)")

###############################################################################
###############################################################################

discontinuity_2 <- NFL_df %>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         sub_contest = ifelse( rptpro_tipo_presenta == "Extensionista", paste0(rptpro_tipo_concurso, " w/ extentionist"), paste0(rptpro_tipo_concurso, " w/o extentionist"))
  )%>%
  rename(property_size = rptpre_superficie_predial)%>%
  filter(property_size <= 500)%>%
  drop_na(received_bonus)%>%
  filter(rptpro_numero_region %in% regions_5tolosrios)%>%
  filter(
    (rptpro_tipo_concurso == "Otros Interesados" & property_size > 200) |
      (rptpro_tipo_concurso != "Otros Interesados" & property_size <= 200)
  )%>%
  filter(sub_contest != "Otros Interesados w/ extentionist")

ggplot(data = discontinuity_2, aes(x = property_size, y= received_bonus, color = rptpro_tipo_concurso))+
  geom_point()+
  geom_vline(xintercept = 500, size = 1.25, linetype = "dashed")+
  #stat_summary_bin(fun = "mean", geom = "point")+
  stat_summary_bin(fun = "mean", geom = "line", size = 1.25)+
  theme_minimal()+
  ylab ("received bonus") + xlab("property size (ha)")
