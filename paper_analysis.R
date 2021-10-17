
library(stringi)
library(tidyverse)
library(ggplot2)
library(did)
source(here::here("compute_attgt_interaction.R"))
#garci/agarcia
nativeforestcontest <- readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/submittedmp_analysis/NFL_df.rds")
mgmtplan_analysis <- readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/submittedmp_analysis/mgmtplan_analysis_updated.rds")
mgmtplan_analysis$geometry.x <- NULL
mgmtplan_analysis$geometry.y <- NULL
mgmtplan_analysis$geometry <- NULL
dta <- mgmtplan_analysis %>%
  mutate(area_diff = abs(property_area_ha - rptpre_superficie_predial),
         #cut = ifelse( area_diff > quantile(subset(mgmtplan_analysis, treat==1)$property_area_ha, .9), 1, 0),
         G = first.treat,
         period = Year,
         Y = EVI2,
         id = as.numeric(id),
         utm_per_ha = rptpre_monto_total/property_area_ha)
dta <- subset(dta, area_diff <= 1000 | treat ==0)
#############################################################################################
##### all enrolled project analysis
##############################################################################

unique_proj <- nativeforestcontest %>%
  distinct(rptpro_id, .keep_all = TRUE)%>%
  filter(rptpro_ano<2020)

project_objective_proportions <- table(unique_proj$rptpro_objetivo_manejo)/nrow(unique_proj)


evi_avg <- mean(subset(dta, Year == first.treat - 1 & treat ==1)$Y)

#############################################################################################
##### trends plots
##############################################################################


group_df <- dta %>%
  group_by(first.treat, period) %>%
  summarise(EVI2 = mean(EVI2),
            cohort = as.character(first.treat))%>%
  filter(first.treat %in% c(0, 2011, 2016))

cohort_trends_plot <- ggplot(group_df, aes(x = period, y = EVI2, color = cohort)) +
  geom_point() +
  geom_line() +
  theme_minimal()
cohort_trends_plot  

treat_df <- dta %>%
  group_by(treat, period) %>%
  summarise(EVI2 = mean(EVI2),
            group = as.character(treat))

trends_plot <- ggplot(treat_df, aes(x = period, y = EVI2, color = group)) +
  geom_point() +
  geom_line()

##############################################################################
##### DID analysis
##############################################################################





######### main results
#################################################################################
set.seed(0930)

main_regions <- subset(dta, rptpre_region == "Región de La Araucanía" | rptpre_region == "Región del Maule" | rptpre_region == "Región de Los Ríos"| rptpre_region == "Región de Ñuble" | rptpre_region == "Región del Bío-Bío" | treat == 0)

did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              control_group = "notyettreated",
              xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
              data=main_regions, clustervars = "id"
              , panel=TRUE, bstrap = TRUE
)
did.es <- aggte(did, type="dynamic")
did.ovr <- aggte(did, type="simple")

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects") +
  xlab("years since property enrollment") + ylab("EVI")+ ylim(-.012, 0.03)
plot_mgmt
ggsave(plot = plot_mgmt, width = 8, height = 5, path = "figs", filename = "main_did.png", dpi = 500)

####################################################################################

accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}
dta <- as.data.frame(dta)%>%
  dplyr::mutate(female = ifelse(rptprop_sexo == "Femenino", 1, ifelse(rptprop_sexo == "Masculino", 0, NA)),
         indig_etnia = ifelse(is.na(rptprop_etnia) & treat==1, 0, ifelse(treat==0, NA, 1)),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         otros = ifelse(rptpro_tipo_concurso == "Otros Interesados", 1, 0),
         small = ifelse(rptpro_tipo_concurso != "Otros Interesados", 1, 0))%>%
  dplyr::group_by(id)%>%
  dplyr::mutate(indigenous = ifelse(max(ind_comunidad) ==1 | max(indig_etnia) ==1, 1, indig_etnia),
         smallholder = ifelse(max(small)==1,1, small),
         other_interested = ifelse(max(otros)==1,1, otros)
  )%>%
  dplyr::ungroup()

timber_dta <- subset(dta, rptpro_objetivo_manejo == "PRODUCCION MADERERA" | treat == 0)
nottimber_dta <- subset(dta, rptpro_objetivo_manejo != "PRODUCCION MADERERA" | treat == 0)
indig_dta <- subset(dta, indigenous == 1 | treat == 0)
notindig_dta <- subset(dta, indigenous != 1 | treat == 0)
small_dta <- subset(dta, smallholder==1 | treat == 0)
other_dta <- subset(dta, other_interested==1 | treat == 0)

####################################################################################
#### results on timber production subset

timber_did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
              data=timber_dta, clustervars = "id"
              , panel=TRUE, bstrap = TRUE
)
timber_did.es <- aggte(timber_did, type="dynamic")
timber_did.ovr <- aggte(timber_did, type="simple")

timber_dyn.es <- data.frame("att" = timber_did.es$att.egt, "se" = timber_did.es$se.egt, "e" = timber_did.es$egt, "crit.val" = timber_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

timber_plot <- ggplot(data= timber_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for projects with timber production objective") +
  xlab("years since property enrollment") + ylab("EVI")+ ylim(-.012, 0.03)
timber_plot

ggsave(plot = timber_plot, width = 8, height = 5, path = "figs", filename = "timber_did.png", dpi = 500)

####################################################################################
#### results on not timber production subset

ntimber_did <- att_gt(yname="EVI2",
                     tname="Year",
                     idname="id",
                     gname="first.treat",
                     est_method = "reg",
                     xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                     data=nottimber_dta, clustervars = "id"
                     , panel=TRUE, bstrap = TRUE
)
ntimber_did.es <- aggte(ntimber_did, type="dynamic")
ntimber_did.ovr <- aggte(ntimber_did, type="simple")

ntimber_dyn.es <- data.frame("att" = ntimber_did.es$att.egt, "se" = ntimber_did.es$se.egt, "e" = ntimber_did.es$egt, "crit.val" = ntimber_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

ntimber_plot <- ggplot(data= ntimber_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for projects without timber production objective") +
  xlab("years since property enrollment") + ylab("EVI") + ylim(-.041, 0.048)
ntimber_plot

ggsave(plot = ntimber_plot, width = 8, height = 5, path = "figs", filename = "ntimber_did.png", dpi = 500)

####################################################################################
#### results on indigenous subset

indig_did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
              data=indig_dta, clustervars = "id"
              , panel=TRUE, bstrap = TRUE
)
indig_did.es <- aggte(indig_did, type="dynamic")
indig_did.ovr <- aggte(indig_did, type="simple")

indig_dyn.es <- data.frame("att" = indig_did.es$att.egt, "se" = indig_did.es$se.egt, "e" = indig_did.es$egt, "crit.val" = indig_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

indig_plot <- ggplot(data= indig_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for indigenous peoples") +
  xlab("years since property enrollment") + ylab("EVI") 
indig_plot

ggsave(plot = indig_plot, width = 8, height = 5, path = "figs", filename = "indig_did.png", dpi = 500)

####################################################################################
#### results on non-indigenous subset

notindig_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "reg",
                    xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                    data=notindig_dta, clustervars = "id"
                    , panel=TRUE, bstrap = TRUE
)
notindig_did.es <- aggte(notindig_did, type="dynamic")
notindig_did.ovr <- aggte(notindig_did, type="simple")

indig_dyn.es <- data.frame("att" = indig_did.es$att.egt, "se" = indig_did.es$se.egt, "e" = indig_did.es$egt, "crit.val" = indig_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

indig_plot <- ggplot(data= indig_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for indigenous peoples") +
  xlab("years since property enrollment") + ylab("EVI") 
indig_plot

ggsave(plot = indig_plot, width = 8, height = 5, path = "figs", filename = "indig_did.png", dpi = 500)


####################################################################################
#### results on smallholders subset

smallholder_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "reg",
                    xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                    data=small_dta, clustervars = "id"
                    , panel=TRUE, bstrap = TRUE
)
smallholder_did.es <- aggte(smallholder_did, type="dynamic")
smallholder_did.ovr <- aggte(smallholder_did, type="simple")

smallholder_dyn.es <- data.frame("att" = smallholder_did.es$att.egt, "se" = smallholder_did.es$se.egt, "e" = smallholder_did.es$egt, "crit.val" = smallholder_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

smallholder_plot <- ggplot(data= smallholder_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for the smallholder contest") +
  xlab("years since property enrollment") + ylab("EVI") 
smallholder_plot

ggsave(plot = smallholder_plot, width = 8, height = 5, path = "figs", filename = "smallholder_did.png", dpi = 500)

####################################################################################
#### results on other interested parties subset

otros_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "reg",
                    xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                    data=other_dta, clustervars = "id"
                    , panel=TRUE, bstrap = TRUE
)
otros_did.es <- aggte(otros_did, type="dynamic")
otros_did.ovr <- aggte(otros_did, type="simple")

otros_dyn.es <- data.frame("att" = otros_did.es$att.egt, "se" = otros_did.es$se.egt, "e" = otros_did.es$egt, "crit.val" = otros_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

otros_plot <- ggplot(data= otros_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for other interested parties") +
  xlab("years since property enrollment") + ylab("EVI") 
otros_plot

ggsave(plot = otros_plot, width = 8, height = 5, path = "figs", filename = "otros_did.png", dpi = 500)

####################################################################################
#### results on female subset

female_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "reg",
                    xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                    data=subset(dta, rptprop_sexo == "Femenino"|treat==0), clustervars = "id"
                    , panel=TRUE, bstrap = TRUE
)
female_did.es <- aggte(female_did, type="dynamic")
female_did.ovr <- aggte(female_did, type="simple")

female_dyn.es <- data.frame("att" = female_did.es$att.egt, "se" = female_did.es$se.egt, "e" = female_did.es$egt, "crit.val" = female_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

female_plot <- ggplot(data= female_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for female landowners") +
  xlab("years since property enrollment") + ylab("EVI") 
female_plot

ggsave(plot = female_plot, width = 8, height = 5, path = "figs", filename = "female_did.png", dpi = 500)

####################################################################################
#### results on male subset

male_did <- att_gt(yname="EVI2",
                     tname="Year",
                     idname="id",
                     gname="first.treat",
                     est_method = "reg",
                     xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                     data=subset(dta, rptprop_sexo != "Femenino"|treat==0), clustervars = "id"
                     , panel=TRUE, bstrap = TRUE
)
male_did.es <- aggte(male_did, type="dynamic")
male_did.ovr <- aggte(male_did, type="simple")

male_dyn.es <- data.frame("att" = male_did.es$att.egt, "se" = male_did.es$se.egt, "e" = male_did.es$egt, "crit.val" = male_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

male_plot <- ggplot(data= male_dyn.es, aes(x=e, y=att, color = period)) +
  geom_point() +
  geom_errorbar(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), width=0.1) +
  ggtitle("Event time treatment effects for male landowners") +
  xlab("years since property enrollment") + ylab("EVI") 
male_plot

ggsave(plot = male_plot, width = 8, height = 5, path = "figs", filename = "male_did.png", dpi = 500)
########################################################################################################

ref <- dta %>%
  group_by(id)%>%
  mutate(ref = ifelse(max(reforestation2)==1, 1, 0))%>%
  filter(ref==1|treat==0)
ref_did <- att_gt(yname="EVI2",
                   tname="Year",
                   idname="id",
                   gname="first.treat",
                   est_method = "reg",
                   xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                   data=ref, clustervars = "id"
                   , panel=TRUE, bstrap = TRUE
)

ref_did.es <- aggte(test_did, type="dynamic")
ref_did.ovr <- aggte(test_did, type="simple")
########################################################################################################

Main_ATT <- data.frame( subsample = c("main", "timber", "not timber", "indigenous","smallholder", "other", "female", "male", "ref", "main", "timber", "not timber", "indigenous","smallholder", "other",  "female", "male", "ref"),
                        aggregation = c(rep("ATT_ovr", 9), rep("ATT_equal", 9)),
                        ATT = c(did.es$overall.att, timber_did.es$overall.att, ntimber_did.es$overall.att, indig_did.es$overall.att, smallholder_did.es$overall.att, otros_did.es$overall.att, female_did.es$overall.att, male_did.es$overall.att, ref_did.es$overall.att,
                                did.ovr$overall.att, timber_did.ovr$overall.att, ntimber_did.ovr$overall.att, indig_did.ovr$overall.att, smallholder_did.ovr$overall.att, otros_did.ovr$overall.att, female_did.ovr$overall.att, male_did.ovr$overall.att, ref_did.ovr$overall.att),
                        std.error = c(did.es$overall.se, timber_did.es$overall.se, ntimber_did.es$overall.se, indig_did.es$overall.se, smallholder_did.es$overall.se, otros_did.es$overall.se, female_did.es$overall.se, male_did.es$overall.se, ref_did.es$overall.se,
                                      did.ovr$overall.se, timber_did.ovr$overall.se, ntimber_did.ovr$overall.se, indig_did.ovr$overall.se, smallholder_did.ovr$overall.se, otros_did.ovr$overall.se, female_did.ovr$overall.se, male_did.ovr$overall.se, ref_did.ovr$overall.se),
                        N = c(did$n, timber_did$n, ntimber_did$n, indig_did$n, smallholder_did$n, otros_did$n, female_did$n, male_did$n, ref_did$n,
                              did$n, timber_did$n, ntimber_did$n, indig_did$n, smallholder_did$n, otros_did$n, female_did$n, male_did$n, ref_did$n)
                        )%>%
  mutate(treated_props = N - length(unique(subset(dta, treat==0)$id)),
         p.01 = ifelse(ATT/std.error >= 2.576, 1, 0),
         p.05 = ifelse(ATT/std.error >= 1.96, 1, 0) ,
         p.1 = ifelse(ATT/std.error >= 1.645, 1, 0))

write.csv(Main_ATT, "subsample_results.csv")











#################################################################################
######### adjusting smallholder scores
#################################################################################


sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_dta <- small_dta %>%
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
         adjusted_puntaje = rptpro_puntaje - .2*VI - .35*VP - 0.05*VPS
  ) %>%
  group_by(first.treat) %>%
  mutate(q1 = quantile(rptpro_puntaje, probs = c(0.25), na.rm = TRUE),
         q2 = quantile(rptpro_puntaje, probs = c(0.5), na.rm = TRUE),
         q3 = quantile(rptpro_puntaje, probs = c(0.75), na.rm = TRUE),
         q4 = quantile(rptpro_puntaje, probs = c(1), na.rm = TRUE),
         adj_q1 = quantile(adjusted_puntaje, probs = c(0.25), na.rm = TRUE),
         adj_q2 = quantile(adjusted_puntaje, probs = c(0.5), na.rm = TRUE),
         adj_q3 = quantile(adjusted_puntaje, probs = c(0.75), na.rm = TRUE),
         adj_q4 = quantile(adjusted_puntaje, probs = c(1), na.rm = TRUE)
         )

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_dta <- other_dta %>%
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
         adjusted_puntaje = rptpro_puntaje - .25*VI - .25*VP - 0.05*VPS
  )%>%
  group_by(first.treat) %>%
  mutate(q1 = quantile(rptpro_puntaje, probs = c(0.25), na.rm = TRUE),
         q2 = quantile(rptpro_puntaje, probs = c(0.5), na.rm = TRUE),
         q3 = quantile(rptpro_puntaje, probs = c(0.75), na.rm = TRUE),
         q4 = quantile(rptpro_puntaje, probs = c(1), na.rm = TRUE),
         adj_q1 = quantile(adjusted_puntaje, probs = c(0.25), na.rm = TRUE),
         adj_q2 = quantile(adjusted_puntaje, probs = c(0.5), na.rm = TRUE),
         adj_q3 = quantile(adjusted_puntaje, probs = c(0.75), na.rm = TRUE),
         adj_q4 = quantile(adjusted_puntaje, probs = c(1), na.rm = TRUE)
  )


#################################################################################
######### quartile score DIDs
#################################################################################

other_q1 <- other_dta %>%
  filter(rptpro_puntaje <= q1 | treat == 0)

other_q2 <- other_dta %>%
  filter((rptpro_puntaje <= q2 & rptpro_puntaje > q1) | treat == 0)

other_q3 <- other_dta %>%
  filter((rptpro_puntaje <= q3 & rptpro_puntaje > q2) | treat == 0)

other_q4 <- other_dta %>%
  filter((rptpro_puntaje <= q4 & rptpro_puntaje > q3) | treat == 0)


small_q1 <- small_dta %>%
  filter(rptpro_puntaje <= q1 | treat == 0)

small_q2 <- small_dta %>%
  filter((rptpro_puntaje <= q2 & rptpro_puntaje > q1) | treat == 0)

small_q3 <- small_dta %>%
  filter((rptpro_puntaje <= q3 & rptpro_puntaje > q2) | treat == 0)

small_q4 <- small_dta %>%
  filter((rptpro_puntaje <= q4 & rptpro_puntaje > q3) | treat == 0)


############################################################################################################

small_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = small_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q1.es <- aggte(small_did_q1, type="dynamic")
#smallholder_q1.ovr <- aggte(small_did_q1, type="simple")


small_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q2.es <- aggte(small_did_q2, type="dynamic")
#smallholder_q2.ovr <- aggte(small_did_q2, type="simple")


small_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = small_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q3.es <- aggte(small_did_q3, type="dynamic")
#smallholder_q3.ovr <- aggte(small_did_q3, type="simple")


small_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = small_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q4.es <- aggte(small_did_q4, type="dynamic")
#smallholder_q4.ovr <- aggte(small_did_q4, type="simple")
  
####################################################################################################




other_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = other_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q1.es <- aggte(other_did_q1, type="dynamic")
other_q1.ovr <- aggte(other_did_q1, type="simple")


other_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = other_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q2.es <- aggte(other_did_q2, type="dynamic")
other_q2.ovr <- aggte(other_did_q2, type="simple")


other_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = other_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q3.es <- aggte(other_did_q3, type="dynamic")
other_q3.ovr <- aggte(other_did_q3, type="simple")


other_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = other_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q4.es <- aggte(other_did_q4, type="dynamic")
other_q4.ovr <- aggte(other_did_q4, type="simple")




######################################################################################################

adj_other_q1 <- other_dta %>%
  filter(adjusted_puntaje <= q1_adj | treat == 0)

adj_other_q2 <- other_dta %>%
  filter((adjusted_puntaje <= q2_adj & adjusted_puntaje > q1_adj) | treat == 0)

adj_other_q3 <- other_dta %>%
  filter((adjusted_puntaje <= q3_adj & adjusted_puntaje > q2_adj) | treat == 0)

adj_other_q4 <- other_dta %>%
  filter((adjusted_puntaje <= q4_adj & adjusted_puntaje > q3_adj) | treat == 0)



adj_small_q1 <- small_dta %>%
  filter(adjusted_puntaje <= q1_adj | treat == 0)

adj_small_q2 <- small_dta %>%
  filter((adjusted_puntaje <= q2_adj & adjusted_puntaje > q1_adj) | treat == 0)

adj_small_q3 <- small_dta %>%
  filter((adjusted_puntaje <= q3_adj & adjusted_puntaje > q2_adj) | treat == 0)

adj_small_q4 <- small_dta %>%
  filter((adjusted_puntaje <= q4_adj & adjusted_puntaje > q3_adj) | treat == 0)


################################################################################################

adj_small_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = adj_small_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

adj_smallholder_q1.es <- aggte(adj_small_did_q1, type="dynamic")
adj_smallholder_q1.ovr <- aggte(adj_small_did_q1, type="simple")


adj_small_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = adj_small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

adj_smallholder_q2.es <- aggte(adj_small_did_q2, type="dynamic")
adj_smallholder_q2.ovr <- aggte(adj_small_did_q2, type="simple")


adj_small_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = adj_small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

adj_smallholder_q3.es <- aggte(adj_small_did_q3, type="dynamic")
adj_smallholder_q3.ovr <- aggte(adj_small_did_q3, type="simple")


adj_small_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                       data = adj_small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

adj_smallholder_q4.es <- aggte(adj_small_did_q4, type="dynamic")
adj_smallholder_q4.ovr <- aggte(adj_small_did_q4, type="simple")

#########################################################################################################



adj_other_did_q1 <- att_gt(yname="EVI2",
                           tname="Year",
                           idname="id",
                           gname="first.treat",
                           est_method = "reg",
                           xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                           data = adj_other_q1, clustervars = "id"
                           , panel=TRUE, bstrap = TRUE
)

adj_other_q1.es <- aggte(adj_other_did_q1, type="dynamic")
adj_other_q1.ovr <- aggte(adj_other_did_q1, type="simple")


adj_other_did_q2 <- att_gt(yname="EVI2",
                           tname="Year",
                           idname="id",
                           gname="first.treat",
                           est_method = "reg",
                           xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                           data = adj_other_q2, clustervars = "id"
                           , panel=TRUE, bstrap = TRUE
)

adj_other_q2.es <- aggte(adj_other_did_q2, type="dynamic")
adj_other_q2.ovr <- aggte(adj_other_did_q2, type="simple")


adj_other_did_q3 <- att_gt(yname="EVI2",
                           tname="Year",
                           idname="id",
                           gname="first.treat",
                           est_method = "reg",
                           xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                           data = adj_other_q3, clustervars = "id"
                           , panel=TRUE, bstrap = TRUE
)

adj_other_q3.es <- aggte(adj_other_did_q3, type="dynamic")
adj_other_q3.ovr <- aggte(adj_other_did_q3, type="simple")


adj_other_did_q4 <- att_gt(yname="EVI2",
                           tname="Year",
                           idname="id",
                           gname="first.treat",
                           est_method = "reg",
                           xformla=~  road_dist + proportion_erosion + industry_dist +native_industry_dist + forest + plantation + baresoil + pasture + shrub + urban + water + slope + lat +  elev  + property_area_ha ,
                           data = adj_other_q4, clustervars = "id"
                           , panel=TRUE, bstrap = TRUE
)

adj_other_q4.es <- aggte(adj_other_did_q4, type="dynamic")
adj_other_q4.ovr <- aggte(adj_other_did_q4, type="simple")


es_points <- data.frame(
  "agg_type" = rep("es", 16),
  "contest" = c(rep("other interested", 8), rep("smallholders", 8)),
  "quartile" = rep(seq(1, 4, by = 1), 4), 
  "scoring method" = c(rep("standard", 4), rep("adjusted", 4), rep("standard", 4), rep("adjusted", 4)),
  "ATT" = c(other_q1.es$overall.att, other_q2.es$overall.att, other_q3.es$overall.att, other_q4.es$overall.att,
            adj_other_q1.es$overall.att, adj_other_q2.es$overall.att, adj_other_q3.es$overall.att, adj_other_q4.es$overall.att,
            small_q1.es$overall.att, small_q2.es$overall.att, small_q3.es$overall.att, small_q4.es$overall.att,
            adj_small_q1.es$overall.att, adj_small_q2.es$overall.att, adj_small_q3.es$overall.att, adj_small_q4.es$overall.att),
  "se" = c(other_q1.es$overall.se, other_q2.es$overall.se, other_q3.es$overall.se, other_q4.es$overall.se,
           adj_other_q1.es$overall.se, adj_other_q2.es$overall.se, adj_other_q3.es$overall.se, adj_other_q4.es$overall.se,
           small_q1.es$overall.se, small_q2.es$overall.se, small_q3.es$overall.se, small_q4.es$overall.se,
           adj_small_q1.es$overall.se, adj_small_q2.es$overall.se, adj_small_q3.es$overall.se, adj_small_q4.es$overall.se)
    )











#################################################################################
######### land-use regressions
#################################################################################

# the number of bootstrap iterations
biters <- 60
varname = "plantation"
# list to store bootstrap results
boot.int.dyn <- list()
boot.int.ovr<- list()
boot.dyn <- list()
boot.ovr<- list()

# loop for each nonparametric bootstrap iteration
for (b in 1:biters) {
  # draw a bootstrap sample; here, we'll call an outside function
  bdata <- BMisc::blockBootSample(dta, "id")
  # call our function for estimating dynamic effects on the
  # bootstrapped data
  
  ### forest estimates - interaction and treatment effects
  for_int <- compute_attgt_interaction(bdata, interact_var = varname, interaction = TRUE)
  boot.int.dyn[[b]] <- for_int$dyn.ovr
  boot.int.ovr[[b]] <- for_int$simple.att
  
  for_treat <- compute_attgt_interaction(bdata, interact_var = varname, interaction = FALSE)
  boot.dyn[[b]] <- for_treat$dyn.ovr
  boot.ovr[[b]] <- for_treat$simple.att
  
  print(b)
}

# list to store bootstrap results
boot.int.dyn<- sd(t(simplify2array(boot.int.dyn)))
boot.int.ovr<- sd(t(simplify2array(boot.int.ovr)))
boot.dyn <- sd(t(simplify2array(boot.dyn)))
boot.ovr<- sd(t(simplify2array(boot.ovr)))

results_int <- compute_attgt_interaction(dta, interact_var = varname, interaction = TRUE)
results_treat <- compute_attgt_interaction(dta, interact_var = varname, interaction = FALSE)
# add the standard errors to the main results
df_results <- data.frame(
  "term" = c("interaction", "interaction", "treatment", "treatment"),
  "ATT" = c(results_int$dyn.ovr , results_int$simple.att, results_treat$dyn.ovr , results_treat$simple.att ),
  "type" = c("dyn", "simple", "dyn", "simple"),
  "se" = c(boot.int.dyn, boot.int.ovr, boot.dyn, boot.ovr)
)

write.csv(df_results, "erosion_boot.csv")

