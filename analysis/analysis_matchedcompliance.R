library(tidyverse)
library(stringi)
#############################################################################################
#### matched set to examine heterogeneity in land use characteristics
#############################################################################################

my_rol_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

my_rol_match$geometry.x <- NULL
my_rol_match$geometry.y <- NULL
my_spatial_match$geometry.x <- NULL
my_spatial_match$geometry.y <- NULL

NFL_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/input_files/NFL_df.rds")%>%
  mutate(submitted_mp = ifelse( rptpro_tiene_plan_saff=="Si" | rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         received_bonus = ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         smallholder = ifelse(rptpro_tipo_concurso != "Otros Interesados", 1, 0),
         treat = 1,
         first.treat = rptpro_ano)

native_forest_law <- NFL_df %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")

length(unique(native_forest_law$rptpro_id))

rol_compliance <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  slice_head()

accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

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
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0
  )%>%
  filter(rptpro_ano <= 2018)











#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## random forest 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
# install.packages(urlPackage, repos=NULL, type="source") 
library(randomForest)

predictors <- c("forest", "plantation", "pasture", "urban", "water", "baresoil", "shrub", "snow", "road_dist", "industry_dist", "native_industry_dist", "indigenous", "timber", "nontimber", "reforest", "extensionista", "elev", "slope", "proportion_erosion",
                #   "rptprop_sexo", 
                "lat", "long",
                "rptpro_superficie", "rptpre_superficie_predial", "rptpro_monto_total")
rhs <- paste(predictors, collapse = " + ")

rf_data <- compliance_matches %>% 
  drop_na(predictors) %>% 
  mutate_at(vars(received_bonus, extensionista), as.factor) %>%
  as.data.frame()

set.seed(0930)
rf <-randomForest(as.formula(paste("received_bonus", "~", rhs)), data = rf_data, 
                  ntree = 500, importance=TRUE)
print(rf)                  
plot(rf)

ImpData <- as.data.frame(importance(rf))%>%
  mutate(Var.Names = row.names(.))%>%
  arrange(desc(MeanDecreaseAccuracy))#%>%
# head(10)

varImpPlot(rf)

ggplot(ImpData, aes(x=Var.Names, y=`MeanDecreaseAccuracy`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`MeanDecreaseAccuracy`), color="skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

partialPlot(rf, pred.data = rf_data, x.var = extensionista, which.class = 1,
            ylab = "probability of receiving bonus",
            xlab = "extensionista support",
            main = "Partial dependence on extensionista"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## t-tests compliers vs. non-compliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# variables needing matched set
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
timber <- t.test(timber ~ received_bonus, data = compliance_matches)
erosion <- t.test(proportion_erosion ~ received_bonus, data = compliance_matches)

# other variables - using full set of applicants
area_prop <- t.test(rptpre_superficie_predial ~ received_bonus, data = NFL_df)
area_bono <- t.test(rptpro_superficie ~ received_bonus, data = NFL_df)
payment <- t.test(rptpro_monto_total ~ received_bonus, data = NFL_df)
extensionista <- t.test(extensionista ~ received_bonus, data = NFL_df)


covar <- c("Award (UTM)", "Bonus area (ha)", "Property area (ha)", "Extensionist", "Timber production objective" ,  "Proportion forest", "Proportion plantation", "Proportion pasture", "Proportion shrub", "Mod-to-severe erosion", "Slope", "Elevation", "Dist. to native timber industry (m)", "Dist. to any timber industry (m)", "Dist. to road (m)" )
paid <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], extensionista$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], pasture$estimate[2], shrub$estimate[2], erosion$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2], road_dist$estimate[2])
unpaid <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], extensionista$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], pasture$estimate[1], shrub$estimate[1], erosion$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1], road_dist$estimate[1])

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, extensionista$p.value, timber$p.value, forest$p.value, plantation$p.value, pasture$p.value, shrub$p.value, erosion$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value, road_dist$p.value)

compliance_ttest <- data.frame(covar, paid, unpaid, p_value) %>%
  mutate_at(vars(paid, unpaid, p_value), ~ round(., digits = 3))
  #mutate(p_value = round(p_value, digits = 3))

library(rio)
export(compliance_ttest, "paper/results/compliance_ttest.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## t-tests separate contests
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evi <- t.test(evi_2007 ~ smallholder, data = compliance_matches)
elev <- t.test(elev ~ smallholder, data = compliance_matches)
slope <- t.test(slope ~ smallholder, data = compliance_matches)
industry <- t.test(industry_dist ~ smallholder, data = compliance_matches)
native_ind <- t.test(native_industry_dist ~ smallholder, data = compliance_matches)
road_dist <- t.test(road_dist ~ smallholder, data = compliance_matches)
shrub <- t.test(shrub ~ smallholder, data = compliance_matches)
baresoil <- t.test(baresoil ~ smallholder, data = compliance_matches)
water <- t.test(water ~ smallholder, data = compliance_matches)
urban <- t.test(urban ~ smallholder, data = compliance_matches)
pasture <- t.test(pasture ~ smallholder, data = compliance_matches)
plantation <- t.test(plantation ~ smallholder, data = compliance_matches)
forest <- t.test(forest ~ smallholder, data = compliance_matches)
timber <- t.test(timber ~ smallholder, data = compliance_matches)
erosion <- t.test(proportion_erosion ~ smallholder, data = compliance_matches)

compliance <- t.test(received_bonus ~ smallholder, data = NFL_df)
area_prop <- t.test(rptpre_superficie_predial ~ smallholder, data = NFL_df)
area_bono <- t.test(rptpro_superficie ~ smallholder, data = NFL_df)
payment <- t.test(rptpro_monto_total ~ smallholder, data = NFL_df)
extensionista <- t.test(extensionista ~ smallholder, data = NFL_df)

covar <- c("Complier", "Award (UTM)", "Bonus area (ha)", "Property area (ha)", "Extensionist", "Timber production objective" ,  "Proportion forest", "Proportion plantation", "Proportion pasture", "Proportion shrub", "Mod-to-severe erosion", "Slope", "Elevation", "Dist. to native timber industry", "Dist. to any timber industry", "Dist. to road" )
smallholder <- c(compliance$estimate[2], payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], extensionista$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], pasture$estimate[2], shrub$estimate[2], erosion$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2], road_dist$estimate[2])
other_interested <- c(compliance$estimate[1], payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], extensionista$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], pasture$estimate[1], shrub$estimate[1], erosion$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1], road_dist$estimate[1])

p_value <-  c(compliance$p.value, payment$p.value, area_bono$p.value,  area_prop$p.value, extensionista$p.value, timber$p.value, forest$p.value, plantation$p.value, pasture$p.value, shrub$p.value, erosion$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value, road_dist$p.value)

contest_ttest <- data.frame(covar, smallholder, other_interested, p_value) %>%
  mutate_at(vars(smallholder, other_interested, p_value), ~ round(., digits = 3))

export(contest_ttest, "paper/results/contest_ttest.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## t-tests extensionists in smallholder contest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
small_compliance_matches <- compliance_matches %>% filter(smallholder == 1)
small_NFL <- NFL_df %>% filter(smallholder == 1)

evi <- t.test(evi_2007 ~ extensionista, data = small_compliance_matches)
elev <- t.test(elev ~ extensionista, data = small_compliance_matches)
slope <- t.test(slope ~ extensionista, data = small_compliance_matches)
industry <- t.test(industry_dist ~ extensionista, data = small_compliance_matches)
native_ind <- t.test(native_industry_dist ~ extensionista, data = small_compliance_matches)
road_dist <- t.test(road_dist ~ extensionista, data = small_compliance_matches)
shrub <- t.test(shrub ~ extensionista, data = small_compliance_matches)
baresoil <- t.test(baresoil ~ extensionista, data = small_compliance_matches)
water <- t.test(water ~ extensionista, data = small_compliance_matches)
urban <- t.test(urban ~ extensionista, data = small_compliance_matches)
pasture <- t.test(pasture ~ extensionista, data = small_compliance_matches)
plantation <- t.test(plantation ~ extensionista, data = small_compliance_matches)
forest <- t.test(forest ~ extensionista, data = small_compliance_matches)
timber <- t.test(timber ~ extensionista, data = small_compliance_matches)
erosion <- t.test(proportion_erosion ~ extensionista, data = small_compliance_matches)

compliance <- t.test(received_bonus ~ extensionista, data = small_NFL)
area_prop <- t.test(rptpre_superficie_predial ~ extensionista, data = small_NFL)
area_bono <- t.test(rptpro_superficie ~ extensionista, data = small_NFL)
payment <- t.test(rptpro_monto_total ~ extensionista, data = small_NFL)

covar <- c("Complier", "Award (UTM)", "Bonus area (ha)", "Property area (ha)", "Timber production objective" ,  "Proportion forest", "Proportion plantation", "Proportion pasture", "Proportion shrub", "Mod-to-severe erosion", "Slope", "Elevation", "Dist. to native timber industry", "Dist. to any timber industry", "Dist. to road" )
extensionist <- c(compliance$estimate[2], payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], pasture$estimate[2], shrub$estimate[2], erosion$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2], road_dist$estimate[2])
none <- c(compliance$estimate[1], payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], pasture$estimate[1], shrub$estimate[1], erosion$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1], road_dist$estimate[1])

p_value <-  c(compliance$p.value, payment$p.value, area_bono$p.value,  area_prop$p.value, timber$p.value, forest$p.value, plantation$p.value, pasture$p.value, shrub$p.value, erosion$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value, road_dist$p.value)

extension_ttest <- data.frame(covar, extensionist, none, p_value) %>%
  mutate_at(vars(extensionist, none, p_value), ~ round(., digits = 3))

export(extension_ttest, "paper/results/extension_ttest.rds")