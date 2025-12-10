library(dplyr)
library(fixest)
library(sf)
library(kableExtra)
library(modelsummary)
library(here)
library(ggplot2)
library(ggpubr)
library(janitor)
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark_grey" = "grey30",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520",
                "brown" = "#613104")

dollar_per_utm <- 36679 * 0.0010274427

matched_data_long <- readRDS(paste0(clean_data_dir, "/matched_data_long.rds"))%>%
  mutate(post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         dollars = ifelse(treat == 1, rptpro_monto_total*dollar_per_utm, 0),
         dollars_ha = ifelse(treat == 1, dollars/rptpre_superficie_predial, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Trees
twfe_trees_all <- feols(Trees ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long)
summary(twfe_trees_all, vcov = ~property_ID)

twfe_trees_smallholder <- feols(Trees ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_trees_smallholder, vcov = ~property_ID)

twfe_trees_other <- feols(Trees ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_trees_other, vcov = ~property_ID)

twfe_trees_contests <- feols(Trees ~ treat:post:dollars_ha + treat:post:dollars_ha:`control_contest` | Year + property_ID, data = matched_data_long)
summary(twfe_trees_contests, vcov = ~property_ID)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Grassland

twfe_grassland_all <- feols(Grassland ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long)
summary(twfe_grassland_all, vcov = ~property_ID)

twfe_grassland_smallholder <- feols(Grassland ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_grassland_smallholder, vcov = ~property_ID)

twfe_grassland_other <- feols(Grassland ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_grassland_other, vcov = ~property_ID)

twfe_grassland_contests <- feols(Grassland ~ treat : post : dollars_ha * control_contest
                                 | Year + property_ID, 
                                 data = matched_data_long)
summary(twfe_grassland_contests, vcov = ~property_ID)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Crop

twfe_crop_all <- feols(Crop ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long)
summary(twfe_crop_all, vcov = ~property_ID)

twfe_crop_smallholder <- feols(Crop ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_crop_smallholder, vcov = ~property_ID)

twfe_crop_other <- feols(Crop ~ treat : post : dollars_ha| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_crop_other, vcov = ~property_ID)

twfe_crop_contests <- feols(Crop ~ treat : post : dollars_ha * control_contest
                            | Year + property_ID, 
                            data = matched_data_long)
summary(twfe_crop_contests, vcov = ~property_ID)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Jointly testing whether smallholders are distinct from other interested

matched_data_longer <- matched_data_long %>%
  mutate(treated = treat*post*dollars_ha) %>%
  pivot_longer(cols = c("Trees", "Grassland", "Crop"), names_to = "LU_outcome", values_to = "y")

est <- feols(y ~ LU_outcome + LU_outcome:(treated * control_contest) | Year + property_ID, 
             vcov = ~property_ID,
             data = matched_data_longer)
coef(est)
linearHypothesis(est, c(
  "LU_outcomeTrees:treated:control_contestPequeños Propietarios = 0",
  "LU_outcomeGrassland:treated:control_contestPequeños Propietarios = 0",
  "LU_outcomeCrop:treated:control_contestPequeños Propietarios = 0"
))
