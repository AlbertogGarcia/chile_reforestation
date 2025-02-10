library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(RColorBrewer)

file_dir <- my_data_dir
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
results_dir <- here::here("analysis_main", "results")


carbon_table <- read.csv(paste0(file_dir, "/data/carbon/carbon_table.csv"))%>%
  rename(numero_region = 1)

all_property_wide <- readRDS(paste0(clean_data_dir, "/all_property_share_wide.rds"))

compliers <- all_property_wide %>%
  filter(treat == 1 & received_bonus == 1)%>%
  select(property_ID, rptpro_tipo_concurso, rptpro_numero_region, rptpre_region, rptpro_superficie, rptpre_superficie_predial, rptpro_monto_total)%>%
  rename(numero_region = rptpro_numero_region)%>%
  mutate(proportion_subsidized = ifelse(rptpre_superficie_predial != 0 , rptpro_superficie/rptpre_superficie_predial, NA),
         payment_per_ha = rptpro_monto_total / rptpro_superficie,
         numero_region = ifelse(numero_region == 16, 8, numero_region))

dollar_per_utm <- 36679 * 0.0010274427

# complier_carbon_summary <- left_join(compliers, carbon_table, by = "numero_region")%>%
#   summarise_at(vars(Natural.forests, Plantations, Shrub, Agriculture, rptpre_superficie_predial, rptpro_monto_total, proportion_subsidized, payment_per_ha), mean, na.rm = T)

main_complier_impacts <- data.frame(
  "Trees_main" = 0.013546,
  "Crop_main" = -0.003435,
  "Grassland_main" = -0.009697
)

# carbon_price <- cbind(complier_carbon_summary, main_complier_impacts) %>%
#   mutate(ha_subsidized = proportion_subsidized * rptpre_superficie_predial ,
#     tree_cover_impact = Trees_main * ha_subsidized,
#          nativeforest_carbon_impact = Trees_main * ha_subsidized* Natural.forests,
#          crop_carbon_impact = Crop_main * ha_subsidized* Agriculture,
#          grassland_carbon_impact = Grassland_main * ha_subsidized* Agriculture,
#          carbon_impact_main = nativeforest_carbon_impact + crop_carbon_impact + grassland_carbon_impact,
#          CO2_impact_main = carbon_impact_main *3.67,
#          CO2_price_main = (rptpro_monto_total * dollar_per_utm)/ CO2_impact_main)

# Main carbon price estimate
# carbon_price$CO2_price_main
# carbon_price$tree_cover_impact*nrow(compliers)

# Total additional tree cover and carbon
complier_totals <- left_join(compliers, carbon_table, by = "numero_region")%>%
  mutate(hectares_subsidized = rptpro_superficie,
         tree_cover_impact = main_complier_impacts$Trees_main * hectares_subsidized,
         nativeforest_carbon_impact = tree_cover_impact * Natural.forests,
         crop_carbon_impact = main_complier_impacts$Crop_main * hectares_subsidized * Agriculture,
         grassland_carbon_impact = main_complier_impacts$Grassland_main * hectares_subsidized * Agriculture,
         carbon_impact_main = nativeforest_carbon_impact + crop_carbon_impact + grassland_carbon_impact,
         CO2_impact_main = carbon_impact_main *3.67,
         CO2_price_main = (rptpro_monto_total * dollar_per_utm)/ CO2_impact_main)

mean(complier_totals$CO2_price_main, na.rm = T)

sum(complier_totals$CO2_impact_main, na.rm = T)

sum(complier_totals$carbon_impact_main, na.rm = T)

smallholder_complier_impacts <- data.frame(
  "Trees_main" = 0.01824,
  "Crop_main" = -0.00499,
  "Grassland_main" = -0.01286
)

smallholder_totals <- left_join(compliers %>% filter(rptpro_tipo_concurso != "Otros Interesados"), carbon_table, by = "numero_region")%>%
  mutate(hectares_subsidized = rptpro_superficie,
         tree_cover_impact = smallholder_complier_impacts$Trees_main * hectares_subsidized,
         nativeforest_carbon_impact = tree_cover_impact * Natural.forests,
         crop_carbon_impact = smallholder_complier_impacts$Crop_main * hectares_subsidized * Agriculture,
         grassland_carbon_impact = smallholder_complier_impacts$Grassland_main * hectares_subsidized * Agriculture,
         carbon_impact_main = nativeforest_carbon_impact + crop_carbon_impact + grassland_carbon_impact,
         CO2_impact_main = carbon_impact_main *3.67,
         CO2_price_main = (rptpro_monto_total * dollar_per_utm)/ CO2_impact_main)

mean(smallholder_totals$CO2_price_main, na.rm = T)

sum(smallholder_totals$CO2_impact_main, na.rm = T)

sum(smallholder_totals$carbon_impact_main, na.rm = T)

# Average payment
# carbon_price$rptpro_monto_total * dollar_per_utm


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Based on balanced estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# compliers_balanced <- all_property_wide %>%
#   filter(treat == 1 & submitted_management_plan == 1 & (rptpro_ano %in% c(2009, 2010)))%>%
#   select(property_ID, rptpro_tipo_concurso, rptpro_numero_region, rptpre_region, rptpro_superficie, rptpre_superficie_predial, rptpro_monto_total)%>%
#   rename(numero_region = rptpro_numero_region)%>%
#   mutate(proportion_subsidized = ifelse(rptpre_superficie_predial != 0 , rptpro_superficie/rptpre_superficie_predial, NA),
#          payment_per_ha = rptpro_monto_total / rptpro_superficie,
#          numero_region = ifelse(numero_region == 16, 8, numero_region))
# 
# complier_carbon_balanced_summary <- left_join(compliers_balanced, carbon_table, by = "numero_region")%>%
#   summarise_at(vars(Natural.forests, Plantations, Shrub, Agriculture, rptpre_superficie_predial, rptpro_monto_total, proportion_subsidized, payment_per_ha), mean, na.rm = T)
# 
# balanced_complier_impacts <- data.frame(
#   "Trees_balance" = 6.937484e-03,
#   "Crop_balance" = -1.550001e-03,
#   "Grassland_balance" = -7.300938e-03
# )
# 
# 
# carbon_price_balanced <- cbind(complier_carbon_balanced_summary, balanced_complier_impacts) %>%
#   mutate(tree_carbon_impact_balance = Trees_balance * rptpre_superficie_predial * Natural.forests,
# crop_carbon_impact_balance = Crop_balance * rptpre_superficie_predial * Agriculture,
# grassland_carbon_impact_balance = Grassland_balance * rptpre_superficie_predial * Agriculture,
# carbon_impact_balance = tree_carbon_impact_balance + crop_carbon_impact_balance + grassland_carbon_impact_balance,
# CO2_impact_balance = carbon_impact_balance * 3.67,
# CO2_price_balance = (rptpro_monto_total * dollar_per_utm)/ CO2_impact_balance
# )
# 
# carbon_price_balanced$CO2_price_balance
# carbon_price$CO2_price_main
