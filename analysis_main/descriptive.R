library(tidyverse)
library(ggplot2)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)
library(readxl)
library(here)
library(ggplot2)
library(ggpubr)

clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
results_dir <- here("analysis_main", "results")

dollar_per_utm <- 36679 * 0.0010274427

admin_df <- readRDS(paste0(clean_data_dir, "/admin_df.rds")) %>%
  filter(rptpro_ano <= 2019)%>%
  mutate(proportion_subsidized = rptpre_superficie_bonificada/rptpre_superficie_predial,
         rptpro_monto_total = rptpro_monto_total*dollar_per_utm
         )%>%
  mutate_at(vars(cpov_pct_2007), as.numeric)%>%
  mutate_at(vars(
    proportion_subsidized, received_bonus, submitted_management_plan, timber, extensionista
  ), ~.*100)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Starting to integrate matched properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
covariates_enrolled <- readRDS(paste0(clean_data_dir, "/covariates_enrolled.rds"))

regrowth <- covariates_enrolled %>%
  filter(pixels_count != 0 )%>%
  inner_join(admin_df, by = c("rptpre_id","rptpro_id",  "ROL"))%>%
  group_by(rptpre_id, rptpro_id, ROL, rptpre_comuna)%>%
  slice_head()%>%
  ungroup()%>%
  mutate(lu_pixels_count = Forest + Plantation + Urban + Water + `Snow/ice` + `No data` + `Pasture and ag` + Shrub + `Permanent bare soil` + `Temporary bare soil`)%>%
  mutate_at(vars(Trees_2000:`0_2018`), ~ . / pixels_count)%>%
  mutate_at(vars(Forest, Plantation), ~ . / lu_pixels_count)%>%
  mutate_at(vars(Forest, Plantation, Trees_2000:`0_2018`), ~ . *100)%>%
  mutate(property_hectares = pixels_count / 0.09 ,
         ind_dist = unlist(ind_dist)/1000,
         natin_dist = unlist(natin_dist)/1000,
         city_dist = unlist(city_dist)/1000,
         Trees_0800 = Trees_2008 - Trees_2000
  )

#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- rodal_df %>%
  left_join(read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/actividad.xlsx"))
            , by = "rptro_id")
table(actividades_df$rptac_tipo)
nonrefor <- actividades_df %>% filter(!(rptac_tipo %in% c("regeneracion", "plantacion", "plantacion-suplementaria", "enriquecimiento", "siembra-directa", "corta-regeneracion", "escarificado-manual", "escarificado-mecanico")))
length(unique(nonrefor$rptpre_id))/length(unique(actividades_df$rptpre_id))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Property size and proportion enrolled density plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

psize_density <- ggplot(data = admin_df, aes(x = rptpre_superficie_predial, color = `Contest type`))+
  geom_density()+
  xlim(0,600)+
  scale_colour_manual(values = c(palette$red, palette$blue))+
  xlab("Property size (ha)")+
  theme_minimal()+
  ggtitle("Distribution of enrollee property sizes")
psize_density
ggsave(psize_density, path = "analysis_main/figs", filename = "psize_density.png",
       width = 6, height = 4)

proportion_subsidy_density <- ggplot(data = admin_df, aes(x = proportion_subsidized, color = `Contest type`))+
  geom_density()+
  xlim(0,1)+
  scale_colour_manual(values = c(palette$red, palette$blue))+
  xlab("Proportion of property enrolled")+
  theme_minimal()+
  ggtitle("Proportion of property subsidized amongst enrollees")
proportion_subsidy_density
ggsave(proportion_subsidy_density, path = "analysis_main/figs", filename = "proportion_subsidy_density.png",
       width = 6, height = 4)

ggarrange(psize_density, proportion_subsidy_density, ncol = 1, nrow = 2,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave(paste0(here("analysis_main", "figs"), "/density_duo.png"), width = 8, height = 9)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Main summary statistics table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(psych)

app_labs <- data.frame(name1 = c("rptpre_superficie_predial", "rptpro_superficie", "rptpro_monto_total", "proportion_subsidized", "cpov_pct_2007", "timber", "extensionista", "submitted_management_plan", "received_bonus"),
                   name2 = c("Property size (ha)", "Subsidized area (ha)", "Bonus amount (USD)", "Percent of property subsidized", "Comuna poverty (%)", "Timber production objective (%)", "Received extensionist support (%)", "Submitted management plan (%)", "Received payment (%)"))

app_summary <- describe((admin_df %>% filter(is.finite(proportion_subsidized)))[, app_labs$name1]) %>% select(mean, median, sd) %>%
  rownames_to_column()

app_summary$rowname <- app_labs$name2

regrowth_labs <- data.frame(name1 = c("ind_dist", "natin_dist", "city_dist", "elev", "Forest", "Plantation", "Trees_0800", "Crop_2008", "Grassland_2008", "Shrubs_2008"),
                       name2 = c("Dist. to sawmill (km)", "Dist. to native timber processing (km)", "Dist. to city (km)", "Elevation (m)", "Native forest (2001 %)", "Plantation (2001 %)", "Tree cover change (pp, 00-08)", "Crop (2008 %)", "Grassland (2008 %)",  "Shrubs (2008 %)"))

regrowth_summary <- describe(regrowth[, regrowth_labs$name1]) %>% select(mean, median, sd) %>%
  rownames_to_column() 
regrowth_summary$rowname <- regrowth_labs$name2

main_summarystats <- rbind(
  app_summary, 
  regrowth_summary
  )%>%
  mutate_at(vars(mean:sd), ~ round(., digits = 2))

kbl(main_summarystats,
      format = "latex",
      booktabs = T,
      caption = "Summary statistics describing participants in the Native Forest Law subsidy competition",
      col.names = c("Variable", "Mean", "Median", "Std. dev."),
      align = c("l", "c", "c", "c"),
      label = "summary-main"
)%>%
  pack_rows("Application characteristics", 1, 7) %>%
  pack_rows("Follow-through", 8, 9)%>%
  pack_rows("Property accessibility", 10, 13)%>%
  pack_rows("Land cover", 14, 19)%>%
  kableExtra::save_kable(paste0(results_dir, "/summary_main.tex"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Now preparing to compare enrolled groups
# Smallholders vs. Other interested
# Compliers vs. Non-compliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Contest types

# variables needing matched set
#"Trees_trend", "Forest", "Plantation", "Trees_baseline", "Crop_baseline", "Grassland_baseline", "natin_dist", "ind_dist"

native_forest <- t.test(Forest ~ `Contest type`, data = regrowth)
plantation <- t.test(Plantation ~ `Contest type`, data = regrowth)
trees <- t.test(Trees_2008 ~ `Contest type`, data = regrowth)
crop <- t.test(Crop_2008 ~ `Contest type`, data = regrowth)
shrub <- t.test(Shrubs_2008 ~ `Contest type`, data = regrowth)
grassland <- t.test(Grassland_2008 ~ `Contest type`, data = regrowth)
native_ind <- t.test(natin_dist ~ `Contest type`, data = regrowth)
industry <- t.test(ind_dist ~ `Contest type`, data = regrowth)
city_dist <- t.test(city_dist ~ `Contest type`, data = regrowth)
elev <- t.test(elev ~ `Contest type`, data = regrowth)

# other variables - using full set of applicants
# "rptpre_superficie_predial", "rptpro_superficie", "proportion_subsidized", "rptpro_monto_total", "received_bonus", "submitted_management_plan", "timber", "extensionista"
area_prop <- t.test(rptpre_superficie_predial ~ `Contest type`, data = admin_df)
area_bono <- t.test(rptpro_superficie ~ `Contest type`, data = admin_df)
payment <- t.test(rptpro_monto_total ~ `Contest type`, data = admin_df)
timber <- t.test(timber ~ `Contest type`, data = admin_df)
proportion <- t.test(proportion_subsidized ~ `Contest type`, data = admin_df %>% filter(is.finite(proportion_subsidized)))
managementplan <- t.test(submitted_management_plan ~ `Contest type`, data = admin_df)
received_bonus <- t.test(received_bonus ~ `Contest type`, data = admin_df)
cpov <- t.test(cpov_pct_2007 ~ `Contest type`, data = admin_df)

covar <- c("Award (USD)", "Subsidized area (ha)", "Property area (ha)", "Timber production objective (%)" ,  "Elevation (m)", 
           "Native forest (%)", "Plantation (%)", "Grassland (%)", "Crop (%)", "Shrub (%)", 
           "Dist. to native timber processing (km)", "Dist. to sawmill (km)", "Dist. to city (km)")
smallholder <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], timber$estimate[2], elev$estimate[2],  
                 native_forest$estimate[2], plantation$estimate[2], grassland$estimate[2], crop$estimate[2], shrub$estimate[2], 
                 native_ind$estimate[2], industry$estimate[2], city_dist$estimate[2])



other_interested <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], timber$estimate[1], elev$estimate[1],  
                      native_forest$estimate[1], plantation$estimate[1], grassland$estimate[1], crop$estimate[1], shrub$estimate[1], 
                      native_ind$estimate[1], industry$estimate[1], city_dist$estimate[1])

# other_df <- ttest_df %>% filter(`Contest type` == "Other interested")
# other_interested_med <- c(median())

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, timber$p.value, elev$p.value,  
              native_forest$p.value, plantation$p.value, grassland$p.value, crop$p.value, shrub$p.value, 
              native_ind$p.value, industry$p.value, city_dist$p.value)

contest_ttest <- data.frame(covar, smallholder, other_interested, p_value) %>%
  mutate_at(vars(smallholder, other_interested), ~ round(., digits = 2))%>%
  mutate_at(vars(p_value), ~ round(., digits = 3))%>%
  mutate(p_value = ifelse(p_value < 0.001 , "< 0.001", p_value))

kbl(contest_ttest,
    format = "latex",
    booktabs = T,
    caption = "Comparison of Native Forest Law competition participants in the Smallholder versus Other Interested Parties Contests.",
    col.names = c(" ", "Smallholders", "Other interested", " "),
    align = c("l", "c", "c", "c"),
    label = "contest-ttest"
)%>%
  add_header_above(c("Variable" = 1, "Contest mean" = 2, "T-test p-value"=1))%>%
  kableExtra::save_kable(paste0(results_dir, "/contest_ttest_table.tex"))


