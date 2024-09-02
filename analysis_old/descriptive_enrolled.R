library(readxl)
library(tidyverse)
library(sf)
library(ggplot2)
library(stringi)
library(rio)


file_dir = "C:/Users/garci/Dropbox/chile_reforestation/"

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
         proportion_subsidized = ifelse(rptpre_superficie_predial != 0 & (rptpro_superficie / rptpre_superficie_predial) <= 1, rptpro_superficie / rptpre_superficie_predial,NA),
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

export(property_df, "paper/results/property_df.rds")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Simple statistics for paper
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# percent of compliers

pct_compliers <- mean(property_df$received_bonus)

# percent with management plan
pct_mgmt_plan <- mean(property_df$submitted_management_plan)

# percent compliers conditional on having management plan
pct_complier_given_plan <- mean( 
  subset(property_df, submitted_management_plan == 1)$received_bonus
)

# Total amount saved by not paying non-compliers

utm_saved <- sum( 
  subset(property_df, submitted_management_plan == 0)$rptpro_monto_total
)

usd_saved <- utm_saved * 60.11

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Starting to integrate matched properties
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
covariates_enrolled <- readRDS(paste0(file_dir, "data/analysis_lc/cleaned_properties/enrolled/covariates_enrolled.rds"))%>%
  mutate(ind_dist = unlist(ind_dist),
         natin_dist = unlist(natin_dist))


comunal_pov <- read.csv(paste0(file_dir, "data/analysis_lc/comunal_poverty.csv"), encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))


regrowth <- covariates_enrolled %>%
  filter(pixels_count != 0 )%>%
  inner_join(property_df, by = c("rptpre_id", "ROL"))%>%
  group_by(rptpre_id, rptpro_id, ROL)%>%
  slice_head()%>%
  ungroup()%>%
  mutate(comuna = tolower(accents(comuna)))%>%
  left_join(comunal_pov, by = "comuna")%>%
  mutate(lu2001_pixels_count = (Forest + `NA` + `Snow/ice` + `No data` + `Temporary bare soil` + `Pasture and ag` + Shrub + Urban + Water + `Permanent bare soil`),
         Forest = Forest /lu2001_pixels_count,
         Plantation = Plantation / lu2001_pixels_count,
         Trees_baseline = Trees_2000 / pixels_count,
         Grassland_baseline = Grassland_2000 / pixels_count,
         Crop_baseline = Crop_2000 / pixels_count,
         Shrubs_baseline = Shrubs_2000 / pixels_count,
         Trees_trend = ifelse(is.finite((Trees_2008 - Trees_2000)/ Trees_2000),
                              (Trees_2008 - Trees_2000)/ Trees_2000,
                              0),
         ind_dist = ind_dist /1000,
         natin_dist = natin_dist/1000
  )

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

psize_density <- ggplot(data = property_df, aes(x = rptpre_superficie_predial, color = `Contest type`))+
  geom_density()+
  xlim(0,600)+
  scale_colour_manual(values = c(palette$gold, palette$dark_green))+
  xlab("Property size (ha)")+
  theme_minimal()+
  ggtitle("Distribution of enrollee property sizes")

ggsave(psize_density, path = "paper/figs", filename = "psize_density.png",
       width = 6, height = 4)

proportion_subsidy_density <- ggplot(data = property_df, aes(x = proportion_subsidized, color = `Contest type`))+
  geom_density()+
  xlim(0,1)+
  scale_colour_manual(values = c(palette$gold, palette$dark_green))+
  xlab("Proportion of property enrolled")+
  theme_minimal()+
  ggtitle("Proportion of property subsidized amongst enrollees")

ggsave(proportion_subsidy_density, path = "paper/figs", filename = "proportion_subsidy_density.png",
       width = 6, height = 4)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Main summary statistics table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(psych)

app_labs <- data.frame(name1 = c("rptpre_superficie_predial", "rptpro_superficie", "proportion_subsidized", "rptpro_monto_total", "received_bonus", "submitted_management_plan", "timber", "extensionista"),
                   name2 = c("Property size (ha)", "Subsidized surface (ha)", "Proportion of property subsidized", "Bonus amount (UTM)", "Received payment", "Submitted management plan", "Timber production objective", "Received extensionist support"))

app_summary <- describe(property_df[, app_labs$name1]) %>% select(mean, median, sd) %>%
  rownames_to_column()

app_summary$rowname <- app_labs$name2

regrowth_labs <- data.frame(name1 = c("Trees_trend", "Forest", "Plantation", "Trees_baseline", "Crop_baseline", "Grassland_baseline", "natin_dist", "ind_dist"),
                       name2 = c("Pct. tree cover change (00-08)", "Native forest", "Plantation", "Tree cover", "Crop", "Grassland",  "Dist. to native timber processing (km)", "Dist. to any timber mill (km)"))

regrowth_summary <- describe(regrowth[, regrowth_labs$name1]) %>% select(mean, median, sd) %>%
  rownames_to_column() 
regrowth_summary$rowname <- regrowth_labs$name2

main_summarystats <- rbind(app_summary, regrowth_summary)%>%
  mutate_at(vars(mean:sd), ~ round(., digits = 3))

export(main_summarystats, "paper/results/main_summarystats.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Now preparing to compare enrolled groups
# Smallholders vs. Other interested
# Compliers vs. Non-compliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Contest types

# variables needing matched set
#"Trees_trend", "Forest", "Plantation", "Trees_baseline", "Crop_baseline", "Grassland_baseline", "natin_dist", "ind_dist"

trees_trend <- t.test(Trees_trend ~ `Contest type`, data = regrowth)
native_forest <- t.test(Forest ~ `Contest type`, data = regrowth)
plantation <- t.test(Plantation ~ `Contest type`, data = regrowth)
trees <- t.test(Trees_baseline ~ `Contest type`, data = regrowth)
crop <- t.test(Crop_baseline ~ `Contest type`, data = regrowth)
shrub <- t.test(Shrubs_baseline ~ `Contest type`, data = regrowth)
grassland <- t.test(Grassland_baseline ~ `Contest type`, data = regrowth)
native_ind <- t.test(natin_dist ~ `Contest type`, data = regrowth)
industry <- t.test(ind_dist ~ `Contest type`, data = regrowth)
elev <- t.test(elev ~ `Contest type`, data = regrowth)
slope <- t.test(slope ~ `Contest type`, data = regrowth)

# other variables - using full set of applicants
# "rptpre_superficie_predial", "rptpro_superficie", "proportion_subsidized", "rptpro_monto_total", "received_bonus", "submitted_management_plan", "timber", "extensionista"
area_prop <- t.test(rptpre_superficie_predial ~ `Contest type`, data = property_df)
area_bono <- t.test(rptpro_superficie ~ `Contest type`, data = property_df)
payment <- t.test(rptpro_monto_total ~ `Contest type`, data = property_df)
timber <- t.test(timber ~ `Contest type`, data = property_df)
proportion <- t.test(proportion_subsidized ~ `Contest type`, data = property_df)
managementplan <- t.test(submitted_management_plan ~ `Contest type`, data = property_df)
received_bonus <- t.test(received_bonus ~ `Contest type`, data = property_df)

covar <- c("Award (UTM)", "Bonus area (ha)", "Property area (ha)","Timber production objective" ,  
           "Proportion native forest", "Proportion plantation", "Proportion grassland", "Proportion crop", "Proportion shrub", 
           "Slope", "Elevation", "Dist. to native timber industry (km)", "Dist. to any timber industry (km)")
smallholder <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], timber$estimate[2],  
                 native_forest$estimate[2], plantation$estimate[2], grassland$estimate[2], crop$estimate[2], shrub$estimate[2], 
                 slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2])



other_interested <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], timber$estimate[1],  
                      native_forest$estimate[1], plantation$estimate[1], grassland$estimate[1], crop$estimate[1], shrub$estimate[1], 
                      slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1])

other_df <- ttest_df %>% filter(`Contest type` == "Other interested")
other_interested_med <- c(median())

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, timber$p.value,  
              native_forest$p.value, plantation$p.value, grassland$p.value, crop$p.value, shrub$p.value, 
              slope$p.value, elev$p.value, native_ind$p.value, industry$p.value)

contest_ttest <- data.frame(covar, smallholder, other_interested, p_value) %>%
  mutate_at(vars(smallholder, other_interested, p_value), ~ round(., digits = 3))

library(rio)
export(contest_ttest, "paper/results/contest_ttest_table.rds")


