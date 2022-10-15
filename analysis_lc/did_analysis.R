library(tidyverse)
library(sf)
library(stringi)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

file_dir <- "C:/Users/garci/Dropbox/chile_reforestation/"
results_dir <- "analysis_lc/results/"

library(did)
set.seed(0930)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  main matched did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_wide_main <- readRDS(paste0(file_dir, "data/analysis_lc/main/matched_compliers_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())

matched_long <- pivot_longer(matched_wide_main, Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

### TREES

did <- att_gt(yname="Trees",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline 
              + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
            , 
              data=matched_long, 
              clustervars = "property_ID", 
              panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic", min_e = -10)
did.ovr <- aggte(did, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt
# ggsave(path = "paper/figs", filename = "did_main.png", width = 8, height = 5)

# spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
#                            "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
#                            "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
#                            "notyettreated" = 0,
#                            "area weights" = 0)

### EVI

did <- att_gt(yname="evi",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
              + pretrend_evi
              , 
              data=matched_long, 
              clustervars = "property_ID", 
              panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 15)
did.ovr <- aggte(did, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Evi", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt
# ggsave(path = "paper/figs", filename = "did_main.png", width = 8, height = 5)


### Grassland

did <- att_gt(yname="Grassland",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
              + pretrend_Grassland
              , 
              data=matched_long, 
              clustervars = "property_ID", 
              panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 15)
did.ovr <- aggte(did, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt
# ggsave(path = "paper/figs", filename = "did_main.png", width = 8, height = 5)

### Crop

did <- att_gt(yname="Crop",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
              + pretrend_Grassland
              , 
              data=matched_long, 
              clustervars = "property_ID", 
              panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 15)
did.ovr <- aggte(did, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Cohort specific estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cohorts <- seq(from = 2009, to = 2019, by =1)
cohort_spec_results <- data.frame()
for(i in cohorts){
  print(i)
  
  this_data <- subset(matched_long, first.treat == i | treat == 0)
  
  did <- att_gt(yname="Trees",
                tname="Year",
                idname="property_ID",
                gname="first.treat",
                est_method = "dr",
                xformla= ~ ind_dist + natin_dist + slope + elev + lat 
                + Forest + Plantation 
                + Trees_baseline 
                + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
                , 
                data=this_data, 
                clustervars = "property_ID", 
                panel=TRUE, bstrap = TRUE,
                print_details=FALSE
  )
  
  did.es <- aggte(did, type="dynamic")
  did.ovr <- aggte(did, type = "simple")
  
  did_results <- data.frame("name" = paste(i, "cohort"), "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
    bind_rows(did_results)
  
  dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
    mutate(period = ifelse(e >= 0 , "post", "pre"))
  
  plot_mgmt <- ggplot(data=dyn.es, aes(x=e, y=att, color = period)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
    geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    ggtitle(paste0(i , " cohort event study treatment effects")) +
    xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
    theme_minimal()
  plot_mgmt
  
#  ggsave(path = "paper/figs", filename = paste0("cohort", i, ".png"), width = 8, height = 5)
  
  # cohort_spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
  #                                   "cohort" = i)%>%
  #   bind_rows(spec_results)
  # 
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  TWFE estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_data <- matched_long %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = ifelse(treat == 1, rptpro_superficie/rptpre_superficie_predial, 0))

twfe <- feols(evi ~ treat : post : intensity| Year + property_ID, data = matched_data)
summary(twfe)


twfe <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data)
summary(twfe)


twfe <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data)
summary(twfe)


twfe <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data)
summary(twfe)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############### Noncompliers as control
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}


comunal_pov <- read.csv(paste0(file_dir, "data/analysis_lc/comunal_poverty.csv"), encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))


enrolled_wide <- readRDS(paste0(file_dir, "data/analysis_lc/main/enrolled_wide_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())%>%
  filter(received_bonus == 1 | first.treat == 0)%>%
  mutate(treat = ifelse(first.treat == 0, 0, 1))%>%
  left_join(comunal_pov, by = "comuna")

enrolled_long <- pivot_longer(enrolled_wide, Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)


### TREES

did <- att_gt(yname="Trees",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline 
              + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
              + rptpro_puntaje + rptpre_superficie_predial + rptpro_superficie
              , 
              data=small_long, 
              clustervars = "property_ID", 
           #   weightsname = "rptpre_superficie_predial",
              panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)

did.es <- aggte(did, type="dynamic", min_e = -10)
did.ovr <- aggte(did, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt

enrolled_data <- enrolled_long %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = rptpro_superficie/rptpre_superficie_predial)
library(fixest)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Trees
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Trees ~ treat : post : intensity * rptpro_puntaje| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Trees ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Trees ~ treat : post : intensity * as.numeric(cpov_pct_2007)| Year + property_ID, data = enrolled_data)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Grassland
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Grassland ~ treat : post : intensity * rptpro_puntaje| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Grassland ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Grassland ~ treat : post : intensity * as.numeric(cpov_pct_2007)| Year + property_ID, data = enrolled_data)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Crop
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Crop ~ treat : post : intensity * rptpro_puntaje| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Crop ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(Crop ~ treat : post : intensity * as.numeric(cpov_pct_2007)| Year + property_ID, data = enrolled_data)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Evi
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(evi ~ treat : post : intensity| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(evi ~ treat : post : intensity * rptpro_puntaje| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(evi ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)
summary(twfe)

twfe <- feols(evi ~ treat : post : intensity * as.numeric(cpov_pct_2007)| Year + property_ID, data = enrolled_data)
summary(twfe)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
enrolled_wide <- enrolled_wide %>%
  mutate(indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
       rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
       ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
       indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
       extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
       timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
       nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
       reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0)
sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_df <- enrolled_wide %>%
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

other_df <- enrolled_wide %>%
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

small_long <- small_df%>%
  pivot_longer(Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)


enrolled_long_social <- bind_rows(other_df, small_df)%>%
  pivot_longer(Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

enrolled_data_social <- enrolled_long_social %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = rptpro_superficie/rptpre_superficie_predial)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Trees
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Trees ~ treat : post : intensity +
                treat : post : intensity : adjusted_puntaje + 
                treat : post : intensity : social_puntaje * rptpro_tipo_concurso
                | Year + property_ID, data = enrolled_data_social)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Grassland
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Grassland ~ treat : post : intensity +
                treat : post : intensity : adjusted_puntaje + 
                treat : post : intensity : social_puntaje * rptpro_tipo_concurso
              | Year + property_ID, data = enrolled_data_social)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Crop
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(Crop ~ treat : post : intensity +
                treat : post : intensity : adjusted_puntaje + 
                treat : post : intensity : social_puntaje * rptpro_tipo_concurso
              | Year + property_ID, data = enrolled_data_social)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### EVI
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe <- feols(evi ~ treat : post : intensity +
                treat : post : intensity : adjusted_puntaje + 
                treat : post : intensity : social_puntaje * rptpro_tipo_concurso
              | Year + property_ID, data = enrolled_data_social)
summary(twfe)
