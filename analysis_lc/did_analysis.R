library(tidyverse)
library(sf)

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
###############  area weighted did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
did <- att_gt(yname="Trees",
              tname="Year",
              idname="property_ID",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ ind_dist + natin_dist + slope + elev + lat 
              + Forest + Plantation 
              + Trees_baseline + Water_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
             ,
              data=matched_long, 
              clustervars = "property_ID", 
              weightsname = "pixels_count",
              panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("outcome" = "Trees", "name" = "area weighted", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 1)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  not yet treated control group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

did_nyt <- att_gt(
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
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
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

matched_wide <- readRDS(paste0(file_dir, "data/analysis_lc/main/enrolled_wide_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())%>%
  filter(received_bonus == 1 | first.treat == 0)%>%
  mutate(treat = ifelse(first.treat == 0, 0, 1))%>%
  left_join(comunal_pov, by = "comuna")

matched_long <- pivot_longer(matched_wide, Trees_1999:evi_2020)%>%
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
              + rptpro_tipo_concurso + rptpro_puntaje + rptpre_superficie_predial + rptpro_superficie
              , 
              data=matched_long, 
              clustervars = "property_ID", 
              weightsname = "rptpre_superficie_predial",
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

twfe_data <- matched_long %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = rptpro_superficie/rptpre_superficie_predial)
library(fixest)

twfe <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = twfe_data)
summary(twfe)

twfe <- feols(Trees ~ treat : post : intensity * rptpro_puntaje| Year + property_ID, data = twfe_data)
summary(twfe)

twfe <- feols(Trees ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = twfe_data)
summary(twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Adding pverty, social scores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

comunal_pov <- read.csv("data/analysis_lc/comunal_poverty.csv", encoding = "Latin-1")%>%
  mutate(comuna = tolower(accents(comuna)))

matched_wide <- readRDS(paste0(file_dir, "data/analysis_lc/main/enrolled_wide_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())%>%
  filter(received_bonus == 1 | first.treat == 0)%>%
  mutate(treat = ifelse(first.treat == 0, 0, 1))%>%
  left_join(comunal_pov, by = "comuna")

twfe <- feols(Trees ~ treat : post : intensity * cpov_pct_2007| Year + property_ID, data = twfe_data)
summary(twfe)
