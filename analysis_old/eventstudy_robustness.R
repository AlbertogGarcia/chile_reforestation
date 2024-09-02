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


matched_wide_main <- readRDS(paste0(file_dir, 
                                    "data/analysis_lc/main/matched_compliers_pctg.rds"
)
)%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())

table(matched_wide_main$first.treat)

matched_long <- pivot_longer(matched_wide_main, Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  matched did estimates using not yet treated group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### TREES

did_trees_nyt <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    est_method = "dr",
                    control_group = "notyettreated",
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

did.es <- aggte(did_trees_nyt, type="dynamic", min_e = -10)
did.ovr <- aggte(did_trees_nyt, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

library(rio)
export(dyn.es, "paper/results/dyn.es_trees_nyt.rds")

### Grassland

did_grassland_nyt <- att_gt(yname="Grassland",
                        tname="Year",
                        idname="property_ID",
                        gname="first.treat",
                        est_method = "dr",
                        control_group = "notyettreated",
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

did.es <- aggte(did_grassland_nyt, type="dynamic", min_e = -10)
did.ovr <- aggte(did_grassland_nyt, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

export(dyn.es, "paper/results/dyn.es_grassland_nyt.rds")


### Crop

did_crop_nyt <- att_gt(yname="Crop",
                   tname="Year",
                   idname="property_ID",
                   gname="first.treat",
                   est_method = "dr",
                   control_group = "notyettreated",
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

did.es <- aggte(did_crop_nyt, type="dynamic", min_e = -10)
did.ovr <- aggte(did_crop_nyt, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))


export(dyn.es, "paper/results/dyn.es_crop_nyt.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  unconditional event study did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


did_unconditional <- att_gt(yname="Trees",
                       tname="Year",
                       idname="property_ID",
                       gname="first.treat",
                       data=matched_long, 
                       clustervars = "property_ID", 
                       panel=TRUE, bstrap = TRUE,
                       print_details=TRUE
)
did.es <- aggte(did_unconditional, type="dynamic", min_e = -10)
ggdid(did.es)