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

### TREES

did_trees <- att_gt(yname="Trees",
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
              print_details=FALSE
)

did.es <- aggte(did_trees, type="dynamic", min_e = -8)
did.ovr <- aggte(did_trees, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did_trees$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

library(rio)
export(dyn.es, "paper/results/dyn.es_trees.rds")

plot_mgmt_trees <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.1)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Tree cover event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("#52854C", "black"))
plot_mgmt_trees


### Grassland

did_grassland <- att_gt(yname="Grassland",
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
              print_details=FALSE
)

did.es <- aggte(did_grassland, type="dynamic", min_e = -8)
did.ovr <- aggte(did_grassland, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did_grassland$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

export(dyn.es, "paper/results/dyn.es_grassland.rds")

plot_mgmt_grassland <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.1)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Grassland event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("#C4961A", "black"))
plot_mgmt_grassland

### Crop

did_crop <- att_gt(yname="Crop",
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
              print_details=FALSE
)

did.es <- aggte(did_crop, type="dynamic", min_e = -8)
did.ovr <- aggte(did_crop, type = "simple")
summary(did.ovr)
did_results <- data.frame("outcome" = "Trees", "name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did_crop$n)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))


export(dyn.es, "paper/results/dyn.es_crop.rds")


plot_mgmt_crop <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.1)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Crop event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()+
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values = c("#FFDB6D", "black"))
plot_mgmt_crop

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Unconditional pre-treatment event study
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unconditional_trees <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    control_group = "notyettreated",
                    data=matched_long, 
                    clustervars = "property_ID", 
                    panel=TRUE, bstrap = TRUE,
                    print_details=FALSE
)
did.es <- aggte(unconditional_trees, type="dynamic")
dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))
export(dyn.es, "paper/results/unconditional_es_trees.rds")


unconditional_crop <- att_gt(yname="Crop",
                              tname="Year",
                              idname="property_ID",
                              gname="first.treat",
                             control_group = "notyettreated",
                              data=matched_long, 
                              clustervars = "property_ID", 
                              panel=TRUE, bstrap = TRUE,
                              print_details=FALSE
)
did.es <- aggte(unconditional_crop, type="dynamic")
dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))
export(dyn.es, "paper/results/unconditional_es_crop.rds")


unconditional_grassland <- att_gt(yname="Grassland",
                              tname="Year",
                              idname="property_ID",
                              gname="first.treat",
                              control_group = "notyettreated",
                              data=matched_long, 
                              clustervars = "property_ID", 
                              panel=TRUE, bstrap = TRUE,
                              print_details=FALSE
)
did.es <- aggte(unconditional_grassland, type="dynamic")
dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))
export(dyn.es, "paper/results/unconditional_es_grassland.rds")

