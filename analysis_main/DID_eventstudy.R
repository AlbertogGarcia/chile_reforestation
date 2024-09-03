library(did)
library(dplyr)
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

matched_data_long <- readRDS(paste0(clean_data_dir, "/matched_data_long.rds"))


ovr_results <- data.frame()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### All

did_trees <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    control_group = "notyettreated",
                    xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                    , 
                    base_period = "universal",
                    data=
                      matched_data_long, 
                    clustervars = "property_ID"
)
did.ovr <- aggte(did_trees, type="simple")
did.ovr
did.es <- aggte(did_trees, type="dynamic", min_e = -15)
ggdid(did.es)


ovr_results <- data.frame("outcome" = "Trees", "group" = "all", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Trees, na.rm = T), "Nproperties" = length(unique(matched_data_long$property_ID))
)%>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Trees", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT, color = post)) + 
  ylab("Tree cover")+ xlab("Years since enrollment")+
 # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  theme(legend.title = element_blank())
trees_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Smallholder

did_trees <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    control_group = "notyettreated",
                    xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800 
                    ,  
                    base_period = "universal",
                    data=
                      matched_data_long %>% filter(control_contest != "Otros Interesados")
                    , 
                    clustervars = "property_ID"
)
did.ovr <- aggte(did_trees, type="simple")
did.ovr
did.es <- aggte(did_trees, type="dynamic", min_e = -15)

ovr_results <- data.frame("outcome" = "Trees", "group" = "Smallholders", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Trees, na.rm = T), "Nproperties" = length(unique(matched_data_long$property_ID))
)%>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Trees", "group" = "Smallholders", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

smallholder_trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT, color = post)) + 
  ylab("Tree cover")+ xlab("Years since enrollment")+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  theme(legend.title = element_blank())
smallholder_trees_plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Other interested

did_trees <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    control_group = "notyettreated",
                    xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                    , 
                    base_period = "universal",
                    data=
                      matched_data_long %>% filter(control_contest == "Otros Interesados")
                    , 
                    clustervars = "property_ID"
)
did.ovr <- aggte(did_trees, type="simple")
did.ovr
did.es <- aggte(did_trees, type="dynamic", min_e = -15)


ovr_results <- data.frame("outcome" = "Trees", "group" = "Other Interested", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Trees, na.rm = T), "Nproperties" = length(unique(matched_data_long$property_ID))
)%>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Trees", "group" = "Other Interested", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

other_trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT, color = post)) + 
  ylab("Tree cover")+ xlab("Years since enrollment")+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  theme(legend.title = element_blank())
other_trees_plot


