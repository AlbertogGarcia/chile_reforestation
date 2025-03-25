library(did)
library(Rcpp)
library(tidyverse)
library(fixest)
library(sf)
library(kableExtra)
library(ggpubr)
library(here)
my_data_dir <- here::here("remote")
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


matched_data_long <- readRDS(paste0(clean_data_dir, "/matched_data_long.rds"))

set.seed(0331)
ovr_results <- data.frame()
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### All

######## TREES
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

trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("ATT (binary treatment)")+ xlab("Years since enrollment")+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$green)+
  geom_point(color = palette$green)+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  #scale_color_manual(values = c(palette$red, palette$blue))+
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
  theme(legend.title = element_blank())+
  ylim(-0.018, 0.03)
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

saveRDS(ovr_results, here("analysis_main", "results", "staggeredDID_results.rds"))


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
  theme(legend.title = element_blank())+
  ylim(-0.018, 0.03)
other_trees_plot


ggarrange(smallholder_trees_plot, other_trees_plot, ncol = 2, nrow = 1,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_horiz_duo.png"), width = 12, height = 5)


ggarrange(smallholder_trees_plot, other_trees_plot, ncol = 1, nrow = 2,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_vert_duo.png"), width = 7, height = 9)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Other Land Cover Types

######## CROP
did_crop <- att_gt(yname="Crop",
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
did.ovr <- aggte(did_crop, type="simple")
did.ovr
did.es <- aggte(did_crop, type="dynamic", min_e = -15)
ggdid(did.es)

es_plot_df <- data.frame("outcome" = "Crop", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

crop_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("")+ 
  xlab("Years since enrollment")+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$brown)+
  geom_point(color = palette$brown)+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  #scale_color_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())
crop_plot

######## GRASSLAND
did_grass <- att_gt(yname="Grassland",
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
did.ovr <- aggte(did_grass, type="simple")
did.ovr
did.es <- aggte(did_grass, type="dynamic", min_e = -15)
ggdid(did.es)

es_plot_df <- data.frame("outcome" = "Grassland", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

grass_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  xlab("Years since enrollment")+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$gold)+
  geom_point(color = palette$gold)+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  #scale_color_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  theme(legend.title = element_blank(),
        axis.title.y = element_blank())
grass_plot

eventstudy_trio <- ggarrange(trees_plot + ylim(-0.0248, 0.0273)
                             , grass_plot + ylim(-0.0248, 0.0273)
                             , crop_plot + ylim(-0.0248, 0.0273)
                             , ncol = 3, nrow = 1
                             # labels = c("A", "B", "C"),
)
eventstudy_trio
ggsave(plot = eventstudy_trio, paste0(here("analysis_main", "figs"), "/eventstudy_trio.png"), width = 15, height = 5)

# eventstudy_quad <- ggarrange(eventstudy_trio
#                              , spec_chart_ovr
#                              , ncol = 2, nrow = 1
#                              , widths = c(3.5,1)
#                              , labels = c("A", "B")
#                              , legend = "bottom", common.legend = T
# )
# 
# eventstudy_quad

eventstudy_quad <- ggarrange(trees_plot + ylim(-0.0248, 0.0273)
                             , grass_plot + ylim(-0.0248, 0.0273)
                             , crop_plot + ylim(-0.0248, 0.0273)
                             , spec_chart_ovr
                             , ncol = 4, nrow = 1
                             , widths = c(1.4, 4/3, 4/3, 0.9)
                             , labels = c("A", "B", "C", "D")
                             , legend = "bottom", common.legend = T
)

eventstudy_quad
ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_quad.png"), width = 15, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Use noncompliers as control group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_noncomplier_data_long <- readRDS(paste0(clean_data_dir, "/matched_noncomplier_data_long.rds"))%>%
  filter(first.treat > 0)%>%
  mutate(treat = 0,
         first.treat = 0)

matched_data_long_ncControl <- matched_data_long %>%
  filter(first.treat > 0)%>%
  bind_rows(matched_noncomplier_data_long)

######## Above median
did_ncControl <- att_gt(yname="Trees",
                        tname="Year",
                        idname="property_ID",
                        gname="first.treat",
                        control_group = "notyettreated",
                        xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                        , 
                        base_period = "universal",
                        data= matched_data_long_ncControl, 
                        clustervars = "property_ID"
)
did.ovr <- aggte(did_ncControl, type="simple")
did.ovr
did.es <- aggte(did_ncControl, type="dynamic", min_e = -15)
ggdid(did.es)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Above/Below median treatment "dose"
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_data_long_dose <- matched_data_long %>%
  mutate(post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0),
         aboveMed_intensity = ifelse(intensity >= median(intensity[treat == 1]),
                                     1,
                                     0)
  )

######## Above median
did_aboveMed <- att_gt(yname="Trees",
                       tname="Year",
                       idname="property_ID",
                       gname="first.treat",
                       control_group = "notyettreated",
                       xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                       , 
                       base_period = "universal",
                       data=
                         matched_data_long_dose %>% filter(treat == 0 | aboveMed_intensity == 1)
                       , 
                       clustervars = "property_ID"
)
did.ovr <- aggte(did_aboveMed, type="simple")
did.ovr
did.es <- aggte(did_aboveMed, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "Above Median", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

######## Below median
did_belowMed <- att_gt(yname="Trees",
                       tname="Year",
                       idname="property_ID",
                       gname="first.treat",
                       control_group = "notyettreated",
                       xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                       , 
                       base_period = "universal",
                       data=
                         matched_data_long_dose %>% filter(treat == 0 | aboveMed_intensity == 0), 
                       clustervars = "property_ID"
)
did.ovr <- aggte(did_belowMed, type="simple")
did.ovr
did.es <- aggte(did_belowMed, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "Below Median", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )%>%
  rbind(dose_es_plot_df)


dose_plot <- ggplot(dose_es_plot_df, aes(x = e, y = ATT, color = dose)) + 
  ylab("Tree cover")+ xlab("Years since enrollment")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se, fill = dose), color = NA, alpha=0.15)+
  geom_line() +
  #geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  #geom_point()+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c(palette$red, palette$blue))+
  scale_fill_manual(values = c(palette$red, palette$blue))+
  theme_classic()+
  guides(color = guide_legend(title="Land enrollment"),
         fill=F)
dose_plot

