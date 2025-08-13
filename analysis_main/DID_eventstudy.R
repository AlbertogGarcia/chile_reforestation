library(did)
library(Rcpp)
library(tidyverse)
library(fixest)
library(sf)
library(kableExtra)
library(ggpubr)
library(here)
#my_data_dir <- here::here("remote")
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
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Trees, na.rm = T)
)%>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Trees", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  labs(y = "ATT", x = "Years since enrollment", title = "Forest")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$green)+
  geom_point(color = palette$green)+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
trees_plot


#########################


trees_plot2 <- ggplot(es_plot_df %>%
                        bind_rows(
                          data.frame("ATT" = did.ovr$overall.att, "e" = 11, "se" = did.ovr$overall.se, "crit" = 1.96)
                                  )%>%
                        mutate(type = ifelse(e == max(e), "aggregate", "event study"))
                      , aes(x = e, y = ATT, color = type, shape = type)) + 
  labs(y = "ATT (staggered DID)", x = "Years since enrollment", title = "Forest")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  theme_classic()+
  scale_shape_manual(values = c(19, 16))+
  scale_color_manual(values = c(palette$red, palette$green))+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))
trees_plot2


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

ovr_results_small <- data.frame("outcome" = "Trees", "group" = "Smallholders", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat & rptpro_tipo_concurso != "Otros Interesados"))$Trees, na.rm = T)
)

ovr_results <- ovr_results_small %>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Trees", "group" = "Smallholders", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "Post enrollment", "Pre enrollment")
  )

dyn.es_bycontest <- data.frame("Contest type" = "Smallholder", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0))

smallholder_trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT, fill = post)) + 
  labs(y = "ATT (staggered DID)", x = "Years since enrollment", title = "Smallholders")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$dark)+
  geom_point(shape = 21, color = palette$dark)+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c(palette$red, palette$blue))+
  # geom_errorbar(aes(x = 11, ymin= ovr_results_small$ATT - 1.96*ovr_results_small$se, ymax=ovr_results_small$ATT + 1.96*ovr_results_small$se), width = 0.25, linewidth = 0.7, color = palette$red)+
  # geom_point(aes(x = 11, y = ovr_results_small$ATT), size = 2, color = palette$red)+
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


ovr_results_other <- data.frame("outcome" = "Trees", "group" = "Other Interested", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat & rptpro_tipo_concurso == "Otros Interesados"))$Trees, na.rm = T)
)

ovr_results <- ovr_results_other %>%
  rbind(ovr_results)


es_plot_df <- data.frame("outcome" = "Trees", "group" = "Other interested", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "Post enrollment", "Pre enrollment")
  )

dyn.es_bycontest <- data.frame("Contest type" = "Other interested", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0))%>%
  rbind(dyn.es_bycontest)

other_trees_plot <- ggplot(es_plot_df, aes(x = e, y = ATT, fill = post)) + 
  labs(y = "ATT (staggered DID)", x = "Years since enrollment", title = "Other interested parties")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  # geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  #geom_line() +
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point(shape = 21, color = palette$dark)+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = c(palette$red, palette$blue))+
  # geom_errorbar(aes(x = 11, ymin= ovr_results_other$ATT - 1.96*ovr_results_other$se, ymax=ovr_results_other$ATT + 1.96*ovr_results_other$se), width = 0.25, linewidth = 0.7, color = palette$red)+
  # geom_point(aes(x = 11, y = ovr_results_other$ATT), size = 2, color = palette$red)+
  ylim(-0.018, 0.03)
other_trees_plot


ggarrange(smallholder_trees_plot, other_trees_plot, ncol = 2, nrow = 1,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_horiz_duo.png"), width = 12, height = 5)


ggarrange(smallholder_trees_plot, other_trees_plot, ncol = 1, nrow = 2,
          labels = c("A", "B"),
          legend = "bottom", common.legend = T)
ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_vert_duo.png"), width = 5.5, height = 8)

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

ovr_results <- data.frame("outcome" = "Crop", "group" = "all", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Crop, na.rm = T)
)%>%
  rbind(ovr_results)

es_plot_df <- data.frame("outcome" = "Crop", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

crop_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  labs(x = "Years since enrollment", title = "Crop")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$brown)+
  geom_point(color = palette$brown)+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())
crop_plot



crop_plot2 <- ggplot(es_plot_df %>%
                        bind_rows(
                          data.frame("ATT" = did.ovr$overall.att, "e" = 11, "se" = did.ovr$overall.se, "crit" = 1.96)
                        )%>%
                        mutate(type = ifelse(e == max(e), "aggregate", "event study"))
                      , aes(x = e, y = ATT, color = type, shape = type)) + 
  labs(x = "Years since enrollment", title = "Crop")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  theme_classic()+
  scale_shape_manual(values = c(19, 16))+
  scale_color_manual(values = c(palette$red, palette$brown))+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())
crop_plot2

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

ovr_results <- data.frame("outcome" = "Grassland", "group" = "all", "ATT" = did.ovr$overall.att, "se" = did.ovr$overall.se, 
                          "pre-treat" = mean((matched_data_long %>% filter( first.treat > 0 & Year < first.treat))$Grassland, na.rm = T)
)%>%
  rbind(ovr_results)

saveRDS(ovr_results, here("analysis_main", "results", "staggeredDID_results.rds"))


es_plot_df <- data.frame("outcome" = "Grassland", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

grass_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  labs(x = "Years since enrollment", title = "Grassland")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4, color = palette$gold)+
  geom_point(color = palette$gold)+
  theme_classic()+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())
grass_plot


grass_plot2 <- ggplot(es_plot_df %>%
                       bind_rows(
                         data.frame("ATT" = did.ovr$overall.att, "e" = 11, "se" = did.ovr$overall.se, "crit" = 1.96)
                       )%>%
                       mutate(type = ifelse(e == max(e), "aggregate", "event study"))
                     , aes(x = e, y = ATT, color = type, shape = type)) + 
  labs(x = "Years since enrollment", title = "Grassland")+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark, linewidth = 0.25)+
  geom_hline(yintercept = 0, color = palette$dark, linewidth = 0.25)+
  geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  geom_point()+
  theme_classic()+
  scale_shape_manual(values = c(19, 16))+
  scale_color_manual(values = c(palette$red, palette$gold))+
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())
grass_plot2










eventstudy_trio <- ggarrange(trees_plot + ylim(-0.0248, 0.0273)
                             , grass_plot + ylim(-0.0248, 0.0273)
                             , crop_plot + ylim(-0.0248, 0.0273)
                             , ncol = 3, nrow = 1
                             # labels = c("A", "B", "C"),
)
eventstudy_trio
ggsave(plot = eventstudy_trio, paste0(here("analysis_main", "figs"), "/eventstudy_trio.png"), width = 15, height = 5)


# continuous_Crop = -0.003435449
# se_continuous_Crop = 0.002167897
# continuous_Trees = 0.013545828
# se_continuous_Trees = 0.005214927
# continuous_Grassland = -0.009697250
# se_continuous_Grassland = 0.005269988


eventstudy_trio2 <- ggarrange(trees_plot2 + ylim(-0.0248, 0.0273)
                             , grass_plot2 + ylim(-0.0248, 0.0273)
                             , crop_plot2 + ylim(-0.0248, 0.0273)
                             , ncol = 3, nrow = 1
                             , legend = "none"
                             # labels = c("A", "B", "C"),
)
eventstudy_trio2
ggsave(plot = eventstudy_trio2, paste0(here("analysis_main", "figs"), "/eventstudy_trio2.png"), width = 12.5, height = 4)

# eventstudy_quad <- ggarrange(eventstudy_trio
#                              , spec_chart_ovr
#                              , ncol = 2, nrow = 1
#                              , widths = c(3.5,1)
#                              , labels = c("A", "B")
#                              , legend = "bottom", common.legend = T
# )
# 
# eventstudy_quad

# eventstudy_quad <- ggarrange(trees_plot + ylim(-0.0248, 0.0273)
#                              , grass_plot + ylim(-0.0248, 0.0273)
#                              , crop_plot + ylim(-0.0248, 0.0273)
#                              , spec_chart_ovr
#                              , ncol = 4, nrow = 1
#                              , widths = c(1.4, 4/3, 4/3, 0.9)
#                              , labels = c("A", "B", "C", "D")
#                              , legend = "bottom", common.legend = T
# )
# 
# eventstudy_quad
# ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_quad.png"), width = 15, height = 5)

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

######## noncompliers as control
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
#### treatment "dose" by quartile
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_data_long_dose <- matched_data_long %>%
  mutate(post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0),
         aboveMed_intensity = ifelse(intensity >= median(intensity[treat == 1]), 1, 0),
         intensity_1 = ifelse(between(intensity, 0, quantile(intensity[treat == 1], 1/4)), 1, 0),
         intensity_2 = ifelse(between(intensity, quantile(intensity[treat == 1], 1/4), quantile(intensity[treat == 1], 1/2)), 1, 0),
         intensity_3 = ifelse(between(intensity, quantile(intensity[treat == 1], 1/2), quantile(intensity[treat == 1], 3/4)), 1, 0),
         intensity_4 = ifelse(between(intensity, quantile(intensity[treat == 1], 3/4), 1), 1, 0),
  )


set.seed(0930)


######## quartile 1
did_1 <- att_gt(yname="Trees",
                       tname="Year",
                       idname="property_ID",
                       gname="first.treat",
                       control_group = "notyettreated",
                       xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                       , 
                       base_period = "universal",
                       data=
                         matched_data_long_dose %>% filter(treat == 0 | intensity_1 == 1)
                       , 
                       clustervars = "property_ID"
)
did.ovr <- aggte(did_1, type="simple")
did.ovr
did.es <- aggte(did_1, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "1", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )

######## quartile 2
did_2 <- att_gt(yname="Trees",
                tname="Year",
                idname="property_ID",
                gname="first.treat",
                control_group = "notyettreated",
                xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                , 
                base_period = "universal",
                data=
                  matched_data_long_dose %>% filter(treat == 0 | intensity_2 == 1)
                , 
                clustervars = "property_ID"
)
did.ovr <- aggte(did_2, type="simple")
did.ovr
did.es <- aggte(did_2, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "2", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )%>%
  rbind(dose_es_plot_df)

######## quartile 3
did_3 <- att_gt(yname="Trees",
                tname="Year",
                idname="property_ID",
                gname="first.treat",
                control_group = "notyettreated",
                xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                , 
                base_period = "universal",
                data=
                  matched_data_long_dose %>% filter(treat == 0 | intensity_3 == 1)
                , 
                clustervars = "property_ID"
)
did.ovr <- aggte(did_3, type="simple")
did.ovr
did.es <- aggte(did_3, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "3", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )%>%
  rbind(dose_es_plot_df)

######## quartile 4
did_4 <- att_gt(yname="Trees",
                tname="Year",
                idname="property_ID",
                gname="first.treat",
                control_group = "notyettreated",
                xformla= ~ ind_dist + natin_dist + city_dist + elev + pop + Forest + Plantation + Grassland_baseline + Crop_baseline + Trees_trend + Trees0800
                , 
                base_period = "universal",
                data=
                  matched_data_long_dose %>% filter(treat == 0 | intensity_4 == 1)
                , 
                clustervars = "property_ID"
)
did.ovr <- aggte(did_4, type="simple")
did.ovr
did.es <- aggte(did_4, type="dynamic", min_e = -15)
ggdid(did.es)

dose_es_plot_df <- data.frame("outcome" = "Trees", "dose" = "4", "group" = "all", "ATT" = did.es$att.egt, "e" = did.es$egt, "se" = did.es$se.egt, "crit" = did.es$crit.val.egt)%>%
  mutate(se = replace_na(se, 0),
         post = ifelse(e >= 0, "post enrollment", "pre enrollment")
  )%>%
  rbind(dose_es_plot_df)


dose_plot <- ggplot(dose_es_plot_df, aes(x = e, y = ATT, color = dose)) + 
  ylab("Tree cover")+ xlab("Years since enrollment")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se, fill = dose), color = NA, alpha=0.2)+
  geom_line() +
  #geom_errorbar(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), width = 0.25, linewidth = 0.4)+
  #geom_point()+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$dark)+
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c(palette$brown, palette$green, palette$red, palette$blue))+
  scale_fill_manual(values = c(palette$brown, palette$green, palette$red, palette$blue))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(title="Land enrollment quartile"),
         fill=F)
dose_plot

ggsave(paste0(here("analysis_main", "figs"), "/eventstudy_dose_quartile.png"), width = 7, height = 7)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Aiming to create full trend potential outcomes figure
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enrolled_trends_bycontest <- matched_data_long %>%
  filter(first.treat > 0)%>%
  mutate(e = Year - first.treat)%>%
  dplyr::group_by(e, `Contest type`)%>%
  summarise_at(vars(Trees, Crop, Grassland), funs(mean(., na.rm = T), se=sd(., na.rm = T)/sqrt(n()), n = n()))%>%
  ungroup %>%
  dplyr::group_by(`Contest type`)%>%
  mutate(Trees_lag = lag(Trees_mean, n = 1, default = NA),
         Trees_rate_of_change = (Trees_mean - Trees_lag)/Trees_lag
  )%>%
  ungroup

treatment_trends <- enrolled_trends_bycontest %>%
  rename(Observed = Trees_mean)%>%
  select(Observed, e, `Contest type`)%>%
  right_join(dyn.es_bycontest, by = c("e", "Contest type" = "Contest.type"))%>%
  mutate(Counterfactual = ifelse(e >=0, Observed - ATT, Observed))%>%
  pivot_longer(c("Counterfactual", "Observed"), names_to = "pot_outcome")%>%
  mutate(se = ifelse(pot_outcome == "Observed" | e < 0 , 0, se))

treatment_trees_plot <- ggplot(data = treatment_trends , 
                               aes(x = as.numeric(e), y = value, color = `Contest type`, linetype = reorder(pot_outcome, -value)))+
  theme_classic()+
  geom_vline(xintercept = -1, linetype = "dashed", linewidth = 0.5)+
  geom_line(linewidth = 0.9)+
  geom_ribbon(aes(ymin=(value-1.96*se), ymax=(value+1.96*se)), alpha = 0.2, linewidth = 0)+
  scale_x_continuous(name = "Years since enrollment", breaks = c(-10, -5, 0, 5), limits = c(-15, 8))+
  scale_y_continuous(name = "Proportion of property with forest", labels = scales::percent)+
  scale_color_manual(values = c(palette$dark, palette$blue))+
  guides(color=guide_legend(override.aes=list(fill=NA), title.position="top", title.hjust = 0.5),
         linetype=guide_legend(override.aes=list(fill=NA), title.position="top", title.hjust = 0.5))+
  theme(legend.position = "bottom")+
  labs(linetype = "Potential outcome")
treatment_trees_plot

ggsave(paste0(here("analysis_main", "figs"), "/PO_trends.png"), width = 7, height = 6)

