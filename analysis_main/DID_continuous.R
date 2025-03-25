library(dplyr)
library(fixest)
library(sf)
library(kableExtra)
library(modelsummary)
library(here)
library(ggplot2)
library(ggpubr)
library(janitor)
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


matched_data_long <- readRDS(paste0(clean_data_dir, "/matched_data_long.rds"))%>%
  mutate(post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0))

matched_noncomplier_data_long <- readRDS(paste0(clean_data_dir, "/matched_noncomplier_data_long.rds"))%>%
  mutate(treat = (first.treat > 0)*1,
         post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Trees
twfe_trees_all <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long)
summary(twfe_trees_all, vcov = ~property_ID)
#vcov_conley(twfe_trees_all, cutoff = 100)

twfe_trees_smallholder <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_trees_smallholder, vcov = ~property_ID)

twfe_trees_other <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_trees_other, vcov = ~property_ID)

twfe_trees_binary <- feols(Trees ~ treat : post | Year + property_ID, data = matched_data_long)
summary(twfe_trees_binary, vcov = ~property_ID)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Grassland

twfe_grassland_all <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data_long)
summary(twfe_grassland_all, vcov = ~property_ID)

twfe_grassland_smallholder <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_grassland_smallholder, vcov = ~property_ID)

twfe_grassland_other <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_grassland_other, vcov = ~property_ID)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Crop

twfe_crop_all <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data_long)
summary(twfe_crop_all, vcov = ~property_ID)

twfe_crop_smallholder <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_crop_smallholder, vcov = ~property_ID)

twfe_crop_other <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_crop_other, vcov = ~property_ID)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Noncompliers
twfe_crop_nonc <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_noncomplier_data_long)
summary(twfe_crop_nonc, vcov = ~property_ID)

twfe_trees_nonc <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_noncomplier_data_long)
summary(twfe_trees_nonc, vcov = ~property_ID)

twfe_grassland_nonc <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_noncomplier_data_long)
summary(twfe_grassland_nonc, vcov = ~property_ID)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  regression table for smallholders and other interested parties

models_contest = list("(1)" = twfe_trees_smallholder,
              "(2)" = twfe_crop_smallholder,
              "(3)" = twfe_grassland_smallholder,
              "(4)" = twfe_trees_other,
              "(5)" = twfe_crop_other,
              "(6)" = twfe_grassland_other
)

crop_smallholder <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest != "Otros Interesados")$Crop, na.rm = T), digits = 3) 
grassland_smallholder <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest != "Otros Interesados")$Grassland, na.rm = T) , digits = 3)
trees_smallholder <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest != "Otros Interesados")$Trees, na.rm = T)  , digits = 3)

crop_other <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest == "Otros Interesados")$Crop, na.rm = T), digits = 3) 
grassland_other <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest == "Otros Interesados")$Grassland, na.rm = T) , digits = 3)
trees_other <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 & control_contest == "Otros Interesados")$Trees, na.rm = T)  , digits = 3)

rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
                'Control group', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1'
                , 'Pre-treat mean', as.character(trees_smallholder), as.character(crop_smallholder), as.character(grassland_smallholder), as.character(trees_other), as.character(crop_other), as.character(grassland_other)
)%>%
  as.data.frame()

f1 <- function(x) format(round(x, 5), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models_contest,
             output="latex",
             title = '\\label{tab:twfe-contest}Estimates of subsidy impact across Smallholder and Other Interested Parties contests',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treat:post:intensity" = "Intensity"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1))%>%
  add_header_above(c(" " = 1, "Smallholders" = 3, "Other Interested Parties" = 3))%>%
  kableExtra::save_kable(paste0(here("analysis_main", "results"), "/twfe_contest_table.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  regression table for all properties and then noncompliers

models_compliers = list("(1)" = twfe_trees_all,
              "(2)" = twfe_crop_all,
              "(3)" = twfe_grassland_all,
              "(4)" = twfe_trees_nonc,
              "(5)" = twfe_crop_nonc,
              "(6)" = twfe_grassland_nonc
)


crop_all <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1)$Crop, na.rm = T), digits = 3) 
grassland_all <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1)$Grassland, na.rm = T) , digits = 3)
trees_all <- round(mean(subset(matched_data_long, Year < first.treat & treat == 1 )$Trees, na.rm = T)  , digits = 3)

crop_nonc <- round(mean(subset(matched_noncomplier_data_long, Year < first.treat & treat == 1 )$Crop, na.rm = T), digits = 3) 
grassland_nonc <- round(mean(subset(matched_noncomplier_data_long, Year < first.treat & treat == 1 )$Grassland, na.rm = T) , digits = 3)
trees_nonc <- round(mean(subset(matched_noncomplier_data_long, Year < first.treat & treat == 1 )$Trees, na.rm = T)  , digits = 3)

rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
                'Control group', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1', 'matched 3-to-1'
                , 'Pre-treat mean', as.character(trees_all), as.character(crop_all), as.character(grassland_all), as.character(trees_nonc), as.character(crop_nonc), as.character(grassland_nonc)
)%>%
  as.data.frame()

f1 <- function(x) format(round(x, 5), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models_compliers,
             output="latex",
             title = '\\label{tab:twfe-compliers}Estimates of subsidy impact for all landowners receiving payment versus those who do not follow-through',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treat:post:intensity" = "Intensity"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1))%>%
  add_header_above(c(" " = 1, "All compliers" = 3, "Noncompliers" = 3))%>%
  kableExtra::save_kable(paste0(here("analysis_main", "results"), "/twfe_compliers_table.tex"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Spec chart

library(dotwhisker)
tidy_models <- twfe_trees_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Tree cover") 
tidy_models <- twfe_grassland_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Grassland")  %>% rbind(tidy_models)
tidy_models <- twfe_crop_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Crop") %>% rbind(tidy_models)

tidy_models <- twfe_trees_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Tree cover") %>% rbind(tidy_models)
tidy_models <- twfe_grassland_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Grassland") %>% rbind(tidy_models)
tidy_models <- twfe_crop_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Crop") %>% rbind(tidy_models)


plot_models <- tidy_models %>%
  select(-term)%>%
  rename(model = outcome,
         term = group)

spec_chart <- dwplot(plot_models,
       whisker_args = list(size = 1.1,
                           aes(colour = model)),
       dot_args = list(
         aes(fill = model
           #  , shape = model
             ), 
         size = 3,
         shape = 21,
         color = palette$white)
       ) + 
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(#title = "ATT estimates by contest group", 
       x = "ATT (main specification)", 
       y = "Contest") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)
        ) +
  coord_flip()+
  scale_x_continuous(minor_breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03))+
#  scale_shape_discrete(name  = "Outcome", breaks = c(0, 1, 2)) + # breaks assign shapes
  scale_color_manual(values = c(palette$green, palette$gold, palette$brown), name = "Outcome") +# start/end for light/dark greys
scale_fill_manual(values = c(palette$green, palette$gold, palette$brown), name = "Outcome") # start/end for light/dark greys
spec_chart

ggsave(plot = spec_chart, paste0(here("analysis_main", "figs"), "/spec_chart_DIDcontinuous.png"), width = 7, height = 5)

#%%%%%
### Same for overall results

tidy_models_ovr <- twfe_trees_all %>% tidy(vcov = ~property_ID) %>% mutate(outcome = "Tree cover") 
tidy_models_ovr <- twfe_grassland_all %>% tidy(vcov = ~property_ID) %>% mutate(outcome = "Grassland") %>% rbind(tidy_models_ovr)
tidy_models_ovr <- twfe_crop_all %>% tidy(vcov = ~property_ID) %>% mutate(outcome = "Crop") %>% rbind(tidy_models_ovr)


plot_models_ovr <- tidy_models_ovr %>%
 # select(-term)%>%
  rename(model = outcome)


spec_chart_ovr <- dwplot(plot_models_ovr,
                         whisker_args = list(size = 1,
                                             aes(colour = model)),
                         dot_args = list(
                           aes(fill = model
                               #  , shape = model
                           ), 
                           size = 3,
                           shape = 21,
                           color = palette$white)
) + 
  theme_minimal() + 
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(#title = "ATT estimates by contest group", 
    x = "ATT (main specification)",
    y = "") +
  coord_flip()+
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        # legend.title = element_blank(),
        legend.title.align = .5,
        axis.ticks.x = element_blank(),
        #axis.title.y = element_blank()
        axis.text.x = element_blank()#text(angle = 90, vjust = 0.5, hjust=0.5)
  ) +
  scale_x_continuous(minor_breaks = c(-0.02, -0.01, 0, 0.01, 0.02))+
  #  scale_shape_discrete(name  = "Outcome", breaks = c(0, 1, 2)) + # breaks assign shapes
  scale_color_manual(values = c(palette$green, palette$gold, palette$brown), name = "Outcome") +# start/end for light/dark greys
  scale_fill_manual(values = c(palette$green, palette$gold, palette$brown), name = "Outcome") # start/end for light/dark greys

spec_chart_ovr

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Results table for comparisons w/ staggered DID estimator

staggeredDID_results <- readRDS(here("analysis_main", "results", "staggeredDID_results.rds"))%>%
  filter(outcome == "Trees", group == "all")

continuous_twfe_sum <- summary(twfe_trees_all, vcov = ~property_ID)
binary_twfe_sum <- summary(twfe_trees_binary, vcov = ~property_ID)


comp_results <- data.frame("Model" = c("Continuous TWFE", "TWFE", "Staggered DID"),
                           "PostxIntensity" = c(continuous_twfe_sum$coefficients, NA, NA),
                           "se1" = c(continuous_twfe_sum$se, NA, NA),
                           "Post" = c(NA, binary_twfe_sum$coefficients, staggeredDID_results$ATT),
                           "se2" = c(NA, binary_twfe_sum$se, staggeredDID_results$se),
                           "pre-treat" = trees_all, 
                           "Nprop" = length(unique(matched_data_long$property_ID))
)%>% 
  mutate_at(vars(se1, se2), ~ ifelse(!is.na(.), paste0("(", round(., digits = 5), ")    "), " ")) %>%
  mutate_at(vars(PostxIntensity, Post), ~ ifelse(!is.na(.), paste0(round(., digits = 6), "***"), " ")) %>%
  t()%>%
  row_to_names(row_number = 1) %>%
  as.data.frame()

# comp_results <- cbind( " " = c("Post x Intensity", "Post x Intensity", "Post", "Post", "Pre-treat mean", "N properties"),
#                       comp_results)     

row.names(comp_results) <- c("Post x Intensity", " ", "Post", "  ", "Pre-treat mean", "N properties")

kable(comp_results,
      format = "latex",
      booktabs = T,
      caption = "Estimates of the impact of Native Forest Law subsidy payments by estimator",
     # col.names = NULL,
      align = c("l", "c", "c", "c"),
      label = "estimator-comp-table"
)%>%
 # collapse_rows(columns = 1)%>%
  kableExtra::row_spec(4, hline_after = TRUE)%>%
  #add_header_above(c(" " = 1, "Outcome" = 3))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at property level")%>%
  kable_styling(latex_options = c("hold_position"))%>%
  kableExtra::save_kable(here("analysis_main", "results", "estimator_comparison.tex"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Indigenous landowners
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%















#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Cohort-specific tree cover estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cohorts <- seq(2009, 2018, by = 1)
cohort_results <- data.frame()
for(i in cohorts){
  this_twfe <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(first.treat %in% c(0, i)))

  this_summary <- summary(this_twfe, vcov = ~property_ID)
  
  cohort_results <- data.frame("cohort" = i, "coef" = this_summary$coefficients, "se" = this_summary$se)%>%
    rbind(cohort_results)
  print(i)
  }

library(rio)
export(cohort_results, "results/cohort_results.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Comuna poverty marginal effects plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(patchwork)
library(marginaleffects)
me_data <- matched_data_long %>%
  mutate(treatment = as.numeric(treat) * post * intensity,
         log_cpov = log(cpov_pct_2007 + 0.01),
         cpov_pct_2007 = cpov_pct_2007 / 100
  )
matched_trees_cpov <- feols(Trees ~ treatment   + treatment*log_cpov
                            | Year + property_ID, data = me_data %>% filter(is.finite(treatment)))

min(me_data$log_cpov)
max(me_data$log_cpov)

log_cpov_ME <- marginaleffects::plot_comparisons(matched_trees_cpov, variables = "treatment", condition = list("log_cpov"))+
#  geom_rug(aes(x = log_cpov), data = me_data) +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  #xlab("ln(Comuna Poverty %)")+
  ylab("Expected effect of\naward on forest cover")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_x_continuous(limits = c(1.1, 3.65), breaks = c(1, 2, 3))
#  geom_vline(xintercept = mean(me_data$log_cpov+0.01), linetype = "dashed", color = "#1c86ee")
  #ylim(-0.007, 0.022)
log_cpov_ME

log_cpov_density <- ggplot(me_data %>% filter(treat ==1), aes(x = log_cpov)) +
  geom_histogram(aes(y=..density..), fill = palette$light_grey, color = palette$dark) + 
  geom_density(aes(y=..density..), bw = 0.15) +
  theme_minimal()+theme(plot.margin = unit(c(0,0,0,0.35), "cm"))+
  xlab("logarithm of participants' comuna-level poverty") + ylab("Density\n")+
  scale_x_continuous(limits = c(1.1, 3.65), breaks = c(1, 2, 3)
                     )
log_cpov_density

MEplot <- ggarrange(log_cpov_ME, log_cpov_density, ncol = 1, nrow = 2,
          labels = NULL#c("A", "B"),
        #  legend = "bottom", common.legend = T
          )
ggsave(plot = MEplot, paste0(here("analysis_main", "figs"), "/Meffects_comunapov.png"), width = 7, height = 7)

#%%%%%%%%%%%%%%%%%%%%
#### Combining spec chart and comuna poverty marginal effects plots
#%%%%%%%%%%%%%%%%%%%%


schart_MEplot <- ggarrange(spec_chart, MEplot 
                               , ncol = 2, nrow = 1,
                               labels = c("A", "B")
                               #  legend = "bottom", common.legend = T
)
schart_MEplot

ggsave(plot = schart_MEplot, paste0(here("analysis_main", "figs"), "/schart_ME_duo.png"), width = 10, height = 5)



#%%%%%%%%%%%%%%%%%%%%
#### Same marginal effects plot separated by contest
#%%%%%%%%%%%%%%%%%%%%

matched_trees_cpov_contest <- feols(Trees ~ treatment   + treatment*log_cpov*control_contest
                            | Year + property_ID, data = me_data %>% filter(is.finite(treatment)))


log_cpov_ME <- marginaleffects::plot_comparisons(matched_trees_cpov_contest, variables = "treatment", condition = list("log_cpov", "control_contest"))+
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  ylab("Expected effect of\naward on forest cover")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")+
  scale_color_manual(values = c(palette$red, palette$blue))+
  scale_fill_manual(values = c(palette$red, palette$blue))+
  scale_x_continuous(limits = c(1.1, 3.65), breaks = c(1, 2, 3))
log_cpov_ME

log_cpov_density <- ggplot(me_data %>% filter(treat ==1), aes(x = log_cpov)) +
  geom_histogram(aes(y=..density.., fill = `Contest type`), color = palette$dark, alpha = 0.65) + 
  geom_density(aes(y=..density.., color = `Contest type`), bw = 0.15) +
  theme_minimal()+
  theme(plot.margin = unit(c(0,0,0,0.8), "cm"),
        legend.position = "bottom")+
  xlab("logarithm of participants' comuna-level poverty") + ylab("Density\n")+
  scale_color_manual(values = c(palette$red, palette$blue))+
  scale_fill_manual(values = c(palette$red, palette$blue))+
  scale_x_continuous(limits = c(1.1, 3.65), breaks = c(1, 2, 3))
log_cpov_density

MEplot_contest <- ggarrange(log_cpov_ME, log_cpov_density, ncol = 1, nrow = 2,
                    labels = NULL#c("A", "B")
                   # , legend = "bottom", common.legend = F
)
MEplot_contest
ggsave(plot = MEplot_contest, paste0(here("analysis_main", "figs"), "/Meffects_comunapov_contest.png"), width = 7, height = 7)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Alternative matching approaches
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ratios <- seq(from = 1, to = 5)
calipers <- c(0.25, 0.5,1,2, "none")
altmatch_results <- twfe_trees_all %>% tidy(vcov = ~property_ID) %>% mutate(ratio = 3, caliper = "none", outcome = "Tree cover") 
for(calip in calipers){
  
  for(r in ratios){
    
    if(r == 3 & calip == "none"){
      altmatch_results <- altmatch_results
      
    } else{
      
      this_data <- readRDS(paste0(clean_data_dir, "/matched_data_long_", r, "_", calip,  ".rds"))%>%
        mutate(post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
               intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0))
      
      ##### Trees
      twfe_trees <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = this_data)
      altmatch_results <- twfe_trees %>% tidy(vcov = ~property_ID) %>% mutate(ratio = r, caliper = calip, outcome = "Tree cover") %>% rbind(altmatch_results)
      
    }
    
  }
  
}



plot_models <- altmatch_results %>%
  select(-term)%>%
  mutate(term = as.numeric(ratio),
         model = caliper)%>%
  arrange(-term)

altmatch_pal <- c(palette$blue, palette$red, palette$dark_green, palette$gold, palette$dark)

spec_altmatch <- dwplot(plot_models,
                     whisker_args = list(size = 1.1,
                                         aes(colour = model)),
                     dot_args = list(
                       aes(fill = model
                           #  , shape = model
                       ), 
                       size = 3,
                       shape = 21,
                       color = palette$white)
) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(#title = "ATT estimates by contest group", 
    x = "ATT for tree cover (main specification)", 
    y = "# of nearest neighbors") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5,
        axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)
  ) +
  coord_flip()+
  scale_x_continuous(minor_breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03),
                     limits = c(-0.01, 0.035))+
  #  scale_shape_discrete(name  = "Outcome", breaks = c(0, 1, 2)) + # breaks assign shapes
  scale_color_manual(values = altmatch_pal, name = "Caliper") +# start/end for light/dark greys
  scale_fill_manual(values = altmatch_pal, name = "Caliper") # start/end for light/dark greys
spec_altmatch

ggsave(plot = spec_altmatch, paste0(here("analysis_main", "figs"), "/spec_chart_altmatch.png"), width = 8, height = 5)



          