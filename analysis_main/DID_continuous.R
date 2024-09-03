library(dplyr)
library(fixest)
library(sf)
library(kableExtra)
library(modelsummary)

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
  mutate(treat = (first.treat > 0)*1,
         post = ifelse(treat == 1 & Year >= first.treat, 1, 0),
         intensity = ifelse(treat == 1, rptpre_superficie_bonificada/rptpre_superficie_predial, 0))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Trees
twfe_trees_all <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long)
summary(twfe_trees_all, vcov = ~property_ID)

twfe_trees_smallholder <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest != "Otros Interesados"))
summary(twfe_trees_smallholder, vcov = ~property_ID)

twfe_trees_other <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data_long %>% filter(control_contest == "Otros Interesados"))
summary(twfe_trees_other, vcov = ~property_ID)

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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  regression table

models = list("(1)" = twfe_trees_smallholder,
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
modelsummary(models,
        #     output="latex",
             title = 'Estimates of subsidy impact using matched control group',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treat1:post:intensity" = "Intensity"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1, "Tree cover" = 1, "Crop" = 1, "Grassland" = 1))%>%
  add_header_above(c(" " = 1, "Smallholders" = 3, "Other Interested Parties" = 3))
#  kableExtra::save_kable(paste0(results_dir, "twfe_main_table.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Spec chart

library(dotwhisker)
tidy_models <- twfe_crop_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Crop") 
tidy_models <- twfe_grassland_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Grassland") %>% rbind(tidy_models)
tidy_models <- twfe_trees_other %>% tidy(vcov = ~property_ID) %>% mutate(group = "Other interested", outcome = "Tree cover") %>% rbind(tidy_models)

tidy_models <- twfe_crop_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Crop") %>% rbind(tidy_models)
tidy_models <- twfe_grassland_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Grassland")  %>% rbind(tidy_models)
tidy_models <- twfe_trees_smallholder %>% tidy(vcov = ~property_ID) %>% mutate(group = "Smallholders", outcome = "Tree cover") %>% rbind(tidy_models)


plot_models <- tidy_models %>%
  select(-term)%>%
  rename(model = outcome,
         term = group)

dwplot(plot_models,
       whisker_args = list(size = 1.1),
       dot_args = list(
         aes(colour = model
           #  , shape = model
             ), 
         size = 3)
       ) + 
  theme_bw() + 
  geom_vline(xintercept = 0, linetype = "dashed")+
  labs(#title = "ATT estimates by contest group", 
       x = "ATT Estimate with 95% CI", 
       y = "") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "bottom",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5
        ) +
  scale_x_continuous(minor_breaks = c(-0.02, -0.01, 0, 0.01, 0.02, 0.03))+
#  scale_shape_discrete(name  = "Outcome", breaks = c(0, 1, 2)) + # breaks assign shapes
  scale_color_manual(values = c(palette$brown, palette$gold, palette$green), name = "Outcome") # start/end for light/dark greys


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
matched_trees_cpov <- feols(Trees ~ treatment   + treatment*log_cpov*control_contest
                            | Year + property_ID, data = me_data %>% filter(is.finite(treatment)))


log_cpov_ME <- marginaleffects::plot_comparisons(matched_trees_cpov, variables = "treatment", condition = list("log_cpov", "control_contest"))+
  geom_rug(aes(x = log_cpov), data = me_data) +
  theme_minimal()+
  geom_hline(yintercept = 0)+
  xlab("ln(ComunaPov)")+ylab("ATT")+
  theme(axis.text.y = element_blank())+
  geom_vline(xintercept = mean(me_data$log_cpov+0.01), linetype = "dashed", color = "#1c86ee")
  #ylim(-0.007, 0.022)
log_cpov_ME
