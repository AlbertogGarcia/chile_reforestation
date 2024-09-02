library(tidyverse)
library(sf)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

file_dir <- "C:/Users/garci/Dropbox/chile_reforestation/"
results_dir <- "paper/results/"

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  TWFE estimates using matched group for compliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_wide_main <- readRDS(paste0(file_dir, "data/analysis_lc/main/matched_compliers_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())

control_contest <- matched_wide_main %>% filter(treat == 1) %>% 
  mutate(control_contest = rptpro_tipo_concurso)%>%
  select(subclass, control_contest)

matched_long <- matched_wide_main %>%
  left_join(control_contest, by = "subclass") %>%
  pivot_longer(Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year),
         property_hectares = pixels_count / 0.09 )%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

matched_data <- matched_long %>%
  filter(is.finite(rptpro_superficie/rptpre_superficie_predial) | treat ==0)%>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = ifelse(treat == 1, rptpro_superficie/rptpre_superficie_predial, 0))

twfe_evi <- feols(evi ~ treat : post : intensity| Year + property_ID, data = matched_data)

twfe_trees <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data)
summary(twfe_trees, vcov = ~property_ID)

twfe_grassland <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data)

twfe_crop <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data)

# weights 
twfe_trees_weights <- feols(Trees ~ treat : post : intensity| Year + property_ID, weights = ~property_hectares, data = matched_data )#%>% filter( control_contest != "Otros Interesados"))
summary(twfe_trees_weights, vcov = ~property_ID)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  regression table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("Tree cover" = twfe_trees,
              "Crop" = twfe_crop,
              "Grassland" = twfe_grassland
              )

crop_2008 <- round(mean(subset(matched_data, Year == 2008 & treat == 1)$Crop, na.rm = T), digits = 3) 
grassland_2008 <- round(mean(subset(matched_data, Year == 2008 & treat == 1)$Grassland, na.rm = T) , digits = 3)
trees_2008 <- round(mean(subset(matched_data, Year == 2008 & treat == 1)$Trees, na.rm = T)  , digits = 3)

rows <- tribble(~term, ~`Tree cover`, ~Crop, ~Grassland,
                'Control group', 'matched 2-to-1', 'matched 2-to-1', 'matched 2-to-1'
                , '2008 mean', as.character(trees_2008), as.character(crop_2008), as.character(grassland_2008)
                )%>%
  as.data.frame()

f1 <- function(x) format(round(x, 5), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,
             output="latex",
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
  kableExtra::save_kable(paste0(results_dir, "twfe_main_table.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Cohort twfe impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cohorts <- seq(2009, 2018, by = 1)
cohort_results <- data.frame()
for(i in cohorts){
  this_twfe <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data %>% filter(first.treat %in% c(0, i)))

  this_summary <- summary(this_twfe, vcov = ~property_ID)
  
  cohort_results <- data.frame("cohort" = i, "coef" = this_summary$coefficients, "se" = this_summary$se)%>%
    rbind(cohort_results)
  print(i)
  }

library(rio)
export(cohort_results, "paper/results/cohort_results.rds")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  TWFE estimates using matched group for noncompliers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_nc_wide <- readRDS(paste0(file_dir, "data/analysis_lc/main/matched_noncompliers_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())

matched_nc_long <- pivot_longer(matched_nc_wide, Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

matched_nc_data <- matched_nc_long %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = ifelse(treat == 1, rptpro_superficie/rptpre_superficie_predial, 0))

twfe_nc_evi <- feols(evi ~ treat : post : intensity| Year + property_ID, data = matched_nc_data)

twfe_nc_trees <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_nc_data)
summary(twfe_nc_trees, vcov = ~property_ID)

twfe_nc_grassland <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_nc_data)

twfe_nc_crop <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_nc_data)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  regression table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("Tree cover" = twfe_nc_trees,
              "Crop" = twfe_nc_crop,
              "Grassland" = twfe_nc_grassland
)

crop_nc_2008 <- round(mean(subset(matched_nc_data, Year == 2008 & treat == 1)$Crop, na.rm = T), digits = 3) 
grassland_nc_2008 <- round(mean(subset(matched_nc_data, Year == 2008 & treat == 1)$Grassland, na.rm = T) , digits = 3)
trees_nc_2008 <- round(mean(subset(matched_nc_data, Year == 2008 & treat == 1)$Trees, na.rm = T)  , digits = 3)

rows <- tribble(~term, ~`Tree cover`, ~Crop, ~Grassland,
                'Control group', 'matched 2-to-1', 'matched 2-to-1', 'matched 2-to-1', 
                '2008 mean', as.character(trees_nc_2008), as.character(crop_nc_2008), as.character(grassland_nc_2008))%>%
  as.data.frame()

modelsummary(models,
             output="latex",
             title = 'Estimates of subsidy impact on non-compliers using matched control group',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treat1:post:intensity" = "Intensity"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "twfe_nc_main_table.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############### Noncompliers as control
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents

enrolled_wide <- readRDS(paste0(file_dir, "data/analysis_lc/main/enrolled_wide_pctg.rds"))%>%
  select(Trees_1999:NA_2018, evi_2005:evi_2020, everything())%>%
  filter(received_bonus == 1 | first.treat == 0)%>%
  mutate(treat = ifelse(first.treat == 0, 0, 1))

enrolled_long <- pivot_longer(enrolled_wide, Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

enrolled_data <- enrolled_long %>%
  mutate(post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = rptpro_superficie/rptpre_superficie_predial,
         treat = as.factor(treat))

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Main impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

twfe_enrolled_evi <- feols(evi ~ treat : post : intensity| Year + property_ID, data = enrolled_data)

twfe_enrolled_trees <- feols(Trees ~ treat : post : intensity | Year + property_ID, data = enrolled_data)

twfe_enrolled_grassland <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = enrolled_data)

twfe_enrolled_crop <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = enrolled_data)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Enrollment regression table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("Tree cover" = twfe_enrolled_trees,
              "Crop" = twfe_enrolled_crop,
              "Grassland" = twfe_enrolled_grassland
)

rows <- tribble(~term, ~`Tree cover`, ~Crop, ~Grassland,
                'Control group', 'non-compliers', 'non-compliers', 'non-compliers', 
                '2008 mean', as.character(trees_2008), as.character(crop_2008), as.character(grassland_2008))%>%
  as.data.frame()

modelsummary(models,
             output="latex",
             title = 'Estimates of subsidy impact on compliers using non-complier control group',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treat1:post:intensity" = "Intensity"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "twfe_enrolled_main_table.tex"))


#%%%%%%%%%%%%%%%%%%%%%%%%%
### contest
#%%%%%%%%%%%%%%%%%%%%%%%%%

#trees

matched_trees_other <- feols(Trees ~ treat : post : intensity| Year + property_ID, data = matched_data %>% filter(control_contest == "Otros Interesados"))
summary(matched_trees_other)

matched_trees_smallholder <- feols(Trees ~ treat : post : intensity | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))
summary(matched_trees_smallholder)

enrolled_trees_contest <- feols(Trees ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)

enrolled_trees_smallholder <- feols(Trees ~ treat : post : intensity 
                                    | Year + property_ID, data = enrolled_data %>% filter(rptpro_tipo_concurso != "Otros Interesados"))

# crop

matched_crop_other <- feols(Crop ~ treat : post : intensity| Year + property_ID, data = matched_data %>% filter(control_contest == "Otros Interesados"))

matched_crop_smallholder <- feols(Crop ~ treat : post : intensity | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))

enrolled_crop_contest <- feols(Crop ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)

enrolled_crop_smallholder <- feols(Crop ~ treat : post : intensity | Year + property_ID, data = enrolled_data %>% filter(rptpro_tipo_concurso != "Otros Interesados"))

#grassland

matched_grassland_other <- feols(Grassland ~ treat : post : intensity| Year + property_ID, data = matched_data %>% filter(control_contest == "Otros Interesados"))

matched_grassland_smallholder <- feols(Grassland ~ treat : post : intensity | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))


enrolled_grassland_contest <- feols(Grassland ~ treat : post : intensity * rptpro_tipo_concurso| Year + property_ID, data = enrolled_data)

enrolled_grassland_smallholder <- feols(Grassland ~ treat : post : intensity | Year + property_ID, data = enrolled_data %>% filter(rptpro_tipo_concurso != "Otros Interesados"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Regression tables by contest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("(1)" = matched_trees_smallholder,
              "(2)" = matched_trees_other,
              "(3)" = matched_crop_smallholder,
              "(4)" = matched_crop_other,
              "(5)" = matched_grassland_smallholder,
              "(6)" = matched_grassland_other
)

rows <- tribble(~term,  ~smallholder_trees,  ~other_trees, ~smallholder_crop,  ~other_crop, ~smallholder_grassland, ~other_grassland, 
                'Subsample', 'Smallholder', 'Other interested', 'Smallholder', 'Other interested', 'Smallholder', 'Other interested', 
)%>%
  as.data.frame()

my_coef_names <- c("treat1:post:intensity" = "Intensity"
)

modelsummary(models,
             output="latex",
             title = 'Heterogeneous tree cover impacts by contest',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             gof_omit = 'DF|Deviance|R2|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Tree Cover" = 2, "Crop" = 2, "Grassland" = 2))%>%
  kableExtra::save_kable(paste0(results_dir, "twfe_matched_contest_table.tex"))



# Fore enrolled group (non-compliers as control)

models = list("(1)" = enrolled_trees_contest,
              "(2)" = enrolled_trees_smallholder,
              "(3)" = enrolled_crop_contest,
              "(4)" = enrolled_crop_smallholder,
              "(5)" = enrolled_grassland_contest,
              "(6)" = enrolled_grassland_smallholder
)

rows <- tribble(~term, ~contest_trees, ~smallholder_trees,  ~contest_crop, ~smallholder_crop,  ~contest_grassland, ~smallholder_grassland,  
                'Subsample', 'Full sample', 'Smallholder', 'Full sample', 'Smallholder', 'Full sample', 'Smallholder',
                'Control group', 'Non-compliers', 'Non-compliers', 'Non-compliers', 'Non-compliers', 'Non-compliers', 'Non-compliers'
)%>%
  as.data.frame()

modelsummary(models,
             output="latex",
             title = 'Heterogeneous tree cover impacts by contest',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             gof_omit = 'DF|Deviance|R2|N|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Tree Cover" = 2, "Crop" = 2, "Grassland" = 2))%>%
  kableExtra::save_kable(paste0(results_dir, "twfe_enrolled_contest_table.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%
### comuna poverty marginal effects plots
#%%%%%%%%%%%%%%%%%%%%%%%%%
library(patchwork)
library(marginaleffects)
me_data <- matched_data %>%
  mutate(treatment = as.numeric(treat) * post * intensity,
         log_cpov = log(cpov_pct_2007 + 1),
         cpov_pct_2007 = cpov_pct_2007 / 100
         )
matched_trees_cpov <- feols(Trees ~ treatment   + treatment*log_cpov
                            | Year + property_ID, data = me_data)


log_cpov_ME <- plot_cme(matched_trees_cpov, effect = "treatment", condition = "log_cpov")+
  #geom_rug(aes(x = cpov_pct_2007), data = me_data) +
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("ln(ComunaPov)")+ylab("ATT")+
  theme(axis.text.y = element_blank())+
  geom_vline(xintercept = mean(me_data$log_cpov), linetype = "dashed", color = "#1c86ee")+
  ylim(-0.007, 0.022)
log_cpov_ME

matched_trees_rawcpov <- feols(Trees ~ treatment   + treatment*cpov_pct_2007
                            | Year + property_ID, data = me_data)

cpov_ME <- plot_cme(matched_trees_rawcpov, effect = "treatment", condition = "cpov_pct_2007")+
  #geom_rug(aes(x = cpov_pct_2007), data = me_data) +
  theme_minimal()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("ComunaPov")+ylab("ATT")+
  geom_vline(xintercept = mean(me_data$cpov_pct_2007), linetype = "dashed", color = "#1c86ee")+
  scale_x_continuous(labels = scales::percent)+
  ylim(-0.007, 0.022)
cpov_ME

p4 <- ggplot(data.frame(l = log_cpov_ME$labels$y, x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

log_cpov_ME$labels$y <- cpov_ME$labels$y <- " "

combined <- p4 + ( cpov_ME | log_cpov_ME)
cpov_marginaleffects <- combined + plot_layout(
  widths = c(1, 100)) 
cpov_marginaleffects

ggsave(cpov_marginaleffects, path = "paper/figs", filename = "cpov_marginaleffects.png", width = 6, height = 3.5)


#%%%%%%%%%%%%%%%%%%%%%%%%%
### comuna poverty
#%%%%%%%%%%%%%%%%%%%%%%%%%


#Trees

matched_trees_cpov <- feols(Trees ~ treat : post : intensity   +
                              treat : post : intensity : log(cpov_pct_2007 + 1)
                            | Year + property_ID, data = matched_data)
summary(matched_trees_cpov)

matched_trees_cpov_smallholder <- feols(Trees ~ treat : post : intensity * log(cpov_pct_2007 + 1)
                            | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))
summary(matched_trees_cpov_smallholder)

#Crop
matched_crop_cpov <- feols(Crop ~ treat : post : intensity   +
                              treat : post : intensity : log(cpov_pct_2007 + 1)
                            | Year + property_ID, data = matched_data)


matched_crop_cpov_smallholder <- feols(Crop ~ treat : post : intensity * log(cpov_pct_2007 + 1)
                                        | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))

# Grassland
matched_grassland_cpov <- feols(Grassland ~ treat : post : intensity   +
                             treat : post : intensity : log(cpov_pct_2007 + 1)
                           | Year + property_ID, data = matched_data)


matched_grassland_cpov_smallholder <- feols(Grassland ~ treat : post : intensity * log(cpov_pct_2007 + 1)
                                       | Year + property_ID, data = matched_data %>% filter( control_contest != "Otros Interesados"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Conumal-poverty regression tables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("(1)" = matched_trees_cpov,
              "(2)" = matched_trees_cpov_smallholder,
              "(3)" = matched_crop_cpov,
              "(4)" = matched_crop_cpov_smallholder,
              "(5)" = matched_grassland_cpov,
              "(6)" = matched_grassland_cpov_smallholder
)

rows <- tribble(~term,  ~cpov_trees,  ~smallholder_trees, ~cpov_crop, ~smallholder_crop, ~cpov_grassland, ~smallholder_grassland,  
                'Subsample', 'Full sample', 'Smallholder', 'Full sample', 'Smallholder', 'Full sample', 'Smallholder'
)%>%
  as.data.frame()

my_coef_names <- c("treat1:post:intensity" = "Intensity",
                   "treat1:post:intensity:log(cpov_pct_2007 + 1)" = "Intensity x ln(ComunaPov)"
)

modelsummary(models,
             output="latex",
             title = 'Heterogeneous tree cover impacts by comuna-level poverty',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             gof_omit = 'DF|Deviance|R2|N|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Tree Cover" = 2, "Crop" = 2, "Grassland" = 2))%>%
  kableExtra::save_kable(paste0(results_dir, "twfe_matched_cpov_table.tex"))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

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
         intensity = rptpro_superficie/rptpre_superficie_predial,
         treat = as.factor(treat))

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Creating for matched group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_wide <- matched_wide_main %>%
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

small_df <- matched_wide %>%
  filter(rptpro_tipo_concurso != "Otros Interesados", treat == 1)%>%
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
         med_social = median(social_puntaje, na.rm = T),
         med_puntaje = median(rptpro_puntaje, na.rm = T),
         smallholder = 1) 

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- matched_wide %>%
  filter(rptpro_tipo_concurso == "Otros Interesados", treat == 1)%>%
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
         smallholder = 0,
         med_social = median(social_puntaje, na.rm = T),
         med_puntaje = median(rptpro_puntaje, na.rm = T)
  )

control_df <- matched_wide %>%
  filter(treat == 0)

matched_long_social <- bind_rows(other_df, small_df, control_df)%>%
  left_join(control_contest, by = "subclass")%>%
  pivot_longer(Trees_1999:evi_2020)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  filter(class %in% c("Trees", "Grassland", "evi", "Crop"))%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

matched_data_social <- matched_long_social %>%
  filter(is.finite(rptpro_superficie/rptpre_superficie_predial) | treat == 0)%>%
  mutate(Year= as.numeric(Year),
         above_medsocial = ifelse(social_puntaje > med_social, 1, 0),
         above_medpuntaje = ifelse(rptpro_puntaje > med_puntaje, 1, 0),
         post = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = ifelse(treat == 1, rptpro_superficie/rptpre_superficie_predial, 0),
         treat = as.factor(treat),
         norm_social = (social_puntaje - mean(social_puntaje, na.rm = T))/ sd(social_puntaje, na.rm = T),
         norm_project = (adjusted_puntaje - mean(adjusted_puntaje, na.rm = T))/ sd(adjusted_puntaje, na.rm = T))
  


#%%%%%%%%%%%%%%%%%%%%%%%%%
### Social and adjusted scores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_trees_social_abovemed <- feols(Trees ~ treat : post : intensity 
                                       | Year + property_ID, data = matched_data_social %>% filter(above_medpuntaje == 1 | treat == 0))

summary(matched_trees_social_smallholder_abovemed)

matched_trees_social_belowmed <- feols(Trees ~ treat : post : intensity
                           | Year + property_ID, data = matched_data_social %>% filter(above_medpuntaje == 0 | treat == 0))
summary(matched_trees_social_belowmed)

matched_trees_social_smallholder_abovemed <- feols(Trees ~ treat : post : intensity 
                                           | Year + property_ID, data = matched_data_social %>% filter(control_contest != "Otros Interesados" , above_medsocial == 1 | treat == 0))

matched_trees_social_smallholder_belowmed <- feols(Trees ~ treat : post : intensity 
                                                    | Year + property_ID, data = matched_data_social %>% filter(control_contest != "Otros Interesados", above_medsocial == 0 | treat == 0))

summary(matched_trees_social_smallholder_belowmed)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  Matched heterogeneous impacts regression tables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
models = list("(1)" = matched_trees_social,
            #  "(2)" = matched_trees_social_interact,
              "(2)" = matched_trees_social_smallholder,
              "(3)" = matched_trees_cpov,
              "(4)" = matched_trees_cpov_smallholder
)

rows <- tribble(~term, ~social_trees,  ~socialsmallholder_trees, ~cpov_trees, ~cpovsmallholder_trees, 
                'Subsample', 'Full sample',  'Smallholder', 'Full sample', 'Smallholder',
                'Control group', 'Matched 2-to-1', 'Matched 2-to-1', 'Matched 2-to-1', 'Matched 2-to-1'
)%>%
  as.data.frame()

my_coef_names <- c("treat1:post:intensity" = "Intensity",
                   "treat1:post:intensity:log(cpov_pct_2007)"= "Intensity x ln(Comuna poverty)",
                  # "treat1:post:intensity:cpov_pct_2007:rptpro_tipo_concursoPequeños Propietarios"= "Intensity x Comuna pov. x Smallholder",
              #     "treat1:post:intensity:rptpro_tipo_concursoPequeños Propietarios" = "Intensity x Smallholder",
                   "treat1:post:intensity:log(adjusted_puntaje)" = "Intensity x ln(Project score)",
                   "treat1:post:intensity:log(social_puntaje)" = "Intensity x ln(Social score)"
                  # "treat1:post:intensity:social_puntaje:rptpro_tipo_concursoPequeños Propietarios" = "Intensity x Social score x Smallholder"
                   )

modelsummary(models,
             output="latex",
             title = 'Heterogeneous tree cover impacts by priority characteristics',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             gof_omit = 'DF|Deviance|R2|N|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Tree Cover" = 4))%>%
 # column_spec(c(1,2,3,4,5, 6), width = "2.5cm", latex_valign = "m")%>%
  kableExtra::save_kable(paste0(results_dir, "twfe_matched_targeting_trees.tex"))


models = list("(1)" = enrolled_trees_social,
              #  "(2)" = matched_trees_social_interact,
              "(2)" = enrolled_trees_social_smallholder,
              "(3)" = enrolled_trees_cpov,
              "(4)" = enrolled_trees_cpov_smallholder
)

rows <- tribble(~term, ~social_trees,  ~socialsmallholder_trees, ~cpov_trees, ~cpovsmallholder_trees, 
                'Subsample', 'Full sample',  'Smallholder', 'Full sample', 'Smallholder',
                'Control group', 'Non-compliers', 'Non-compliers', 'Non-compliers', 'Non-compliers'
)%>%
  as.data.frame()


modelsummary(models,
             output="latex",
             title = 'Heterogeneous tree cover impacts by priority characteristics based on non-complier control group',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~property_ID,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             gof_omit = 'DF|Deviance|R2|N|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Tree Cover" = 4))%>%
  # column_spec(c(1,2,3,4,5, 6), width = "2.5cm", latex_valign = "m")%>%
  kableExtra::save_kable(paste0(results_dir, "twfe_enrolled_targeting_trees.tex"))
