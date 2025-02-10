library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)

dollar_per_utm <- 36679 * 0.0010274427

file_dir <- "C:/Users/garci/Dropbox/chile_reforestation/"
results_dir <- "paper/results/"

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/proyecto.xlsx"))

#properties and coordinates
predio_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/predio.xlsx"))
#owners
propietario_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/propietario.xlsx"))
#stands
rodal_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(file_dir, "external_data/concurso_conaf/program/actividad.xlsx"))

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  filter(rptpro_ano <= 2019)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Descriptive
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

past_2_years <- property_df %>% filter(rptpro_ano <= 2019)

# proportion submitting management plan
mean(past_2_years$submitted_management_plan)

# proportion receiving bonus
mean(past_2_years$received_bonus)

# proportion receiving bonus conditional on submitting management plan
df_with_management_plan <- past_2_years %>% filter(submitted_management_plan == 1)
mean(df_with_management_plan$received_bonus)

activity_df <- property_df %>%
  left_join(rodal_df, by = "rptpre_id") %>%
  left_join(actividades_df, by = "rptro_id") %>%
  dcast(rptpro_id
        + rptpro_ano
        ~ rptac_tipo, 
        fun.aggregate = function(x){as.integer(length(x) > 0)})


accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

NFL_df <- property_df %>%
  left_join(activity_df, by = c("rptpro_id", "rptpro_ano")) %>%
  mutate(indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0)%>%
  distinct(rptpro_id, rptpre_id, received_bonus, .keep_all = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Simple statistics for paper
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# total awarded (allocated) in USD
total_awarded <- sum(NFL_df$rptpro_monto_total, na.rm = T) * dollar_per_utm

# total land enrolled
total_land <- sum(NFL_df$rptpre_superficie_bonificada, na.rm = T)

# percent of awarded money actually paid to landowners
pct_compliance <- mean(NFL_df$received_bonus)

# percent with management plan
pct_mgmt_plan <- mean(property_df$submitted_management_plan)

# percent compliers conditional on having management plan
pct_complier_given_plan <- mean( 
  subset(property_df, submitted_management_plan == 1)$received_bonus
)

# Total amount saved by not paying non-compliers

utm_saved <- sum( 
  subset(NFL_df, submitted_management_plan == 0 
  )$rptpro_monto_total
)

usd_saved <- utm_saved * dollar_per_utm


### Smallholder or indigenous applicants
marginalized_df <- NFL_df %>%
  filter(indigenous == 1 | `Contest type` == "Smallholder")

# Amount awarded to smallholder or indigenous applicants
marginalized_awarded <- sum(marginalized_df$rptpro_monto_total) * dollar_per_utm

# Percent of total awarded to smallholder or indigenous applicants
pct_targeted_to_marginalized <- marginalized_awarded / total_awarded

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Recreating social and project score components
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readr)
guess_encoding("C:/Users/garci/Dropbox/chile_reforestation/data/analysis_lc/comunal_poverty.csv")

comunal_pov <- read.csv("C:/Users/garci/Dropbox/chile_reforestation/data/analysis_lc/comunal_poverty.csv", fileEncoding = "ISO-8859-1" #encoding = "UTF-8"#"Latin-1"
                        )%>%
  mutate(comuna = tolower(accents(comuna)))%>%
  mutate_at(vars(cpov_pct_2007), ~as.numeric(as.character(.)))


sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_df <- NFL_df %>%
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
         smallholder = 1) %>%
  mutate(comuna = tolower(accents(rptpre_comuna)))%>%
  left_join(comunal_pov, by = "comuna")

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- NFL_df %>%
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
  )%>%
  mutate(comuna = tolower(accents(rptpre_comuna)))%>%
  left_join(comunal_pov, by = "comuna")
#fuzzyjoin::stringdist_left_join(comunal_pov, by = "comuna", max_dist = 2, distance_col = "sdist_comuna")

NFL_df <- rbind(other_df, small_df)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Describing flows to different groups
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

NFL_df <- within(NFL_df, cpov_quartile <- as.integer(cut(cpov_pct_2007, quantile(cpov_pct_2007, probs=0:4/4, na.rm = T), include.lowest=TRUE)))

cpov_quartile <- NFL_df %>%
  filter(`Contest type` != "Smallholder")%>%
  group_by(`Contest type`, cpov_quartile)%>%
  summarise(total_allocated_usd = sum(rptpre_monto_total*dollar_per_utm, na.rm = T),
            total_paid_usd =  sum(rptpre_monto_total * received_bonus * dollar_per_utm, na.rm = T),
            #total_notpaid_usd =  sum(rptpre_monto_total * (1-received_bonus) * dollar_per_utm, na.rm = T)
            )%>%
  mutate(proportion_paid = total_paid_usd/total_allocated_usd)%>%
  pivot_longer(cols = c("total_allocated_usd", "total_paid_usd"))%>%
  drop_na(cpov_quartile)



ggplot(data = cpov_quartile, aes(x = cpov_quartile, y = value, fill = name))+
  geom_col()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Does score predict poverty
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

score_pov <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                    , data = NFL_df)
summary(score_pov, vcov = ~rptpre_id)

score_pov_smallholder <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                    , data = NFL_df %>% filter(rptpro_tipo_concurso != "Otros Interesados"))
summary(score_pov_smallholder, vcov = ~rptpre_id)

score_povfe <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                      + as.factor(rptpre_region) 
                   #  + as.factor(rptpro_objetivo_manejo)
                   , data = NFL_df)

score_povfe_smallholder <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                                 + as.factor(rptpre_region) 
                               #+ as.factor(rptpro_objetivo_manejo)
                               , data = NFL_df %>% filter(rptpro_tipo_concurso != "Otros Interesados"))


models = list( "(1)" = score_pov,
               "(2)" = score_pov_smallholder,
               "(3)" = score_povfe,
               "(4)" = score_povfe_smallholder
)

rows <- tribble(~term, ~mod1,  ~mod2, ~mod3, ~mod4,
                'Subsample', 'Full sample', 'Smallholder', 'Full sample', 'Smallholder',
                'Region FE', 'No', 'No', 'Yes', 'Yes'
)%>%
  as.data.frame()

my_coef_names <- c("log(social_puntaje + 1)"= "ln(Social score)",
                   "log(adjusted_puntaje + 1)" = "ln(Project score)"
)

options("modelsummary_format_numeric_latex" = "plain")

modelsummary(models,
             output="latex",
             title = 'Social score is associated with relatively higher comuna level poverty',
             fmt = 5, # 4 digits and trailing zero
             vcov = ~rptpre_id,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             coef_omit = 'factor|Inter',
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|BIC|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "ln(ComunaPov)" =4 ) ) %>%
  kableExtra::save_kable(paste0(results_dir, "poverty-score_assoc.tex"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Determinants of compliance
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compliance_mod1 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           smallholder +
                       + as.factor(rptpre_region) 
                     #  + as.factor(rptpro_objetivo_manejo)
                      , data = NFL_df)
summary(compliance_mod1)

compliance_mod2 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                               log(cpov_pct_2007 + 1)
                         +smallholder
                         + as.factor(rptpre_region) 
                     #    + as.factor(rptpro_objetivo_manejo)
                         , data = NFL_df)

compliance_mod3 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           log(cpov_pct_2007 + 1)
                         + log(social_puntaje + 1)*smallholder
                         + as.factor(rptpre_region) 
                     #    + as.factor(rptpro_objetivo_manejo)
                         , data = NFL_df)

compliance_mod4 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           log(cpov_pct_2007 + 1)
                         + as.factor(rptpre_region) 
                     #    + as.factor(rptpro_objetivo_manejo)
                         , data = small_df)

compliance_mod5 <- feols(received_bonus ~ 
                           log(cpov_pct_2007 + 1)
                         + log(cpov_pct_2007 + 1)*smallholder
                          + log(rptpro_monto_total)
                         + extensionista
                         + as.factor(rptpre_region) 
                     #    + as.factor(rptpro_objetivo_manejo)
                         , data = NFL_df)

models = list( "(1)" = compliance_mod1,
               "(2)" = compliance_mod2,
               "(3)" = compliance_mod3,
               "(4)" = compliance_mod4,
               "(5)" = compliance_mod5
)

rows <- tribble(~term, ~mod1,  ~mod2, ~mod3, ~mod1_log, ~mod2_log, 
                'Subsample', 'Full sample', 'Full sample', 'Full sample', 'Smallholder','Full sample',
                'Region FE' , 'Yes', 'Yes', 'Yes', 'Yes', 'Yes'
)%>%
  as.data.frame()

my_coef_names <- c("log(social_puntaje + 1)"= "ln(Social score)",
                   "log(cpov_pct_2007 + 1)" = "ln(ComunaPov)",
                   "log(adjusted_puntaje + 1)" = "ln(Project score)",
                   "smallholder" = "Smallholder",
                   "extensionista" = "Extensionist",
                   "log(social_puntaje + 1):smallholder" = "ln(Social score) x Smallholder",
                   "log(rptpro_monto_total)" = "ln(Subsidy amount)",
                   "log(cpov_pct_2007 + 1):smallholder" = "ln(ComunaPov) x Smallholder"
)


modelsummary(models,
             output="latex",
             title = 'Social score is negatively associated with compliance',
             fmt = 5, # 4 digits and trailing zero
             vcov = ~rptpre_id,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
           coef_omit = 'factor|Inter',
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|BIC|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Complied" = 5) ) %>%
  kableExtra::save_kable(paste0(results_dir, "compliance_targeting.tex"))


# simpler table for slides

slides_mod1 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                     + as.factor(rptpre_region) 
                     , data = NFL_df)

slides_mod2 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                     +smallholder   +
                       smallholder:log(social_puntaje + 1)
                      + as.factor(rptpre_region) 
                         , data = NFL_df)

slides_mod3 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                         + as.factor(rptpre_region) 
                         , data = small_df)







models = list( "(1)" = slides_mod1,
               "(2)" = slides_mod2,
               "(3)" = slides_mod3
)

rows <- tribble(~term, ~mod1,  ~mod2, ~mod3, 
                'Subsample', 'Full sample', 'Full sample', 'Smallholder',
                'Region FE' , 'Yes', 'Yes', 'Yes'
)%>%
  as.data.frame()

my_coef_names <- c("log(social_puntaje + 1)"= "ln(Social score)",
                   "log(cpov_pct_2007 + 1)" = "ln(ComunaPov)",
                   "log(adjusted_puntaje + 1)" = "ln(Project score)",
                   "smallholder" = "Smallholder",
                   "log(social_puntaje + 1):smallholder" = "ln(Social score) x Smallholder",
                   "log(rptpro_monto_total)" = "ln(Subsidy amount)",
                   "log(cpov_pct_2007 + 1):smallholder" = "ln(ComunaPov) x Smallholder"
)


modelsummary(models,
             output="latex",
             title = 'Social score is negatively associated with compliance',
             fmt = 3, # 4 digits and trailing zero
             vcov = ~rptpre_id,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             coef_omit = 'factor|Inter',
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|BIC|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "Complied" = 3) ) %>%
  kableExtra::save_kable(paste0(results_dir, "compliance_targeting_slides.tex"))

