library(tidyverse)
#library(reshape2)
library(ggplot2)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)

dollar_per_utm <- 36679 * 0.0010274427

clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
results_dir <- here("analysis_main", "results")

admin_df <- readRDS(paste0(clean_data_dir, "/admin_df.rds")) %>%
  filter(rptpro_ano <= 2019)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Descriptive
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

past_2_years <- admin_df %>% filter(rptpro_ano <= 2019)

# proportion submitting management plan
mean(past_2_years$submitted_management_plan)

# proportion receiving bonus
mean(past_2_years$received_bonus)

# proportion receiving bonus conditional on submitting management plan
df_with_management_plan <- past_2_years %>% filter(submitted_management_plan == 1)
mean(df_with_management_plan$received_bonus)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### Simple statistics for paper
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# total awarded (allocated) in USD
total_awarded <- sum(admin_df$rptpro_monto_total, na.rm = T) * dollar_per_utm

# total land enrolled
total_land <- sum(admin_df$rptpre_superficie_bonificada, na.rm = T)

# percent of awarded money actually paid to landowners
pct_compliance <- mean(admin_df$received_bonus)

# percent with management plan
pct_mgmt_plan <- mean(admin_df$submitted_management_plan)

# percent compliers conditional on having management plan
pct_complier_given_plan <- mean( 
  subset(admin_df, submitted_management_plan == 1)$received_bonus
)

# Total amount saved by not paying non-compliers

utm_saved <- sum( 
  subset(admin_df, submitted_management_plan == 0 
  )$rptpro_monto_total
)

usd_saved <- utm_saved * dollar_per_utm


### Smallholder or indigenous applicants
marginalized_df <- admin_df %>%
  filter(indigenous == 1 | `Contest type` == "Smallholder")

smallholder_df <- admin_df %>%
  filter(`Contest type` == "Smallholder")

indigenous_df <- admin_df %>%
  filter(indigenous == 1)


# Amount awarded to smallholder applicants
smallholder_awarded <- sum(smallholder_df$rptpro_monto_total) * dollar_per_utm
pct_targeted_to_smallholder <- smallholder_awarded / total_awarded

# Amount awarded to indigenous applicants
indigenous_awarded <- sum(indigenous_df$rptpro_monto_total) * dollar_per_utm
pct_targeted_to_ingigenous <- indigenous_awarded / total_awarded

# Amount awarded to smallholder or indigenous applicants
marginalized_awarded <- sum(marginalized_df$rptpro_monto_total) * dollar_per_utm

# Percent of total awarded to smallholder or indigenous applicants
pct_targeted_to_marginalized <- marginalized_awarded / total_awarded


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Describing flows to different groups
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

admin_df <- within(
  admin_df %>% mutate(cpov_pct_2007 = as.numeric(as.character(cpov_pct_2007)))
  , cpov_quartile <- as.integer(cut(cpov_pct_2007, quantile(cpov_pct_2007, probs=0:4/4, na.rm = T), include.lowest=TRUE)))

cpov_quartile <- admin_df %>%
  #filter(`Contest type` != "Smallholder")%>%
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
                      + as.factor(rptpre_region) 
                   , data = admin_df)

score_pov2 <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                    + timber + log(rptpro_monto_total)
                   + as.factor(rptpre_region) 
                   , data = admin_df)

score_pov_smallholder <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                               + timber + log(rptpro_monto_total)
                               + as.factor(rptpre_region) 
                      , data = admin_df %>% filter(rptpro_tipo_concurso != "Otros Interesados"))

score_pov_other <- feols(log(cpov_pct_2007 + 1) ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                         + timber + log(rptpro_monto_total)        
                         + as.factor(rptpre_region) 
                               , data = admin_df %>% filter(rptpro_tipo_concurso == "Otros Interesados"))


models = list( "(1)" = score_pov,
               "(2)" = score_pov2,
               "(3)" = score_pov_smallholder,
               "(4)" = score_pov_other
)

rows <- tribble(~term, ~mod1,  ~mod2, ~mod3, ~mod4, 
                'Subsample', 'Full sample', 'Full sample', 'Smallholders', 'Other interested parties', 
                'Region FE', 'Yes', 'Yes', 'Yes', "Yes"
)%>%
  as.data.frame()

my_coef_names <- c("log(social_puntaje + 1)"= "ln(Social score)",
                   "log(adjusted_puntaje + 1)" = "ln(Project score)",
                   "timber" = "Timber production objective",
                   "log(rptpro_monto_total)" = "ln(Subsidy amount)"
)

options("modelsummary_format_numeric_latex" = "plain")

modelsummary(models,
             output="latex",
             title = 'Social score is associated with relatively higher comuna level poverty for smallholders.',
             fmt = 5, # 4 digits and trailing zero
             vcov = ~rptpre_id,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = my_coef_names,
             coef_omit = 'factor|Inter',
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|BIC|Year|FE|Std|RMSE|AIC'
             , add_rows = rows
             , notes = "Standard errors are clustered at the property level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>%
  add_header_above(c("Outcome var." = 1, "ln(ComunaPov)" =4 ) ) %>%
  kableExtra::save_kable(paste0(results_dir, "/poverty-score_assoc.tex"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Determinants of compliance
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compliance_mod1 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           log(cpov_pct_2007 + 1)
                         +  smallholder 
                       + as.factor(rptpre_region) 
                      , data = admin_df)
summary(compliance_mod1)

compliance_mod2 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                               log(cpov_pct_2007 + 1)
                         + log(social_puntaje + 1)*smallholder
                         + as.factor(rptpre_region) 
                         , data = admin_df)

compliance_mod3 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           log(cpov_pct_2007 + 1)
                         + log(social_puntaje + 1)*smallholder
                         + as.factor(rptpre_region) 
                         + extensionista
                         , data = admin_df)

compliance_mod4 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) +
                           log(cpov_pct_2007 + 1)
                         + log(social_puntaje + 1)*smallholder
                         + as.factor(rptpre_region) 
                         + extensionista
                         + log(rptpro_monto_total)
                         , data = admin_df)

models = list( "(1)" = compliance_mod1,
               "(2)" = compliance_mod2,
               "(3)" = compliance_mod3,
               "(4)" = compliance_mod4
)

rows <- tribble(~term, ~mod1,  ~mod2, ~mod3, ~mod4,
                'Subsample', 'Full sample', 'Full sample', 'Full sample','Full sample', 
                'Region FE' , 'Yes', 'Yes', 'Yes', 'Yes'
)%>%
  as.data.frame()

my_coef_names <- c("log(social_puntaje + 1)"= "ln(Social score)",
                   "log(social_puntaje + 1):smallholder" = "ln(Social score) x Smallholder",
                   "log(adjusted_puntaje + 1)" = "ln(Project score)",
                   "log(cpov_pct_2007 + 1)" = "ln(ComunaPov)",
                   "smallholder" = "Smallholder",
                   "timber" = "Timber production objective",
                   "extensionista" = "Extensionist",
                   "extensionista:smallholder" = "Extensionist x Smallholder",
                   "log(rptpro_monto_total)" = "ln(Subsidy amount)",
                   "log(cpov_pct_2007 + 1):smallholder" = "ln(ComunaPov) x Smallholder"
)


modelsummary(models,
             output="latex",
             title = '\\label{compliance-determinants}Social score is negatively associated with compliance for smallholders.',
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
  add_header_above(c("Outcome var." = 1, "Complied" = 4) ) %>%
  kableExtra::save_kable(paste0(results_dir, "/compliance_determinants.tex"))


# simpler table for slides

slides_mod1 <- feols(received_bonus ~ log(social_puntaje + 1) + log(adjusted_puntaje + 1) 
                     + as.factor(rptpre_region) 
                     , data = admin_df)

slides_mod2 <- feols(received_bonus ~ log(social_puntaje + 1)*smallholder + log(adjusted_puntaje + 1)
                      + as.factor(rptpre_region) 
                         , data = admin_df)

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
             title = 'Social score is negatively associated with compliance for smallholders',
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
  kableExtra::save_kable(paste0(results_dir, "/compliance_targeting_slides.tex"))

