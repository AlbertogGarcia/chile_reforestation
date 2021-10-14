library(synthdid)
library(tidyverse)
library(stringi)
source('panel_matrices2.R')
# nativeforestcontest <- readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/submittedmp_analysis/NFL_df.rds")
# mgmtplan_analysis <- readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/submittedmp_analysis/mgmtplan_analysis_updated.rds")

nativeforestcontest <- readRDS("C:/Users/agarcia/Dropbox/chile_reforestation/data/submittedmp_analysis/NFL_df.rds")
mgmtplan_analysis <- readRDS("C:/Users/agarcia/Dropbox/chile_reforestation/data/submittedmp_analysis/mgmtplan_analysis_updated.rds")

mgmtplan_analysis$geometry.x <- NULL
mgmtplan_analysis$geometry.y <- NULL
mgmtplan_analysis$geometry <- NULL
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}
dta <- mgmtplan_analysis %>%
  mutate(area_diff = abs(property_area_ha - rptpre_superficie_predial),
         cut = ifelse( area_diff > quantile(subset(mgmtplan_analysis, treat==1)$property_area_ha, .9), 1, 0),
         G = first.treat,
         period = Year,
         Y = EVI2,
         id = as.numeric(id),
         treated = ifelse(Year >= first.treat & first.treat != 0, 1, 0),
         intensity = treated * (rptpre_superficie_bonificada/property_area_ha),
         indig_etnia = ifelse(is.na(rptprop_etnia) & treat==1, 0, ifelse(treat==0, NA, 1)),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1)


treated_ids = unique(subset(dta, treat == 1)$id)

# random_id <- sample(treated_ids, 1)
# # 856, 820, 345
# random_dta <- dta %>%
#   filter(id == random_id | treat == 0)%>%
#   select(id, Year, Y, treated, first.treat, rptpro_puntaje, rptpre_monto_total, rptpre_superficie_predial, rptpre_superficie_bonificada, indig_etnia, ind_comunidad, property_area_ha, rptpro_tipo_concurso) 
# 
# this_random_id <- random_dta %>%
#   filter(id == random_id) 
# 
# my_random_setup = panel_matrices2(random_dta)
# tau_hat = synthdid_estimate(my_random_setup$Y, my_random_setup$N0, my_random_setup$T0)
# 
# plot(tau_hat, overlay=1, 
#      effect.alpha = 0.5, onset.alpha = 1,  point.size = 0.5, trajectory.alpha=1)
# 
# my_guy <- subset(dta, id == random_id)
# 
# 
# 
# 
# 
# plot(tau_hat, overlay=0.0, diagram.alpha = 0.5, 
#      effect.alpha = 0.5, onset.alpha = 1, point.size = 0.5, trajectory.alpha=1, lambda.plot.scale = 0)

# reforest_ids = unique(subset(dta, reforestation2 == 1 & treat == 1)$id)
# 
# treated_ids <- reforest_ids

results_df <- data.frame()

for(i in treated_ids){
  this_dta <- dta %>%
    filter(id == i | treat == 0)%>%
    select(id, Year, Y, treated, first.treat, rptpro_puntaje, rptpro_objetivo_manejo, rptpre_monto_total, rptpre_superficie_predial, rptpre_superficie_bonificada, indig_etnia, ind_comunidad, property_area_ha, rptpro_tipo_concurso, anillado:zanja) 
  
  my_setup = panel_matrices2(this_dta)
  tau_hat = synthdid_estimate(my_setup$Y, my_setup$N0, my_setup$T0)
  #tau.sc   = sc_estimate(my_setup$Y, my_setup$N0, my_setup$T0)
  #se = sqrt(vcov(tau_hat, method='placebo'))
  
  weights = attr(tau_hat, 'weights')
  Y = attr(tau_hat, 'setup')$Y
  T0 = attr(tau_hat, 'setup')$T0
  T1 = ncol(Y) - T0
  N0 = attr(tau_hat, 'setup')$N0
  N1 = nrow(Y) - N0
  lambda.synth = c(weights$lambda, rep(0, T1))
  lambda.target = c(rep(0, T0), rep(1 / T1, T1))
  omega.synth = c(weights$omega, rep(0, N1))
  omega.target = c(rep(0, N0), rep(1 / N1, N1))
  over=1
  intercept.offset = over * c((omega.target - omega.synth) %*% Y %*% lambda.synth)
  obs.trajectory = as.numeric(omega.target %*% Y)
  syn.trajectory = as.numeric(omega.synth %*% Y) + intercept.offset
  
  this_id <- this_dta %>%
    filter(id == i & treated == 1) %>%
    drop_na(rptpro_tipo_concurso, rptpro_puntaje, rptpre_monto_total) %>%
    mutate(indigenous = ifelse(max(ind_comunidad) ==1 | max(indig_etnia) ==1, 1, indig_etnia),
           tau.ovr = tau_hat[1],
           tau.2020 = tail(obs.trajectory, n=1) - tail(syn.trajectory, n=1),
           tau.2019 = tail(obs.trajectory, n=2)[1] - tail(syn.trajectory, n=2)[1],
           tau.1920 = (tau.2020 + tau.2019)/2,
           smallholder = ifelse(rptpro_tipo_concurso == "Pequeños Propietarios", 1, 0)
    ) %>%
    distinct(id, .keep_all = TRUE)
  
  results_df <- rbind(results_df, this_id)
  
  print(i)
}
library(rio)
library(ggplot2)

summary(lm(tau.2020 ~ as.factor(first.treat) + `plantacion-suplementaria`  +plantacion +regeneracion + rptpre_monto_total + rptpre_superficie_bonificada + rptpre_superficie_predial + rptpro_puntaje ,data = results_df))



export(results_df, "results_df.rds")

results_mat <- readRDS("results_mat.rds")
results <- results_df %>%
  drop_na(te.2020) %>%
  mutate_at(vars(ovr.te, te.2020, te.2019, property_area_ha, rptpre_superficie_bonificada, payment_monto, score), as.numeric) %>%
  mutate(per_ha.1920 = (te.1920/property_area_ha),
         per_intensity = per_ha.1920*rptpre_superficie_bonificada
)

te.smallholders <- subset(results_df, smallholder == 1)$te.1920
te.other <- subset(results_df, smallholder == 0)$te.1920
  
te_plot <- ggplot()+
  geom_density(data = results_df, aes(te.1920, colour = rptpro_tipo_concurso))+
  geom_vline(xintercept = mean(te.smallholders, na.rm = TRUE), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = mean(te.other, na.rm = TRUE), color = "red", linetype = "dashed")+
  xlim(-.15, .15)

### first, we'll see whether these distributions are different to one another
ks.test(te.smallholders, te.other)
# D = 0.082081, p-value = 0.0003341

# We have sufficient evidence to say that the two sample datasets do not come from the same distribution
t.test(te.1920 ~ smallholder, results_df)
# t = -4.2504, df = 1699.7, p-value = 2.25e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.009418969 -0.003470888
# sample estimates:
#   mean in group 0 mean in group 1 
# 0.01155010      0.01799503

te.smallholders_meanadjust <- te.smallholders - (mean(te.smallholders) - mean(te.other))
ks.test(te.smallholders_meanadjust, te.other)
# D = 0.025753, p-value = 0.7854



per_intensity.smallholders <- subset(results_df, smallholder == 1)$per_intensity
per_intensity.smallholders[is.infinite(per_intensity.smallholders)] <- NA
per_intensity.other <- subset(results_df, smallholder == 0)$per_intensity

ha_te_plot <- ggplot()+
  geom_density(data = results_df, aes(per_intensity, colour = rptpro_tipo_concurso))+
  geom_vline(xintercept = mean(per_intensity.smallholders, na.rm = TRUE), color = "blue", linetype = "dashed")+
  geom_vline(xintercept = mean(per_intensity.other, na.rm = TRUE), color = "red", linetype = "dashed")+
  geom_vline(xintercept = median(per_intensity.smallholders, na.rm = TRUE), color = "lightblue", linetype = "dashed")+
  geom_vline(xintercept = median(per_intensity.other, na.rm = TRUE), color = "pink", linetype = "dashed")+
  xlim(-.01, .01)

results_df <- results_df %>%
  mutate(price_per_treated_ha = payment_monto/rptpre_superficie_bonificada,
         adj_price = ifelse(smallholder == 1,  .85*payment_monto, payment_monto),
         adj_ppth = adj_price/rptpre_superficie_bonificada,
         score_per_treated_ha = score/rptpre_superficie_bonificada)%>%
  filter(is.finite(per_intensity))

summary(lm(per_intensity ~ as.factor(first.treat) + price_per_treated_ha + score, data = results_df))



