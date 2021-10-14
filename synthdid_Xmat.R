library(synthdid)
library(tidyverse)
library(data.table)
library(abind)
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
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         small = ifelse(rptpro_tipo_concurso != "Otros Interesados", 1, 0),
         other = ifelse(rptpro_tipo_concurso == "Otros Interesados", 1, 0)
         ) %>%
  group_by(id) %>%
  mutate(indigenous = ifelse(max(ind_comunidad) == 1| max(indig_etnia) == 1, 1, 0),
         smallholder = ifelse(max(small) == 1, 1, 0),
         other_interested = ifelse(max(other) == 1, 1, 0)
         )



cohort_results <- data.frame()

cohorts <- seq(2009, 2019, by = 1)


for(i in cohorts){
  this_dta <- dta %>%
    filter(first.treat == i | treat == 0)%>%
    select(id, Year, Y, treated, first.treat, rptpro_puntaje, rptpre_monto_total, rptpre_superficie_predial, rptpre_superficie_bonificada, indig_etnia, ind_comunidad, property_area_ha, rptpro_tipo_concurso,
             forest, plantation, baresoil, pasture, industry_dist, native_industry_dist, shrub, slope, lat, elev, proportion_erosion) 
    
  my_setup = panel_matrices2(this_dta)
  
  Xs <- as.data.table(this_dta)[, .(id, Year, 
                                    # start covariates
                                    forest, 
                                    plantation,
                                    baresoil,
                                    pasture,
                                    industry_dist, 
                                    native_industry_dist,
                                    shrub,
                                    slope, 
                                    lat,
                                    elev,
                                    proportion_erosion
  )
  ] 
  
  X_mat <- 
    Xs %>% 
    melt(id.var = c("id", "Year")) %>% 
    #=== dataset by variable ===#
    nest_by(variable) %>% 
    mutate(X = list(
      dcast(data.table(data), id ~ Year, value.var = "value") %>% 
        #=== order the observations to match that of sdid_setup$Y ===#
        .[data.table(id = as.numeric(rownames(my_setup$Y))), on = "id"] %>% 
        .[, id := NULL] %>% 
        as.matrix() 
    )) %>% 
    .$X %>% 
    #=== list of matrices to 3-D array of N * T * C ===#
    # C: number of covariates
    abind(along = 3) 
  
  
  #tau_hat = synthdid_estimate(my_setup$Y, my_setup$N0, my_setup$T0)
  
  tau_hat <- synthdid_estimate(Y = my_setup$Y, N0 = my_setup$N0, T0 = my_setup$T0,
                               X = X_mat
  )
  
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
  
  this_cohort <- data.frame("cohort" = rep(i, length(obs.trajectory)), 
                            "obs.trajectory" = obs.trajectory, 
                            "syn.trajectory" = syn.trajectory,
                            "te_gt" = obs.trajectory - syn.trajectory,
                            "year" = seq(min(this_dta$Year), max(this_dta$Year), by = 1)
                            )
  
  
  cohort_results <- rbind(cohort_results, this_cohort)
  
  print(i)
}
library(rio)
library(ggplot2)

export(cohort_results, "cohort_results_withX.rds")


cohort_results_event <- cohort_results %>%
  mutate( e = year - cohort) %>%
  group_by(e) %>%
  summarise(event_te = mean(te_gt))



ggplot(data = cohort_results_event, aes( x = e, y = event_te)) +
  geom_point() +
  geom_line()


################################################################################################

other_cohort <- data.frame()

for(i in cohorts){
  this_dta <- dta %>%
    filter((first.treat == i & other_interested == 1 )| treat == 0)%>%
    select(id, Year, Y, treated, first.treat, treat, rptpro_puntaje, rptpre_monto_total, rptpre_superficie_predial, rptpre_superficie_bonificada, indig_etnia, ind_comunidad, property_area_ha, rptpro_tipo_concurso,
           forest, plantation, baresoil, pasture, industry_dist, native_industry_dist, shrub, slope, lat, elev, proportion_erosion) 
  
  my_setup = panel_matrices2(this_dta)
  
  Xs <- as.data.table(this_dta)[, .(id, Year, 
                                    # start covariates
                                    forest, 
                                    plantation,
                                    baresoil,
                                    pasture,
                                    industry_dist, 
                                    native_industry_dist,
                                    shrub,
                                    slope, 
                                    lat,
                                    elev,
                                    proportion_erosion
  )
  ] 
  
  X_mat <- 
    Xs %>% 
    melt(id.var = c("id", "Year")) %>% 
    #=== dataset by variable ===#
    nest_by(variable) %>% 
    mutate(X = list(
      dcast(data.table(data), id ~ Year, value.var = "value") %>% 
        #=== order the observations to match that of sdid_setup$Y ===#
        .[data.table(id = as.numeric(rownames(my_setup$Y))), on = "id"] %>% 
        .[, id := NULL] %>% 
        as.matrix() 
    )) %>% 
    .$X %>% 
    #=== list of matrices to 3-D array of N * T * C ===#
    # C: number of covariates
    abind(along = 3) 
  
  
  #tau_hat = synthdid_estimate(my_setup$Y, my_setup$N0, my_setup$T0)
  
  tau_hat <- synthdid_estimate(Y = my_setup$Y, N0 = my_setup$N0, T0 = my_setup$T0,
                               X = X_mat
  )
  
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
  
  this_cohort <- data.frame("cohort" = rep(i, length(obs.trajectory)), 
                            "obs.trajectory" = obs.trajectory, 
                            "syn.trajectory" = syn.trajectory,
                            "te_gt" = obs.trajectory - syn.trajectory,
                            "year" = seq(min(this_dta$Year), max(this_dta$Year), by = 1)
  )
  
  
  other_cohort <- rbind(other_cohort, this_cohort)
  
  print(i)
}
library(rio)
library(ggplot2)

export(other_cohort, "cohort_other_withX.rds")



cohort_results_event <- other_cohort %>%
  mutate( e = year - cohort) %>%
  group_by(e) %>%
  summarise(event_te = mean(te_gt))



ggplot(data = cohort_results_event, aes( x = e, y = event_te)) +
  geom_point() +
  geom_line()






treated_ids = unique(subset(dta, treat == 1)$id)


results_df <- data.frame()

for(i in treated_ids){
  this_dta <- dta %>%
    filter(id == i | treat == 0)%>%
    select(id, Year, Y, treated, first.treat, rptpro_puntaje, rptpre_monto_total, rptpre_superficie_predial, rptpre_superficie_bonificada, indig_etnia, ind_comunidad, property_area_ha, rptpro_tipo_concurso, anillado:zanja,
           forest, plantation, baresoil, pasture, industry_dist, native_industry_dist, shrub, slope, lat, elev, proportion_erosion) 
  
  this_id <- this_dta %>%
    filter(id == i) 
  
  my_setup = panel_matrices2(this_dta)
  
  
  
  Xs <- as.data.table(this_dta)[, .(id, Year, 
                                    # start covariates
                                    forest, 
                                    plantation,
                                    baresoil,
                                    pasture,
                                    industry_dist, 
                                    native_industry_dist,
                                    shrub,
                                    slope, 
                                    lat,
                                    elev,
                                    proportion_erosion
                                    )
                                ] 
  
  X_mat <- 
    Xs %>% 
    melt(id.var = c("id", "Year")) %>% 
    #=== dataset by variable ===#
    nest_by(variable) %>% 
    mutate(X = list(
      dcast(data.table(data), id ~ Year, value.var = "value") %>% 
        #=== order the observations to match that of sdid_setup$Y ===#
        .[data.table(id = as.numeric(rownames(my_setup$Y))), on = "id"] %>% 
        .[, id := NULL] %>% 
        as.matrix() 
    )) %>% 
    .$X %>% 
    #=== list of matrices to 3-D array of N * T * C ===#
    # C: number of covariates
    abind(along = 3) 

  
  #tau_hat = synthdid_estimate(my_setup$Y, my_setup$N0, my_setup$T0)
  
  tau_hat <- synthdid_estimate(Y = my_setup$Y, N0 = my_setup$N0, T0 = my_setup$T0,
      X = X_mat
    )
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
  
  # plot(tau_hat, overlay=1, 
  #      effect.alpha = 0.5, onset.alpha = 1,  point.size = 0.5, trajectory.alpha=1)
  
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


export(results_df, "results_df_withX.rds")