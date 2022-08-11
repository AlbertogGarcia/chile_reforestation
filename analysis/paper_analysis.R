
library(tidyverse)
library(sf)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

matched_wide_main <- readRDS("analysis/matched_wide_2to1.rds")

# unmatched_long <- pivot_longer(as_tibble(readRDS("analysis/pool_wide_rol.rds")), evi_2005:evi_2020, names_to = "Year", values_to =  "EVI2")%>%
#   mutate(Year = substrRight(Year, 4),
#          Year= as.numeric(Year))


library(did)
set.seed(0930)

# covars = c("road_dist", "proportion_erosion", "industry_dist", "native_industry_dist", "water", "urban", "forest", "plantation", "baresoil", "pasture", "shrub", "slope", "lat", "elev", "area")
# unmatched_panel <- unmatched_long %>% select(covars, EVI2, Year, id, first.treat)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  1 to 1 did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_long <- gather(readRDS("analysis/matched_wide_1to1.rds"), "Year", "EVI2", evi_2005:evi_2020)%>%
  #separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year = substrRight(Year, 4),
         Year= as.numeric(Year))

did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
              data=matched_long, 
              clustervars = "id",
              panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "matched 1 to 1", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 1, "2 to 1" = 0, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  3 to 1 did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_long <- gather(readRDS("analysis/matched_wide_3to1.rds"), "Year", "EVI2", evi_2005:evi_2020)%>%
  #separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year = substrRight(Year, 4),
         Year= as.numeric(Year))

did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
              data=matched_long, 
              clustervars = "id",
              panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "matched 3 to 1", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 0, "3 to 1" = 1, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  mahalanobis matching did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_long <- gather(readRDS("analysis/matched_wide_2to1mahalanobis.rds"), "Year", "EVI2", evi_2005:evi_2020)%>%
  #separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year = substrRight(Year, 4),
         Year= as.numeric(Year))

did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
              data=matched_long, 
              clustervars = "id",
              panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "mahalanobis", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 0, "mahalanobis" = 1,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  main did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

matched_long <- gather(matched_wide_main, "Year", "EVI2", evi_2005:evi_2020)%>%
  #separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year = substrRight(Year, 4),
         Year= as.numeric(Year))

data <- matched_long %>%
  mutate(G = first.treat,
         period = Year,
         Y = EVI2,
         id = as.numeric(id),
         otros = ifelse(rptpro_tipo_concurso == "Otros Interesados", 1, 0),
         small = ifelse(rptpro_tipo_concurso != "Otros Interesados", 1, 0),
         treated = ifelse(period >= first.treat & first.treat > 0, 1, 0))%>%
  group_by(id)%>%
  mutate(smallholder = ifelse(max(small) == 1,1, small),
         other_interested = ifelse(max(otros) == 1,1, otros)
  )


did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
              data=data, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "main", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt
ggsave(path = "paper/figs", filename = "did_main.png", width = 8, height = 5)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  area weighted did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + elev
              , 
              data=data, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              weightsname = "area",
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "area weighted", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 1)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  not yet treated control group
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

did_nyt <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
              control_group = "notyettreated",
              data=data, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              print_details=FALSE
)



did.es <- aggte(did_nyt, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did_nyt, type = "simple")

did_results <- data.frame("name" = "notyettreated", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did_nyt$n)%>%
  bind_rows(did_results)


dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_nyt <- ggplot(data=dyn.es , aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event study treatment effects including not yet treated as control") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_nyt
ggsave(path = "paper/figs", filename = "did_notyettreated.png", width = 8, height = 5)

spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                           "all" = 1, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 1,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cohorts <- seq(from = 2009, to = 2019, by =1)
cohort_spec_results <- data.frame()
for(i in cohorts){
  print(i)
  
  this_data <- subset(data, first.treat == i | treat == 0)
  
  did <- att_gt(yname="EVI2",
                tname="Year",
                idname="id",
                gname="first.treat",
                est_method = "dr",
                xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                , 
                data=this_data, clustervars = "id"
                , panel=TRUE, bstrap = TRUE,
                print_details=FALSE
  )
  
  did.es <- aggte(did, type="dynamic", min_e = -6, max_e = 10)
  did.ovr <- aggte(did, type = "simple")
  
  did_results <- data.frame("name" = paste(i, "cohort"), "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
    bind_rows(did_results)
  
  dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
    mutate(period = ifelse(e >= 0 , "post", "pre"))
  
  plot_mgmt <- ggplot(data=dyn.es, aes(x=e, y=att, color = period)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
    geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    ggtitle(paste0(i , " cohort event study treatment effects")) +
    xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
    theme_minimal()
  plot_mgmt
  
  ggsave(path = "paper/figs", filename = paste0("cohort", i, ".png"), width = 8, height = 5)
  
  cohort_spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
                             "cohort" = i)%>%
    bind_rows(spec_results)
  
  
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

`%notin%` <- Negate(`%in%`)

pretrend_data <- data %>%
  filter(first.treat %notin% c(2011, 2014, 2017, 2018, 2019))
#these cohorts violate pretrends 
# drop these cohorts

did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "dr",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
              , 
           data=pretrend_data, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              print_details=TRUE
)



did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type="simple")

did_results <- data.frame("name" = "only passing pretrend cohorts", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

dyn.es <- data.frame("att" = did.es$att.egt, "se" = did.es$se.egt, "e" = did.es$egt, "crit.val" = did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=dyn.es, aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  xlab("Years since property enrollment") + ylab("EVI")+ ylim(-.01, 0.025)+
  theme_minimal()
plot_mgmt
ggsave(path = "paper/figs", filename = "good_cohorts_es.png", width = 8, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(stringi)
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

data <- data %>%
  mutate(female = ifelse(rptprop_sexo == "Femenino", 1, ifelse(rptprop_sexo == "Masculino", 0, NA)),
         indig_etnia = ifelse(is.na(rptprop_etnia) & treat==1, 0, ifelse(treat==0, NA, 1)),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1)%>%
  dplyr::group_by(id)%>%
  dplyr::mutate(indigenous = ifelse(max(ind_comunidad) ==1 | max(indig_etnia) == 1, 1, indig_etnia)
  )%>%
  dplyr::ungroup()

small_dta <- subset(data, smallholder==1 | treat == 0)
other_dta <- subset(data, other_interested==1 | treat == 0)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

small_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "dr",
                    xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                    , 
                    data=small_dta, clustervars = "id"
                    , panel=TRUE, bstrap = TRUE,
                    print_details=TRUE
)

small_did.es <- aggte(small_did, type="dynamic", min_e = -10, max_e = 10)
small_did.ovr <- aggte(small_did, type = "simple")

did_results <- data.frame("name" = "smallholder", "ATT_ovr" = small_did.ovr$overall.att, "ovr_se" = small_did.ovr$overall.se, "ATT_dyn" = small_did.es$overall.att, "dyn_se" = small_did.es$overall.se, "N" = small_did$n)%>%
  bind_rows(did_results)

small_dyn.es <- data.frame("att" = small_did.es$att.egt, "se" = small_did.es$se.egt, "e" = small_did.es$egt, "crit.val" = small_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=small_dyn.es, aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Smallholder event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt

ggsave(path = "paper/figs", filename = "did_smallholder.png", width = 8, height = 5)


spec_results <- data.frame("ATT_ovr" = small_did.ovr$overall.att, "ovr_se" = small_did.ovr$overall.se, 
                           "all" = 0, "smallholders" = 1, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

other_did <- att_gt(yname="EVI2",
                    tname="Year",
                    idname="id",
                    gname="first.treat",
                    est_method = "dr",
                    xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                    , 
                    data=other_dta, clustervars = "id"
                    , panel=TRUE, bstrap = TRUE,
                    print_details=TRUE
)

other_did.es <- aggte(other_did, type="dynamic", min_e = -10, max_e = 10)
other_did.ovr <- aggte(other_did, type = "simple")

did_results <- data.frame("name" = "other interested", "ATT_ovr" = other_did.ovr$overall.att, "ovr_se" = other_did.ovr$overall.se, "ATT_dyn" = other_did.es$overall.att, "dyn_se" = small_did.es$overall.se, "N" = other_did$n)%>%
  bind_rows(did_results)

other_dyn.es <- data.frame("att" = other_did.es$att.egt, "se" = other_did.es$se.egt, "e" = other_did.es$egt, "crit.val" = other_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

plot_mgmt <- ggplot(data=other_dyn.es, aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Other interested party event study treatment effects") +
  xlab("Years since property enrollment") + ylab("EVI")+# ylim(-.005, 0.022)
  theme_minimal()
plot_mgmt


ggsave(path = "paper/figs", filename = "did_other.png", width = 8, height = 5)

spec_results <- data.frame("ATT_ovr" = other_did.ovr$overall.att, "ovr_se" = other_did.ovr$overall.se,
                           "all" = 0, "smallholders" = 0, "other interested" = 1, "timber production" = 0, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  smallholder area weighted did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + elev
              , 
              data=small_dta, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              weightsname = "area",
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "smallholder area weighted", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

# spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
#                            "all" = 0, "smallholders" = 1, "other interested" = 0, "timber production" = 0, "nontimber" = 0, 
#                            "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
#                            "notyettreated" = 0,
#                            "area weights" = 1)%>%
#   bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  other interested area weighted did estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
did <- att_gt(yname="EVI2",
              tname="Year",
              idname="id",
              gname="first.treat",
              est_method = "reg",
              xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + elev
              , 
              data=other_dta, clustervars = "id"
              , panel=TRUE, bstrap = TRUE,
              weightsname = "area",
              print_details=FALSE
)

did.es <- aggte(did, type="dynamic", min_e = -10, max_e = 10)
did.ovr <- aggte(did, type = "simple")

did_results <- data.frame("name" = "other interested area weighted", "ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, "ATT_dyn" = did.es$overall.att, "dyn_se" = did.ovr$overall.se, "N" = did$n)%>%
  bind_rows(did_results)

# spec_results <- data.frame("ATT_ovr" = did.ovr$overall.att, "ovr_se" = did.ovr$overall.se, 
#                            "all" = 0, "smallholders" = 0, "other interested" = 1, "timber production" = 0, "nontimber" = 0, 
#                            "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
#                            "notyettreated" = 0,
#                            "area weights" = 1)%>%
#   bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timber_dta <- subset(data, rptpro_objetivo_manejo == "PRODUCCION MADERERA" | treat == 0)
nottimber_dta <- subset(data, rptpro_objetivo_manejo != "PRODUCCION MADERERA" | treat == 0)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

timber_did <- att_gt(yname="EVI2",
                     tname="Year",
                     idname="id",
                     gname="first.treat",
                     est_method = "reg",
                     ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                     ,  
                     data=timber_dta, clustervars = "id"
                     , panel=TRUE, bstrap = TRUE
)
timber_did.es <- aggte(timber_did, type="dynamic", min_e = -10, max_e = 10)
timber_did.ovr <- aggte(timber_did, type="simple")

did_results <- data.frame("name" = "timber", "ATT_ovr" = timber_did.ovr$overall.att, "ovr_se" = timber_did.ovr$overall.se, "ATT_dyn" = timber_did.es$overall.att, "dyn_se" = timber_did.es$overall.se, "N" = timber_did$n)%>%
  bind_rows(did_results)

timber_dyn.es <- data.frame("att" = timber_did.es$att.egt, "se" = timber_did.es$se.egt, "e" = timber_did.es$egt, "crit.val" = timber_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

timber_plot <- ggplot(data= timber_dyn.es, aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event time treatment effects for timber production objective") +
  xlab("Years since property enrollment") + ylab("EVI")+ 
  theme_minimal()
timber_plot

ggsave(plot = timber_plot, width = 8, height = 5, path = "paper/figs", filename = "timber_es.png", dpi = 500)

spec_results <- data.frame("ATT_ovr" = timber_did.ovr$overall.att, "ovr_se" = timber_did.ovr$overall.se, 
                           "all" = 0, "smallholders" = 0, "other interested" = 0, "timber production" = 1, "nontimber" = 0, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###############  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

ntimber_did <- att_gt(yname="EVI2",
                      tname="Year",
                      idname="id",
                      gname="first.treat",
                      est_method = "reg",
                      xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                      , 
                      data=nottimber_dta, clustervars = "id"
                      , panel=TRUE, bstrap = TRUE
)
ntimber_did.es <- aggte(ntimber_did, type="dynamic", min_e = -10, max_e = 10)
ntimber_did.ovr <- aggte(ntimber_did, type="simple")

did_results <- data.frame("name" = "ntimber", "ATT_ovr" = ntimber_did.ovr$overall.att, "ovr_se" = ntimber_did.ovr$overall.se, "ATT_dyn" = ntimber_did.es$overall.att, "dyn_se" = ntimber_did.es$overall.se, "N" = ntimber_did$n)%>%
  bind_rows(did_results)

ntimber_dyn.es <- data.frame("att" = ntimber_did.es$att.egt, "se" = ntimber_did.es$se.egt, "e" = ntimber_did.es$egt, "crit.val" = ntimber_did.es$crit.val.egt)%>%
  mutate(period = ifelse(e >= 0 , "post", "pre"))

ntimber_plot <- ggplot(data= ntimber_dyn.es, aes(x=e, y=att, color = period)) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.01)+
  geom_ribbon(aes(ymin=(att-crit.val*se), ymax=(att+crit.val*se)), color =NA ,alpha = 0.1) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  ggtitle("Event time treatment effects without timber production objective") +
  xlab("Years since property enrollment") + ylab("EVI") + 
  theme_minimal()
ntimber_plot

ggsave(plot = ntimber_plot, width = 8, height = 5, path = "paper/figs", filename = "ntimber_es.png", dpi = 500)

spec_results <- data.frame("ATT_ovr" = ntimber_did.ovr$overall.att, "ovr_se" = ntimber_did.ovr$overall.se, 
                           "all" = 0, "smallholders" = 0, "other interested" = 0, "timber production" = 0, "nontimber" = 1, 
                           "1 to 1" = 0, "2 to 1" = 1, "3 to 1" = 0, "logit" = 1, "mahalanobis" = 0,
                           "notyettreated" = 0,
                           "area weights" = 0)%>%
  bind_rows(spec_results)

library(rio)
export(did_results, "paper/results/did_results.rds")
export(spec_results, "paper/results/spec_results.rds")


#################################################################################
######### adjusting smallholder scores
#################################################################################


sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_dta <- small_dta %>%
  group_by(id)%>%
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
         rptpro_puntaje = mean(rptpro_puntaje, na.rm = TRUE),
         adjusted_puntaje = rptpro_puntaje - .2*VI - .35*VP - 0.05*VPS,
         social_puntaje = .2*VI + .35*VP + 0.05*VPS) %>%
  ungroup()%>%
  group_by(first.treat) %>%
  mutate(q1 = quantile(rptpro_puntaje, probs = c(0.25), na.rm = TRUE),
         q2 = quantile(rptpro_puntaje, probs = c(0.5), na.rm = TRUE),
         q3 = quantile(rptpro_puntaje, probs = c(0.75), na.rm = TRUE),
         q4 = quantile(rptpro_puntaje, probs = c(1), na.rm = TRUE),
         adj_q1 = quantile(adjusted_puntaje, probs = c(0.25), na.rm = TRUE),
         adj_q2 = quantile(adjusted_puntaje, probs = c(0.5), na.rm = TRUE),
         adj_q3 = quantile(adjusted_puntaje, probs = c(0.75), na.rm = TRUE),
         adj_q4 = quantile(adjusted_puntaje, probs = c(1), na.rm = TRUE),
         soc_q1 = quantile(social_puntaje, probs = c(.5), na.rm = TRUE),
         soc_q2 = quantile(social_puntaje, probs = c(1), na.rm = TRUE)
  )%>%
  ungroup()

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_dta <- other_dta %>%
  group_by(id)%>%
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
         rptpro_puntaje = mean(rptpro_puntaje, na.rm = TRUE),
         adjusted_puntaje = rptpro_puntaje - .25*VI - .25*VP - 0.05*VPS,
         social_puntaje = .25*VI + .25*VP + 0.05*VPS
  ) %>%
  ungroup()%>%
  group_by(first.treat) %>%
  mutate(q1 = quantile(rptpro_puntaje, probs = c(0.25), na.rm = TRUE),
         q2 = quantile(rptpro_puntaje, probs = c(0.5), na.rm = TRUE),
         q3 = quantile(rptpro_puntaje, probs = c(0.75), na.rm = TRUE),
         q4 = quantile(rptpro_puntaje, probs = c(1), na.rm = TRUE),
         adj_q1 = quantile(adjusted_puntaje, probs = c(0.25), na.rm = TRUE),
         adj_q2 = quantile(adjusted_puntaje, probs = c(0.5), na.rm = TRUE),
         adj_q3 = quantile(adjusted_puntaje, probs = c(0.75), na.rm = TRUE),
         adj_q4 = quantile(adjusted_puntaje, probs = c(1), na.rm = TRUE),
         soc_q1 = quantile(social_puntaje, probs = c(1/3), na.rm = TRUE),
         soc_q2 = quantile(social_puntaje, probs = c(2/3), na.rm = TRUE),
         soc_q3 = quantile(social_puntaje, probs = c(1), na.rm = TRUE)
  )%>%
  ungroup()

#################################################################################
######### quartile score dataframes
#################################################################################

other_q1 <- other_dta %>%
  filter(rptpro_puntaje <= q1 | treat == 0)

other_q2 <- other_dta %>%
  filter((rptpro_puntaje <= q2 & rptpro_puntaje > q1) | treat == 0)

other_q3 <- other_dta %>%
  filter((rptpro_puntaje <= q3 & rptpro_puntaje > q2) | treat == 0)

other_q4 <- other_dta %>%
  filter((rptpro_puntaje <= q4 & rptpro_puntaje > q3) | treat == 0)


small_q1 <- small_dta %>%
  filter(rptpro_puntaje <= q1 | treat == 0)

small_q2 <- small_dta %>%
  filter((rptpro_puntaje <= q2 & rptpro_puntaje > q1) | treat == 0)

small_q3 <- small_dta %>%
  filter((rptpro_puntaje <= q3 & rptpro_puntaje > q2) | treat == 0)

small_q4 <- small_dta %>%
  filter((rptpro_puntaje <= q4 & rptpro_puntaje > q3) | treat == 0)


############################################################################################################
### quartile real score dids
##########################################################################################################
small_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q1.es <- aggte(small_did_q1, type="dynamic")
smallholder_q1.ovr <- aggte(small_did_q1, type="simple")


small_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q2.es <- aggte(small_did_q2, type="dynamic")
smallholder_q2.ovr <- aggte(small_did_q2, type="simple")


small_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q3.es <- aggte(small_did_q3, type="dynamic")
smallholder_q3.ovr <- aggte(small_did_q3, type="simple")


small_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q4.es <- aggte(small_did_q4, type="dynamic")
smallholder_q4.ovr <- aggte(small_did_q4, type="simple")

####################################################################################################




other_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q1.es <- aggte(other_did_q1, type="dynamic")
other_q1.ovr <- aggte(other_did_q1, type="simple")


other_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)
other_q2.es <- aggte(other_did_q2, type="dynamic")
other_q2.ovr <- aggte(other_did_q2, type="simple")


other_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q3.es <- aggte(other_did_q3, type="dynamic")
other_q3.ovr <- aggte(other_did_q3, type="simple")


other_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q4.es <- aggte(other_did_q4, type="dynamic")
other_q4.ovr <- aggte(other_did_q4, type="simple")

assigned_results <- data.frame(
  "agg_type" = c(rep("es", 8),rep("ovr", 8)),
  "contest" = c(rep("other interested", 4), rep("smallholders", 4), rep("other interested", 4), rep("smallholders", 4)),
  "quartile" = rep(seq(1, 4, by = 1), 4), 
  "scoring method" = "assigned", 
  "ATT" = c(other_q1.es$overall.att, other_q2.es$overall.att, other_q3.es$overall.att, other_q4.es$overall.att,
            smallholder_q1.es$overall.att, smallholder_q2.es$overall.att, smallholder_q3.es$overall.att, smallholder_q4.es$overall.att,
            other_q1.ovr$overall.att, other_q2.ovr$overall.att, other_q3.ovr$overall.att, other_q4.ovr$overall.att,
            smallholder_q1.ovr$overall.att, smallholder_q2.ovr$overall.att, smallholder_q3.ovr$overall.att, smallholder_q4.ovr$overall.att
  ),
  "se" = c(other_q1.es$overall.se, other_q2.es$overall.se, other_q3.es$overall.se, other_q4.es$overall.se,
           smallholder_q1.es$overall.se, smallholder_q2.es$overall.se, smallholder_q3.es$overall.se, smallholder_q4.es$overall.se,
           other_q1.ovr$overall.se, other_q2.ovr$overall.se, other_q3.ovr$overall.se, other_q4.ovr$overall.se,
           smallholder_q1.ovr$overall.se, smallholder_q2.ovr$overall.se, smallholder_q3.ovr$overall.se, smallholder_q4.ovr$overall.se
  )
)
export(assigned_results, "assigned_results.rds")



#################################################################################
######### ADJUSTED SCORES
#################################################################################

#################################################################################
######### adjusted quartile score dataframes
#################################################################################

other_q1 <- other_dta %>%
  filter(adjusted_puntaje <= adj_q1 | treat == 0)

other_q2 <- other_dta %>%
  filter((adjusted_puntaje <= adj_q2 & adjusted_puntaje > adj_q1) | treat == 0)

other_q3 <- other_dta %>%
  filter((adjusted_puntaje <= adj_q3 & adjusted_puntaje > adj_q2) | treat == 0)

other_q4 <- other_dta %>%
  filter((adjusted_puntaje <= adj_q4 & adjusted_puntaje > adj_q3) | treat == 0)


small_q1 <- small_dta %>%
  filter(adjusted_puntaje <= adj_q1 | treat == 0)

small_q2 <- small_dta %>%
  filter((adjusted_puntaje <= adj_q2 & adjusted_puntaje > adj_q1) | treat == 0)

small_q3 <- small_dta %>%
  filter((adjusted_puntaje <= adj_q3 & adjusted_puntaje > adj_q2) | treat == 0)

small_q4 <- small_dta %>%
  filter((adjusted_puntaje <= adj_q4 & adjusted_puntaje > adj_q3) | treat == 0)


############################################################################################################
### quartile adjusted score dids
##########################################################################################################
small_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q1.es <- aggte(small_did_q1, type="dynamic")
smallholder_q1.ovr <- aggte(small_did_q1, type="simple")


small_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q2.es <- aggte(small_did_q2, type="dynamic")
smallholder_q2.ovr <- aggte(small_did_q2, type="simple")


small_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q3.es <- aggte(small_did_q3, type="dynamic")
smallholder_q3.ovr <- aggte(small_did_q3, type="simple")


small_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=small_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

smallholder_q4.es <- aggte(small_did_q4, type="dynamic")
smallholder_q4.ovr <- aggte(small_did_q4, type="simple")

####################################################################################################




other_did_q1 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q1, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q1.es <- aggte(other_did_q1, type="dynamic")
other_q1.ovr <- aggte(other_did_q1, type="simple")


other_did_q2 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q2, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)
other_q2.es <- aggte(other_did_q2, type="dynamic")
other_q2.ovr <- aggte(other_did_q2, type="simple")


other_did_q3 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q3, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q3.es <- aggte(other_did_q3, type="dynamic")
other_q3.ovr <- aggte(other_did_q3, type="simple")


other_did_q4 <- att_gt(yname="EVI2",
                       tname="Year",
                       idname="id",
                       gname="first.treat",
                       est_method = "reg",
                       xformla= ~ road_dist + proportion_erosion + industry_dist + native_industry_dist + water + urban + forest + plantation + baresoil + pasture + shrub + slope + lat + elev + area
                       , 
                       data=other_q4, clustervars = "id"
                       , panel=TRUE, bstrap = TRUE
)

other_q4.es <- aggte(other_did_q4, type="dynamic")
other_q4.ovr <- aggte(other_did_q4, type="simple")

adjusted_results <- data.frame(
  "agg_type" = c(rep("es", 8),rep("ovr", 8)),
  "contest" = c(rep("other interested", 4), rep("smallholders", 4), rep("other interested", 4), rep("smallholders", 4)),
  "quartile" = rep(seq(1, 4, by = 1), 4), 
  "scoring method" = "adjusted", 
  "ATT" = c(other_q1.es$overall.att, other_q2.es$overall.att, other_q3.es$overall.att, other_q4.es$overall.att,
            smallholder_q1.es$overall.att, smallholder_q2.es$overall.att, smallholder_q3.es$overall.att, smallholder_q4.es$overall.att,
            other_q1.ovr$overall.att, other_q2.ovr$overall.att, other_q3.ovr$overall.att, other_q4.ovr$overall.att,
            smallholder_q1.ovr$overall.att, smallholder_q2.ovr$overall.att, smallholder_q3.ovr$overall.att, smallholder_q4.ovr$overall.att
  ),
  "se" = c(other_q1.es$overall.se, other_q2.es$overall.se, other_q3.es$overall.se, other_q4.es$overall.se,
           smallholder_q1.es$overall.se, smallholder_q2.es$overall.se, smallholder_q3.es$overall.se, smallholder_q4.es$overall.se,
           other_q1.ovr$overall.se, other_q2.ovr$overall.se, other_q3.ovr$overall.se, other_q4.ovr$overall.se,
           smallholder_q1.ovr$overall.se, smallholder_q2.ovr$overall.se, smallholder_q3.ovr$overall.se, smallholder_q4.ovr$overall.se
  )
)

export(adjusted_results, "adjusted_results.rds")

###########################################################
##### generate plots
###########################################################
adjusted_results <- readRDS("adjusted_results.rds") %>% filter(agg_type == "ovr")
assigned_results <- readRDS("assigned_results.rds")%>% filter(agg_type == "ovr")

smallholder_qplot_df <- assigned_results %>%
  filter(contest == "smallholders")

other_color = "#D55E00"
small_color = "#009E73"

ggplot(data = smallholder_qplot_df, aes(x = quartile, y = ATT)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha = 0.15, color = NA, fill = small_color) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("Smallholder ATT by assigned score quartile")+
  theme_minimal()

ggsave(width = 7, height = 5, path = "paper/figs", filename = "small_q_assigned.png", dpi = 500)


other_qplot_df <- assigned_results %>%
  filter(contest != "smallholders")

ggplot(data = other_qplot_df, aes(x = quartile, y = ATT)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha = 0.15, color = NA, fill = other_color) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("Other interested ATT by assigned score quartile")+
  theme_minimal()

ggsave(width = 7, height = 5, path = "paper/figs", filename = "other_q_assigned.png", dpi = 500)

###########################

smallholder_qplot_df <- adjusted_results %>%
  filter(contest == "smallholders")

ggplot(data = smallholder_qplot_df, aes(x = quartile, y = ATT)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha = 0.15, color = NA, fill = small_color) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("smallholder ATT by adjusted score quartile")+
  theme_minimal()


ggsave(width = 7, height = 5, path = "paper/figs", filename = "small_q_adjusted.png", dpi = 500)


other_qplot_df <- adjusted_results %>%
  filter(contest != "smallholders")

ggplot(data = other_qplot_df, aes(x = quartile, y = ATT)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha = 0.15, color = NA, fill = other_color) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("other interested ATT by adjusted score quartile")+
  theme_minimal()


ggsave(width = 7, height = 5, path = "paper/figs", filename = "other_q_adjusted.png", dpi = 500)


other_qplot_df <- adjusted_results %>%
  rbind(assigned_results)%>%
  filter(contest != "smallholders")

small_qplot_df <- adjusted_results %>%
  rbind(assigned_results)%>%
  filter(contest == "smallholders")

ggplot(data = other_qplot_df, aes(x = quartile, y = ATT, fill = scoring.method, color = scoring.method)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha  = 0.25, color = NA)+
  geom_point(position=position_dodge(width = 0.2),shape = 21, size = 3) +
  geom_line(position=position_dodge(width = 0.2))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("Other interested ATT by score quartile")+
  theme_minimal()

ggsave(width = 7, height = 5, path = "paper/figs", filename = "other_q.png", dpi = 500)

ggplot(data = small_qplot_df, aes(x = quartile, y = ATT, fill = scoring.method, color = scoring.method)
) + 
  geom_ribbon(aes(ymin=ATT-1.96*se, ymax=ATT+1.96*se), alpha  = 0.25, color = NA)+
  geom_point(position=position_dodge(width = 0.2),shape = 21, size = 3) +
  geom_line(position=position_dodge(width = 0.2))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Quartile")+
  ylab("ATT")+ggtitle("Smallholder ATT by score quartile")+
  theme_minimal()

ggsave(width = 7, height = 5, path = "paper/figs", filename = "small_q.png", dpi = 500)


# source(here::here("compute_attgt_interaction.R"))
# 
# # the number of bootstrap iterations
# biters <- 200
# varname = "plantation"
# # list to store bootstrap results
# boot.int.dyn <- list()
# boot.int.ovr<- list()
# boot.dyn <- list()
# boot.ovr<- list()
# 
# # loop for each nonparametric bootstrap iteration
# for (b in 1:biters) {
#   # draw a bootstrap sample; here, we'll call an outside function
#   bdata <- BMisc::blockBootSample(dta, "id")
#   # call our function for estimating dynamic effects on the
#   # bootstrapped data
#   
#   ### forest estimates - interaction and treatment effects
#   for_int <- compute_attgt_interaction(bdata, interact_var = varname, interaction = TRUE)
#   boot.int.dyn[[b]] <- for_int$dyn.ovr
#   boot.int.ovr[[b]] <- for_int$simple.att
#   
#   for_treat <- compute_attgt_interaction(bdata, interact_var = varname, interaction = FALSE)
#   boot.dyn[[b]] <- for_treat$dyn.ovr
#   boot.ovr[[b]] <- for_treat$simple.att
#   
#   print(b)
# }
# 
# # list to store bootstrap results
# boot.int.dyn<- sd(t(simplify2array(boot.int.dyn)))
# boot.int.ovr<- sd(t(simplify2array(boot.int.ovr)))
# boot.dyn <- sd(t(simplify2array(boot.dyn)))
# boot.ovr<- sd(t(simplify2array(boot.ovr)))
# 
# results_int <- compute_attgt_interaction(dta, interact_var = varname, interaction = TRUE)
# results_treat <- compute_attgt_interaction(dta, interact_var = varname, interaction = FALSE)
# # add the standard errors to the main results
# df_results <- data.frame(
#   "term" = c("interaction", "interaction", "treatment", "treatment"),
#   "ATT" = c(results_int$dyn.ovr , results_int$simple.att, results_treat$dyn.ovr , results_treat$simple.att ),
#   "type" = c("dyn", "simple", "dyn", "simple"),
#   "se" = c(boot.int.dyn, boot.int.ovr, boot.dyn, boot.ovr)
# )
# 
# write.csv(df_results, "erosion_boot.csv")
