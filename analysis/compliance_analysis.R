library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(stringi)
# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/proyecto.xlsx")

#properties and coordinates
predio_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/predio.xlsx")
#owners
propietario_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/propietario.xlsx")
#stands
rodal_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/rodal.xlsx")
#activities
actividades_df <- read_xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/actividad.xlsx")

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")

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
  filter(rptpro_ano <= 2019)%>%
  full_join(activity_df, by = c("rptpro_id", "rptpro_ano")) %>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff), 0, 1)),
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0)%>%
  distinct(rptpro_id, rptpre_id, received_bonus, .keep_all = TRUE)



mod <- lm(received_bonus ~  rptpro_tipo_concurso*rptpro_puntaje,
           data = NFL_df)
summary(mod)

mod <- lm(received_bonus ~  timber
          ,
          data = NFL_df)
summary(mod)
#### factors that predict compliance


mod2 <- lm(received_bonus ~ rptpro_tipo_concurso*rptpro_puntaje + extensionista +reforest +timber + nontimber + rptpro_superficie + rptpro_monto_total + rptpre_superficie_predial + rptpro_superficie,
          data = NFL_df)
summary(mod2)

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
         social_puntaje = .2*VI + .35*VP + 0.05*VPS) 

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
         social_puntaje = .25*VI + .25*VP + 0.05*VPS
  )

NFL_df <- rbind(other_df, small_df)
library(rio)
#export(NFL_df, "compliance_df.rds")

compliance_mod1 <- lm(received_bonus ~ social_puntaje + adjusted_puntaje , data = NFL_df)
compliance_mod2 <- lm(received_bonus ~ social_puntaje + adjusted_puntaje + extensionista , data = NFL_df)
compliance_mod3 <- lm(received_bonus ~ social_puntaje + adjusted_puntaje + extensionista + as.factor(rptpro_tipo_concurso), data = NFL_df)
compliance_mod4 <- lm(received_bonus ~ social_puntaje + adjusted_puntaje + extensionista + as.factor(rptpro_tipo_concurso)*social_puntaje, data = NFL_df)


ext_mod1 <- lm(extensionista  ~ social_puntaje + adjusted_puntaje , data = NFL_df)
ext_mod2 <- lm(extensionista  ~ social_puntaje + adjusted_puntaje + as.factor(rptpro_tipo_concurso) , data = NFL_df)


library("stargazer")
library("sandwich")


model.lst = list(compliance_mod1, compliance_mod2, compliance_mod3, compliance_mod4)

stargazer(compliance_mod1, compliance_mod2, compliance_mod3,
          title="Displaying results for multiple response variables",
          type = "latex",
          float = TRUE,
          report = "vcs*",
          se=lapply(model.lst, function(x) sqrt(diag(vcovHC(x, type = "HC1")))),
          no.space = TRUE,
          header=FALSE,
          single.row = TRUE,
          font.size = "small",
          intercept.bottom = T,
          covariate.labels = c("social score", "adjusted score", "extentionist", "contest", "intercept"),
          column.labels = c("received bonus"),
          #column.separate = c(1, 1),
          digits = 4
)

model.lst = list(ext_mod1, ext_mod2)

stargazer(ext_mod1, ext_mod2,
          title="Displaying results for multiple response variables",
          type = "latex",
          float = TRUE,
          report = "vcs*",
          se=lapply(model.lst, function(x) sqrt(diag(vcovHC(x, type = "HC1")))),
          no.space = TRUE,
          header=FALSE,
          single.row = TRUE,
          font.size = "small",
          intercept.bottom = T,
          covariate.labels = c("social score", "adjusted score", "contest", "intercept"),
          column.labels = c("received bonus"),
          #column.separate = c(1, 1),
          digits = 4
)



other_color = "#D55E00"
small_color = "#009E73"

small_score_plot <- ggplot(data = small_df, aes(x = rptpro_puntaje, y = received_bonus))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm", color = small_color) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  xlab("project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by project score") +
  scale_color_discrete("contest")+
  theme_minimal()
small_score_plot
ggsave(path = "figs", filename = "small_compliance_plot.png", width = 7, height = 5)

other_score_plot <- ggplot(data = other_df, aes(x = rptpro_puntaje, y = received_bonus))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm", color = other_color) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = F)+
  xlab("project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by project score") +
  scale_color_discrete("contest")+
  theme_minimal()
other_score_plot
ggsave(path = "figs", filename = "other_compliance_plot.png", width = 7, height = 5)

small_long <- small_df %>%
  mutate("assigned score" = (rptpro_puntaje - mean(rptpro_puntaje))/sd(rptpro_puntaje),
         "adjusted score" = (adjusted_puntaje - mean(adjusted_puntaje))/sd(adjusted_puntaje))%>%
  gather(key = "score_type", value = "score", c("assigned score", "adjusted score"))%>%
  select(received_bonus, `score_type`, score)

other_long <- other_df %>%
  mutate("assigned score" = (rptpro_puntaje - mean(rptpro_puntaje))/sd(rptpro_puntaje),
         "adjusted score" = (adjusted_puntaje - mean(adjusted_puntaje))/sd(adjusted_puntaje))%>%
  gather(key = "score_type", value = "score", c("assigned score", "adjusted score"))%>%
  select(received_bonus, `score_type`, score)


small_adjustedscore_plot <- ggplot(data = small_long, aes(x = score, y = received_bonus, color = `score_type`))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm", se = T, size = 1.2, level=0.90) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T)+
  xlab("standardized project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by standardized score for other interested parties") +
  scale_color_discrete("scoring method")+
  scale_x_continuous(breaks = c(-3, -1, 0, 1, 3))+
  theme_minimal() #+ theme(axis.text.x = element_blank())
small_adjustedscore_plot

ggsave(path = "figs", filename = "compliance_smallholder2.png", width = 7, height = 5)

other_adjustedscore_plot <- ggplot(data = other_long, aes(x = score, y = received_bonus, color = `score_type`))+
  #geom_point()+
  #geom_smooth(se = F)+
  geom_smooth(method = "lm", se = T, size = 1.2, level=0.90) +
  #geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = T)+
  xlab("standardized project score") + ylab("probability of compliance") +
  ggtitle("compliance probability by standardized score for other interested parties") +
  scale_color_discrete("scoring method")+
  scale_x_continuous(breaks = c(-3, -1, 0, 1, 3))+
  theme_minimal() #+ theme(axis.text.x = element_blank())
other_adjustedscore_plot

ggsave(path = "figs", filename = "compliance_other2.png", width = 7, height = 5)


mod <- lm(received_bonus ~ score*as.factor(score_type), data = small_long)
summary(mod)

mod <- lm(received_bonus ~ score*as.factor(score_type), data = other_long)
summary(mod)

#############################################################################################
#### matched set to examine heterogeneity in land use characteristics
#############################################################################################

my_rol_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

my_rol_match$geometry.x <- NULL
my_rol_match$geometry.y <- NULL
my_spatial_match$geometry.x <- NULL
my_spatial_match$geometry.y <- NULL


native_forest_law <- readRDS("C:/Users/garci/Dropbox/chile_collab/input_files/NFL_df.rds") %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")%>%
  mutate(submitted_mp = ifelse( rptpro_tiene_plan_saff=="Si" | rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         received_bonus = ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         treat = 1,
         first.treat = rptpro_ano)

length(unique(native_forest_law$rptpro_id))

rol_compliance <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  slice_head()

compliance_matches <- rol_compliance %>%
  mutate(forest = c_1,
         plantation = c_3,
         pasture = c_9,
         urban = c_16,
         water = c_15,
         baresoil = c_12,
         shrub = c_5,
         snow = c_11,
         road_dist = unlist(road_dist),
         industry_dist = unlist(industry_dist),
         native_industry_dist = unlist(native_industry_dist),
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         reforest = (regeneracion + `siembra-directa` + `corta-regeneracion` + `plantacion-suplementaria` + plantacion) > 0
         )%>%
  filter(rptpro_ano <= 2018)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## random forest 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"
# install.packages(urlPackage, repos=NULL, type="source") 
library(randomForest)

predictors <- c("forest", "plantation", "pasture", "urban", "water", "baresoil", "shrub", "snow", "road_dist", "industry_dist", "native_industry_dist", "indigenous", "timber", "nontimber", "reforest", "extensionista", "elev", "slope", "proportion_erosion",
             #   "rptprop_sexo", 
                "lat", "long",
                "rptpro_superficie", "rptpre_superficie_predial", "rptpro_monto_total")
rhs <- paste(predictors, collapse = " + ")

rf_data <- compliance_matches %>% 
  drop_na(predictors) %>% 
 mutate_at(vars(received_bonus, extensionista), as.factor) %>%
  as.data.frame()

set.seed(0930)
rf <-randomForest(as.formula(paste("received_bonus", "~", rhs)), data = rf_data, 
                  ntree = 500, importance=TRUE)
print(rf)                  
plot(rf)

ImpData <- as.data.frame(importance(rf))%>%
  mutate(Var.Names = row.names(.))%>%
  arrange(desc(MeanDecreaseAccuracy))#%>%
 # head(10)

varImpPlot(rf)

ggplot(ImpData, aes(x=Var.Names, y=`MeanDecreaseAccuracy`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`MeanDecreaseAccuracy`), color="skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

partialPlot(rf, pred.data = rf_data, x.var = extensionista, which.class = 1,
            ylab = "probability of receiving bonus",
            xlab = "extensionista support",
            main = "Partial dependence on extensionista"
            )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## t-tests
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  

evi <- t.test(evi_2007 ~ received_bonus, data = compliance_matches)
elev <- t.test(elev ~ received_bonus, data = compliance_matches)
slope <- t.test(slope ~ received_bonus, data = compliance_matches)
industry <- t.test(industry_dist ~ received_bonus, data = compliance_matches)
native_ind <- t.test(native_industry_dist ~ received_bonus, data = compliance_matches)
road_dist <- t.test(road_dist ~ received_bonus, data = compliance_matches)
shrub <- t.test(shrub ~ received_bonus, data = compliance_matches)
baresoil <- t.test(baresoil ~ received_bonus, data = compliance_matches)
water <- t.test(water ~ received_bonus, data = compliance_matches)
urban <- t.test(urban ~ received_bonus, data = compliance_matches)
pasture <- t.test(pasture ~ received_bonus, data = compliance_matches)
plantation <- t.test(plantation ~ received_bonus, data = compliance_matches)
forest <- t.test(forest ~ received_bonus, data = compliance_matches)
area_prop <- t.test(rptpre_superficie_predial ~ received_bonus, data = compliance_matches)
area_bono <- t.test(rptpro_superficie ~ received_bonus, data = compliance_matches)
payment <- t.test(rptpro_monto_total ~ received_bonus, data = compliance_matches)
timber <- t.test(timber ~ received_bonus, data = compliance_matches)
extensionista <- t.test(extensionista ~ received_bonus, data = compliance_matches)
erosion <- t.test(proportion_erosion ~ received_bonus, data = compliance_matches)

covar <- c("award (UTM)", "bonus area (ha)", "property area (ha)", "prob. had extensionist", "prob. timber production objective" ,  "proportion forest", "proportion plantation", "proportion pasture", "proportion shrub", "mod-to-severe erosion", "slope", "elevation", "dist. to native timber processing", "dist. to any timber processing", "dist. to road" )
paid <- c(payment$estimate[2], area_bono$estimate[2],  area_prop$estimate[2], extensionista$estimate[2], timber$estimate[2],  forest$estimate[2], plantation$estimate[2], pasture$estimate[2], shrub$estimate[2], erosion$estimate[2], slope$estimate[2], elev$estimate[2], native_ind$estimate[2], industry$estimate[2], road_dist$estimate[2])
unpaid <- c(payment$estimate[1], area_bono$estimate[1],  area_prop$estimate[1], extensionista$estimate[1], timber$estimate[1],  forest$estimate[1], plantation$estimate[1], pasture$estimate[1], shrub$estimate[1], erosion$estimate[1], slope$estimate[1], elev$estimate[1], native_ind$estimate[1], industry$estimate[1], road_dist$estimate[1])

p_value <-  c(payment$p.value, area_bono$p.value,  area_prop$p.value, extensionista$p.value, timber$p.value, forest$p.value, plantation$p.value, pasture$p.value, shrub$p.value, erosion$p.value, slope$p.value, elev$p.value, native_ind$p.value, industry$p.value, road_dist$p.value)

compliance_ttest <- data.frame(covar, paid, unpaid, p_value) %>%
  mutate(p_value = round(p_value, digits = 5))

library(rio)
export(compliance_ttest, "compliance_ttest.rds")
