library(readxl)
library(tidyverse)
library(ggplot2)
library(rio)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)

#my_data_dir <- here::here("remote")
output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

select <- dplyr::select

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in and combine Native Forest Law admin data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# fcn to remove accents
accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/proyecto.xlsx"))

#properties and coordinates
predio_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/predio.xlsx"))
#owners
propietario_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/propietario.xlsx"))
#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/actividad.xlsx"))

coordinadas_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/coordinadas_predio.xlsx"))

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")%>%
  mutate(`Contest type` = ifelse(rptpro_tipo_concurso == "Otros Interesados", "Other interested", "Smallholder"),
         received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "No" | is.na(rptpro_tiene_bonificacion_saff) , 0, 1)),
         submitted_management_plan = as.numeric(ifelse(rptpro_tiene_plan_saff == "No" | is.na(rptpro_tiene_plan_saff), 0, 1))
  )%>%
  separate(rptpre_rol, into = c("rol1", "rol2", "additional_props"), remove = F)%>%
  mutate(ROL = paste0(rol1, "-", rol2),
         additional_props = ifelse(is.na(additional_props), 0, 1),
         indig_etnia = ifelse(is.na(rptprop_etnia) , 0, 1),
         rptprop_razon_social = accents(tolower(rptprop_razon_social)), 
         ind_comunidad = grepl(pattern = "indigena", rptprop_razon_social)*1,
         indigenous = ifelse(ind_comunidad == 1 | indig_etnia == 1, 1, 0),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0)
  )

comunal_pov <- readxl::read_excel(paste0(my_data_dir,"/data/analysis_lc/comunal_poverty.xlsx")
)%>%
  mutate(comuna = tolower(accents(comuna)))

admin_df <- property_df %>%
  mutate(comuna = tolower(accents(rptpre_comuna)))%>%
  left_join(comunal_pov, by = "comuna")

rodal_admin_df <- admin_df %>%
  left_join(rodal_df, by = c("rptpre_id"))

# see all forest types entered
table(rodal_df$rptro_tipo_forestal)
# Get scores specific to the stand/rodal level
tf_maderera_100 <- c("ro-ra-co", "lenga", "siempreverde")
tf_maderera_75 <- c("roble-hualo", "esclerofilo", "co-ra-te") # Esclerófilo, Roble Hualo, Coihue-Raulí-Tepa
tf_maderera_50 <- c("coihue-magallanes")
tf_maderera_25 <- c("cipres-cordillera", "cipres-guaitecas")

tf_nomaderera_100 <- c("ro-ra-co", "roble-hualo", "esclerofilo")
tf_nomaderera_75 <- c("co-ra-te") 
tf_nomaderera_50 <- c("lenga", "siempreverde")
tf_nomaderera_25 <- c("cipres-cordillera", "cipres-guaitecas", "coihue-magallanes")

tf_ecologico_100 <- c("roble-hualo", "esclerofilo", "xerofitico")
tf_ecologico_75 <- c("alerce", "araucaria", "cipres-cordillera", "palma")
tf_ecologico_50 <- c("lenga", "siempreverde")
tf_ecologico_25 <- c("cipres-guaitecas", "ro-ra-co", "co-ra-te", "coihue-magallanes")


table(rodal_df$rptro_categoria_conservacion)
table(rodal_df$rptro_especie)
ve_maderera_100 <- c("monte_bravo_bajo")
ve_maderera_75 <- c("monte_bravo_alto")
ve_maderera_50 <- c("latizal_bajo")
ve_maderera_25 <- c("latizal_alto")
ve_maderera_15 <- c("otro")

ve_ecologico_100 <- c("peligro", "peligro-critico")
ve_ecologico_75 <- c("vulnerable")
ve_ecologico_50 <- c("rara", "especie-unica")

ve_nomaderera_productos_100 <- c("quillay", "ulmo", "avellano")
ve_nomaderera_productos_75 <- c("boldo") 
ve_nomaderera_productos_50 <- c("fuinque", "maqui", "fuinque - maqui")

rodal_score_df <- rodal_admin_df %>%
  mutate(tf = case_when(rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_tipo_forestal %in% tf_maderera_100 ~ 100,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_tipo_forestal %in% tf_maderera_75 ~ 75,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_tipo_forestal %in% tf_maderera_50 ~ 50,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_tipo_forestal %in% tf_maderera_25 ~ 25,
                        rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptro_tipo_forestal %in% tf_nomaderera_100 ~ 100,
                        rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptro_tipo_forestal %in% tf_nomaderera_75 ~ 75,
                        rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptro_tipo_forestal %in% tf_nomaderera_50 ~ 50,
                        rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptro_tipo_forestal %in% tf_nomaderera_25 ~ 25,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_tipo_forestal %in% tf_ecologico_100 ~ 100,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_tipo_forestal %in% tf_ecologico_75 ~ 75,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_tipo_forestal %in% tf_ecologico_50 ~ 50,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_tipo_forestal %in% tf_ecologico_25 ~ 25
                        ),
         ve = case_when(rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_categoria_conservacion %in% ve_maderera_100 ~ 100,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_categoria_conservacion %in% ve_maderera_75 ~ 75,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_categoria_conservacion %in% ve_maderera_50 ~ 50,
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" & rptro_categoria_conservacion %in% ve_maderera_25 ~ 25,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% tf_ecologico_100 ~ 100,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% tf_ecologico_75 ~ 75,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% tf_ecologico_50 ~ 50,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% tf_ecologico_25 ~ 25
         ),
         ve_productos = case_when(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_100 | rptro_especie %in% ve_nomaderera_productos_100) ~ 100,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_75 | rptro_especie %in% ve_nomaderera_productos_75) ~ 75,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_50 | rptro_especie %in% ve_nomaderera_productos_50) ~ 50,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_100) & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_75) & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_50) ~ 25
                                  ),
         ve_proteccion = case_when(rptro_bosque_quemado == "Si" ~ 100
         )
         )%>%
  mutate_at(vars(tf, ve), ~replace_na(., 0))%>%
  select(rptpro_id, rptpro_puntaje, rptpro_tipo_concurso, rptpro_objetivo_manejo, rptpro_superficie, rptro_id, rptro_superficie, rptro_tipo_forestal, rptro_categoria_conservacion, rptro_especie, rptro_bosque_quemado, tf, ve, ve_productos, ve_proteccion, cpov_pct_2007)


ve_recuperacion_100 <- c("plantacion-suplementaria", "corta-recuperacion", "limpia")

actividades_admin_df <- rodal_score_df %>%
  left_join(actividades_df, by = c("rptro_id")) %>%
  mutate(ve_recuperacion = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptac_tipo %in% ve_recuperacion_100, 100, 0)
  )%>%
  select(rptro_id, ve_recuperacion) %>%
  group_by(rptro_id) %>%
  summarise(ve_recuperacion= max(ve_recuperacion, na.rm = T))%>%
  ungroup

rodal_score <- rodal_score_df %>%
  left_join(actividades_admin_df, by = "rptro_id") %>%
  group_by(rptro_id)%>%
  mutate(ve = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA",
                     max(ve, ve_productos, ve_proteccion, na.rm = T),
                     ve)
         )%>%
  ungroup %>%
  mutate(tf_weighted = tf*(rptro_superficie/rptpro_superficie),
         ve_weighted = ve*(rptro_superficie/rptpro_superficie))
  
  VT_score <- rodal_score%>%
  group_by(rptpro_id)%>%
  summarise(tf = sum(tf_weighted, na.rm = T),
            ve = sum(ve_weighted, na.rm = T))%>%
  ungroup %>%
  mutate(VT = 0.3*tf + 0.7*ve)%>%
  full_join(admin_df %>% 
              select(rptpro_id, rptpro_puntaje, rptpro_tipo_concurso, rptpro_objetivo_manejo, rptpro_superficie, cpov_pct_2007),
            by = "rptpro_id"
            )%>%
    distinct()
  
  
### Figures example
  
#%%%%  
# First get rankings for everyone
#%%%%
  
# number of smallholders
n_smallholders <- VT_score %>% 
  filter(rptpro_tipo_concurso != "Otros Interesados")%>%
  nrow()
  
VT_score_ranked <- VT_score %>%
  mutate(overall_rank = dense_rank(desc(rptpro_puntaje)),
         overall_VT_rank = dense_rank(desc(VT)))%>%
  group_by(rptpro_tipo_concurso)%>%
  mutate(contest_rank = dense_rank(desc(rptpro_puntaje)),
         contest_VT_rank = dense_rank(desc(rptpro_puntaje)))%>%
  ungroup%>%
  mutate(VT_rank = dense_rank(desc(VT)),
         rank = ifelse(rptpro_tipo_concurso == "Otros Interesados", contest_rank + n_smallholders, contest_rank),
         rank_pctl = percent_rank(rank),
         VT_pctl = percent_rank(VT_rank),
         delta_rank = rank - VT_rank,
         delta_rank_pctl = rank_pctl - VT_pctl)
  
ggplot(VT_score_ranked, aes(delta_rank)) +
  geom_density()+
  theme_classic()
  
ggplot(VT_score_ranked, aes(x = as.numeric(cpov_pct_2007), y = delta_rank)) +
  geom_smooth()+
  theme_classic()
  
ggplot(VT_score_ranked, aes(x = as.numeric(cpov_pct_2007), y = delta_rank_pctl)) +
  geom_smooth()+
  theme_classic()







# Get scores specific to the activity

ve_nomaderera_servicios_100 <- c("sendero")
ve_nomaderera_proteccion_100 <- 0 # activities in forests affected by forest fire
ve_nomaderera_recuperacion_100 <- c("corta-recuperacion", "plantacion-suplementaria", "limpia")


ve_ecologico_100 <- c("peligro", "peligro-critico")
ve_ecologico_75 <- c("vulnerable")
ve_ecologico_50 <- c("especie-unica", "rara")






rodal_scores <- rodal_df %>%
  mutate(cg = case_when(rptro_tipo_forestal == "ro-ra-co" ~ 100,
                        rptro_tipo_forestal > 2  ~ "high"))
  

sizecut_points <- c(0, 40, 80, 120, 200)
bonuscut_points <- c(0, 70, 150, 300, 500)

small_df <- admin_df %>%
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

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Now the same for other interested party contest
#%%%%%%%%%%%%%%%%%%%%%%%%%

sizecut_points <- c(0, 300, 600, 2000)
bonuscut_points <- c(0, 300, 600, 1000)

other_df <- admin_df %>%
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
#fuzzyjoin::stringdist_left_join(comunal_pov, by = "comuna", max_dist = 2, distance_col = "sdist_comuna")


# adjust rank of other interested to account for all the smallholders

XXXXXX

#%%%%%%%%%%%%%%%%%%%%%%%%%
### Recombine the two
#%%%%%%%%%%%%%%%%%%%%%%%%%

admin_df <- rbind(other_df, small_df) %>%
  distinct(rptpre_id, rptpro_id, ROL, rptpre_comuna, .keep_all = T)

# Get adjusted rank in each contest year
