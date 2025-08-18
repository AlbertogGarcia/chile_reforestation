library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rio)
library(stringi)
library(fixest)
library(modelsummary)
library(kableExtra)

`%ni%` <- Negate(`%in%`)  # "not in" function

#my_data_dir <- here::here("remote")
output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
clean_data_dir <- output_dir
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

comunal_pov <- readxl::read_excel(paste0(my_data_dir,"/data/analysis_lc/comunal_poverty.xlsx")
)%>%
  mutate(comuna = tolower(accents(comuna)))

#%%%%%%%%%%%%%%%%%%%%%%%%
### Combine together
#%%%%%%%%%%%%%%%%%%%%%%%
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
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0))

admin_df <- property_df %>%
  mutate(comuna = tolower(accents(rptpre_comuna))
         )%>%
  left_join(comunal_pov, by = "comuna")

# Get specific stands added in
rodal_admin_df <- admin_df %>%
  left_join(rodal_df, by = c("rptpre_id"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### VT = 0.3*TF + 0.7*VE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
         ve_ecologico = case_when(rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% ve_ecologico_100 ~ 100,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% ve_ecologico_75 ~ 75,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" & rptro_categoria_conservacion %in% ve_ecologico_50 ~ 50
         ),
         ve_productos = case_when(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_100 | rptro_especie %in% ve_nomaderera_productos_100) ~ 100,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_75 | rptro_especie %in% ve_nomaderera_productos_75) ~ 75,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & (rptro_categoria_conservacion %in% ve_nomaderera_productos_50 | rptro_especie %in% ve_nomaderera_productos_50) ~ 50,
                                  rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_100) & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_75) & !(rptro_categoria_conservacion %in% ve_nomaderera_productos_50) ~ 25
                                  ),
         ve_proteccion = case_when(rptro_bosque_quemado == "Si" ~ 100
         )
         )%>%
  mutate_at(vars(tf), ~replace_na(., 0))%>%
  select(rptpro_id, rptpro_puntaje, rptpro_tipo_concurso, rptpro_objetivo_manejo, rptpro_superficie, rptro_id, rptro_superficie, rptro_tipo_forestal, rptro_categoria_conservacion, rptro_especie, rptro_bosque_quemado, tf, ve_ecologico, ve_productos, ve_proteccion, cpov_pct_2007)


ve_recuperacion_100 <- c("plantacion-suplementaria", "corta-recuperacion", "limpia")

ve_maderera_100 <- c("monte_bravo_bajo", "regeneracion")
ve_maderera_75 <- c("monte_bravo_alto")
ve_maderera_50 <- c("latizal_bajo")
ve_maderera_25 <- c("latizal_alto", "fustal_joven", "fustal")


actividades_admin_df <- rodal_score_df %>%
  left_join(actividades_df, by = c("rptro_id")) %>%
  mutate(ve_recuperacion = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" & rptac_tipo %in% ve_recuperacion_100, 100, 0),
         ve_maderera = case_when(
           rptpro_objetivo_manejo == "PRODUCCION MADERERA" & (rptro_categoria_conservacion %in% ve_maderera_100 | rptac_estado_desarrollo %in% ve_maderera_100) ~ 100,
           rptpro_objetivo_manejo == "PRODUCCION MADERERA" & (rptro_categoria_conservacion %in% ve_maderera_75 | rptac_estado_desarrollo %in% ve_maderera_100) ~ 75,
           rptpro_objetivo_manejo == "PRODUCCION MADERERA" & (rptro_categoria_conservacion %in% ve_maderera_50 | rptac_estado_desarrollo %in% ve_maderera_100) ~ 50,
           rptpro_objetivo_manejo == "PRODUCCION MADERERA" & (rptro_categoria_conservacion %in% ve_maderera_25 | rptac_estado_desarrollo %in% ve_maderera_100) ~ 25,
         ),
         ve_maderera = replace_na(ve_maderera, 15)
  )%>%
  select(rptro_id, ve_recuperacion, ve_maderera) %>%
  group_by(rptro_id) %>%
  summarise(ve_recuperacion= max(ve_recuperacion, na.rm = T),
            ve_maderera= max(ve_maderera, na.rm = T))%>%
  ungroup

rodal_score <- rodal_score_df %>%
  left_join(actividades_admin_df, by = "rptro_id") %>%
  group_by(rptro_id)%>%
  mutate(ve = case_when(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA" ~ max(ve_recuperacion, ve_productos, ve_proteccion, na.rm = T),
                        rptpro_objetivo_manejo == "PRODUCCION MADERERA" ~ ve_maderera,
                        rptpro_objetivo_manejo == "BOSQUE PRESERVACION Y FORMACIONES XEROFITICAS DE ALTO VALOR ECOLOGICO" ~ ve_ecologico
                        ),
         ve = replace_na(ve, 0)
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
              select(rptpro_id, rptpro_ano, rptpro_puntaje, rptpro_tipo_concurso, rptpro_objetivo_manejo, rptpro_superficie, cpov_pct_2007),
            by = "rptpro_id"
            )%>%
    group_by(rptpro_id)%>%
    slice_head()%>%
    select(rptpro_id, VT, tf, ve)
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### VI = 0.5*TP + 0.5*VPI
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
sizecut_points_small <- c(0, 40, 80, 120, 200)
  
VI_score_small <- admin_df %>%
  filter(rptpro_tipo_concurso != "Otros Interesados")%>%
  mutate(VPI = ifelse(indigenous == 1, 100, 50),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_small[1], sizecut_points_small[2]), 100, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_small[2], sizecut_points_small[3]), 75, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_small[3], sizecut_points_small[4]), 50, 25),
         VI_contest = TP*.5 + .5*VPI
         )%>%
  select(rptpro_id, VI_contest)

         # VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[1], bonuscut_points[2]), 100, 15),
         # VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[2], bonuscut_points[3]), 75, 15),
         # VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[3], bonuscut_points[4]), 50, 15),
         # VMBS = ifelse(between(rptpro_monto_total, bonuscut_points[4], bonuscut_points[5]), 25, 15),
         # VP = .1*VMBS,
         # VPS = ifelse(is.na(rptprop_razon_social), 0, 100),
         # adjusted_puntaje = rptpro_puntaje - .2*VI - .35*VP - 0.05*VPS,
         # social_puntaje = .2*VI + .35*VP + 0.05*VPS,
         # smallholder = 1)
  
sizecut_points_other <- c(0, 300, 600, 2000)
  
VI_score_other <- admin_df %>%
  filter(rptpro_tipo_concurso == "Otros Interesados")%>%
  mutate(VPI = ifelse(indigenous == 1, 100, 50),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[1], sizecut_points_other[2]), 100, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[2], sizecut_points_other[3]), 75, 25),
          TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[3], sizecut_points_other[4]), 50, 25),
          VI_contest = TP*.5 + .5*VPI
         )%>%
  select(rptpro_id, VI_contest)


# Both contests subject to the same criteria -- using other interested thresholds
VI_score_together <- admin_df %>%
  mutate(VPI = ifelse(indigenous == 1, 100, 50),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[1], sizecut_points_other[2]), 100, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[2], sizecut_points_other[3]), 75, 25),
         TP = ifelse(between(rptpre_superficie_predial, sizecut_points_other[3], sizecut_points_other[4]), 50, 25),
         VI = TP*.5 + .5*VPI
  )%>%
  group_by(rptpro_id)%>%
  slice_head()%>%
  select(rptpro_id, VPI, TP, VI)
  
  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### VP = 0.45*VBSU + 0.35*VASOB + 0.1*VMBS + 0.1*VFCI
# VBSU = 0.3*PM + 0.3*MO + 0.4*BQ

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
actividad_rendimiento <- rodal_admin_df %>%
  left_join(actividades_df, by = "rptro_id")%>%
  mutate(rendimiento = case_when(
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("xerofitico") ~ 0.5,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("esclerofilo", "palma", "cipres-cordillera") ~ 1.5,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 2.08,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("alerce") ~ 2.8,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %ni% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera", "alerce", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 2.73,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("xerofitico") ~ 1.65,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("esclerofilo", "palma", "cipres-cordillera") ~ 4.95,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 6.86,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("alerce") ~ 9.21,
    rptpro_literal == "A" & rptac_tipo %in% c("enriquecimiento") & rptac_densidad >= 330 & rptro_tipo_forestal %ni% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera", "alerce", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 9.01,
    rptpro_literal == "A" & rptac_tipo %in% c("zanja") & rptac_densidad >= 100 & rptac_densidad < 330 ~ 6.78,
    rptpro_literal == "A" & rptac_tipo %in% c("zanja") & rptac_densidad >= 330 ~ 22.38,
    rptpro_literal == "A" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %in% c("xerofitico") ~ 2.34,
    rptpro_literal == "A" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %in% c("esclerofilo", "palma", "cipres-cordillera") ~ 6.66,
    rptpro_literal == "A" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 7.86,
    rptpro_literal == "A" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %ni% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 9.46,
    rptpro_literal == "A" & rptac_tipo %in% c("control") & rptro_tipo_forestal %in% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera") ~ 2,
    rptpro_literal == "A" & rptac_tipo %in% c("control") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 3.2,
    rptpro_literal == "A" & rptac_tipo %in% c("control") & rptro_tipo_forestal %in% c("araucaria", "siempreverde", "alerce", "cipres-guaitecas", "coihue-magallanes", "lenga") ~ 3.2,
    rptpro_literal == "A" & rptac_tipo %in% c("control") & rptro_tipo_forestal %ni% c("araucaria", "siempreverde", "alerce", "cipres-guaitecas", "coihue-magallanes", "lenga", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga", "xerofitico", "esclerofilo", "palma", "cipres-cordillera") ~ 4.8,
    rptpro_literal == "A" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("co-ra-te", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 1.38,
    rptpro_literal == "A" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %ni% c("co-ra-te", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 1.24,
    rptpro_literal == "A" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %in% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera") ~ 10.67,
    rptpro_literal == "A" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 13.07,
    rptpro_literal == "A" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %ni% c("xerofitico", "esclerofilo", "palma", "cipres-cordillera", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 16.27,
    rptpro_literal == "A" & rptac_tiene_cerco %in% c("Si", 1)~ 19.62,
    rptpro_literal == "B" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 6.66,
    rptpro_literal == "B" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 7.86,
    rptpro_literal == "B" & rptac_tipo %in% c("siembra-directa") & rptro_tipo_forestal %ni% c("xerofitico", "esclerofilo", "cipres-cordillera", "co-ra-te", "ro-ra-co", "roble-hualo", "lenga") ~ 9.46,
    rptpro_literal == "B" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %in% c("esclerofilo") ~ 1.33,
    rptpro_literal == "B" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 3.2,
    rptpro_literal == "B" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %ni% c("esclerofilo", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 2.13,
    rptpro_literal == "B" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("esclerofilo") ~ 4,
    rptpro_literal == "B" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 8.61,
    rptpro_literal == "B" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %ni% c("esclerofilo", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 6.85,
    rptpro_literal == "B" & rptac_tipo %in% c("regeneracion") & rptro_tipo_forestal %in% c("esclerofilo") ~ 32.7,
    rptpro_literal == "B" & rptac_tipo %in% c("regeneracion") & rptro_tipo_forestal %in% c("roble-hualo") ~ 38.22,
    rptpro_literal == "B" & rptac_tipo %in% c("regeneracion") & rptro_tipo_forestal %in% c("cipres-cordillera") ~ 36.34,
    rptpro_literal == "B" & rptac_tipo %in% c("regeneracion") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "lenga") ~ 15.84,
    rptpro_literal == "B" & rptac_tipo %in% c("regeneracion") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 20.83,
    rptpro_literal == "B" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 1.11,
    rptpro_literal == "B" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "co-ra-te", "lenga") ~ 1.28,
    rptpro_literal == "B" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "co-ra-te", "lenga", "esclerofilo", "cipres-cordillera") ~ 1.42,
    rptpro_literal == "B" & 
      #(rptac_estado_desarrollo == "monte_bravo_bajo" | rptro_categoria_conservacion == "monte_bravo_bajo") & 
      str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "cipres-cordillera", "lenga") ~ 9.5,
    rptpro_literal == "B" & str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "cipres-cordillera", "lenga") ~ 14.29,
    rptpro_literal == "B" & str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "cipres-cordillera", "lenga") ~ 5.7,
    rptpro_literal == "B" & str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "cipres-cordillera", "lenga") ~ 8.57,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 7.03,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga") ~ 7.53,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("co-ra-te") ~ 9.2,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "lenga", "esclerofilo", "cipres-cordillera", "co-ra-te") ~ 9.7,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 4.6,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga") ~ 5.1,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %in% c("co-ra-te") ~ 5.6,
    rptpro_literal == "B" & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto")) & str_detect(rptac_tipo, "raleo") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "lenga", "esclerofilo", "cipres-cordillera", "co-ra-te") ~ 6.1,
    rptpro_literal == "B" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 2,
    rptpro_literal == "B" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga") ~ 3.33,
    rptpro_literal == "B" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "lenga", "esclerofilo", "cipres-cordillera") ~ 4,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 1.6,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga", "co-ra-te") ~ 2.08,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %ni% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga", "esclerofilo", "cipres-cordillera") ~ 2.73,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 4.98,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga", "co-ra-te") ~ 6.86,
    rptpro_literal == "B" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad >= 330 & rptro_tipo_forestal %ni% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga", "esclerofilo", "cipres-cordillera") ~ 9.02,
    rptpro_literal == "B" & rptac_tipo %in% c("zanja") & (rptac_densidad < 330 | is.na(rptac_densidad)) ~ 6.78,
    rptpro_literal == "B" & rptac_tipo %in% c("zanja") & rptac_densidad >= 330 ~ 22.38,
    rptpro_literal == "B" & rptac_tipo %in% c("proteccion") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 10.67,
    rptpro_literal == "B" & rptac_tipo %in% c("proteccion") & rptac_densidad >= 330 & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga", "co-ra-te") ~ 13.07,
    rptpro_literal == "B" & rptac_tipo %in% c("proteccion") & rptac_densidad >= 330 & rptro_tipo_forestal %ni% c("co-ra-te", "ro-ra-co", "roble-hualo", "lenga", "esclerofilo", "cipres-cordillera") ~ 16.27,
    rptpro_literal == "C" & rptac_tipo %in% c("siembra-directa", "regeneracion") & rptro_tipo_forestal %in% c("esclerofilo") ~ 4,
    rptpro_literal == "C" & rptac_tipo %in% c("siembra-directa", "regeneracion") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 8.61,
    rptpro_literal == "C" & rptac_tipo %in% c("siembra-directa", "regeneracion") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo") ~ 6.85,
    rptpro_literal == "C" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("esclerofilo") ~ 32.3,
    rptpro_literal == "C" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("roble-hualo", "cipres-cordillera") ~ 38.5,
    rptpro_literal == "C" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("co-ra-te", "ro-ra-co", "lenga") ~ 16.1,
    rptpro_literal == "C" & rptac_tipo %in% c("enriquecimiento") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 21.9,
    rptpro_literal == "C" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %in% c("esclerofilo") ~ 1.6,
    rptpro_literal == "C" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 4.41,
    rptpro_literal == "C" & rptac_tipo %in% c("limpia") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo") ~ 3.09,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-regeneracion") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 1.13,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-regeneracion") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 1.52,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-regeneracion") & rptro_tipo_forestal %in% c("co-ra-te") ~ 1.37,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-regeneracion") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo", "cipres-cordillera", "co-ra-te") ~ 1.28,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("esclerofilo") ~ 1.5,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 2.97,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & (rptac_densidad < 330 | is.na(rptac_densidad)) & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo") ~ 2.09,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad == 330 & rptro_tipo_forestal %in% c("esclerofilo") ~ 4.95,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad == 330 & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 6.43,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad == 330 & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo") ~ 9.81,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad == 660 & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 19.62,
    rptpro_literal == "C" & rptac_tipo %in% c("plantacion-suplementaria") & rptac_densidad == 660 & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 13.82,
    rptpro_literal == "C" & rptac_tipo %in% c("zanja") & (rptac_densidad < 330 | is.na(rptac_densidad)) ~ 6.78, 
    rptpro_literal == "C" & rptac_tipo %in% c("zanja") & rptac_densidad == 330 ~ 22.38, 
    rptpro_literal == "C" & rptac_tipo %in% c("zanja") & rptac_densidad > 330 ~ 44.76, 
    rptpro_literal == "C" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %in% c("esclerofilo") ~ 10.66,
    rptpro_literal == "C" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 17.22,
    rptpro_literal == "C" & rptac_tipo %in% c("proteccion") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo") ~ 13.7,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-liberacion", "corta-mejoramiento") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 1.08,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-liberacion", "corta-mejoramiento") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 1.46,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-liberacion", "corta-mejoramiento") & rptro_tipo_forestal %in% c("co-ra-te") ~ 1.31,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-liberacion", "corta-mejoramiento") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo", "cipres-cordillera", "co-ra-te") ~ 1.11,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 1.11,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 1.42,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %in% c("co-ra-te") ~ 1.28,
    rptpro_literal == "C" & rptac_tipo %in% c("corta-sanitaria") & rptro_tipo_forestal %ni% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "esclerofilo", "cipres-cordillera", "co-ra-te") ~ 1.22,
    rptpro_literal == "C" & rptac_tipo %in% c("anillado") & rptro_tipo_forestal %in% c("co-ra-te", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ .28,
    rptpro_literal == "C" & rptac_tipo %in% c("anillado") & rptro_tipo_forestal %ni% c("co-ra-te", "siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ .24,
    rptpro_literal == "C" & str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %in% c("roble-hualo", "cipres-cordillera", "ro-ra-co", "lenga") ~ 9.5,
    rptpro_literal == "C" & str_detect(rptac_tipo, "clareo") & rptro_tipo_forestal %ni% c("roble-hualo", "cipres-cordillera", "ro-ra-co", "lenga") ~ 14.29,
    rptpro_literal == "C" & str_detect(rptac_tipo, "clareo") & (rptac_estado_desarrollo == "monte_bravo_alto"  | rptro_categoria_conservacion == "monte_bravo_alto") & rptro_tipo_forestal %in% c("roble-hualo", "cipres-cordillera", "ro-ra-co", "lenga") ~ 5.7,
    rptpro_literal == "C" & str_detect(rptac_tipo, "clareo") & (rptac_estado_desarrollo == "monte_bravo_alto" | rptro_categoria_conservacion == "monte_bravo_alto") & rptro_tipo_forestal %ni% c("roble-hualo", "cipres-cordillera", "ro-ra-co", "lenga") ~ 8.57,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo"))  & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 7.33,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 10,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo"))  & rptro_tipo_forestal %in% c("co-ra-te") ~ 9.5,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_bajo" | rptro_categoria_conservacion == "latizal_bajo" | rptac_tipo %in% c("raleo-latizal-bajo", "raleo-no-maderero-latizal-bajo")) & rptro_tipo_forestal %ni% c("esclerofilo", "cipres-cordillera", "siempreverde", "cipres-guaitecas", "coihue-magallanes", "co-ra-te") ~ 7.83,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto"))  & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 5,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_alto"| rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto"))  & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 6.5,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto"))  & rptro_tipo_forestal %in% c("co-ra-te") ~ 6,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("raleo-latizal-alto", "raleo-no-maderero-latizal-alto")) & rptro_tipo_forestal %ni% c("esclerofilo", "cipres-cordillera", "siempreverde", "cipres-guaitecas", "coihue-magallanes", "co-ra-te") ~ 5.5,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "fustal_joven" | rptro_categoria_conservacion == "fustal_joven" | rptac_tipo %in% c("raleo-fustal-joven"))  & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 4.03,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "fustal_joven"| rptro_categoria_conservacion == "fustal_joven" | rptac_tipo %in% c("raleo-fustal-joven"))  & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes") ~ 5.27,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "fustal_joven" | rptro_categoria_conservacion == "fustal_joven" | rptac_tipo %in% c("raleo-fustal-joven"))  & rptro_tipo_forestal %in% c("co-ra-te") ~ 4.77,
    rptpro_literal == "C" & str_detect(rptac_tipo, "raleo") & (rptac_estado_desarrollo == "fustal_joven"| rptro_categoria_conservacion == "fustal_joven" | rptac_tipo %in% c("raleo-fustal-joven"))  & rptro_tipo_forestal %ni% c("esclerofilo", "cipres-cordillera", "siempreverde", "cipres-guaitecas", "coihue-magallanes", "co-ra-te") ~ 4.53,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 2,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga") ~ 3.33,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "lenga", "esclerofilo", "cipres-cordillera") ~ 4,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("poda-latizal-alto")) & rptro_tipo_forestal %in% c("esclerofilo", "cipres-cordillera") ~ 2.67,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & (rptac_estado_desarrollo == "latizal_alto" | rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("poda-latizal-alto"))  & rptro_tipo_forestal %in% c("roble-hualo", "ro-ra-co", "lenga") ~ 4,
    rptpro_literal == "C" & str_detect(rptac_tipo, "poda") & (rptac_estado_desarrollo == "latizal_alto"| rptro_categoria_conservacion == "latizal_alto" | rptac_tipo %in% c("poda-latizal-alto"))  & rptro_tipo_forestal %ni% c("roble-hualo", "ro-ra-co", "lenga", "esclerofilo", "cipres-cordillera") ~ 5.6,
    rptpro_literal == "C" & str_detect(rptac_tipo, "corta") & rptac_tipo %ni% c("corta-liberacion", "corta-mejoramiento", "corta-sanitaria", "corta-regeneracion") & rptro_tipo_forestal %in% c("esclerofilo") ~ 9.17,
    rptpro_literal == "C" & str_detect(rptac_tipo, "corta") & rptac_tipo %ni% c("corta-liberacion", "corta-mejoramiento", "corta-sanitaria", "corta-regeneracion")  & rptro_tipo_forestal %in% c("cipres-cordillera") ~ 7.64,
    rptpro_literal == "C" & str_detect(rptac_tipo, "corta") & rptac_tipo %ni% c("corta-liberacion", "corta-mejoramiento", "corta-sanitaria", "corta-regeneracion")  & rptro_tipo_forestal %in% c("siempreverde", "cipres-guaitecas", "coihue-magallanes", "co-ra-te") ~ 11.38,
    rptpro_literal == "C" & str_detect(rptac_tipo, "corta") & rptac_tipo %ni% c("corta-liberacion", "corta-mejoramiento", "corta-sanitaria", "corta-regeneracion") & rptro_tipo_forestal %ni% c("esclerofilo", "cipres-cordillera", "siempreverde", "cipres-guaitecas", "coihue-magallanes", "co-ra-te") ~ 8.5
    ),
  MO_val = ifelse(is.na(rendimiento*rptro_superficie), 0, rendimiento*rptro_superficie),
  BQ = ifelse(rptro_bosque_quemado == "Si", 100, 0),
  PM = ifelse(rptpro_tiene_plan_saff %in% c("Si", 1), 100, 0)
  )

score_VBSU <- actividad_rendimiento %>%
  group_by(rptpro_id)%>%
  summarise(BQ = max(BQ, na.rm = T),
            PM = max(PM, na.rm = T),
            MO_val = sum(MO_val, na.rm = T))%>%
  mutate(MO = case_when(MO_val >= 3000 ~ 100,
                        MO_val >= 2100 & MO_val < 3000 ~ 80,
                        MO_val >= 1500 & MO_val < 2100 ~ 60,
                        MO_val >= 750 & MO_val < 1500 ~ 40,
                        MO_val < 750 ~ 20
                        ),
         VBSU = 0.5*PM + 0.2*MO + 0.3*BQ
         )


bonuscut_points_small <- c(0, 70, 150, 300, 500)
bonuscut_points_other <- c(0, 300, 600, 1000)

  
small_VMBSVFCIVASOB <- admin_df %>%
  filter(rptpro_tipo_concurso != "Otros Interesados")%>%
  mutate(VMBS_contest = case_when(
    between(rptpro_monto_total, bonuscut_points_small[1], bonuscut_points_small[2]) ~ 100,
    between(rptpro_monto_total, bonuscut_points_small[2], bonuscut_points_small[3]) ~ 75,
    between(rptpro_monto_total, bonuscut_points_small[3], bonuscut_points_small[4]) ~ 50,
    between(rptpro_monto_total, bonuscut_points_small[4], bonuscut_points_small[5]) ~ 25,
    rptpro_monto_total > bonuscut_points_small[5] ~ 15
  )
  )%>%
  select(rptpro_id, VMBS_contest)


other_VMBSVFCIVASOB <- admin_df %>%
  filter(rptpro_tipo_concurso == "Otros Interesados")%>%
  mutate(VMBS_contest = case_when(
    between(rptpro_monto_total, bonuscut_points_other[1], bonuscut_points_other[2]) ~ 100,
    between(rptpro_monto_total, bonuscut_points_other[2], bonuscut_points_other[3]) ~ 75,
    between(rptpro_monto_total, bonuscut_points_other[3], bonuscut_points_other[4]) ~ 50,
    rptpro_monto_total > bonuscut_points_other[4] ~ 25
  )
  )%>%
  select(rptpro_id, VMBS_contest)
  
together_VMBSVFCIVASOB <- admin_df %>%
  mutate(VMBS = case_when(
    between(rptpro_monto_total, bonuscut_points_other[1], bonuscut_points_other[2]) ~ 100,
    between(rptpro_monto_total, bonuscut_points_other[2], bonuscut_points_other[3]) ~ 75,
    between(rptpro_monto_total, bonuscut_points_other[3], bonuscut_points_other[4]) ~ 50,
    rptpro_monto_total > bonuscut_points_other[4] ~ 25
  ),
  VFCI = case_when(
    rptpro_aporte/rptpro_monto_total > 0.5 ~ 100,
    between(rptpro_aporte/rptpro_monto_total, 0.3, 0.5) ~ 75,
    between(rptpro_aporte/rptpro_monto_total, 0.2, 0.3) ~ 50,
    between(rptpro_aporte/rptpro_monto_total, 0.1, 0.2) ~ 25
  ),
  VFCI = replace_na(VFCI, 15),
  VASOB = ifelse(is.na(rptpro_bono_segun_saff), 0, 100)
  )%>%
  select(rptpro_id, VMBS, VFCI, VASOB)
  
score_VP_together <- together_VMBSVFCIVASOB %>%
  full_join(score_VBSU, by = "rptpro_id")%>%
  group_by(rptpro_id)%>%
  slice_head()%>%
  ungroup
  
  
together_score_all <- admin_df %>%
  select(rptpro_id, rptpro_tipo_concurso, cpov_pct_2007, rptpro_puntaje, received_bonus)%>%
  group_by(rptpro_id)%>%
  slice_head()%>%
  ungroup %>%
  left_join(score_VP_together, by = "rptpro_id")%>%
  left_join(VI_score_together, by = "rptpro_id")%>%
  left_join(VT_score, by = "rptpro_id")%>%
  mutate(VP = 0.45*VBSU + 0.35*VASOB + 0.1*VMBS + 0.1*VFCI,
         rptpro_puntaje2 = 0.45*VT + 0.2*VI + 0.35*VP,
         rptpro_puntaje3 = 0.45*VT + 0.35*VP)
  
  
  
  
  
  
  
  
  
  
  

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
  
#%%%%  
# First get rankings for everyone
#%%%%
  
# number of smallholders
n_smallholders <- together_score_all %>% 
  filter(rptpro_tipo_concurso != "Otros Interesados")%>%
  nrow()
  
scores_all <- together_score_all %>%
  mutate(#combined_rank = rank(desc(rptpro_puntaje)),
         #combined_rank_pctl = percent_rank(combined_rank),
         combined_rank = rank(desc(rptpro_puntaje2), ties.method = "random"),
         combined_rank_pctl = percent_rank(combined_rank),
         combined_VT_rank = rank(desc(VT), ties.method = "random"),
         combined_VT_rank_pctl = percent_rank(combined_VT_rank)
         )%>%
  group_by(rptpro_tipo_concurso)%>%
  mutate(contest_rank = rank(desc(rptpro_puntaje)),
         contest_VT_rank = rank(desc(VT), ties.method = "random")
         )%>%
  ungroup%>%
  mutate(real_rank = ifelse(rptpro_tipo_concurso == "Otros Interesados", contest_rank + n_smallholders, contest_rank),
         real_rank_pctl = percent_rank(real_rank),
         VT_rank = ifelse(rptpro_tipo_concurso == "Otros Interesados", contest_VT_rank + n_smallholders, contest_VT_rank),
         VT_rank_pctl = percent_rank(VT_rank),
         cpov_pct_2007 = as.numeric(cpov_pct_2007),
         Smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", 1, 0)) %>% drop_na(cpov_pct_2007) 

# real_rank_binned <- scores_all %>%
#   select(cpov_pct_2007, Smallholder, real_rank) %>%
#   mutate(quantile_bin = ntile(real_rank, n = 5))%>%
#   group_by(quantile_bin) %>%
#   summarise(real_cpov = mean(cpov_pct_2007, na.rm = T),
#             real_small_prop = mean(Smallholder, na.rm = T))
# 
# combined_rank_binned <- scores_all %>%
#   select(cpov_pct_2007, Smallholder, combined_rank) %>%
#   mutate(quantile_bin = ntile(combined_rank, n = 5))%>%
#   group_by(quantile_bin) %>%
#   summarise(combined_cpov = mean(cpov_pct_2007, na.rm = T),
#             combined_small_prop = mean(Smallholder, na.rm = T))
# 
# scores_all %>%
#   select(cpov_pct_2007, Smallholder, combined_VT_rank) %>%
#   mutate(quantile_bin = ntile(combined_VT_rank, n = 5))%>%
#   group_by(quantile_bin) %>%
#   summarise(combined_VT_cpov = mean(cpov_pct_2007, na.rm = T),
#             combined_VT_small_prop = mean(Smallholder, na.rm = T))%>%
#   left_join(real_rank_binned, by = "quantile_bin") %>%
#   left_join(combined_rank_binned, by = "quantile_bin")%>%
#   mutate(combined_VT_cpov = combined_VT_cpov - real_cpov,
#          combined_VT_small_prop = combined_VT_small_prop - real_small_prop,
#          combined_cpov = combined_cpov - real_cpov,
#          combined_small_prop = combined_small_prop - real_small_prop)%>%
#   ggplot(aes(x = quantile_bin, y = combined_small_prop)) +
#   geom_point()+
#   geom_line()



score_all <- scores_all%>%
  pivot_longer(cols = c("real_rank_pctl", 
                        "VT_rank_pctl", 
                        "combined_rank_pctl", 
                        "combined_VT_rank_pctl"), names_to = "rank_type", values_to = "rank_pctl")


export(scores_all, paste0(clean_data_dir, "/scores_all.rds"))

score_all %>%
  filter(rank_type %in% c("real_rank_pctl", "combined_rank_pctl"))%>%
  ggplot(aes(x = rank_pctl, y = Smallholder, color = rank_type
       )) +
  geom_smooth(method = lm)+
  #geom_smooth()+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = T, linetype = 2)+
  theme_classic()+
  theme(legend.position = "top")+
  guides(color=guide_legend(title="Scoring method",
                            title.position="top", title.hjust = 0.5,
                            override.aes=list(fill=NA)
                            )
         )+
  scale_x_continuous(labels = scales::percent,
                     breaks = c(.25, .50, .75),
                     name = "Application rank percentile (higher = better)")+
  ylab("Proportion smallholders")+
  scale_color_manual(values = c(palette$dark, palette$blue)
                     , breaks = c("real_rank_pctl", "combined_rank_pctl"),
                     labels = c("Actual", "Combined contests")
  )
ggsave(paste0(here("analysis_main", "figs"), "/priority_score_smallholder.png"), width = 6, height = 7)


priority_cpov_plot <- ggplot(score_all, 
       aes(x = cpov_pct_2007, y = rank_pctl, color = rank_type
       )) +
 # geom_smooth(n=6)+
 # geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = T, alpha = 0.25, linetype = 2)+
  geom_smooth(method = lm, alpha = .25)+
  theme_classic()+
  theme(legend.position = "top")+
  guides(color=guide_legend(title="Scoring method",
                            title.position="top", title.hjust = 0.5, 
                            override.aes=list(fill=NA)
  )
                            )+
  scale_y_continuous(labels = scales::percent,
                     name = "Application rank percentile (higher = better)")+
  xlab("Comuna poverty (%)")+
  scale_color_manual(values = c(palette$dark, palette$blue, palette$gold, palette$red)
                     , breaks = c("real_rank_pctl", "VT_rank_pctl", "combined_rank_pctl", "combined_VT_rank_pctl"),
                     labels = c("Actual", "Removed social components", "Combined contests", "Combined contests &\nremoved social components")
  )
priority_cpov_plot

ggsave(paste0(here("analysis_main", "figs"), "/priority_score_cpov.png"), width = 6, height = 7)






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Counterfactual impact analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alpha = -0.080739
beta_cpov = 0.027469
beta_small = 0.030873  
beta_smallxlogcpov = -0.004330
  
  
scores_counterfactual <- scores_all %>%
  mutate(log_cpov = log(cpov_pct_2007 + 0.01))%>%
  select(Smallholder, log_cpov, real_rank_pctl, combined_rank_pctl, VT_rank_pctl, combined_VT_rank_pctl)%>%
  mutate(treatment_effect = alpha + beta_cpov*log_cpov + beta_small*Smallholder + beta_smallxlogcpov*Smallholder*log_cpov)%>%
  pivot_longer(cols = 
                 c("real_rank_pctl", "VT_rank_pctl", "combined_rank_pctl", "combined_VT_rank_pctl"), 
               names_to = "rank_type", values_to = "rank_pctl"
               )



counterfactual_impact_plot <- ggplot(scores_counterfactual, 
                             aes(x = rank_pctl, y = treatment_effect, color = rank_type
                                # , linetype = as.factor(Smallholder)
                             )) +
  #geom_smooth(n=5, linewidth = 1.5)+
  #geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = F)+
  geom_smooth(linewidth = 2)+
  theme_classic(16)+
  theme(legend.position = "top")+
  guides(color=guide_legend(title="Scoring method",
                            title.position="top", title.hjust = 0.5, 
                            override.aes=list(fill=NA)
  )
  )+
  scale_x_continuous(labels = scales::percent,
                     name = "Applicant priority percentile")+
  ylab("Expected treatment effect (Forest)")+
  scale_color_manual(values = c(palette$dark, palette$blue, palette$gold, palette$red)
                     , breaks = c("real_rank_pctl", "VT_rank_pctl", "combined_rank_pctl", "combined_VT_rank_pctl"),
                     labels = c("Actual", "       Removed\nsocial components", "Combined contests", "Combined contests\n       & removed\nsocial components")
  )
counterfactual_impact_plot
  
ggsave(paste0(here("analysis_main", "figs"), "/counterfactual_impact_plot.png"), width = 7.5, height = 7.5)

scores_counterfactual_binned <- scores_counterfactual %>%
  group_by(rank_type)%>%
  mutate(bin = case_when(
    rank_pctl < 0.2 ~ 0.1,
    rank_pctl < 0.4 & rank_pctl >= 0.2 ~ 0.3,
    rank_pctl < 0.6 & rank_pctl >= 0.4 ~ 0.5,
    rank_pctl < 0.8 & rank_pctl >= 0.6 ~ 0.7,
    rank_pctl >= 0.8 ~ 0.9,
  )
         )%>%
  ungroup %>% 
  group_by(rank_type, bin)%>%
  summarise(treatment_effect = mean(treatment_effect, na.rm = T)
            )%>%
  ungroup


ggplot(data = scores_counterfactual_binned,
       aes(x = bin, y = treatment_effect, color = rank_type)
       )  +
  geom_point()
  
  
  
  
  
  
  
