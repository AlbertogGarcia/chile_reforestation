library(tidyverse)
library(stringi)
library(readxl)

my_data_dir <- here::here("remote")
output_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

select <- dplyr::select

accents <- function(x){
  x <- stri_trans_general(x, id = "Latin-ASCII")
}




data_enrolled <- readRDS(paste0(output_dir, "/covariates_enrolled.rds"))%>%
  mutate(treat = 1,
         pre_comuna = accents(tolower(pre_comuna)))

data_neverenrolled <- readRDS(paste0(output_dir, "/covariates_neverenrolled.rds"))%>%
  mutate(treat = 0,
         first.treat = 0,
         pre_comuna = accents(tolower(desccomu)))%>% 
  mutate_at("polyarea", as.double)



# xls files downloaded from CONAF
#stands
rodal_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/rodal.xlsx"))
#activities
actividades_df <- read_xlsx(paste0(my_data_dir, "/external_data/concurso_conaf/program/actividad.xlsx"))


property_df <- readRDS(paste0(output_dir, "/property_df.rds")) %>%
  mutate(pre_comuna = accents(tolower(rptpre_comuna)),
         timber = as.numeric(ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0)),
         proportion_subsidized = ifelse(rptpre_superficie_predial != 0, rptpro_superficie / rptpre_superficie_predial,NA),
         extensionista = ifelse(rptpro_tipo_presenta == "Extensionista", 1, 0)
  )%>%
  group_by(pre_comuna, rptpre_id, ROL)%>%
  slice_head()

activity_df <- rodal_df %>%
  full_join(actividades_df, by = "rptro_id") %>%
  mutate(TRUE_col = TRUE)%>%
  select(rptac_tipo, rptpre_id, TRUE_col
         )%>%
  distinct()%>%
  pivot_wider(names_from = rptac_tipo, values_from = TRUE_col, values_fill = FALSE)


enrolled_data <- data_enrolled %>%
  left_join(property_df, by = c("rptpre_id", "rptpro_id", "pre_comuna", "ROL", "rptpro_ano"))%>%
  group_by(ID)%>%
  mutate(first.treat = min(rptpro_ano))%>%
  filter(rptpro_ano == min(rptpro_ano))%>%
  slice_head()%>%
  ungroup %>%
  left_join(activity_df, by = "rptpre_id")

comunal_pov <- readxl::read_excel(paste0(my_data_dir,"/data/analysis_lc/comunal_poverty.xlsx")
                        )%>%
  mutate(comuna = tolower(accents(comuna)))

table(comunal_pov$comuna)

all_property_wide <- enrolled_data %>%
  bind_rows(data_neverenrolled)%>%
  select(- c(Trees_1999:`0_1999`))%>%
  group_by(rptpre_id, objectid, treat, ciren_region)%>%
  dplyr::mutate(property_ID = cur_group_id())%>%
  ungroup %>%
  left_join(comunal_pov, by = c("pre_comuna" = "comuna"))%>%
  mutate_at(vars(cpov_index_2007, cpov_pct_2007), as.numeric)
 
table(all_property_wide$first.treat)
summary(all_property_wide$cpov_index_2007)

library(rio)
export(all_property_wide, paste0(output_dir, "/all_property_wide.rds"))

all_property_share_wide <- all_property_wide %>%
  mutate(lu_pixels_count = Forest + Plantation + Urban + Water + `Snow/ice` + `No data` + `Pasture and ag` + Shrub + `Permanent bare soil` + `Temporary bare soil`)%>%
  mutate_at(vars(Trees_2000:`0_2018`), ~ . / pixels_count)%>%
  mutate_at(vars(Forest, Plantation), ~ . / lu_pixels_count)%>%
  mutate(property_hectares = pixels_count / 0.09 ,
         ind_dist = unlist(ind_dist),
         natin_dist = unlist(natin_dist),
         city_dist = unlist(city_dist)
  )

export(all_property_share_wide, paste0(output_dir, "/all_property_share_wide.rds"))

# property_share_long <- all_property_share_wide %>%
#   pivot_longer(Trees_2000:`0_2018`)%>%
#   separate(name, into = c("class", "Year"), sep = "_") %>%
#   pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

