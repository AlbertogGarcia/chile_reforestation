library("openxlsx")
library(tidyverse)
library(stringi)
library(stringdist)
# specifying the path name
path <- "C:/Users/garci/Dropbox/indap_subsidios.xlsx"
df <- data.frame()
for(x in 1:length(c(2008:2021))) {
  subsidios_indap <- rbind(read.xlsx(path
, sheet = x))
} 

library(rio)
export(subsidios_indap, "subsidios_indap.rds")

subsidios_indap <- subsidios_indap %>%
  separate(APELLIDOS, into = c("apellido_paterno", "apellido_materno"), sep = " ")%>%
  separate(NOMBRES, into = c("nombre1", "nombre2"), sep = " ")

proyecto <- read.xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/proyecto.xlsx")
propietario <- read.xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/propietario.xlsx")
predio <- read.xlsx("C:/Users/garci/Dropbox/chile_reforestation/external_data/concurso_conaf/program/predio.xlsx")

# fcn to remove accents
loweraccents <- function(x){
  x <- tolower(stri_trans_general(x, id = "Latin-ASCII"))
}

forestlaw <- proyecto %>%
  inner_join(predio, by = c("rptpro_id"))%>%
  inner_join(propietario, by = c("rptpre_id"))
  
  
forestlaw_names <- forestlaw %>%  
  mutate_at(vars(rptpre_comuna, rptprop_apellido_paterno, rptprop_apellido_materno, rptprop_nombre), loweraccents)%>%
  select(rptpre_id, rptpre_comuna, rptprop_apellido_paterno, rptprop_apellido_materno, rptprop_nombre)%>%
  separate(rptprop_nombre, into = c("rptprop_nombre1", "rptprop_nombre2"), sep = " ") 

subsidios <- subsidios_indap %>%
  mutate_at(vars(REGION, DESAREA, apellido_paterno, apellido_materno, nombre1, nombre2), loweraccents)%>%
  inner_join(forestlaw_names, by = c("DESAREA" = "rptpre_comuna"))%>%
  mutate(paterno_dist = stringdist(rptprop_apellido_paterno, apellido_paterno),
         materno_dist = stringdist(rptprop_apellido_materno, apellido_materno),
         nombre1_dist = stringdist(nombre1, rptprop_nombre1),
         nombre2_dist = stringdist(nombre2, rptprop_nombre2)
         )

subsidios_matches <- subsidios %>%
  filter(paterno_dist + materno_dist <= 2)%>%
  filter(nombre1_dist <= 1 | nombre1_dist + nombre2_dist <= 2)%>%
  group_by(rptpre_id)%>%
  filter(paterno_dist + materno_dist + nombre1_dist == min(paterno_dist + materno_dist + nombre1_dist))%>%
  ungroup()%>%
  separate(Nombre_Programa, into = c("program key", "program"), sep = " - ")%>%
  mutate(subsidy = 1)%>%
  select(rptpre_id, program, rutinnominado, Monto, FECHA, REGION, DESAREA, apellido_paterno, apellido_materno, nombre1, nombre2, subsidy)
  
cross <- forestlaw %>%
  left_join(subsidios_matches, by = "rptpre_id")%>%
  mutate(has_mgmt_plan = ifelse(rptpro_tiene_plan_saff == "Si", 1, 0),
         ag_subsidy = ifelse(is.na(subsidy), 0, subsidy),
         Monto = ifelse(is.na(Monto), 0, Monto))

mod <- lm(has_mgmt_plan ~ ag_subsidy:Monto + rptpro_tipo_concurso + rptpro_monto_total + as.factor(rptpre_region), data = cross)
summary(mod)

