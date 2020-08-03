# this function converts the projections for each property into the same projection

library(tidyverse)
library(stringi)
library(stringdist)

join_metrics <- function(df){
  
  byregion_assess <- df %>%
    select(rptpro_id, rptpre_region, ROL, rptpre_rol, NOM_PREDIO, rptpre_nombre) %>%
    separate(col = ROL, into = c("partial_rol_CIREN", NA), sep = "-", remove = FALSE) %>%
    separate(col = rptpre_rol, into = c("partial_rol_NFL", NA), sep = "-", remove = FALSE) %>%
    mutate(partial_rol_match = 
             ifelse(partial_rol_CIREN==partial_rol_NFL, 1, 0)
    ) %>%
    mutate(rol_match = 
             ifelse(ROL==rptpre_rol, 1, 0)
    ) %>%
    mutate(rptpre_nombre =
             tolower(rptpre_nombre)
    ) %>%
    mutate(NOM_PREDIO =
             tolower(NOM_PREDIO)
    ) %>%
    mutate(stringd =
             stringdist(NOM_PREDIO, rptpre_nombre, method = "dl")
    ) %>%
    mutate(under10 = 
             ifelse(stringd <=10, 1, 0)) 

  
  return(byregion_assess)
}
