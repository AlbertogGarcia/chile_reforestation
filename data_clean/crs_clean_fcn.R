# this function converts the projections for each property into the same projection

library(tidyverse)
library(stringi)

crs_clean_fcn <- function(df){
  
  #if datum contains, 84, wgs, or WGS, change the name to WGS84
  df <- df %>%
    mutate(datum = gsub(".*(84|wgs|WGS).*", "WGS84", datum, ignore.case = T)) %>%
    mutate(datum = gsub(".*(56|PSAD|PASAD|PSDA).*", "PSAD56", datum, ignore.case = T)) %>%
    mutate(datum = gsub(".*(69|SAD|Sad).*", "SAD69", datum, ignore.case = T)) %>%
    filter(datum == "WGS84" | datum == "SAD69" | datum == "PSAD56")

  df_sad69 <- df %>%
    filter(datum == "SAD69") %>%
    mutate(huso =
      ifelse(is.na(huso) & easting > 600000 & northing > 5000000, 18, huso )
    ) %>%
    mutate(huso =
              ifelse(is.na(huso), 19, huso )
    )
  
  df_sad69_18 <- df_sad69 %>%
    filter(huso == 18) %>%
    st_as_sf(coords = c( "easting", "northing"), na.fail = FALSE) %>%
    st_set_crs("+proj=utm +zone=18 +south +ellps=aust_SA +units=m +no_defs") %>%
    st_transform("+proj=utm +zone=19 +south +ellps=aust_SA +units=m +no_defs") #transform crs
  
  df_sad69_19 <- df_sad69 %>%
    filter(huso == 19) %>%
    st_as_sf(coords = c( "easting", "northing"), na.fail = FALSE) %>%
    st_set_crs("+proj=utm +zone=19 +south +ellps=aust_SA +units=m +no_defs") #set crs
  
  
  df_wgs84 <- df %>%
    filter(datum == "WGS84") %>%
    mutate(huso =
             ifelse(is.na(huso) & easting > 600000 & northing > 5000000, 18, huso )
    ) %>%
    mutate(huso =
             ifelse(is.na(huso), 19, huso )
    ) 
   
  df_wgs84_18 <- df_wgs84 %>%
    filter(huso == 18) %>%
    st_as_sf(coords = c( "easting", "northing"), na.fail = FALSE) %>%
    st_set_crs("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs") %>%
    st_transform("+proj=utm +zone=19 +south +ellps=aust_SA +units=m +no_defs") #transform crs
  
  df_wgs84_19 <- df_wgs84 %>%
    filter(huso == 19) %>%
    st_as_sf(coords = c( "easting", "northing"), na.fail = FALSE) %>%
    st_set_crs("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs") %>%
    st_transform("+proj=utm +zone=19 +south +ellps=aust_SA +units=m +no_defs") #transform crs
   
  new_df.sf <- df_wgs84_19 %>%
    rbind(df_wgs84_18) %>%
    rbind(df_sad69_19) %>%
    rbind(df_sad69_18)
  
  #outputs = list('new_df.sf' = new_df.sf)
  #return(outputs)
  return(new_df.sf)
}
