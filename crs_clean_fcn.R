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
  
  #turn into spatial object
  df.sf <- st_as_sf(df, coords = c( "easting", "northing"), na.fail = FALSE)
 
   df_grouped <- df.sf %>%
    group_split(datum, huso)
  
  df_grouped[[1]] <- df_grouped[[1]] %>%
    st_set_crs("+proj=utm +zone=19 +south +datum=WGS84") #set crs 
  
  df_grouped <- df_grouped %>%
    st_transform(st_crs())   #change crs to same
  
  
  new_df.sf <- df_grouped %>%
    ungroup()
  
  outputs = list('new_df.sf' = new_df.sf)
  return(outputs)
}