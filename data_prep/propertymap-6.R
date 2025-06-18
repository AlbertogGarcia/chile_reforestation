library(terra)
library(tmap)
library(tmaptools)
library(here)
library(sf)
library(rlist)
library(sjmisc)
library(tidyverse)
library(htmltools)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark_grey" = "grey30",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                #"base" = "#F5F18550",
                "base" = "#FDFBD4")

clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")
output_dir <- clean_data_dir


# Load matched properies (created in ciren_matches-2.R)
enrolled_properties <- st_read(paste0(output_dir, "/enrolled_match_rol.shp"))%>%
  select(rptpro_ano)

ciren_path <- paste0(my_data_dir, "/external_data/ciren_simef/PROPIEDADES_RURALES")
file_list <- list.files(ciren_path, pattern = "*shp", full.names = TRUE)

roi <- c("BIOBÍO" ,"ARAUCANÍA")

file_list_roi <- list.filter(file_list, str_contains(., roi, logic = "OR"))

my_read <- function(x){
  temp <- st_read(x) %>% 
    st_make_valid()%>%
    mutate(ciren_region = substr(x, nchar(ciren_path)+2 , nchar(x) - 15),
           polyarea = st_area(.))
  
  return <- temp#[!lengths(st_intersects(temp, enrolled_sf)), ]
  return(return)
}

enrolled_sf <- enrolled_properties

nonparticipant_list <- lapply(file_list_roi, my_read)

araucania_properties <- nonparticipant_list[[1]] 
biobio_properties <- nonparticipant_list[[2]] 

library(tmap)
library(basemaps)

Chile.shp <- read_sf(
  here::here(my_data_dir, "external_data", "administrative_boundaries", "chl_adm_2021_shp", "chl_admbnda_adm0_bcn_20211008.shp")
)

cities.shp <- read_sf(
  here::here(my_data_dir, "external_data", "administrative_boundaries", "places", "places.shp"))%>%
    filter(name %in% c("Santiago"))

enrolled_properties_webmerc<- enrolled_properties %>%
  st_transform(crs = 3857)
#, "Talca", "Temuco", "Concepción"))))

# Create sfc object with multiple sfg objects
# points_sfc <- st_sfc(st_point(c(-69.5, -37.47049)), 
#                      #st_point(c(-72.35168, -38)), 
#                        crs = 4326)
# countryname.shp <- st_sf(data.frame(name = c("Argentina")#, "Chile")
#                                     ), geometry = points_sfc)


library(sp)

e_inset <- as(raster::extent(-72.33, -71.93, -39, -38.5), "SpatialPolygons")
proj4string(e_inset) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# tmap_mode("view")
# tm_shape(bg) + tm_rgb()+
# tm_shape(e_inset) + tm_borders() +
#  # tm_shape(araucania_properties) + tm_borders(lwd = 0.25)+
#   tm_shape(enrolled_properties) + 
#   tm_fill(col = "rptpro_ano",
#           breaks = Breaks, labels = Labels,
#           palette = c("#ed195a", "#1c86ee")
#   )

tmap_mode("plot")

inset_map <- tm_shape(e_inset) + tm_polygons(col = palette$base, alpha = 0.5)+
  tm_shape(araucania_properties %>% st_transform(crs = 3857)) + tm_borders(lwd = 0.4)+
  tm_shape(enrolled_properties_webmerc) + 
  tm_fill(col = "rptpro_ano",
          palette = c(palette$red, palette$blue),
          legend.show = F
  )+
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0))
#inset_map







e <- as(raster::extent(-77, -68, -43.5, -32), "SpatialPolygons")
e <- as(raster::extent(-78.5, -69.5, -43.5, -32), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# tmap_mode("view")
# roi_map <- tm_shape(e) +
#   tm_borders()
#   #tm_shape(extent_roi) +
#  # tm_polygons(col = "grey85")
# roi_map

tmap_mode("plot")

get_maptypes()

# extent_roi_webmerc <- extent_roi %>%
#   st_transform(crs = 3857)

bg <- basemaps::basemap_terra(ext=e, 
                              map_service = "carto", map_type = "voyager_no_labels"
)


# bg_labels <- basemaps::basemap_terra(ext=e,
#                                      map_service = "esri", 
#                                    #  map_type = "world_boundaries_and_places_alternate"
#                                      map_type = "world_reference_overlay"
# )%>% terra::intersect(Chile.shp)

Breaks = seq(from = 2009, to = 2020)
Labels = as.character(Breaks)


set.seed(0930)
chile_map <- tm_shape(bg) + tm_rgb()+
  tm_shape(Chile.shp) + tm_polygons(col = palette$base, alpha = 0.4)+
  tm_shape(
    enrolled_properties_webmerc %>% 
      rbind(araucania_properties[1,] %>% mutate(rptpro_ano = NA) %>% select(rptpro_ano)%>% st_transform(crs = 3857))
  ) + 
  tm_fill(col = "rptpro_ano",
          breaks = Breaks, labels = Labels,
          palette = c(palette$red, palette$blue),
          title = "Property enrollment year",
          textNA = "Unenrolled properties", 
          colorNA = palette$base
  )+
  tm_shape(e_inset) + tm_borders(lwd = 1.5, col = palette$dark)+
  tm_shape(cities.shp) + tm_dots(size = .1) +
  tm_shape(cities.shp %>% st_jitter(0.3)) + tm_text("name", size = 3/4) +
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) +
  tm_scale_bar(breaks = c(0, 50, 100))+
  tm_compass(type = "4star", size = 1, position = c("right", "top"))
#chile_map

#%%%%%%%%%%%%%%%%%%%%%%%%
#### Mini inset in context of south america
#%%%%%%%%%%%%%%%%%%%%%%%%
library(rnaturalearth)
bg_mini <- basemaps::basemap_terra(ext=ne_countries(continent = "South America", returnclass = 'sf'), 
                                   map_service = "carto", map_type = "voyager_no_labels"
)

SA_countries <- ne_countries(returnclass = 'sf')%>%
  terra::vect()%>%
  terra::project(terra::crs(bg_mini))%>%
  terra::crop(st_bbox(bg_mini))%>%
  st_as_sf()


mini_inset <- tm_shape(bg_mini) + tm_rgb()+
  tm_shape(SA_countries) + tm_polygons(col = palette$light_grey)+
  tm_shape(e) + tm_borders(lwd = 1.5, col = palette$red)+
  tm_layout(legend.outside = FALSE,
            inner.margins = c(0,0,0,0),
            outer.margins = c(0,0,0,0)) 
mini_inset
#%%%%%%%%%%%%%%%%%%%%%%%%
#### Combine all maps into single png
#%%%%%%%%%%%%%%%%%%%%%%%%

library(grid)
# 1. Open png file
png(paste0(here("analysis_main", "figs"), "/propertymap.png")
    , width = 21, height = 17, res = 300, units = "cm")

grid.newpage()

print(chile_map, vp = viewport(0.25, 0.5)) #x,y coordinates of the grid here
print(inset_map, vp = viewport(0.75, 0.5)) #x,y coordinates of the grid here
print(mini_inset, vp = viewport(0.06, 0.115, height = 0.2))
# 3. Close the file
dev.off()

