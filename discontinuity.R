library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables
library(ggplot2)

regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)
setwd("C:/Users/garci/Dropbox")
NFL_df <- readRDS("chile_collab/input_files/NFL_df.rds")

discontinuity_main <- NFL_df %>%
  rename(property_size = rptpre_superficie_predial)%>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         sub_contest = ifelse( rptpro_tipo_presenta == "Extensionista", paste0(rptpro_tipo_concurso, " w/ extentionist"), paste0(rptpro_tipo_concurso, " w/o extentionist")),
         size_cutoff = ifelse(
           rptpro_numero_region %in% regions_200, 200,
           ifelse(rptpro_numero_region %in% regions_800, 800, 500)
         ),
         size_centered = property_size - size_cutoff,
         below_cutoff = property_size <= size_cutoff,
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", 0, 1)
  )%>%
  drop_na(received_bonus)%>%
  filter(rptpro_ano < 2019)

discontinuity_main %>%
  group_by(rptpro_tipo_concurso, property_size <= size_cutoff) %>% 
  summarize(count = n()) %>% 
  group_by(rptpro_tipo_concurso) %>% 
  mutate(prop = count / sum(count))

ggplot(discontinuity_main, aes(x = property_size)) +
  geom_histogram(binwidth = 20, color = "white", boundary = 200) + 
  geom_vline(xintercept = 200) + 
  labs(x = "property size")+xlim(0, 400)

test_density <- rddensity(discontinuity_main$property_size, c = 200)
summary(test_density)
plot_density_test <- rdplotdensity(rdd = test_density, 
                                   X = discontinuity_main$property_size,
                                   type = "both")  # This adds both points and lines

ggplot(discontinuity_main, aes(x = property_size, y = received_bonus, color = rptpro_tipo_concurso)) +
  geom_point(alpha = 0.5) + 
  stat_summary_bin(fun = "mean", geom = "line", alpha = 0.5, bins = 50)+
  # Add a line based on a linear model for the properties less than 200
  geom_vline(xintercept = 200) +
  labs(x = "property size", y = "received bonus", color = "contest type")+
  xlim(0, 400)

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the contest column, since most people are compliers. But some people didn’t comply,

library(estimatr)

model_fuzzy_150 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, size_centered >= -150 & size_centered <= 150)# & rptpro_numero_region %in% regions_200)
)
tidy(model_fuzzy_150)

model_fuzzy_100 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, size_centered >= -100 & size_centered <= 100)# & rptpro_numero_region %in% regions_200)
)
tidy(model_fuzzy_100)

model_fuzzy_75 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, size_centered >= -75 & size_centered <= 75)# & rptpro_numero_region %in% regions_200)
)
tidy(model_fuzzy_75)

model_donut_150 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 15, 150) | between(size_centered, -150, -15) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_150)

model_donut_100 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 10, 100) | between(size_centered, -100, -10) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_100)

model_donut_75 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 10, 75) | between(size_centered, -75, -10) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75)

model_donut_75b <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 15, 75) | between(size_centered, -75, -15) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75b)

model_donut_75c <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 20, 75) | between(size_centered, -75, -15) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75c)

modelsummary(list("Bandwidth = 150" = model_fuzzy_150, 
                  "Bandwidth = 75" = model_fuzzy_75,
                  "Bandwidth = 150, donut (15)" = model_donut_150,
                  "Bandwidth = 75, donut (10)" = model_donut_75,
                  "Bandwidth = 75, donut (15)" = model_donut_75b,
                  "Bandwidth = 75, donut (20)" = model_donut_75c))

# Based on this model, using below_cutoff as an instrument, 
# we can see that the coefficient for smallholder is different now! 
# It’s .47, which means that the smallholder contest causes an increased follow through of 47%
# for compliers in the bandwidth.


# non-parametric rd on full sample
nonprdd_df <- discontinuity_main 

donut_size = 25

donut_nonprdd_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
                   )


nonp_rd <- rdrobust(y = nonp_rd_df$received_bonus, x = nonp_rd_df$size_centered, c = 0,
         fuzzy = nonp_rd_df$smallholder) %>% 
  summary()

donut_nonp_rd <- rdrobust(y = donut_nonprdd_df$received_bonus, x = donut_nonprdd_df$size_centered, c = 0,
                    fuzzy = donut_nonprdd_df$smallholder) %>% 
  summary()


###############################################################################
### EVI as outcome
###############################################################################
library(sf)
my_rol_match <- data.frame(readRDS("chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

native_forest_law <- NFL_df %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")%>%
  mutate(submitted_mp = ifelse( rptpro_tiene_plan_saff=="Si" | rptpro_tiene_bonificacion_saff == "Si", 1, 0),
         treat = 1,
         first.treat = rptpro_ano)%>%
  filter(submitted_mp == 1)

length(unique(native_forest_law$rptpro_id))

rol_priority <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "spatial", 1, 0),
         max_priority = max(priority))%>%
  filter(priority == max_priority)%>%
  filter(area_diff == min(area_diff))%>%
  ungroup()%>%
  distinct(rptpro_id, .keep_all = TRUE)

evi_discontinuity <- rol_priority
