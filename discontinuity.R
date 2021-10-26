library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(rddensity)  # For nonparametric regression discontinuity density tests
library(modelsummary)  # Create side-by-side regression tables
library(ggplot2)
library(bunchr)
regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)
setwd("C:/Users/garci/Dropbox")
NFL_df <- readRDS("chile_collab/input_files/NFL_df.rds")

assistance_df <- NFL_df %>%
  rename(property_size = rptpre_superficie_predial)%>%
  mutate(size_cutoff = ifelse(
    rptpro_numero_region %in% regions_200, 200,
    ifelse(rptpro_numero_region %in% regions_800, 800, 500)
  ),
  size_centered = property_size - size_cutoff)%>%
  drop_na(rptpro_monto_asistencia_tecnica)
table(assistance_df$rptpro_monto_asistencia_tecnica)
table(assistance_df$rptpre_monto_asistencia_tecnica)
table(assistance_df$rptpro_monto_informe_ejecucion)
table(assistance_df$rptpro_monto_asesoria_total)
table(assistance_df$rptpro_monto_elaboracion_plan)

discontinuity_main <- NFL_df %>%
  rename(property_size = rptpre_superficie_predial)%>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         size_cutoff = ifelse(
           rptpro_numero_region %in% regions_200, 200,
           ifelse(rptpro_numero_region %in% regions_800, 800, 500)
         ),
         size_centered = property_size - size_cutoff,
         below_cutoff = property_size <= size_cutoff,
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", 0, 1)
  )

bunch_viewer(subset(discontinuity_main, rptpro_numero_region %in% regions_200 & property_size < 300 & property_size > 50 )$property_size, 200, cf_start = 10, cf_end = 10, exclude_before = 3,
             exclude_after = 3, binw = 5)

discontinuity_main %>%
  group_by(rptpro_tipo_concurso, property_size <= size_cutoff) %>% 
  summarize(count = n()) %>% 
  group_by(rptpro_tipo_concurso) %>% 
  mutate(prop = count / sum(count))

test_density <- rddensity(discontinuity_main$property_size, c = 200)
summary(test_density)
rdplotdensity(rdd = test_density, 
                                   X = discontinuity_main$property_size,
                                   type = "both",
              #title = "",
              xlabel = "reported property size",
              ylabel = "density")  # This adds both points and lines

test_density_adjusted <- rddensity(subset(discontinuity_main, property_size > 200 | property_size <= 197)$property_size, c = 200)
summary(test_density_adjusted)
rdplotdensity(rdd = test_density_adjusted, 
                                   X = subset(discontinuity_main, property_size > 200 | property_size <= 197)$property_size,
                                   type = "both")  # This adds both points and lines



discontinuity_with_bins <- discontinuity_main %>% 
  filter(rptpro_numero_region %in% c(5,6,7,8,9, 10, 14))%>%
  mutate(size_binned = cut(property_size, breaks = seq(0, 500, 25)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned, smallholder) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "smallholder", values_from = "n", values_fill = 0) %>% 
  rename(small_yes = `TRUE`, small_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_smallholder = small_yes / (small_yes + small_no),
         bin_end = as.numeric(size_binned)*25,
         below_cutoff = bin_end <= 200)%>%
  drop_na(size_binned)

# Plot this puppy
ggplot(discontinuity_with_bins, aes(x = bin_end, y = prob_smallholder, color = below_cutoff)) +
  geom_col(fill = "grey90") +
  geom_vline(xintercept = 210)+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

ggplot(discontinuity_with_bins, aes(x = bin_end, y = prob_smallholder, color = below_cutoff)) +
  geom_line(fill = "grey90") +
  geom_vline(xintercept = 210)+
  labs(x = "property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

discontinuity_with_bins2 <- discontinuity_main %>% 
  filter(rptpro_numero_region %in% c(11, 12))%>%
  mutate(size_binned = cut(property_size, breaks = seq(0, 1000, 50)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned, smallholder) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "smallholder", values_from = "n", values_fill = 0) %>% 
  rename(small_yes = `TRUE`, small_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_smallholder = small_yes / (small_yes + small_no),
         bin_end = as.numeric(size_binned)*50,
         below_cutoff = bin_end <= 200)%>%
  drop_na(size_binned)

# Plot this puppy
ggplot(discontinuity_with_bins2, aes(x = bin_end, y = prob_smallholder)) +
  geom_col(fill = "grey90", color = "orange") +
  geom_vline(xintercept = 225, linetype = "dashed")+
  geom_vline(xintercept = 825)+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the contest column, since most people are compliers. But some people didn’t comply,

library(estimatr)

model_fuzzy_150 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, size_centered >= -150 & size_centered <= 150)# & rptpro_numero_region %in% regions_200)
)
tidy(model_fuzzy_150)



model_fuzzy_75 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, size_centered >= -75 & size_centered <= 75)# & rptpro_numero_region %in% regions_200)
)
tidy(model_fuzzy_75)

model_donut_150 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 0, 150) | between(size_centered, -150, -5) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_150)

model_donut_150b <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 5, 150) | between(size_centered, -150, -5) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_150b)


model_donut_75 <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 0, 75) | between(size_centered, -75, -5) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75)

model_donut_75b <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 5, 75) | between(size_centered, -75, -5) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75b)

model_donut_75c <- iv_robust(
  received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
  data = filter(discontinuity_main, between(size_centered, 0, 75) | between(size_centered, -75, -10) )# & rptpro_numero_region %in% regions_200)
)
tidy(model_donut_75c)


modelsummary(list("Bandwidth = 150" = model_fuzzy_150, 
                  "Bandwidth = 75" = model_fuzzy_75,
                  "Bandwidth = 150, donut (5)" = model_donut_150,
                  "Bandwidth = 150, donut (5, both sides)" = model_donut_150b,
                  "Bandwidth = 75, donut (5)" = model_donut_75,
                  "Bandwidth = 75, donut (5, both sides)" = model_donut_75b,
                  "Bandwidth = 75, donut (10)" = model_donut_75c))

# Based on this model, using below_cutoff as an instrument, 
# we can see that the coefficient for smallholder is different now! 
# It’s .47, which means that the smallholder contest causes an increased follow through of 47%
# for compliers in the bandwidth.

# non-parametric rd on full sample
nonprdd_df <- discontinuity_main 
donut_size = 3

donut_3_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_3 <- rdrobust(y = donut_3_df$received_bonus, x = donut_3_df$size_centered, c = 0,
                         fuzzy = donut_3_df$smallholder) 

results_donut_3 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp_3$bws[1], "coeff" = donut_nonp_3$coef[1], "se" = donut_nonp_3$se[1], "pval" = donut_nonp_3$pv[1])
##############################################################################################

donut_size = 5

donut_5_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_5 <- rdrobust(y = donut_5_df$received_bonus, x = donut_5_df$size_centered, c = 0,
                          fuzzy = donut_5_df$smallholder)
results_donut_5 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp_5$bws[1], "coeff" = donut_nonp_5$coef[1], "se" = donut_nonp_5$se[1], "pval" = donut_nonp_5$pv[1])

###############################################################################################

donut_size = 10

donut_10_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_10 <- rdrobust(y = donut_10_df$received_bonus, x = donut_10_df$size_centered, c = 0,
                    fuzzy = donut_10_df$smallholder) 
results_donut_10 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp_10$bws[1], "coeff" = donut_nonp_10$coef[1], "se" = donut_nonp_10$se[1], "pval" = donut_nonp_10$pv[1])

###############################################################################################

donut_size = 25

donut_25_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_25 <- rdrobust(y = donut_25_df$received_bonus, x = donut_25_df$size_centered, c = 0,
                          fuzzy = donut_25_df$smallholder) 
results_donut_25 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp_25$bws[1], "coeff" = donut_nonp_25$coef[1], "se" = donut_nonp_25$se[1], "pval" = donut_nonp_25$pv[1])

###############################################################################################

donut_size = 50

donut_50_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_50 <- rdrobust(y = donut_50_df$received_bonus, x = donut_50_df$size_centered, c = 0,
                          fuzzy = donut_50_df$smallholder) 
results_donut_50 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp_50$bws[1], "coeff" = donut_nonp_50$coef[1], "se" = donut_nonp_50$se[1], "pval" = donut_nonp_50$pv[1])


results_rdd <- rbind(results_donut_3, results_donut_5, results_donut_10, results_donut_25, results_donut_50)

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
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")

length(unique(native_forest_law$rptpro_id))

rol_priority <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type == "rol", 1, 0),
         max_priority = max(priority))#%>%
#filter(priority == max_priority)%>%
#filter(area_diff == min(area_diff))



checking_manipulation <- rol_priority %>%
  filter(between(rptpre_superficie_predial, 175, 225) & rptpro_tipo_concurso != "Otros Interesados")%>%
  select(rptpro_id, NOM_PREDIO, PROPIETARI, rptpre_nombre, rptprop_nombre, evi_2007, evi_2020)

write.csv(checking_manipulation, "checking_manipulation.csv")

ggplot(data = checking_manipulation) +
  geom_histogram(aes(rptpre_superficie_predial), fill = "white", color = "red", binwidth = 5)+
  geom_histogram(aes(area_ha), fill = "grey70", color = "darkgreen", binwidth = 5) +
  geom_vline(xintercept = 202.5, linetype = "dashed")+
  theme_minimal()+
  xlim(150,300)

property_discontinuity <- rol_priority %>%
  rename(reported_size = rptpre_superficie_predial,
         true_size = area_ha)%>%
  mutate(size_cutoff = ifelse(
           rptpro_numero_region %in% regions_200, 200,
           ifelse(rptpro_numero_region %in% regions_800, 800, 500)
         ),
         size_centered = reported_size - size_cutoff,
         true_centered = true_size - size_cutoff,
         below_cutoff = reported_size <= size_cutoff,
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", 0, 1),
         received_bonus = rptpro_tiene_bonificacion_saff == "Si"
         )




discontinuity_with_bins <- property_discontinuity %>% 
  filter(rptpro_numero_region %in% c(5,6,7,8,9, 10, 14))%>%
  mutate(size_binned = cut(true_size, breaks = seq(0, 500, 25)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)
         ) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned, smallholder) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "smallholder", values_from = "n", values_fill = 0) %>% 
  rename(small_yes = `TRUE`, small_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_smallholder = small_yes / (small_yes + small_no),
         bin_end = as.numeric(size_binned)*25,
         below_cutoff = bin_end <= 200)%>%
  drop_na(size_binned)

# Plot this puppy
ggplot(discontinuity_with_bins, aes(x = bin_end, y = prob_smallholder, color = below_cutoff)) +
  geom_col(fill = "grey90") +
  geom_vline(xintercept = 210)+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()






reported_density <- rddensity(property_discontinuity$size_centered, c = 0)
summary(reported_density)
rdplotdensity(rdd = reported_density, 
              X = property_discontinuity$size_centered,
              type = "both")  # This adds both points and lines

area_density <- rddensity(subset(property_discontinuity, rptpro_numero_region %in% regions_200 & true_size < 5000)$true_size, c = 200)
summary(area_density)
rdplotdensity(rdd = area_density, 
              X = subset(property_discontinuity, rptpro_numero_region %in% regions_200)$true_size,
              type = "both")  # This adds both points and lines


evi_discontinuity <- property_discontinuity %>%
  filter(rptpro_tiene_bonificacion_saff == "Si" | rptpro_tiene_plan_saff == "Si")


donut_size = 5

donut_5_df <- evi_discontinuity %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_5 <- rdrobust(y = donut_5_df$evi_2020, x = donut_5_df$size_centered, c = 0,
                          fuzzy = donut_5_df$smallholder,
                         covs = cbind(
                           donut_5_df$c_1,
                           donut_5_df$c_3,
                           donut_5_df$c_9,
                           donut_5_df$c_5,
                           donut_5_df$rptpro_puntaje,
                           donut_5_df$rptpro_monto_total,
                           donut_5_df$rptpre_superficie_bonificada,
                           donut_5_df$evi_2007,
                           as.factor(donut_5_df$rptpro_tipo_presenta)
                         )
                           ) %>% 
  summary()

###############################################################################
### synthetic did as outcome
###############################################################################

results_df <- results_df %>%#readRDS("results_df") %>%
  mutate(size_centered = rptpre_superficie_predial - 200)

donut_size = 5

donut_5_df <- results_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, donut_size, max(size_centered))
  )

donut_nonp_5 <- rdrobust(y = donut_5_df$evi_2020, x = donut_5_df$size_centered, c = 0,
                         fuzzy = donut_5_df$smallholder,
                         covs = cbind(
                           donut_5_df$c_1,
                           donut_5_df$c_3,
                           donut_5_df$c_9,
                           donut_5_df$c_5,
                           donut_5_df$rptpro_puntaje,
                           donut_5_df$rptpro_monto_total,
                           donut_5_df$rptpre_superficie_bonificada,
                           donut_5_df$evi_2007,
                           as.factor(donut_5_df$rptpro_tipo_presenta)
                         )
) %>% 
  summary()