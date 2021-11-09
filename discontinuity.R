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
NFL_df <- readRDS("C:/Users/garci/Dropbox/chile_collab/input_files/NFL_df.rds")

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
  )%>%
  group_by(rptpro_id)%>%
  # mutate_at(.vars = vars(anillado:zanja),
  #           .funs = list(~ max(.)))
  ungroup()%>%
distinct(rptpro_id, .keep_all = TRUE)

#################################################################################################
### table shows compliance across the threshold for all properties
#################################################################################################
discontinuity_main %>%
  group_by(rptpro_tipo_concurso, property_size <= size_cutoff) %>% 
  summarize(count = n()) %>% 
  group_by(rptpro_tipo_concurso) %>% 
  mutate(prop = count / sum(count))


discontinuity_main200 <- subset(discontinuity_main, rptpro_numero_region %in% regions_200)
discontinuity_main800 <- subset(discontinuity_main, rptpro_numero_region %in% regions_800)
reported_200 <- subset(discontinuity_main, between(property_size, 198, 200))%>%
  select(rptpro_id, rptpro_ano, property_size, rptpro_tipo_presenta, rptpro_tipo_concurso)
#################################################################################################
### Density tests show that there is manipulation at the 200 hectare threshold
#################################################################################################

test_density <- rddensity(discontinuity_main200$property_size, c = 200)
summary(test_density)
rdplotdensity(rdd = test_density, 
                                   X = discontinuity_main200$property_size,
                                   type = "both",
              #title = "",
              xlabel = "reported property size",
              ylabel = "density")  # This adds both points and lines


#ggsave(path = "figs", filename = "psize_manipulation_200.png", width = 8, height = 5)

# plot zooming in on manipulation at 200

count_with_bins200 <- discontinuity_main200 %>%
  mutate(size_binned = cut(property_size, breaks = seq(0, 500, 5)),
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", FALSE, TRUE)) %>% 
  # Group by each of the new bins and tutoring status
  group_by(size_binned) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n())%>%
  mutate(bin_end = as.numeric(size_binned)*5)%>%
  drop_na(size_binned)%>%
  filter(between(bin_end, 125, 275))

ggplot(count_with_bins200, aes(x = bin_end, y = n)) +
  geom_point(fill = "grey90") +
  geom_line() +
  geom_vline(xintercept = 202.5)+
  labs(x = "reported property size", y = "Proportion of properties designated as smallholders")+
  theme_minimal()

ggplot(data = discontinuity_main200) +
  geom_histogram(aes(property_size), binwidth = 1, alpha = .7 ,fill = "#E69F00", color = "white", size = 1, boundary = 0)+
  geom_vline(xintercept = 200, linetype = "dashed", size = 1.25)+
  #scale_fill_manual(values=c("grey20", "grey60")) + 
  theme_minimal()+
  xlim(190, 210)

#################################################################################################
### Showing that smallholder probability changes across thresholds
#################################################################################################

discontinuity_with_bins <- discontinuity_main200 %>% 
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

## We see something similar for the 800 hectare regions

discontinuity_with_bins2 <- discontinuity_main800 %>% 
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

#################################################################################################
### Implementing fuzzy rdd
#################################################################################################

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the contest column, since most people are compliers. But some people didn’t comply,

library(estimatr)

#################################################################################################
### parametric fuzzy rdd w/ received_bonus outcome
#################################################################################################

analysis_df <- discontinuity_main %>%
  mutate(regeneration = (regeneracion ) > 0,
         planting = (`siembra-directa`
                     + plantacion + `plantacion-suplementaria`
                      + enriquecimiento
                          ) > 0,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo != "PRODUCCION MADERERA" & rptpro_objetivo_manejo != "PRODUCCION NO MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0)
  )%>%
  filter(property_size != 200 & property_size != 150 & property_size != 250 & property_size != 100)


bw_list = c(75, 50, 30)

rdd_results <- data.frame()

for(k in bw_list){
    
    i = "drop heap points"
    bw = k 

    rdd <- iv_robust(
      received_bonus ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region , 
      #diagnostics = TRUE,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw & rptpro_ano < 2019)
    )
    
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      regeneration ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      planting ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      rptpro_monto_total ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      rptpre_superficie_bonificada ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      timber ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      nontimber ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      ecological_recovery ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
    
    rdd <- iv_robust(
      cutting ~ size_centered + smallholder | size_centered + below_cutoff,
      fixed_effects = ~ rptpre_region,
      data = filter(analysis_df, size_centered >= -bw & size_centered <= bw)
    )
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = i
    )%>%
      rbind(rdd_results)
  
  }

library(rio)
export(rdd_results, "rdresults_main.rds")


modelsummary(list("received bonus" = rdd_bonus,
                  "reforestation" = rdd_reforest,
                  "timber" = rdd_timber,
                  "received bonus" = rdd_bonus_10,
                  "reforestation" = rdd_reforest_10,
                  "timber" = rdd_timber_10) ,
             stars = TRUE)

modelsummary(list("payment" = rdd_payment,
                  "project area" = rdd_projectarea,
                  "payment (10)" = rdd_payment_10,
                  "project area (10)" = rdd_projectarea_10) ,
             stars = TRUE)


# Based on this model, using below_cutoff as an instrument, 
# we can see that the coefficient for smallholder is different now! 
# It’s .47, which means that the smallholder contest causes an increased follow through of 47%
# for compliers in the bandwidth.

# non-parametric rd
nonprdd_df <- discontinuity_main %>%
  mutate(reforestation = (regeneracion + `siembra-directa` + plantacion + `plantacion-suplementaria` + enriquecimiento) > 0,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0)
  )
right_donut_size = 0
bandwidth = 40

donut_size = 0


donut_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, right_donut_size, max(size_centered))
  )

donut_nonp <- rdrobust(y = donut_df$received_bonus, x = donut_df$size_centered, c = 0, h = bandwidth,
                         fuzzy = donut_df$smallholder
                       , covs = cbind(
                         as.factor(donut_df$rptpre_region),
                         as.factor(donut_df$rptpro_ano)
                       )
                         ) 

results_donut_0 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp$bws[1], "coeff" = donut_nonp$coef[1], "se" = donut_nonp$se[1], "pval" = donut_nonp$pv[1])
##############################################################################################

donut_size = 3

donut_df <- nonprdd_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, right_donut_size, max(size_centered))
  )

donut_nonp <- rdrobust(y = donut_df$received_bonus, x = donut_df$size_centered, c = 0, h = bandwidth,
                       fuzzy = donut_df$smallholder
                       , covs = cbind(
                         as.factor(donut_df$rptpre_region),
                         as.factor(donut_df$rptpro_ano)
                         )
) 

results_donut_3 <- data.frame("donut_size" = donut_size, "bw" = donut_nonp$bws[1], "coeff" = donut_nonp$coef[1], "se" = donut_nonp$se[1], "pval" = donut_nonp$pv[1])

##############################################################################################

###############################################################################
### EVI as outcome
###############################################################################
library(sf)
my_rol_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/garci/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
  mutate(match_type = "spatial")

native_forest_law <- NFL_df %>%
  select(-c(rptprop_nombre, rptpre_rol, rptpre_nombre))%>%
  inner_join(bind_rows(my_spatial_match, my_rol_match), by = "rptpro_id")

length(unique(native_forest_law$rptpro_id))

rol_priority <- native_forest_law %>%
  group_by(rptpro_id)%>%
  mutate(priority = ifelse(match_type != "rol", 1, 0),
         max_priority = max(priority))%>%
distinct(rptpro_tipo_concurso, rptpro_id, NOM_PREDIO, PROPIETARI, rptpre_nombre, rptprop_nombre, evi_2007, evi_2020, rptpre_superficie_predial, area_ha, .keep_all = TRUE)%>%
  filter(priority == max(priority)) %>%
  filter(area_diff == min(area_diff))%>%
  ungroup()


checking_manipulation <- rol_priority %>%
  filter( rptpre_superficie_predial < 1500)%>%
  select(rptpro_tipo_concurso, rptpre_superficie_predial, area_ha, NOM_PREDIO, PROPIETARI, rptpre_nombre, rptprop_nombre, rptpro_id, evi_2007, evi_2020)

bunchers_200 <- rol_priority %>%
  filter(between(rptpre_superficie_predial, 190, 200) )%>%
  #filter( area_diff/rptpre_superficie_predial < 1)%>%
  select(rptpro_tipo_concurso, rptpre_superficie_predial, area_ha, NOM_PREDIO, PROPIETARI, rptpre_nombre, rptprop_nombre, rptpro_id, evi_2007, evi_2020)

heap_points <- c(25, 50)
bunchers_50 <- rol_priority %>%
  filter(rptpre_superficie_predial %in% heap_points )%>%
  #filter( area_diff/rptpre_superficie_predial < 1)%>%
  select(rptpro_tipo_concurso, rptpre_superficie_predial, area_ha, NOM_PREDIO, PROPIETARI, rptpre_nombre, rptprop_nombre, rptpro_id, evi_2007, evi_2020)


x <- nrow(subset(bunchers_200, area_ha <= 200))
y <- nrow(subset(bunchers_200, area_ha > 200))
y/(y+x)

#write.csv(checking_manipulation, "checking_manipulation.csv")
size_reported <- checking_manipulation$rptpre_superficie_predial
full_size_reported <- discontinuity_main$property_size
size_true <- checking_manipulation$area_ha

ggplot() +
  geom_density(aes(size_reported))+
  geom_density(aes(full_size_reported), color = "red")+
  geom_density(aes(size_true), color = "blue")+
  xlim(0, 1000)

### first, we'll see whether these distributions are different to one another
ks.test(size_reported, full_size_reported)
# D = 0.021268, p-value = 0.06798

ggplot(data = subset(bunchers_200)) +
  geom_histogram(aes(rptpre_superficie_predial,  fill = "rptpre_superficie_predial"), binwidth = 5, alpha = .7 ,color = "white", size = 1, boundary = 0)+
  geom_histogram(aes(area_ha, fill = "area_ha",), binwidth = 5,  alpha = .8, boundary = 0) +
  geom_density(aes(area_ha)) +
  geom_vline(xintercept = 200, linetype = "dashed", size = 1.25)+
  #scale_fill_manual(values=c("grey20", "grey60")) + 
  theme_minimal()+
  scale_fill_manual(name="", 
                     labels = c("reported", 
                                "matched"), 
                     values = c("rptpre_superficie_predial"="#E69F00", 
                                "area_ha"="grey40"))+
  xlab("property size (ha)")+
  xlim(50, 400)
ggsave(#path = "figs", 
  filename = "psize_distributions_window.png", width = 8, height = 5)

ggplot(data = subset(bunchers_50)) +
  geom_histogram(aes(rptpre_superficie_predial,  fill = "rptpre_superficie_predial"), binwidth = 1, alpha = .7 ,color = "white", size = 1, boundary = 0)+
  geom_histogram(aes(area_ha, fill = "area_ha",), binwidth = 1,  alpha = .8, boundary = 0) +
  geom_density(aes(area_ha)) +
  geom_vline(xintercept = 50, linetype = "dashed", size = 1.25)+
  #scale_fill_manual(values=c("grey20", "grey60")) + 
  theme_minimal()+
  scale_fill_manual(name="", 
                    labels = c("reported", 
                               "matched"), 
                    values = c("rptpre_superficie_predial"="#E69F00", 
                               "area_ha"="grey40"))+
  xlab("property size (ha)")+
  xlim(0, 70)

ggplot(data = subset(checking_manipulation)) +
  geom_histogram(aes(rptpre_superficie_predial,  fill = "rptpre_superficie_predial"), binwidth = 10, alpha = .7, color = "white", size = 1, boundary = 0)+
  geom_histogram(aes(area_ha, fill = "area_ha",), binwidth = 10,  alpha = .7, boundary = 0) +
  geom_vline(xintercept = 200, linetype = "dashed", , size = 1.25)+
  #scale_fill_manual(values=c("grey20", "grey60")) + 
  theme_minimal()+
  scale_fill_manual(name="", 
                    labels = c("reported", 
                               "matched"), 
                    values = c("rptpre_superficie_predial"="#E69F00", 
                               "area_ha"="grey40"))+
  xlab("property size (ha)")+
  xlim(0, 600)
ggsave(#path = "figs", 
  filename = "psize_distributions.png", width = 8, height = 5)

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


################################################################
####### actual rdd analysis
###############################################################


evi_discontinuity <- property_discontinuity 

donut_size = 5
right_donut_size = 0
bw = 80

donut_df <- evi_discontinuity %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, right_donut_size, max(size_centered))
  )

evi_rdd <- iv_robust(
  evi_2007 ~ size_centered + smallholder + c_1 + c_3 +c_5 +c_9 + lat| size_centered + below_cutoff + c_1 + c_3 +c_5 +c_9 + lat,
  fixed_effects = ~ rptpre_region ,
  data = filter(donut_df, size_centered >= -bw & size_centered <= bw)
)
rdd_results <- data.frame(
  "outcome" = evi_rdd$outcome, "coeff" = evi_rdd$coefficients['smallholder'], "se" = evi_rdd$std.error['smallholder'], "p.val" = evi_rdd$p.value['smallholder']
)

###############################################################################
### synthetic did as outcome
###############################################################################

results_df <- readRDS("results_df.rds") %>%
  mutate(size_centered = rptpre_superficie_predial - 200,
         below_cutoff = size_centered <= 0)%>%
  filter(first.treat < 2015)

donut_size = 5

donut_df <- results_df %>%
  filter(between(size_centered, min(size_centered), - donut_size) | between(size_centered, right_donut_size, max(size_centered))
  )

synth_rdd <- iv_robust(
  tau.2020 ~ size_centered + smallholder | size_centered + below_cutoff ,
  #fixed_effects = ~ first.treat,
  data = filter(donut_df, size_centered >= -bw & size_centered <= bw)
)
rdd_results <- data.frame(
  "outcome" = synth_rdd$outcome, "coeff" = synth_rdd$coefficients['smallholder'], "se" = synth_rdd$std.error['smallholder'], "p.val" = synth_rdd$p.value['smallholder']
)
  
  
  
  