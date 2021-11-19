
library(reshape2)
library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables
library(kableExtra)  # Fancy table formatting
library(ggplot2)
library(Hmisc)
library(bunchr)
regions_200 <- c(5,6,7,8,9, 10,14)
regions_500 <- c(1, 2, 3, 4, 15)
regions_800 <- c(11, 12)
NFL_df <- readRDS("C:/Users/agarcia/Dropbox/chile_collab/input_files/NFL_df.rds")




library(readxl)

# xls files downloaded from CONAF
#projects
proyecto_df <- read_xlsx("C:/Users/agarcia/Dropbox/chile_reforestation/external_data/concurso_conaf/program/proyecto.xlsx")

#properties and coordinates
predio_df <- read_xlsx("C:/Users/agarcia/Dropbox/chile_reforestation/external_data/concurso_conaf/program/predio.xlsx")
#owners
propietario_df <- read_xlsx("C:/Users/agarcia/Dropbox/chile_reforestation/external_data/concurso_conaf/program/propietario.xlsx")
#stands
rodal_df <- read_xlsx("C:/Users/agarcia/Dropbox/chile_reforestation/external_data/concurso_conaf/program/rodal.xlsx")
#activities
actividades_df <- read_xlsx("C:/Users/agarcia/Dropbox/chile_reforestation/external_data/concurso_conaf/program/actividad.xlsx")

property_df <- proyecto_df %>%
  full_join(predio_df, by = "rptpro_id") %>%
  full_join(propietario_df, by = "rptpre_id")

activity_df <- property_df %>%
  left_join(rodal_df, by = "rptpre_id") %>%
  left_join(actividades_df, by = "rptro_id") %>%
  dcast(rptpro_id
        + rptpro_ano
        ~ rptac_tipo, 
        fun.aggregate = function(x){as.integer(length(x) > 0)})

NFL_df <- property_df %>%
  full_join(activity_df, by = c("rptpro_id", "rptpro_ano")) 



discontinuity_main <- NFL_df %>%
  rename(property_size = rptpre_superficie_predial)%>%
  mutate(received_bonus = as.numeric(ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)),
         size_cutoff = ifelse(
           rptpro_numero_region %in% regions_200, 200,
           ifelse(rptpro_numero_region %in% regions_800, 800, 500)
         ),
         size_centered = property_size - size_cutoff,
         below_cutoff = property_size <= size_cutoff,
         smallholder = ifelse(rptpro_tipo_concurso == "Otros Interesados", 0, 1),
         price_per_hectare = rptpro_monto_total/property_size,
         proportion_bonusarea = rptpro_superficie/property_size
  )%>%
  # group_by(rptpro_id)%>%
  # mutate_at(.vars = vars(anillado:zanja),
  #           .funs = list(~ max(.)))
  # ungroup()%>%
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
### visualize fuzzy gap
#################################################################################################
ggplot(discontinuity_main, aes(x = size_centered, y = proportion_bonusarea, color = rptpro_tipo_concurso)) +
  geom_point(size = 1.5, alpha = 0.5) + 
  # Add a line based on a linear model for the people scoring less than 70
  geom_smooth(data = filter(discontinuity_main, size_centered <= 0), method = "lm") +
  # Add a line based on a linear model for the people scoring 70 or more
  geom_smooth(data = filter(discontinuity_main, size_centered > 0), method = "lm") +
  geom_vline(xintercept = 0) +
  labs(x = "property size (centered)", y = "proportion of property enrolled")+
  xlim(-100, 100)+ylim(0, 1)+theme_minimal()




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

ggplot(count_with_bins, aes(x = bin_end, y = n)) +
  geom_point(fill = "grey90", color = "blue") +
  geom_line() +
  geom_vline(xintercept = c(10, 20, 50), linetype = "dashed")+
  scale_x_continuous(name="reported property size", breaks=c(10, 20, 50))+
  scale_y_continuous(name="count", limits=c(0, 600))+
  ggtitle("heaping at round property sizes")+
  theme_minimal()

# ggsave(path = "figs", filename = "round_heaping.png", width = 8, height = 5)

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
### characteristics jump at heaps
#################################################################################################
#heaps <- c(10, 20, 50, 100, 150, 250)
heaps <- c(seq(from = 10, to = 50, by = 10), 70, 100, 150, 200, 300, 400, 500)

# outcomes:
# received_bonus
# rptpro_monto_total
# rptpro_superficie
# rptpro_puntaje
# timber


analysis_df <- discontinuity_main %>%
  mutate(regeneration = (regeneracion ) > 0,
         planting = (`siembra-directa`
                     + plantacion + `plantacion-suplementaria`
                     + enriquecimiento
         ) > 0,
         digital = ifelse(rptpro_tipo_presentacion == "Digital", 1, 0),
         reforestation = ((regeneration + planting) > 0)*1,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo != "PRODUCCION MADERERA" & rptpro_objetivo_manejo != "PRODUCCION NO MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         extensionist_consultant = ifelse(rptpro_tipo_presenta == "Extensionista" | rptpro_tipo_presenta == "Consultor", 1, 0)
  )


heaps_results <- data.frame()
for(i in heaps){
  
bw_side = ifelse(i < 100, 9.99, 
                 ifelse(i >= 200, 99.99, 49.99)
                 )  
  
df_heap <- analysis_df %>%
  mutate(R_Z = property_size - i,
         heap = ifelse(property_size == i, 1, 0)) %>%
  filter(between(property_size, i - bw_side, i + bw_side))
reg <- as.data.frame(summary(lm(extensionist_consultant ~ heap + R_Z, data = df_heap))$coefficients) %>%
  rownames_to_column()%>%
  filter(rowname == "heap")

heaps_results <- data.frame("heap_point" = i, "coeff" = reg$Estimate, "se" = reg$`Std. Error`, "p_value" = reg$`Pr(>|t|)`)%>%
  rbind(heaps_results)

}

ggplot(data = heaps_results, aes(x = heap_point, y = coeff ))+
  geom_point(color = "blue")+
  geom_errorbar(aes(ymin = coeff - 1.96*se, ymax = coeff + 1.96*se), width = .5)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_continuous(breaks = heaps)+
  ylab("estimate") + xlab("heap point")+
  ggtitle("used extensionist or consultant")+
  theme_minimal()

ggsave(path = "figs", filename = "heap_covars_extensionista2.png", width = 7, height = 5)

#################################################################################################
### Implementing fuzzy rdd
#################################################################################################

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the contest column, since most people are compliers. But some people didn’t comply,

library(estimatr)

#################################################################################################
### parametric fuzzy rdd w/ received_bonus outcome
#################################################################################################

analysis_df <- discontinuity_main200 %>%
  mutate(regeneration = (regeneracion ) > 0,
         planting = (`siembra-directa`
                     + plantacion + `plantacion-suplementaria`
                      + enriquecimiento
                          ) > 0,
         reforest = (regeneration + planting) > 0,
         cutting = (`corta-liberacion` + `corta-mejoramiento` + `corta-recuperacion` +`corta-regeneracion` + `corta-selectiva` + `corta-sanitaria`) > 0,
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo != "PRODUCCION MADERERA" & rptpro_objetivo_manejo != "PRODUCCION NO MADERERA", 1, 0),
         nontimber = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0)
  )

bw_list = c(120, 80, 60)
heaps <- c(seq(from = 10, to = 50, by = 10), 70, 100, 150, 200)#, 300, 400, 500, 600, 700, 800)



rdd_results <- data.frame()

for(k in bw_list){
    
  donut = 3
  
    
    donut_df <- analysis_df %>%
      mutate(drop = ifelse(property_size %in% heaps | between(size_centered, -donut, 0), 1, 0)
             )%>%
      filter(drop != 1)
      
      
    bw = k 
    
    #proportion_bonusarea
    #rptpro_monto_total
    #timber
    #

    rdd <- iv_robust(
      timber ~ size_centered  + smallholder | size_centered  + below_cutoff,
      fixed_effects = ~ rptpre_region , 
      #diagnostics = TRUE,
      data = filter(donut_df, size_centered >= -bw - donut & size_centered <= bw )
    )
    
    rdd_results <- data.frame(
      "outcome" = rdd$outcome, "coeff" = rdd$coefficients['smallholder'], "se" = rdd$std.error['smallholder'], "p.val" = rdd$p.value['smallholder'],
      "bw" = k, "donut" = donut
    )%>%
      rbind(rdd_results)
    
  }

library(rio)
export(rdd_results, "rdresults_main.rds")

###############################################################################
### EVI as outcome
###############################################################################
library(sf)
NFL_df <- readRDS("C:/Users/agarcia/Dropbox/chile_collab/input_files/NFL_df.rds")
my_rol_match <- data.frame(readRDS("C:/Users/agarcia/Dropbox/chile_reforestation/data/analysis/my_rol_match.rds"))%>%
  mutate(match_type = "rol")
my_spatial_match <- data.frame(readRDS("C:/Users/agarcia/Dropbox/chile_reforestation/data/analysis/my_spatial_match.rds"))%>%
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
  mutate(over = ifelse(area_ha > 200, 1, 0),
         timber = ifelse(rptpro_objetivo_manejo == "PRODUCCION MADERERA", 1, 0),
         ecological_recovery = ifelse(rptpro_objetivo_manejo == "PRODUCCION NO MADERERA", 1, 0),
         digital = ifelse(rptpro_tipo_presentacion == "Digital", 1, 0),
         extentionist_consultant = ifelse(rptpro_tipo_presenta == "Extensionista" | rptpro_tipo_presenta == "Consultor", 1, 0),
         received_bonus = ifelse(rptpro_tiene_bonificacion_saff == "Si", 1, 0)
  )


timber <- t.test(timber ~ over, bunchers_200)

digital <- t.test(digital ~ over, bunchers_200)

ecological_recovery <- t.test(ecological_recovery ~ over, bunchers_200)

extentionist_consultant <- t.test(extentionist_consultant ~ over, bunchers_200)

rptpro_monto_total <- t.test(rptpro_monto_total ~ over, bunchers_200)

score <- t.test(rptpro_puntaje ~ over, bunchers_200)

received_bonus <- t.test(received_bonus ~ over, bunchers_200)

evi_2007 <- t.test(evi_2007 ~ over, bunchers_200)

results_df <- data.frame("variable" = c("timber production", "non-timber", "digital application", "used extensionist or consultant", "payment (UTM)", "score", "received payment", "2007 EVI"), 
                         "over_mean" = c(timber$estimate[2], ecological_recovery$estimate[2], digital$estimate[2], extentionist_consultant$estimate[2], rptpro_monto_total$estimate[2], score$estimate[2], received_bonus$estimate[2], evi_2007$estimate[2]),
                         "under_mean" = c(timber$estimate[1], ecological_recovery$estimate[1], digital$estimate[1], extentionist_consultant$estimate[1], rptpro_monto_total$estimate[1], score$estimate[1], received_bonus$estimate[1], evi_2007$estimate[1]),
                         "p value" = c(timber$p.value, ecological_recovery$p.value, digital$p.value, extentionist_consultant$p.value, rptpro_monto_total$p.value, score$p.value, received_bonus$p.value, evi_2007$p.value)
)


library(rio)
export(results_df, "matched_heap.rds")


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
  
  
  
  