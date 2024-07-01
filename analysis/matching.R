library(tidyverse)
library(sf)
library(MatchIt)
library(cobalt)
library(rio)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark_grey" = "grey30",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

my_data_dir <- here::here("remote")
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

all_property_wide <- readRDS(paste0(clean_data_dir, "/all_property_share_wide.rds"))%>%
  filter(pixels_count != 0)%>%
  mutate(Trees_baseline = Trees_2005,
         Crop_baseline = Crop_2005,
         Shrubs_baseline = Shrubs_2005,
         Grassland_baseline = Grassland_2005,
         Development_baseline = Development_2005,
         Water_baseline = Water_2005,
         Trees0800 = Trees_2008 - Trees_2000,
         Grassland0800 = Grassland_2008 - Grassland_2000,
         Crop0800 = Crop_2008 - Crop_2000
  )


table(all_property_wide$first.treat)

match_vars <- c(#"Trees0800", 
                #"Grassland0800", "Crop0800", 
                "Forest", "Plantation",
                "Trees_baseline",
                "Grassland_baseline", "Crop_baseline", "Shrubs_baseline", "Development_baseline", "Water_baseline",
                "property_hectares",
                "elev", 
                "lat",
                "pop",
                "precip",
                "ind_dist", "natin_dist",
                "city_dist"
)


enrolled_wide <- all_property_wide %>%
  filter(treat != 0)

complier_pool_wide <- all_property_wide %>%
  filter(treat == 0 | submitted_management_plan == 1)%>%
  drop_na(match_vars)

noncomplier_pool_wide <- all_property_wide %>%
  filter(treat == 0 | submitted_management_plan != 1)%>%
  drop_na(match_vars)


r=3

my_match_method = "nearest"
my_distance = "glm"
#my_distance = "mahalanobis"






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Matching by cohort
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


cohort_years = seq(from = 2009, to = 2021)
matched_compliercohorts <- data.frame()
for(c in cohort_years){
  
  pool_data <- complier_pool_wide %>% 
    filter(first.treat == c | treat == 0)
  
  these_match_vars = match_vars
  
  if(c <= 2018){
    
    pool_data <- pool_data %>% 
      filter(
        !(property_ID %in% matched_compliercohorts$property_ID)
      )%>%
      rename(Trees_start = paste0("Trees_", c - 1),
             Crop_start = paste0("Crop_", c - 1),
             Grassland_start = paste0("Grassland_", c - 1))%>%
      mutate(Trees_trend = Trees_start - Trees_2007,
             Crop_trend = Crop_start - Crop_2007,
             Grassland_trend = Grassland_start - Grassland_2007)%>%
      select(match_vars, property_ID, treat, first.treat, rptpro_tipo_concurso, rptpro_ano, Trees_trend, Crop_trend, Grassland_trend)
    
    
  } else{
    
    pool_data <- pool_data %>% 
      filter(
        !(property_ID %in% matched_compliercohorts$property_ID)
      )%>%
      mutate(Trees_trend = Trees_2018 - Trees_2007,
             Crop_trend = Crop_2018 - Crop_2007,
             Grassland_trend = Grassland_2018 - Grassland_2007)%>%
      select(match_vars, property_ID, treat, first.treat, rptpro_tipo_concurso, rptpro_ano, Trees_trend, Crop_trend, Grassland_trend)
    
  }
  
  matches <- matchit(reformulate(match_vars, "treat")
                     ,
                     data = pool_data,
                     method = my_match_method , ratio = r, distance = my_distance)
  
  matched_ids <- match.data(matches) 
  
  subclass_contest <- matched_ids %>%
    select(subclass, rptpro_tipo_concurso, rptpro_ano)%>%
    drop_na(rptpro_tipo_concurso)%>%
    dplyr::rename(control_contest = rptpro_tipo_concurso,
                  control_year = rptpro_ano)%>%
    select(subclass, control_contest, control_year)
  
  matched_compliercohorts <- matched_ids %>%
    left_join(subclass_contest, by = "subclass") %>%
    select(property_ID, treat, subclass, control_contest, control_year, Trees_trend, Crop_trend, Grassland_trend) %>%
    bind_rows(matched_compliercohorts)
  
  
}

matched_data_wide <- matched_compliercohorts %>%
  group_by(subclass, control_year)%>%
  mutate(subclass = cur_group_id())%>%
  ungroup()%>%
  left_join(complier_pool_wide, by = c("property_ID", "treat"))

matched_data_long <- matched_data_wide %>%
  pivot_longer(Trees_2000:`0_2018`)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year),
         property_hectares = pixels_count / 0.09 )%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Matching diagnostics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
diag_vars <- match_vars

matched_diag_df <- matched_data_wide %>% select(diag_vars, treat)%>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))
unmatched_diag_df <- complier_pool_wide %>% select(diag_vars, treat) %>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))


matched_balance <- bal.tab(matched_diag_df, stats = c("mean.diffs", "variance.ratios"), treat = matched_diag_df$treat)$Balance %>% as.data.frame() %>%
  dplyr::rename(matched_mean = 2,
                matched_variance = 3)%>%
  select(2, 3)%>%
  rownames_to_column("variable")

unmatched_balance <- bal.tab(unmatched_diag_df, stats = c("mean.diffs", "variance.ratios"), treat = unmatched_diag_df$treat)$Balance %>% as.data.frame() %>%
  dplyr::rename(unmatched_mean = 2,
                unmatched_variance = 3)%>%
  select(2, 3)%>%
  rownames_to_column("variable")



named_vars <- c("Tree cover trend (00-08)",
              "Crop trend (00-08)",
              "Grassland trend (00-08)",
              "Native forest (Heilmayr et al., 2020)",
              "Plantation forest (Heilmayr et al., 2020)",
              "Tree cover",
              "Grassland", "Crop", "Shrubs", "Development", "Water",
              "Slope", "Elevation", "Lattitude",
              "Area",
              "Dist. to industry", "Dist. to native specific industry"
)

named_vars <- c("Trees0800" = "Tree cover trend (00-08)", 
                "Grassland0800" = "Grassland trend (00-08)", 
                "Crop0800" = "Crop trend (00-08)", 
                "Forest" = "Native forest", 
                "Plantation" = "Plantation forest",
                "Trees_baseline" = "Tree cover",
                "Grassland_baseline" = "Grassland", 
                "Crop_baseline" = "Crop", 
                "Shrubs_baseline" = "Shrubs", 
                "Development_baseline" = "Developed", 
                "Water_baseline" = "Water",
                "property_hectares" = "Property area (Ha)",
                "elev" = "Elevation", 
                "lat" = "Latitude",
                "pop" = "Population",
                "precip" = "Mean annual precip.",
                "ind_dist" = "Dist. to sawmill", 
                "natin_dist" = "Dist. to native industry",
                "city_dist" = "Dist. to nearest city"
)

covar_balance <- cbind(#data.frame("covariate" = varnames),
                       matched_balance, unmatched_balance %>% select(- variable))%>%
 mutate_at(vars(matched_mean:unmatched_variance), ~round(., digits = 5))%>%
  mutate(variable = named_vars[variable]) %>% drop_na(variable)%>%
  select(variable, unmatched_mean, matched_mean, unmatched_variance, matched_variance)

export(covar_balance, paste0(clean_data_dir, "/covar_balance_table.rds"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Raw pre-trends
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tree_vars <- paste0("Trees_", seq(2002, 2018, by = 1))

rawtreetrends_cmatches <- matched_data_wide %>%
  select(property_ID, first.treat, treat, tree_vars) %>%
  pivot_longer(tree_vars)%>%
  separate(name, into = c(NA, "Year"))%>%
  dplyr::rename(Trees = value)%>%
  dplyr::group_by(Year, treat)%>%
  dplyr::summarise(Trees = mean(Trees, na.rm = T))%>%
  mutate(group = ifelse(treat == 0, "Complier matches", "Compliers"))

TreatTrees08 <- (rawtreetrends_cmatches %>% filter(treat ==1 & Year == 2008))$Trees
MatchTrees08 <- (rawtreetrends_cmatches %>% filter(treat ==0 & Year == 2008))$Trees

rawtreetrends_cmatches_even <- rawtreetrends_cmatches %>%
  mutate(Trees = ifelse(treat == 1, Trees - TreatTrees08, Trees - MatchTrees08))

rawtreetrends_unenrolled <- complier_pool_wide %>% filter(treat == 0)%>%
  select(tree_vars) %>%
  summarise_at(vars(Trees_2002:Trees_2018), mean, na.rm = T)%>%
  pivot_longer(tree_vars)%>%
  separate(name, into = c(NA, "Year"))%>%
  dplyr::rename(Trees = value)%>%
  mutate(group = "Unenrolled")

rawtreetrends <- bind_rows(rawtreetrends_unenrolled, rawtreetrends_cmatches)
  
raw_trends <- ggplot(rawtreetrends_cmatches, aes(x = as.numeric(Year), y = Trees, color = group))+
  geom_line() + geom_point() + theme_minimal()+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  scale_color_manual(values = c(palette$blue, palette$red))+
  ylab("Share of property with tree cover") + xlab("Year") + guides(color = guide_legend(title = "Group"))

raw_trends_even <- ggplot(rawtreetrends_cmatches_even, aes(x = as.numeric(Year), y = Trees, color = group))+
  geom_line() + geom_point() + theme_minimal()+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  scale_color_manual(values = c(palette$blue, palette$red))+
  ylab("Change in tree cover relative to 2008") + xlab("Year")+ guides(color = guide_legend(title = "Group"))
raw_trends_even

raw_trends_all <- ggplot(rawtreetrends, aes(x = as.numeric(Year), y = Trees, color = group))+
  geom_line() + geom_point() + theme_minimal()+
  geom_vline(xintercept = 2008, linetype = "dashed")+
  scale_color_manual(values = c(palette$blue, palette$red, palette$dark))+
  ylab("Share of property with tree cover") + xlab("Year") + guides(color = guide_legend(title = "Group"))
raw_trends_all
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID test with panel
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(did)

### TREES

did_trees <- att_gt(yname="Trees",
                    tname="Year",
                    idname="property_ID",
                    gname="first.treat",
                    control_group = "notyettreated",
                    xformla= ~ ind_dist + natin_dist + city_dist + elev + pop
                  + Trees_trend + Trees0800 
                  # + Crop_trend + Crop0800 
                  # + Grassland_trend + Grassland0800
                    + Forest + Plantation + Trees_baseline + Grassland_baseline + Shrubs_baseline + Crop_baseline 
                    , 
                    base_period = "universal",
                #  anticipation = 1,
                    data=matched_data_long, 
                    clustervars = "property_ID"
)
did.ovr <- aggte(did_trees, type="simple")
did.ovr
did.es <- aggte(did_trees, type="dynamic", min_e = -10)
ggdid(did.es)

