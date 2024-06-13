library(tidyverse)
library(augsynth)
my_data_dir <- here::here("remote")
clean_data_dir <- here::here(my_data_dir, "data", "native_forest_law", "cleaned_output")

matched_data_wide <- readRDS(paste0(clean_data_dir, "/matched_data_wide.rds"))



matched_data_long <- matched_data_wide %>%
  pivot_longer(Trees_2000:`0_2018`)%>%
  separate(name, into = c("class", "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year),
         property_hectares = pixels_count / 0.09 )%>%
  pivot_wider(values_from = value, names_from = class, names_repair = "unique", values_fn = sum)

analysis_df <- matched_data_long %>%
  group_by(property_ID, Year)%>%
  slice_head()%>%
  ungroup %>%
  mutate(D = ifelse(first.treat > 0 & Year >= first.treat, 1, 0))

test <- analysis_df %>%
  select(property_ID, Year, first.treat, D, treat)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Partially pooled SCM with an intercept

# with a choice of nu
ppool_syn <- multisynth(Trees ~ D, property_ID, Year, 
                        #nu = 0.5, 
                        analysis_df %>% select(Trees, D, property_ID, Year)
                        )
# with default nu
ppool_syn <- multisynth(lnppexpend ~ cbr, State, year, 
                        analysis_df)

print(ppool_syn$nu)

ppool_syn_summ <- summary(ppool_syn)

plot(ppool_syn_summ)

plot(ppool_syn_summ, levels = "Average")





# with default nu
ppool_syn_time <- multisynth(lnppexpend ~ cbr, State, year,
                             analysis_df, time_cohort = TRUE)

print(ppool_syn_time$nu)
#> [1] 0.3655737

ppool_syn_time
ppool_syn_time_summ <- summary(ppool_syn_time)
ppool_syn_time_summ

plot(ppool_syn_time_summ)
plot(ppool_syn_time_summ)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### with covariates

# with default nu
ppool_syn_cov <- multisynth(lnppexpend ~ cbr | perinc_1959 + studteachratio_1959,
                            State, year, analysis_df_covs)

print(ppool_syn_cov$nu)

ppool_syn_cov