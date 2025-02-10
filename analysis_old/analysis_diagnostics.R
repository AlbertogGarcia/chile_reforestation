library(cobalt)
library(tidyverse)
library(sf)

matched_wide_main <- readRDS("analysis/matched_wide_2to1.rds")
pool_wide <- readRDS("analysis/pool_wide_rol.rds") %>%
  drop_na(elev, slope, baresoil)

vars <- c("treat", "forest", "plantation", "baresoil", "pasture", "shrub", "proportion_erosion", "slope", "elev", "lat", "area_ha", "road_dist", "industry_dist", "native_industry_dist"
          ,"urban" , "water")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Matching diagnostics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
matched_diag_df <- matched_wide_main %>% select(vars)%>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))
unmatched_diag_df <- pool_wide %>% select(vars) %>%
  mutate(treat = ifelse(treat ==1, "Enrollees", "Non-enrollees"))

matched_balance <- bal.tab(matched_diag_df, treat = matched_diag_df$treat, thresholds = c(m = .25))$Balance %>% as.data.frame() %>%
  dplyr::rename(matched = 2,
         matched_threshold = 3)%>%
  select(2, 3)%>%
  rownames_to_column("variable")
  
unmatched_balance <- bal.tab(unmatched_diag_df, treat = unmatched_diag_df$treat, thresholds = c(m = .25))$Balance %>% as.data.frame() %>%
  dplyr::rename(unmatched = 2,
         unmatched_threshold = 3)%>%
  select(2, 3)

varnames <- c("Native forest", "Plantation forest", "Baresoil", "Pasture", "Shrub", "Mod.-to-severe erosion", "Slope", "Elevation", "Lattitude", "Area (ha)", "Dist. to road", "Dist. to industry", "Dist. to native specific industry"
              ,"Urban" , "Water")

covar_balance <- cbind(data.frame("covariate" = varnames), matched_balance, unmatched_balance)%>%
  select( - variable)
library(rio)
export(covar_balance, "paper/results/covar_balance.rds")

for(i in 2:length(vars)){
  
  this_var <- vars[i]
  print(this_var)
  this_varname <- varnames[i - 1]
  
  unmatched_balplot <- bal.plot(unmatched_diag_df, treat = unmatched_diag_df$treat,
                                var.name = this_var,
                                sample.names = "Unmatched"
  ) + 
    xlab(this_varname) + theme(legend.position = "none") + ggtitle("") 
  ggsave(unmatched_balplot, path = "paper/figs", filename = paste0("balplot_unmatched_", this_var, ".png"), width = 5, height = 2)
  
  
  matched_balplot <- bal.plot(matched_diag_df, treat = matched_diag_df$treat,
                              var.name = this_var,
                              sample.names = "Matched"
  ) + 
    xlab(this_varname) + theme(legend.position = "none") + ggtitle("")
  matched_balplot <- matched_balplot +
    ylim( 0, max(
      layer_scales(unmatched_balplot)$y$get_limits(), layer_scales(matched_balplot)$y$get_limits()
    )
      )
  
  ggsave(matched_balplot, path = "paper/figs", filename = paste0("balplot_matched_", this_var, ".png"), width = 5, height = 2)
  
}

### include and put legend on bottom for one of the variables

unmatched_balplot <- bal.plot(unmatched_diag_df, treat = unmatched_diag_df$treat,
                              var.name = "elev",
                              sample.names = "Unmatched"
) + 
  xlab("Elevation") + ggtitle("") + theme(legend.position = "bottom") + scale_fill_discrete("")
ggsave(unmatched_balplot, path = "paper/figs", filename = "balplot_unmatched_elev_legend.png", width = 5, height = 2.5)


matched_balplot <- bal.plot(matched_diag_df, treat = matched_diag_df$treat,
                            var.name = "elev",
                            sample.names = "Matched"
) + 
  xlab("Elevation") + ggtitle("")+ theme(legend.position = "bottom")+
  ylim(layer_scales(unmatched_balplot)$y$get_limits()) + scale_fill_discrete("")
ggsave(matched_balplot, path = "paper/figs", filename = "balplot_matched_elev_legend.png", width = 5, height = 2.5)
