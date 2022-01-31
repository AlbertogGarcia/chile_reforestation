
pool_wide <- readRDS("pool_wide_rol.rds")

library(MatchIt)
pool_wide <- pool_wide %>%
  drop_na(elev, slope, baresoil)

#love.plot(matched_2009, thresholds = c(m = .25))

r=2

my_match_method = "nearest"
my_distance = "glm"
#my_distance = "mahalanobis"

vars <- c("forest", "plantation", "baresoil", "pasture", "shrub", "proportion_erosion", "slope", "elev", "lat", "area_ha", "road_dist", "industry_dist", "native_industry_dist"
          ,"urban" , "water")

matched_2009 <- matchit(reformulate(c(vars, "evi_trend0706", "evi_trend0605", "evi_2007"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2009),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2010 <- matchit(reformulate(c(vars, "evi_trend0807", "evi_trend0706", "evi_trend0605", "evi_2008"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2010),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2011 <- matchit(reformulate(c(vars, "evi_trend0908", "evi_trend0807", "evi_trend0706", "evi_2009"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2011),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2012 <- matchit(reformulate(c(vars, "evi_trend1009", "evi_trend0908", "evi_trend0807", "evi_2010"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2012),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2013 <- matchit(reformulate(c(vars, "evi_trend1110", "evi_trend1009", "evi_trend0908", "evi_2011"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2013),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2014 <- matchit(reformulate(c(vars, "evi_trend1211", "evi_trend1110", "evi_trend1009", "evi_2012"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2014),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2015 <- matchit(reformulate(c(vars, "evi_trend1312", "evi_trend1211", "evi_trend1110", "evi_2013"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2015),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2016 <- matchit(reformulate(c(vars, "evi_trend1413", "evi_trend1312", "evi_trend1211", "evi_2014"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2016),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2017 <- matchit(reformulate(c(vars, "evi_trend1514", "evi_trend1413", "evi_trend1312", "evi_2015"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2017),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2018 <- matchit(reformulate(c(vars, "evi_trend1615", "evi_trend1514", "evi_trend1413", "evi_2016"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2018),
                        method = my_match_method, ratio = r, distance = my_distance)

matched_2019 <- matchit(reformulate(c(vars, "evi_trend1716", "evi_trend1615", "evi_trend1514", "evi_2017"), "treat")
                        ,
                        data = subset(pool_wide, first.treat == 0 | first.treat == 2019),
                        method = my_match_method, ratio = r, distance = my_distance)


map_covars <- function(df){
  # return_df <- within(df, quartile <- as.integer(cut(rptpro_puntaje, quantile(rptpro_puntaje, probs=0:4/4, na.rm = TRUE), include.lowest=TRUE)))
  
  return_df <- df %>%
    group_by(subclass)%>%
    arrange(treat)%>%
    mutate(
     control_contest = rptpro_tipo_concurso[2],
     control_year = max(first.treat)
    )%>%
    ungroup()
  
  return(return_df)
}


# group by matched pairs
matched_wide <- map_covars(match.data(matched_2009, subclass = "subclass"))%>%
  bind_rows(map_covars(match.data(matched_2010, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2011, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2012, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2013, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2014, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2015, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2016, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2017, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2018, subclass = "subclass")))%>%
  bind_rows(map_covars(match.data(matched_2019, subclass = "subclass")))%>%
  distinct(id, .keep_all = TRUE)

library(rio)
export(matched_wide, "my_matched_wide_rollevel.rds")


library(tidyverse)
library(sf)

matched_wide <- readRDS("my_matched_wide_rollevel.rds")%>%
  mutate(group = ifelse(first.treat == 0, "matched control", first.treat))
  
raw_trend_df <- readRDS("pool_wide_rol.rds") %>%
  filter(treat == 0) %>%
  mutate(group = "unenrolled")%>%
  bind_rows(matched_wide) %>%
  select(id, group, treat, first.treat, evi_2005:evi_2020)%>%
  gather("Year", "EVI2", evi_2005:evi_2020)%>%
  separate(Year, into = c(NA, "Year"), sep = "_") %>%
  mutate(Year= as.numeric(Year))


trend_df <- raw_trend_df %>%
  filter((Year < first.treat  | treat == 0) & Year < 2019)%>%
  group_by(group, Year, treat)%>%
  dplyr::summarize(EVI2 = mean(EVI2))%>%
  mutate(main_group = ifelse(treat == 1, "enrolled", group),
         group_size = as.factor(ifelse(treat == 1, 1, 1.2)))

ggplot(data = trend_df, aes(x = as.character(Year), y = EVI2, group = group, color = main_group))+
  geom_line()+
  theme_minimal()+
  scale_color_manual(values=c("grey85", "green", "red"))+
  ylab("EVI")+xlab("year")

ggsave(path = "figs", filename = "trends.png", width = 7, height = 5)


