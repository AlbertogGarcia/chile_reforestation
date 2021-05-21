library(tidyverse)

compute.attgt <- function(dta) {
  
  
  # pick up all groups
  groups <- unique(dta$G)
  
  # pick up all time periods
  time.periods <- unique(dta$period)
  
  # sort the groups and drop the untreated group
  groups <- sort(groups)[-1]
  
  # sort the time periods and drop the first 
  time.periods <- sort(time.periods)[-c(1)]
  
  # drop last time period (because we don't know if
  # these units are affected by anticipation or not
  # and we are being very careful)
  # (if you were worried about more than one anticipation
  # period here, would need to drop more time periods
  # from the end)
  # time.periods <- time.periods[-length(time.periods)]
  
  # list to store all group-time average treatment effects
  # that we calculate
  attgt.list <- list()
  counter <- 1
  
  # loop over all groups
  for (g in groups) {
    
    # get the correct "base" period for this group
    main.base.period <- g - 1
    
    # loop over all time periods
    for (tp in time.periods) {
      
      #----------------------------------------------------
      # if it's a pre-treatment time period (used for the
      # pre-test, we need to adjust the base period)
      
      # group not treated yet
      if (tp < g) {
        # move before
        base.period <- tp - 1
      } else {
        # this is a post-treatment period
        base.period <- main.base.period
      }
      #----------------------------------------------------
      
      #----------------------------------------------------
      # now, all we need to do is collect the right subset
      # of the data and estimate a 2x2 DiD
      
      # get group g and untreated group
      this.data <- subset(dta, G==g | G==0)
      #this.data <- subset(dta, G = did_group)
      
      
      # get current period and base period data
      this.data <- subset(this.data, period==tp | period==base.period) %>%
        mutate(treat = ifelse(G==g, 1, 0),
               post = ifelse(period==tp, 1, 0),
               treatpost = treat*post)
      
      this.data <- this.data %>%
        drop_na(id)
      
      # compute 2x2 estimator using DRDID package
      # (in this unconditional case, it would be straightforward
      # to calculate the 2x2 att just using averages, but we
      # like the DRDID package as it will work for more complicated
      # cases as well)
      
      attgt <- DRDID::drdid(yname="Y", tname = "period", idname = "id", dname = "treat",
                            xformla =  ~lat + forest + plantation + baresoil + pasture + urban +water+ slope + elev  + area_ha + road_dist
                            ,
                            data = this.data,
                            panel = TRUE
                            )$ATT
      
      
      this.data$geometry.x <- NULL

       mod <- glm(treat ~ lat + forest + plantation + baresoil + pasture + urban +water+ slope + elev  + area_ha + road_dist
                  , data = this.data, family = "binomial")

       this.data <- this.data %>%
         mutate(propglm = predict(mod, type = "response"),
                psweights = 1 / propglm * treat +
                  1 / (1 - propglm) * (1 - treat))

      #### simple did to estimate interaction terms
      # attgt <- tail(lm(Y ~  treat*post*forest*smallholder + smallholder + lat + forest + plantation + baresoil + pasture + urban +water+ slope + elev  + area_ha + road_dist
      #                  , data   = this.data
      #              , weights = psweights
      #              )$coefficients
      #              , n=11)[11]
       attgt <- tail(lm(Y ~  treat*post + lat + forest + plantation + baresoil + pasture + urban +water+ slope + elev  + area_ha + road_dist
                        , data   = this.data
                        , weights = psweights
       )$coefficients
       , n=1)

      
      # save results
      attgt.list[[counter]] <- list(att=attgt, group=g, time.period=tp)
      counter <- counter+1
      #----------------------------------------------------
      
    }
  }
  
  #-----------------------------------------------------------------------------
  # aggregate into dynamic effects
  
  # turn results into a data.frame
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  
  # add event time to the results
  attgt.results$e <- attgt.results$time.period - attgt.results$group
  
  # calculate relative sizes of each group
  # (will be used as weights)
  n.group <- sapply(groups, function(gg) nrow(subset(dta, G==gg)))
  # merge in group sizes
  ngroup.mat <- cbind(groups, n.group)
  attgt.results <- merge(attgt.results, ngroup.mat, by.x = "group", by.y = "groups")
  
  
  simple_df <- subset(attgt.results, e >=0)%>%
    mutate(weight= n.group / sum(n.group))
  simple.att <- sum(simple_df$att * simple_df$weight)
  
  
  # event times to calculate dynamic effects
  eseq <- unique(attgt.results$e) 
  eseq <- sort(eseq)
  
  # calculate average effects by event time
  att.e <- c()
  counter <- 1
  for (this.e in eseq) {
    # get subset of results at this event time
    res.e <- subset(attgt.results, e==this.e)
    
    # calculate weights by group size
    res.e$weight <- res.e$n.group / sum(res.e$n.group)
    
    # calculate dynamic effect as weighted average
    att.e[counter] <- sum(res.e$att * res.e$weight)
    
    # on to the next one
    counter <- counter+1
  }
  
  # store dynamic effects results
  dyn.results <- cbind.data.frame(e = eseq, att.e = att.e)
  
  # event times to calculate dynamic effects
  gseq <- unique(attgt.results$group) 
  gseq <- sort(gseq)
  
  
  ### cohort level average treatment effects
  # calculate average effects by group
  att.g <- c()

  counter <- 1
  for (this.g in groups) {
    # get subset of results at this event time
    res.g <- subset(attgt.results, e>=0 & group==this.g)
    
    # calculate weights by group size
    res.g$weight <- res.g$n.group / sum(res.g$n.group)
    
    # # calculate cohort effects as weighted average by cohort
    att.g[counter] <- sum(res.g$att * res.g$weight)
    
    # on to the next one
    counter <- counter+1
  }
  
  dynamic.ovr.att <- mean(subset(dyn.results, e >= 0)$att.e )
  
  # store dynamic effects results
  cohort.results <- cbind.data.frame(g = gseq, att.g = att.g)
  
  n.group_df <- as.data.frame(ngroup.mat)%>%
    dplyr::rename(g = groups)
  
  ovr.result_df <- cohort.results %>%
    inner_join(n.group_df, by = "g") %>%
    mutate(weight = n.group / sum(n.group))
  
  
  att.ovr <- sum(ovr.result_df$att * ovr.result_df$weight)
 
  # return group-time average treatment effects and dynamic effects
  
  
  
  
  return(list(attgt.results=attgt.results[,c("group","att","time.period")],
              dyn.results=dyn.results,
              dyn.ovr =  dynamic.ovr.att,
              simple.att = simple.att,
              att.ovr = att.ovr,
              cohort.results = cohort.results))
}



