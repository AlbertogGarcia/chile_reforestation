library(tidyverse)

dta <- otros_panel %>%
  rename(G = first.treat,
         id = ID,
         period = Year,
         Y = NDVI)

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
                            xformla =  ~rptpro_monto_total +lat + rptpre_superficie_bonificada + area_ha
                            ,
                            data = this.data,
                            panel = TRUE
                            )$ATT
      
      #### simple did to estimate interaction terms
      # attgt <- lm(Y ~  treat*post, 
      #              data   = this.data)$coefficients
      
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
  
  # store dynamic effects results
  cohort.results <- cbind.data.frame(g = gseq, att.g = att.g)
  
  n.group_df <- as.data.frame(ngroup.mat)%>%
    rename(g = groups)
  ovr.result_df <- cohort.results %>%
    inner_join(n.group_df, by = "g") %>%
    mutate(weight = n.group / sum(n.group))
  
  att.ovr <- sum(ovr.result_df$att * ovr.result_df$weight)
  
  # return group-time average treatment effects and dynamic effects
  
  
  
  
  return(list(attgt.results=attgt.results[,c("group","att","time.period")],
              dyn.results=dyn.results,
              att.ovr = att.ovr,
              cohort.results = cohort.results))
}


otros_results <- compute.attgt(dta)

# the number of bootstrap iterations
biters <- 100

# list to store bootstrap results
boot.res <- list()

# loop for each nonparametric bootstrap iteration
for (b in 1:biters) {
  # draw a bootstrap sample; here, we'll call an outside function
  bdata <- BMisc::blockBootSample(dta, "id")
  
  # call our function for estimating dynamic effects on the
  # bootstrapped data
  boot.res[[b]] <- otros_results$dyn.results$att.e
  print(b)
}

# use the bootstrapped results to compute standard errors
boot.res <- t(simplify2array(boot.res))
boot.se <- apply(boot.res, 2, sd)

# add the standard errors to the main results
dyn.results <- compute.aggte(otros_panel)$dyn.results
dyn.results$att.se <- boot.se

dyn.results <- dyn.results %>%
  filter(e >= -5)
p <- ggplot(data=dyn.results, aes(x=e, y=att.e)) +
  geom_line() +
  geom_point() +
  #geom_errorbar(aes(ymin=(att.e-1.96*att.se), ymax=(att.e+1.96*att.se)), width=0.1) +
  theme_bw()

p <- p + geom_line(data=truth, aes(x=e, y=att.e), inherit.aes=FALSE, color="blue")
p <- p + geom_point(data=truth, aes(x=e, y=att.e), inherit.aes=FALSE, color="blue")
p





