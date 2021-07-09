lu_att_gt <- function(yname,
                   tname,
                   idname=NULL,
                   gname,
                   xformla=NULL,
                   data,
                   panel=TRUE,
                   allow_unbalanced_panel=FALSE,
                   control_group=c("nevertreated","notyettreated"),
                   anticipation=0,
                   weightsname=NULL,
                   alp=0.05,
                   bstrap=TRUE,
                   cband=TRUE,
                   biters=1000,
                   clustervars=NULL,
                   est_method="dr",
                   print_details=FALSE,
                   pl=FALSE,
                   cores=1,
                   lu_varname,
                   coeff_name,
                   interaction = TRUE) {
  
  # this is a DIDparams object
  dp <- pre_process_did(yname=yname,
                        tname=tname,
                        idname=idname,
                        gname=gname,
                        xformla=xformla,
                        data=data,
                        panel=panel,
                        allow_unbalanced_panel=allow_unbalanced_panel,
                        control_group=control_group,
                        anticipation=anticipation,
                        weightsname=weightsname,
                        alp=alp,
                        bstrap=bstrap,
                        cband=cband,
                        biters=biters,
                        clustervars=clustervars,
                        est_method=est_method,
                        print_details=print_details,
                        pl=pl,
                        cores=cores,
                        call=match.call()
  )
  
  #-----------------------------------------------------------------------------
  # Compute all ATT(g,t)
  #-----------------------------------------------------------------------------
  results <- lu_compute_attgt(dp, lu_varname, coeff_name)
  
  
  # extract ATT(g,t) and influence functions
  attgt.list <- results$attgt.list
  inffunc <- results$inffunc
  
  # process results
  attgt.results <- process_attgt(results)
  group <- attgt.results$group
  att <- attgt.results$att
  tt <- attgt.results$tt
  inffunc <- attgt.results$inf.func
  
  
  
  # analytical standard errors
  # estimate variance
  # this is analogous to cluster robust standard errors that
  # are clustered at the unit level
  
  # note to self: this def. won't work with unbalanced panel,
  # but it is always ignored b/c bstrap has to be true in that case
  n <- dp$n
  V <- Matrix::t(inffunc)%*%inffunc/n
  se <- sqrt(Matrix::diag(V)/n)
  
  # Zero standard error replaced by NA
  se[se <= sqrt(.Machine$double.eps)*10] <- NA
  
  # if clustering along another dimension...we require using the
  # bootstrap (in principle, could come up with analytical standard
  # errors here though)
  if ( (length(clustervars) > 0) & !bstrap) {
    warning("clustering the standard errors requires using the bootstrap, resulting standard errors are NOT accounting for clustering")
  }
  
  # Identify entries of main diagonal V that are zero or NA
  #zero_na_sd_entry <- unique(which(is.na(Matrix::diag(V)))) # unique( c( which(diag(V)==0),  which(is.na(diag(V)))))
  zero_na_sd_entry <- unique(which(is.na(se))) # unique( c( which(diag(V)==0),  which(is.na(diag(V)))))
  
  # bootstrap variance matrix
  if (bstrap) {
    
    bout <- mboot(inffunc, DIDparams=dp)
    bres <- bout$bres
    #zero_na_sd_entry <- unique(c(zero_na_sd_entry, which(bout$se==0)))
    if(length(zero_na_sd_entry)>0) {
      #V[-zero_na_sd_entry, -zero_na_sd_entry] <- bout$V
      se[-zero_na_sd_entry] <- bout$se[-zero_na_sd_entry]
    } else {
      #V <- bout$V
      se <- bout$se
    }
  }
  # Zero standard error replaced by NA
  se[se <= sqrt(.Machine$double.eps)*10] <- NA
  
  
  #-----------------------------------------------------------------------------
  # compute Wald pre-test
  #-----------------------------------------------------------------------------
  
  # select which periods are pre-treatment
  pre <- which(group > tt)
  
  # Drop group-periods that have variance equal to zero (singularity problems)
  if(length(zero_na_sd_entry)>0){
    pre <- pre[!(pre %in% zero_na_sd_entry)]
  }
  # pseudo-atts in pre-treatment periods
  preatt <- as.matrix(att[pre])
  
  # covariance matrix of pre-treatment atts
  preV <- as.matrix(V[pre,pre])
  
  # check if there are actually any pre-treatment periods
  if (length(preV) == 0) {
    message("No pre-treatment periods to test")
    W  <- NULL
    Wpval <- NULL
  } else if(sum(is.na(preV))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
    W <- NULL
    Wpval <- NULL
  } else if (rcond(preV) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
    W <- NULL
    Wpval <- NULL
  } else {
    # everything is working...
    W <- n*t(preatt)%*%solve(preV)%*%preatt
    q <- length(pre) # number of restrictions
    Wpval <- round(1-pchisq(W,q),5)
  }
  
  
  #-----------------------------------------------------------------------------
  # compute confidence intervals / bands
  #-----------------------------------------------------------------------------
  
  # critical value from N(0,1), for pointwise
  cval <- qnorm(1-alp/2)
  
  # in order to get uniform confidencs bands
  # HAVE to use the bootstrap
  if (bstrap){
    if (cband) {
      # for uniform confidence band
      # compute new critical value
      # see paper for details
      bSigma <- apply(bres, 2,
                      function(b) (quantile(b, .75, type=1, na.rm = T) -
                                     quantile(b, .25, type=1, na.rm = T))/(qnorm(.75) - qnorm(.25)))
      
      bSigma[bSigma <= sqrt(.Machine$double.eps)*10] <- NA
      
      # sup-t confidence band
      bT <- apply(bres, 1, function(b) max( abs(b/bSigma), na.rm = TRUE))
      cval <- quantile(bT, 1-alp, type=1, na.rm = T)
      if(cval >= 7){
        warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
      }
    }
  }
  
  
  # Return this list
  return(MP(group=group, t=tt, att=att, V_analytical=V, se=se, c=cval, inffunc=inffunc, n=n, W=W, Wpval=Wpval, alp = alp, DIDparams=dp))
  
}