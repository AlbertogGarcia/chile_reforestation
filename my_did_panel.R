
my_did_panel <-function(y1, y0, D, covariates, i.weights = NULL,
                          boot = FALSE, boot.type = "weighted", nboot = NULL,
                          inffunc = TRUE){
  #-----------------------------------------------------------------------------
  # D as vector
  D <- as.vector(D)
  # Sample size
  n <- length(D)

  
  # Add constant to covariate vector
  int.cov <- as.matrix(rep(1,n))
  if (!is.null(covariates)){
    if(all(as.matrix(covariates)[,1]==rep(1,n))){
      int.cov <- as.matrix(covariates)
    } else {
      int.cov <- as.matrix(cbind(1, covariates))
    }
  }
  
  # Weights
  if(is.null(i.weights)) {
    i.weights <- as.vector(rep(1, n))
  } else if(min(i.weights) < 0) stop("i.weights must be non-negative")
  #-----------------------------------------------------------------------------
  
  #Pscore estimation (logit) and also its fitted values
  PS <- suppressWarnings(stats::glm(D ~ -1 + int.cov, family = "binomial", weights = i.weights))
  if(PS$converged == FALSE){
    warning(" glm algorithm did not converge")
  }
  if(anyNA(PS$coefficients)){
    stop("Propensity score model coefficients have NA components. \n Multicollinearity (or lack of variation) of covariates is a likely reason.")
  }
  ps.fit <- as.vector(PS$fitted.values)
  # Do not divide by zero
  ps.fit <- pmin(ps.fit, 1 - 1e-16)
  
  p.weights <- i.weights * D + i.weights * ps.fit * (1 - D)/(1 - ps.fit)
  #-----------------------------------------------------------------------------
  
  #Create dataset for TWFE approach
  if (is.null(covariates)) {
    x = NULL
  } else {
    if(all(as.matrix(covariates)[,1] == rep(1,n))) {
      # Remove intercept if you include it
      covariates <- as.matrix(covariates)
      covariates <- covariates[,-1]
      if(dim(covariates)[2]==0) {
        covariates = NULL
        x = NULL
      }
    }
  }
  
  if (!is.null(covariates)){
    if (ncol(as.matrix(covariates)) == 1) {
      x = as.matrix(c(covariates, covariates))
    } else {
      x <- as.matrix(rbind(covariates, covariates))
    }
  }
  
  interaction1 <- as.data.frame(covariates)$plantation
  interaction1 <- as.vector((c(interaction1, interaction1)))
  
  interaction2 <- as.data.frame(covariates)$smallholder
  interaction2 <- as.vector((c(interaction2, interaction2)))
  
  # Post treatment indicator
  post <- as.vector(c(rep(0, length(y0)), rep(1,length(y1))))
  # treatment group
  dd <- as.vector((c(D, D)))
  # outcome
  y <- as.vector(c(y0, y1))
  # weights
  p.weights <- as.vector(c(p.weights, p.weights))
  #p.weights <- as.vector(c(i.weights, i.weights))
  
  # If there are covariates, proceed like this
  if(!is.null(x)){
    #---------------------------------------------------------------------------
    #Estimate TWFE regression
    
    reg <- stats::lm(y ~ # dd:post:interaction1:interaction2 +
                     dd:post:interaction1 + dd:interaction1 + post:interaction1
                     #+dd:post:interaction2 + dd:interaction2 + post:interaction2
                     + dd:post + post + dd + x, x = TRUE, weights = p.weights)
    
    
    twfe.att <- reg$coefficients["dd:post:interaction1"]
    #-----------------------------------------------------------------------------
    #Elemenets for influence functions
    inf.reg <- (i.weights * reg$x * reg$residuals) %*%
      base::solve(crossprod(i.weights * reg$x, reg$x) / dim(reg$x)[1])
    
    sel.theta <- matrix(c(rep(0, dim(inf.reg)[2])))
    
    index.theta <- which(dimnames(reg$x)[[2]]=="dd:post",
                         arr.ind = TRUE)
    
    sel.theta[index.theta, ] <- 1
    #-----------------------------------------------------------------------------
    #get the influence function of the TWFE regression
    twfe.inf.func <- as.vector(inf.reg %*% sel.theta)
    #-----------------------------------------------------------------------------
    if (boot == FALSE) {
      # Estimate of standard error
      se.twfe.att <- stats::sd(twfe.inf.func)/sqrt(length(twfe.inf.func))
      # Estimate of upper boudary of 95% CI
      uci <- twfe.att + 1.96 * se.twfe.att
      # Estimate of lower doundary of 95% CI
      lci <- twfe.att - 1.96 * se.twfe.att
      #Create this null vector so we can export the bootstrap draws too.
      twfe.boot <- NULL
    }
    
    if (boot == TRUE) {
      if (is.null(nboot) == TRUE) nboot = 999
      if(boot.type == "multiplier"){
        # do multiplier bootstrap
        twfe.boot <- mboot.twfep.did(n, twfe.inf.func, nboot)
        # get bootstrap std errors based on IQR
        se.twfe.att <- stats::IQR(twfe.boot) / (stats::qnorm(0.75) - stats::qnorm(0.25))
        # get symmtric critival values
        cv <- stats::quantile(abs(twfe.boot/se.twfe.att), probs = 0.95)
        # Estimate of upper boudary of 95% CI
        uci <- twfe.att + cv * se.twfe.att
        # Estimate of lower doundary of 95% CI
        lci <- twfe.att - cv * se.twfe.att
      } else {
        # do weighted bootstrap
        twfe.boot <- unlist(lapply(1:nboot, wboot.twfe.panel,
                                   n = n, y = y, dd = dd, post = post, x = x, i.weights = i.weights))
        # get bootstrap std errors based on IQR
        se.twfe.att <- stats::IQR((twfe.boot - twfe.att)) / (stats::qnorm(0.75) - stats::qnorm(0.25))
        # get symmtric critival values
        cv <- stats::quantile(abs((twfe.boot - twfe.att)/se.twfe.att), probs = 0.95)
        # Estimate of upper boudary of 95% CI
        uci <- twfe.att + cv * se.twfe.att
        # Estimate of lower doundary of 95% CI
        lci <- twfe.att - cv * se.twfe.att
        
      }
    }
  }
  
  #If no covariates, call ordid
  if(is.null(x)){
    
    # Create dta_long
    dta_long <- as.data.frame(cbind( y = y, post = post, d = dd,
                                     id = rep(1:n,2), w = i.weights))
    
    reg <- ordid(yname="y",
                 tname = "post",
                 idname = "id",
                 dname = "d",
                 weightsname  = "w",
                 xformla= NULL,
                 data = dta_long,
                 panel = TRUE,
                 boot = boot, boot.type = boot.type, nboot = nboot,
                 inffunc = inffunc)
    twfe.att <- reg$ATT
    se.twfe.att <- reg$se
    uci <- reg$uci
    lci <- reg$lci
    twfe.boot <- reg$boots
    att.inf.func <- reg$att.inf.func
  }
  
  att.inf.func <- twfe.inf.func
  if(inffunc == FALSE) att.inf.func <- NULL
  return(list(ATT = twfe.att,
              se = se.twfe.att,
              uci = uci,
              lci = lci,
              boots = twfe.boot,
              att.inf.func = att.inf.func))
}