lu_did_panel <-function(y1, y0, D, covariates, i.weights = NULL,
                        boot = FALSE, boot.type = "weighted", nboot = NULL,
                        lu_varname, 
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
    
    cov <- as.data.frame(covariates)
    
    cov['outcome'] = cov[, lu_varname]
    
    outcome <- cov$outcome
    outcome <- as.vector((c(outcome, outcome)))
    
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
      
      reg <- stats::lm(y ~  #dd:post:smallholder:plantation +
                         # dd:post:smallholder + dd:smallholder + post:smallholder+
                         #  dd:post:plantation + dd:plantation + post:plantation+
                         #  dd:post:shrub + dd:shrub + post:shrub+
                         dd:post:outcome + dd:outcome + post:outcome+
                         # dd:post:pasture + dd:pasture + post:pasture+
                         dd:post+ post + dd + x, x = TRUE, weights = p.weights)
      
      # if(interaction==TRUE) {
      # twfe.att <- reg$coefficients["dd:post:outcome"]
      #   } else {
      #    twfe.att <- reg$coefficients["dd:post"]
      # }
      twfe.att <- reg$coefficients["dd:post:outcome"]
      
      
      #-----------------------------------------------------------------------------
      #Elemenets for influence functions
      inf.reg <- (i.weights * reg$x * reg$residuals) %*%
        base::solve(crossprod(i.weights * reg$x, reg$x) / dim(reg$x)[1])
      
      sel.theta <- matrix(c(rep(0, dim(inf.reg)[2])))
      
      # if(interaction==TRUE) {
      #   index.theta <- which(dimnames(reg$x)[[2]]=="dd:post:outcome",
      #                        arr.ind = TRUE)
      # } else {
      #   index.theta <- which(dimnames(reg$x)[[2]]=="dd:post",
      #                        arr.ind = TRUE)
      # }
      index.theta <- which(dimnames(reg$x)[[2]]=="dd:post:outcome",
                                                   arr.ind = TRUE)
      
      
      sel.theta[index.theta, ] <- 1
      #-----------------------------------------------------------------------------
      #get the influence function of the TWFE regression
      twfe.inf.func <- as.vector(inf.reg %*% sel.theta)
      #-----------------------------------------------------------------------------

        
    }
    
    
    att.inf.func <- twfe.inf.func
    if(inffunc == FALSE) att.inf.func <- NULL
    return(list(ATT = twfe.att,
                att.inf.func = att.inf.func))
  }
  
  