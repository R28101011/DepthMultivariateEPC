OspaEPC <- function(Ospa, pn=0.95){
  # Set coverage probability
  alpha <- pnorm(-3) * 2
  # Function to determine cutoff depths
  m <- function(pn){
    if (0<pn && pn<=0.8){370}
    else if (0.8<pn && pn<=0.95){740}
    else if (0.95<pn && pn<=0.99){1111}
  }
  sigma <- function(pn){
    if (0<pn && pn<=0.8){2}
    else if (0.8<pn && pn<=0.9){1}
    else if (0.9<pn && pn<=0.99){0.5}
  }
  pp0 <- function(n){
    if(0<pn && pn<=0.7){1}
    else {pnorm(n/m(pn)-1, mean =0, sd =sigma(pn))}
  }
  
  limit <- function(u){
    if (0<u && u<=1/(n1)) {
      Rx[1]*(n1*u-floor(n1*u))
    } else if (1/(n1)<u && u<1-1/(n1*pp0(n))) {
      (1-(n1*u-floor(n1*u)))*Rx[floor(n1*u)]+(n1*u-floor(n1*u))*Rx[floor(n1*u)+1]
    } else {
      Rx[n]-(Rx[n]-Rx[n-1])*log(pp0(n)*n1*(1-u))+(Rx[n]-Rx[n-1])*(log(pp0(n)*n1*(1-u)))^2
    }
  }
  
  ################################################################################
  # Calculate Fractional Order Cutoffs
  n <- length(Ospa) # Total number of data points
  n1 <- n + 1       # Adjustment for indices
  Rx <- sort(Ospa)  # Sort depth values
  
  # Define a function to calculate fractional order for a given alpha
  
  calculate_fractional_order <- function(alpha, n, pn) {
    r_pn <- function(pn){
      if (0<pn && pn<=0.8){1}
      else if (0.8<pn && pn<=0.85){1.25}
      else if (0.85<pn && pn<=0.90){2}
      else if (0.90<pn && pn<=0.95){4}
      else if (0.95<pn && pn<=0.99){7}
    }
    
    fun1 <- function(r) {pbeta(1-alpha,r,n-r+1)-(1-pn)/(1+r_pn(pn)*pn)}
    uniroot(fun1, c(0, n + 1))$root / n1
  }
  # Calculate r_n values for alpha level
  r_n <- calculate_fractional_order(alpha, n, pn)
  r_n_0 <- 0.5
  # Calculate cutoff depths (Z_rn) using the limit function
  Z_rn <- as.numeric(limit(r_n))
  Z_rn_0 <- as.numeric(limit(r_n_0))
  return(list(UCL=Z_rn, ML=Z_rn_0))
}