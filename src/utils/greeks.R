# Utility Functions for calculating Option Greeks.

"
This R file contains functions to calculation options greeks for use in the GreekSurface object, for plotting in the Shiny app.

Also contains other useful utilities needed for the greek calculations such as d1.

Currently Supported Greeks are as follows:

First Order:
  Delta == Dprice/Dspot
  Vega == Dprice/Dvol
  Theta == Dprice/Dtime
  Rho == Dprice/Drfr

Second Order:
  Gamma == Ddelta/Dspot
  Vanna == Dvega/Dspot or Ddelta/Dvol
  Charm == Ddelta/Dtime
  Vomma == Dvega/Dvol
  Veta == Dvega/Dtime

Third Order:
  Colour == DgammaDtime
  Zomma == DgammaDvol
  Thega == 
  Speed == DgammaDspot
  Ultima == DvommaDvol
"
# Calculation for d1, which can then be used to compute d2. Needed for all greeks.
calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
  return(out)
}

# First Order Greeks
calc_delta <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Delta's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  if(contract=='call'){
    delta <- pnorm(d1)
  }
  else if(contract=='put'){
    delta <- pnorm(-d1)
  }
  else{
    return(glue("Contract type: {contract} is not a valid input"))
  }
  return(delta)
}  #Dprice/Dspot


calc_vega <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Vega's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  
  vega <- strike * exp((-rfr) * grid$Y) * dnorm(self.d2) * sqrt(grid$Y)
  return(vega)
}  #Dprice/Dvol


calc_theta <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Theta's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  
  if(contract == 'call'){
    theta = -((X * dnorm(d1) * vol) / (2 * grid$Y)) - (rfr * strike * exp((-rfr) * grid$Y) * pnorm(d2))
  }
  else if(contract == 'put'){
    theta = -((X * dnorm(d1) * vol) / (2 * grid$Y)) + (self.rfr * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(-self.d2))
  }
  
  else{
    return(glue("Contract type: {contract} is not a valid input"))
  }
  return(theta)
}  #Dprice/Dtime
  

calc_rho <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Theta's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)  
  
  if(contract == 'call'){
    rho = strike * grid$Y * exp((-rfr) * grid$Y) * pnorm(d2)
  }
  
  else if(contract == 'put'){
    rho = -(strike * grid$Y * exp((-rfr) * grid$Y) * pnorm(-d2))
  }
  
  else{
    return(glue("Contract type: {contract} is not a valid input"))
  }
  
  return(rho) 
}  #Dprice/Drfr


# Second-order Greeks
calc_gamma <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Gamma's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)  
  
  gamma <- (strike * exp((-rfr) * grid$Y) * dnorm(d2)) / (grid$X * vol * sqrt(grid$Y))
  return(gamma)
}  #Ddelta/Dspot
  

calc_vanna <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Vanna's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)  
  
  vanna <- dnorm(d1)*(d2/vol)
  return(vanna)
  
}  #Dvega/Dspot or Ddelta/Dvol


calc_charm <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Charm's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)  
  charm <- dnorm(d1)*(((2*rfr*grid$Y)-(d2*vol*sqrt(grid$Y)))/(2*grid$Y*vol*sqrt(grid$Y)))
  
  return(charm)
}  #Ddelta/Dtime


calc_vomma <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Vomma's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  vega <- calc_vega(contract, grid, strike, vol, rfr)
  vomma <- vega* ((d1*d2)/vol)
  
}  #Dvega/Dvol


calc_veta <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Veta's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  veta <- grid$X*dnorm(d1)*sqrt(grid$Y)*(((rfr*d1) / (vol*sqrt(grid$Y))) - ((1 + d1*d2) / (2*grid$Y)))
}  #Dvega/Dtime


# Third-order Greeks
calc_colour <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Colour's surface given parameters for an option
  #
  # UNSURE IF THIS FORMULA IS CORRECT RIGHT NOW
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  colour <- -((dnorm(d1))/(2*grid$X*grid$Y*vol*sqrt(grid$Y))) * (1 + (((2*rfr*grid$Y) - d2*vol*sqrt(grid$Y))/vol*sqrt(grid$Y))) * d1
}  #DgammaDtime


calc_zomma <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Zomma's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  gamma <- calc_gamma(contract, grid, strike, vol, rfr)
  zomma <- gamma*((d1*d2 - 1)/vol)
}  #DgammaDvol


calc_thega <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Thega's surface given parameters for an option
  #
  # Returns 'something'. not sure its thega though.
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)  
  if(contract=='call'){
    thega <- -((grid$X*vol*dnorm(d1))/(4*grid$Y*sqrt(grid$Y)))*(1+d1*(((2*rfr*grid$Y) - (d2*vol*sqrt(grid$Y)))/(vol*sqrt(grid$Y))))
    - ((rfr^2)*strike*exp((-rfr)*grid$Y)*pnorm(d2)) + ((grid$X*dnorm(d1))*(2*(((rfr)^2)&grid$Y) - vol*sqrt(grid$Y)*(rfr*d1)))/(2*vol*grid$Y*sqrt(grid$Y))
  }
  else if(contract=='put'){
    thega <-  -((grid$X * vol * dnorm(d1)) / (4 * grid$Y * sqrt(grid$Y))) * 
      (1 + (((2 * (rfr) * grid$Y) - (d2 * vol * sqrt(grid$Y))) / (vol * sqrt(grid$Y))) * d1) + 
      ((rfr^2) * strike *exp((-rfr) * grid$Y) * pnorm(-d2)) + 
      ((grid$X * dnorm(d1)) * ((2 * (((rfr) ** 2) * grid$Y) - (vol *sqrt(grid$Y) * (rfr * d1))) / (2 * vol *grid$Y * sqrt(grid$Y))))
  }
  else{
    return(glue("Contract type: {contract} is not a valid input"))
  }
  return(thega)
}  # 


calc_speed <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Speed's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  speed <- -(calc_gamma(contract, grid, strike, vol, rfr)/grid$X) * (((d1) / (vol*sqrt(grid$Y))) + 1)
  return(speed)
}  # DgammaDspot


calc_ultima <- function(contract, grid, strike, vol, rfr){
  #
  # Compute Ultima's surface given parameters for an option
  #
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)
  vega <- calc_vega(contract, grid, strike, vol, rfr)  
  ultima <- (-vega/(vol^2)) *((d1*d2*(1-(d1*d2))) + (d1^2) + (d2^2))
  
  
}  # DvommaDvol

