library(shiny)
library(tidyverse)
library(pracma)
library(glue)


calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
}


greekSurface <- function(order, greek, contract, strike=1, vol=0.15, rfr=0.05,
                         maturity=1, money_range=c(0.5, 1.5)) {

  # Surface object for 3d plots of option greeks. Takes in arguments for the parameters of the plot.
  #
  #
  # Args:
  #   strike (numeric):
  #   vol (numeric):
  #   rfr (numeric):
  #   maturity (numeric):
  #   money_range (numeric):
  #
  # Returns:
  #   obj (list(X=grid$X, Y=grid$Y, strike=strike, vol=vol, rfr=rfr,
  #        maturity_range=maturity_range, money_range=money_range):
  #
  #             X is the X component of the meshgrid, Y is the Y component. Capitals denote meshgrid and not original variables.
  #             strike is if using actual stock data rather than a generic surface. vol and rfr are parameters for the greeks.
  #             money_range refers to moneyness, and the ranges are for the axes limits.
  #

    moneyness <-  seq(from=strike*money_range[1],
                 to=strike*money_range[2],by=(strike*money_range[2] - strike*money_range[1])/100)
    maturities <- seq(from=1, to=maturity*365 ,by=1)
  
  
    grid <- meshgrid(moneyness, maturities)
    
    if(order=="First"){
      if(greek=='delta'){
        Z <- calc_delta(contract, grid, strike, vol, rfr)
      }
      else if(greek=='vega'){
        Z <- calc_vega(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'theta'){
        Z <- calc_theta(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'rho'){
        Z <- calc_rho(contract, grid, strike, vol, rfr)
      }
      else{
        return(glue("Greek type: {greek} is not a valid input, or not a first-order greek"))
      }
    }
    
    else if(order=="Second"){
      if(greek == 'gamma'){
        Z <- calc_gamma(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'vanna'){
        Z <- calc_vanna(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'charm'){
        Z <- calc_charm(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'vomma'){
        Z <- calc_vomma(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'veta'){
        Z <- calc_veta(contract, grid, strike, vol, rfr)
      }
      else{
        return(glue("Greek type: {greek} is not a valid input, or not a second-order greek"))
      }
    }
    
    else if(order=="Third"){
      if(greek == 'colour'){
        Z <- calc_colour(contract, grid, strike, vol, rfr) 
      }
      else if(greek == 'zomma'){
        Z <- calc_zomma(contract, grid, strike, vol, rfr)
      }
      else if(greek == 'thega'){
        Z <- calc_thega(contract, grid, strike, vol, rfr) 
      }
      else if(greek == 'speed'){
        Z <- calc_speed(contract, grid, strike, vol, rfr) 
      }
      else if(greek == 'ultima'){
        Z <- calc_ultima(contract, grid, strike, vol, rfr) 
      }
      else{
        return(glue("Greek type: {greek} is not a valid input"))
      }
    }
    
    else{
      return(glue("Greek order: {order} is not a valid input"))
    }
      




  obj <- list(order=order, greek=greek, contract=contract, X=grid$X, Y=grid$Y, Z=Z, strike=strike, vol=vol, rfr=rfr,
              moneyness=moneyness, maturities=maturities, d1=d1)

   # Set the name of the class returned by this class function
  class(obj) <- append(class(obj), "greekSurface")

 return(obj)
}



plot_surface <- function(greekSurface){
  # Plotting method for the greekSurface object. 
  #
  # Args:
  #   greekSurface (obj): greekSurface object containing the option details.
  #
  # Returns:
  #   3D surface plot using Plotly's API.
  #
  #
  plot_ly(z = ~greekSurface$Z) %>% layout(
    title = glue("{capitalize(greekSurface$greek)} Surface"),
    scene = list(
      xaxis = list(title = "Moneyness"),
      yaxis = list(title = "Maturity"),
      zaxis = list(title = glue({greek}))
    )) %>%  add_surface(x=greekSurface$X, y=greekSurface$Y)
  
}
