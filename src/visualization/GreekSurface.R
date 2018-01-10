library(shiny)
library(tidyverse)
library(pracma)
library(glue)


calc_d <- function(grid, strike, vol, rfr){
  # Calculates the d1 portion of the Black Scholes formula
  out <- (1/(vol*sqrt(grid$Y)))*(log(grid$X)/strike) + ((rfr + ((vol^2))*grid$Y))
}


greekSurface <- function(greek, contract, strike=1, vol=0.15, rfr=0.05,
                         maturity=2, money_range=c(0.5, 1.5)) {

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
  #
  #
  #

  moneyness <-  seq(from=strike*money_range[0],
               to=strike*money_range[1],by=(strike*money_range[1] - strike*money_range[0])/100)
  maturities <- seq(from=1, to=maturity*365 ,by=1)


  grid <- meshgrid(moneyness, maturities)
  d1 <- calc_d(grid, strike, vol, rfr)
  d2 <- d1 - vol*sqrt(grid$Y)

  if(greek=='delta'){
      if(contract='call'){
          Z <- pnorm(d1)
      }
      else if(contract='put'){
          Z <- -pnorm(-d1)
      }
      else{
          return(glue("Contract type: {contract} is not a valid input"))
      }
  else if(greek=='vega'){
      Z = strike * exp((-rfr) * grid$Y) * dnorm(self.d2) * sqrt(grid$Y)
  }

  else if(greek == 'theta'){
        if(contract == 'call'){
            Z = -((X * dnorm(d1) * vol) / (2 * grid$Y)) - (rfr * strike * exp((-rfr) * grid$Y) * pnorm(d2))
        }

        else if(contract == 'put'){
            Z = -((X * dnorm(d1) * vol) / (2 * grid$Y)) + (self.rfr * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(-self.d2))
        }

        else{
            return(glue("Contract type: {contract} is not a valid input"))
        }
  }


  else if(greek == 'rho'){
      if(contract == 'call'){
          Z = strike * grid$Y * exp((-rfr) * grid$Y) * pnorm(d2)
      }

      else if(contract == 'put'){
      Z = -(strike * grid$Y * exp((-rfr) * grid$Y) * pnorm(-d2))
      }

      else{
          return(glue("Contract type: {contract} is not a valid input"))
      }
  }
  }




  obj <- list(greek=greek, contract=contract, X=grid$X, Y=grid$Y, Z=Z, strike=strike, vol=vol, rfr=rfr,
              moneyness=moneyness, maturities=maturities, d1=d1)

   # Set the name of the class returned by this class function
  class(obj) <- append(class(obj), "greekSurface")

 return(obj)
}



plot_surface <- function()



    # Python.

  
priceOption <- function(contract, strike, spot, ttm, vol, rfr){
  d1 <- (1/vol*sqrt(ttm))* (log(spot/strike) + (rfr + ((vol^2)/2))*ttm)
  d2 <- d1 - vol*sqrt(ttm)priceOption <- function(contract, strike, spot, ttm, vol, rfr)
    
  if(contract=='call'){
    out <- pnorm(d1)*spot - pnorm(d2)*strike*exp(-rfr*ttm)
    return(out)
  }
  else if(contract=='put'){
    out <- pnorm(-d2)*strike*exp(-rfr*ttm) - pnorm(-d1)*spot
    return(out)
  }
  else{
    return(glue("{capitalize(contract)} is not a valid contract type"))
  }
}

