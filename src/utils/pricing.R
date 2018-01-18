# ------------------------------- Imports ---------------------------------------- #
library(pracma)
library(tidyverse)
library(plotly)
library(Hmisc)
library(glue)


priceOption <- function(contract, strike, spot, ttm, vol, rfr){
  # Calculates the theoretical price of a European-style option contract
  # ## SHOULD I JUST FEED D1 INTO THIS TO MINIMIZE CALCULATIONS HERE? ##
  # 
  # Args:
  #
  #   contract (char):
  #   strike (numeric):
  #   spot (numeric):
  #   ttm (numeric):
  #   vol (numeric):
  #   rfr (numeric):
  #
  #
  # Returns:
  #   out (numeric): Black Scholes theoretical price for a Europoean-style option.
  #
  #
  #
  d1 <- (1/vol*sqrt(ttm))* (log(spot/strike) + (rfr + ((vol^2)/2))*ttm)
  d2 <- d1 - vol*sqrt(ttm)
  
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

implied_vol <- function(price, contract, spot, ttm){
  # Approximates the implied volatility of an option, given its price and other standard parameters.
  # Currently does fairly well within 10% of the current spot price, but errors begin to get large outside of that.
  #
  # Args:
  #   price (numeric):
  #   previous_vol (numeric):
  #   contract (char):
  #   strike (numeric):
  #   spot (numeric):
  #   ttm (numeric):
  #   rfr (numeric):
  #
  # Returns:
  #   vol
  #
  # Notes:
  #   Approximation formula from Brenner and Subrahmanyam (1988)
  #   Will be updated to make use of the improved approximation from Corrado & Miller (1996), or Stefanica and Radoicic (2017).
  #
  vol <- (sqrt((2*pi)/ttm)* price/spot)
  return(vol)
}