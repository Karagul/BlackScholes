plot_pl <- function(strategy, contract1, strike1, premium1, contract2, strike2, premium2){
  #
  # Determines which P/L Plot to render in Shiny App.
  #
  # Args:
  #   contract1 (str):
  #   strike1 (numeric):  
  #   premium1 (numeric):
  #   contract2 (str):
  #   strike2 (numeric):  
  #   premium2 (numeric):
  #
  #
  #
  # Returns:
  #   
  #
  #
  #
  #
  

  
  if(strategy %in% c('Bull Call Spread', 'Bull Put Spread', 'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle', "Collar", "Iron Condor",
                     "Calendar Spread", "Covered Strangle", "Long Call Butterfly","Long Straddle", "Long Strangle", "Call Backspread", "Put Backspread")){
    
    strikes <- c(strike1, strike2)
    mean_strike <- mean(strikes)
    
    lower_x <- mean_strike - 0.3*mean_strike
    upper_x <- mean_strike + 0.3*mean_strike
    price_at_expiry <- c(0:100)
    
    if(strategy %in% c("Bull Call Spread", "Bull Put Spread", "Collar")) {
      
      # Profit limits off either side of the x axis
      negx_limit <- premium1-premium2
      posx_limit <- premium2-premium1
      
      #Plot
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=lower_x, xend=strike1, y=negx_limit, yend=negx_limit)) +
        
        geom_segment(aes(x=c(strike1, strike2),xend=c(strike2, upper_x),
                         y=c(negx_limit, premium2-premium1),yend=c(posx_limit, posx_limit))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=c(strike1, strike2), xend=c(strike1, strike2),
                         y=c(0,0), yend=c(-premium1, premium2-premium1) ), linetype="dotted")+
        
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x + 5, upper_x - 5),ylim=c(-5,5)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      
      
      return(p)
    }
    
    else if(strategy %in% c("Bear Call Spread", "Bear Put Spread")){
      
     negx_limit <- premium2 - premium1
     posx_limit <- premium1 - premium2
      
     p <-  ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=c(30, strike1, strike2),
                         xend=c(strike1, strike2, 100),
                         y=c(premium2-premium1, premium2-premium1, premium1-premium2),
                         yend=c(premium2-premium1, premium1-premium2, premium1-premium2))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=c(strike1, strike2), xend=c(strike1, strike2),
                         y=c(0,0), yend=c(premium1, premium1-premium2) ), linetype="dotted")+
        
        
        
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x + 5,upper_x - 5),ylim=c(-5,5)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
     
     return(p)
    }
    
    else if(strategy == "Short Straddle"){
      strike2 <- strike1
      negx_limit <- (premium1 + premium2) - (strike1 - lower_x)
      posx_limit <- (premium1 + premium2) - (upper_x - strike2)
      
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=c(lower_x, strike1),
                         xend=c(strike1, upper_x),
                         y=c(negx_limit, premium1+premium2),
                         yend=c(premium1+premium2, posx_limit))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=premium2+premium1),
                     linetype="dotted")+
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x + 5, upper_x - 5), ylim=c(-10,10)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      
      return(p)
      
    }
    
    else if(strategy == "Short Strangle"){
      negx_limit <- (premium1 + premium2) - (strike1 - lower_x)
      posx_limit <- (premium1 + premium2) - (upper_x - strike2)
      
      
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=c(lower_x, strike1, strike2),
                         xend=c(strike1, strike2, upper_x),
                         y=c(negx_limit, premium1, premium1),
                         yend=c(premium1, premium1, posx_limit))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=c(strike1, strike2),
                         xend=c(strike1, strike2),
                         y=c(0, 0),
                         yend=c(premium2-premium1, premium2-premium1)),
                     linetype="dotted")+
        
        
        
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x + 5,upper_x - 5),ylim=c(-5,5)) +
        ggtitle("Short Strangle") +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      
      return(p)
    }
  }
  else{
    lower_x <- strike1 - 0.3*strike1
    upper_x <- strike1 + 0.3*strike1
    price_at_expiry <- c(0:100)
    
    
    if(strategy %in% c("Long Call", "Married Put")){
      
      negx_limit <- -premium1
      posx_limit <- (upper_x - strike1)-premium1
      
      
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=lower_x, xend=strike1, y=negx_limit, yend=negx_limit)) +
        
        geom_segment(aes(x=strike1,xend=upper_x ,y=negx_limit, yend=posx_limit)) +
        
        # Dotted segment
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=-premium1),
                     linetype="dotted")+
        
        xlab("Stock Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x + 5, upper_x - 5), ylim=c(-5,5)) +
        scale_y_continuous() +
        ggtitle(glue("{strategy}")) +
        theme_bw()
      
      return(p)
    }
    
    else if(strategy %in% c("Covered Call", "Secured Short Put")){
      negx_limit <- premium1 - (strike1 - lower_x)
      posx_limit <- premium1
      
      
      p <-  ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=c(lower_x, strike1),
                         xend=c(strike1, upper_x),
                         y=c(negx_limit, posx_limit),
                         yend=c(posx_limit, posx_limit))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=premium1),
                     linetype="dotted")+
        
        
        
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(40,60),ylim=c(-5,5)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      return(p)
      
      
    }
    
    else if(strategy == "Long Put"){
      negx_limit <- (strike1 - lower_x) - premium1
      posx_limit <- -premium1
      
      
      p <-  ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=c(lower_x, strike1),
                         xend=c(strike1, upper_x),
                         y=c(negx_limit, posx_limit),
                         yend=c(posx_limit, posx_limit))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=-premium1),
                     linetype="dotted")+
        
        
        
        
        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(40,60),ylim=c(-5,5)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      return(p)
    }
    
  }
  
  
  
  
}
  
  