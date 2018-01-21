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
  if(strategy %in% c('Bull Call Spread', 'Bull Put Spread', 'Secured Short Put',
            'Married Put', 'Bear Call Spread','Bear Put Spread', 'Short Straddle', 'Short Strangle', "Collar")){
    
    strikes <- c(strike1, strike2)
    mean_strike <- mean(strikes)
    
    lower_x <- mean_strike - 0.3*mean_strike
    upper_x <- mean_strike + 0.3*mean_strike
    price_at_expiry <- c(0:100)
    
    if(strategy %in% c("Bull Call Spread", "Bull Put Spread", "Collar")) {
      
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        
        geom_segment(aes(x=0, xend=strike1, y=-premium1, yend=-premium1)) +
        
        geom_segment(aes(x=c(strike1, strike2),xend=c(strike2, 100),
                         y=c(-premium1, premium2-premium1),yend=c(premium2-premium1, premium2-premium1))) +
        
        
        #Plots the dotted guidelines at each of the kinks in the line.
        geom_segment(aes(x=c(strike1, strike2), xend=c(strike1, strike2),
                         y=c(0,0), yend=c(-premium1, premium2-premium1) ), linetype="dotted")+
        

        xlab("Underlying Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x, upper_x),ylim=c(-5,5)) +
        ggtitle(glue("{strategy}")) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      
      return(p)
    }
    
    else if(strategy %in% c("Bear Call Spread", "Bear Put Spread")){
      
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
        coord_cartesian(xlim=c(lower_x,upper_x),ylim=c(-5,5)) +
        ggtitle(glue({strategy})) +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
     
     return(p)
    }
    
    else if(strategy == "Short Straddle"){
      
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
        coord_cartesian(xlim=c(lower_x, upper_x), ylim=c(-10,10)) +
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
        
        geom_segment(aes(x=c(lower_x - 10, strike1, strike2),
                         xend=c(strike1, strike2, upper_x + 10),
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
        coord_cartesian(xlim=c(lower_x,upper_x),ylim=c(-5,5)) +
        ggtitle("Short Strangle") +
        theme_bw() +
        scale_y_continuous()+
        guides(linetype=FALSE)
      
      return(p)
    }
  }
  else{
    lower_x <- strike1 - 0.2*strike1
    upper_x <- strike1 + 0.2*strike1
    price_at_expiry <- c(0:100)
    
    
    if(strategy == "Long Call"){
      
      negx_limit <- -premium1
      posx_limit <- (upper_x - strike1)-premium1 + 10
      
      
      p <- ggplot(x=price_at_expiry) +
        geom_hline(yintercept=0, color='red') +
        geom_segment(aes(x=lower_x - 10, xend=strike1, y=-premium1, yend=negx_limit)) +
        
        geom_segment(aes(x=strike1,xend=upper_x + 10 ,y=-premium1, yend=posx_limit)) +
        
        # Dotted segment
        geom_segment(aes(x=strike1,
                         xend=strike1,
                         y=0,
                         yend=-premium1),
                     linetype="dotted")+
        
        xlab("Stock Price at Expiry") +
        ylab("Profit") +
        coord_cartesian(xlim=c(lower_x, upper_x),ylim=c(-5,5)) +
        scale_y_continuous() +
        ggtitle(glue("{strategy}")) +
        theme_bw()
      
      return(p)
    }
  }
  
  
  
  
}
  
  