"

Utility functions for the Profit-Loss Chart tab.

Included Functions:
plot_pl(): Determines which chart to show; designed for use in a reactive element.

pl_help(): Determines which help text to show next to the chart. Also to be used in a reactive element.


"



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
  
  
pl_help <- function(strategy){
  
  if(strategy=="Long Call"){
    
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract. To enter this strategy, purchase a call. You will enter this strategy when you are bullish on the underlying") 
      max_pl <- glue("<b>Maxmium Profit:</b> Theoretically unlimited (bounded only by how much the underlying rises).  <br/> <b>Maxmium Loss:</b> Premium paid for the contract.")
      breakeven <- glue("<b>Breakeven Spot Price:</b>Strike price + Premium paid")
      tip <- glue("<b>Tip:</b>  Benefits from increased volatility, which raises the option premium, however time decay (theta) is your mortal enemy.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    
    })
    return(react_help)
  }
  
  else if(strategy=="Bull Call Spread"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, purchase a call at strike price A, and sell another call at strike price B where B > A. You will enter this strategy when you are bullish on the underlying, but want to mitigate your risk by capping your upside.") 
      max_pl <- glue("<b>Maxmium Profit:</b> (B-A) + net premium paid  <br/> <b>Maxmium Loss:</b> Net premium paid")
      breakeven <- glue("<b>Breakeven Spot Price:</b>A + net premium paid")
      tip <- glue("<b>Tip:</b> This strategy allows you to enter into a bullish position on an underlying stock while reducing the upfront cost of entry. This allows you to establish an upside target, and potentially take a larger position at the cost of lower maximum profit. If you are in the money, lower volatility is beneficial because your upside is called by the call you sold, which you can exit at a lower cost if volatility falls. If you were wrong, you want volatility to increase so that the value of the call you bought increases, and you can offset some of the losses from when you get assigned.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Bull Put Spread"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, sell a put at strike price A, and buy another put at strike price B where B > A. You will enter this strategy when you are bearish on the underlying, but want to mitigate your risk by capping your upside. If you are in the money, lower volatility is beneficial because your upside is called by the call you sold, which you can exit at a lower cost if volatility falls. If you were wrong, you want volatility to increase so that the value of the call you bought increases, and you can offset some of the losses from when you get assigned.") 
      max_pl <- glue("<b>Maxmium Profit:</b> (B - A) - net premium paid <br/> <b>Maxmium Loss:</b> Net premium paid.")
      breakeven <- glue("<b>Breakeven Spot Price:</b> B - Premium paid")
      tip <- glue("<b>Tip:</b>") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Covered Call"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract, and a position in the underlying.. To enter this strategy, sell a call when you already own the underlying asset. You will enter this strategy when you are bullish on the underlying, but willing to exit your position in the underlying at a certain price.") 
      max_pl <- glue("<b>Maxmium Profit:</b> (Strike - current spot) + premium recieved   <br/> <b>Maxmium Loss:</b> Theoretically unlimited (underlying could go to 0).")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Current spot - premium recived")
      tip <- glue("<b>Tip:</b> This strategy is often used to earn passive income on a portfolio of stocks. While the theoretical loss is unlimited, most of this comes from the decline in the underlying. A decrease in volatility is good for this strategy, as you will be able to close your position when the premium of the contract is lower. Each day, time decay also eats away at the premium, allowing you to exit at a lower cost.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Married Put"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract. To enter this strategy, purchase a put when you already own the underlying.. You will enter this strategy when you are bearish on the underlying, but do not want to exit your position just yet.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Theoretically unlimited (underlying goes to infinity).  <br/> <b>Maxmium Loss:</b> (Current spot - strike) + premium paid")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Current spot + premium paid")
      tip <- glue("<b>Tip: This strategy is often used as an alternative to stop-loss orders, since you can choose whether or not to exercise your option, whereas sometimes a stop-loss will triger when you did not want it to. Time reduces the value of your put, requiring the spot to increase more for you to breakeven. Volatility is your friend, as it will increase the contract value.</b>") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Secured Short Put"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract. To enter this strategy, sell a put while you have enough cash on hand to buy the underlying if you get assigned. You will enter this strategy when you are bullish on the underlying long-term, but not confident short-term") 
      max_pl <- glue("<b>Maxmium Profit:</b> Premium recieved. If you are assigned, then you enter a long position on the underlying.  <br/> <b>Maxmium Loss:</b> Limited to the strike price (if the underlying were to go to 0).")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Strike - premium recieved")
      tip <- glue("<b>Tip:</b> This strategy relies on stability. Decreased volatility and time decay will allow you to exit your short put at a lower cost, allowing you to lock in those profits early, or exit your position more favourably if it goes sour. ") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Long Put"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract. To enter this strategy, purchase a put. You will enter this strategy when you are bearish on the underlying") 
      max_pl <- glue("<b>Maxmium Profit:</b> Strike - premium paid if the underlying goes to 0  <br/> <b>Maxmium Loss:</b> Premium Paid")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Strike price - Premium paid")
      tip <- glue("<b>Tip:</b> This strategy is often used in place of a short position on the underlying. It allows you to enter a bearish position while capping your maximum loss. Time decay will reduce the value of your contract, but volatility is your friend.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Bear Put Spread"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, purchase a put at strike A, and sell another put at strike B where B > A. You will enter this strategy when you are bullish on the underlying, but expect either neutral or bearish movement in the short-term.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Net premium recieved <br/> <b>Maxmium Loss:</b> (B - A) - premium recieved")
      breakeven <- glue("<b>Breakeven Spot Price:</b> B - net premium recieved")
      tip <- glue("<b>Tip:</b> You want to strike a balance between a nice credit earned, and a reasonable upside target. Time decay is a net positive, due to the difference in strike prices on time decay of the two contracts. Volatility will be a negative if you are in the money, as it will make it more difficult for you to exit your short position, but if you are out of the money, increased volatility will let you close out the position early and not get assigned.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Bear Call Spread"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, sell a call at strike A, and then purchase a call at strike B where B > A. You will enter this strategy when you are bearish on the underlying, but think there could be short-term upside swings and want to limit your risk.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Net premium recieved  <br/> <b>Maxmium Loss:</b> (B - A) - net premium recieved")
      breakeven <- glue("<b>Breakeven Spot Price:</b> A + premium recieved")
      tip <- glue("<b>Tip:</b> You want to strike a balance between a nice credit earned, and a reasonable downside target. Time decay is a net positive, due to the difference in strike prices on time decay of the two contracts. Volatility will be a negative if you are in the money, as it will make it more difficult for you to exit your short position, but if you are out of the money, increased volatility will let you close out the position early and not get assigned.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else if(strategy=="Collar"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, purchase a put at strike A, and sell a call at strike B when you own the underlying, and where B > A. You will enter this strategy when you are bullish on the underlying, but afraid of short-term negative movement.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Either: B - (spot price + net premium paid) <b>or</b> B - (spot price - net premium recieved) <br/> <b>Maxmium Loss:</b> Either: Current spot - (A + net premium paid) <b>or</b> current spot + (A - net premium recieved)")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Two possibilities: If you entered the position earning a net credit, your break even is: current spot - net premium recieved. Otherwise, it is current spot + net premium paid.")
      tip <- glue("<b>Tip:</b> This position is kind of like a safer version of a covered call. You can earn some passive income on a stock position, while capping your downside loss. Time decay and volatility affect either side of your position equally so is neutral overall.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  
  else if(strategy=="Short Straddle"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, sell a call and a put at the same strike price. You want the current spot price to be around this level. You will enter this strategy when you do not think the underlying will move much until past the maturity of your contracts.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Net premium recieved  <br/> <b>Maxmium Loss:</b> If the stock goes up, losses are theoretically unlimited. If it goes down, losses are limited to strike - net premium recieved")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Either: A - net premium recieved <b>or</b> A + net premium recieved")
      tip <- glue("<b>Tip:</b> This strategy is not recommended for beginners! Generally this strategy is run when the trader is looking to short volatility, and expects to be able to exit their position early and lock in the profits from the premiums. Time decay will also help you exit your strategy faster, as it reduces the cost of your contracts each day.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  
  else if(strategy=="Short Strangle"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, sell a put at strike A, and sell a call at strike B where B > A. You want the current spot to be between these strike prices. You want the current spot price to be around this level. You will enter this strategy when you do not think the underlying will move much until past the maturity of your contracts.") 
      max_pl <- glue("<b>Maxmium Profit:</b> Net premium recieved  <br/> <b>Maxmium Loss:</b> If the stock goes up, losses are theoretically unlimited. If it goes down, losses are limited to A - net premium recieved")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Either: A - net premium recieved <b>or</b> B + net premium recieved")
      tip <- glue("<b>Tip:</b> This strategy is not recommended for beginners! Generally this strategy is run when the trader is looking to short volatility, and expects to be able to exit their position early and lock in the profits from the premiums. Time decay will also help you exit your strategy faster, as it reduces the cost of your contracts each day.") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  
  else if(strategy=="Long Strangle"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, purchase a put at strike A, and a call at strike B where B > A. You want the current spot to be between these strikes. You will enter this strategy when you expect large movements in the underlying") 
      max_pl <- glue("<b>Maxmium Profit:</b> If the underlying increases, profit is theoretically unlimited. and if the underlying decreases, profit is limited to A - net premium paid  <br/> <b>Maxmium Loss:</b> Net premium paid")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Either: A - net premium paid <b>or</b> B + net premium paid")
      tip <- glue("<b>Tip:</b> This strategy is best entered when you are expecting a significant swing in the underlying in the extremely short term. Time decay is your worst enemy, but increased volatility will make it easier to break even. You should be wary about this strategy around earnings releases, because options suffer from what is known as 'volatility crush' following these planned releases.") 
      
      HTML(paste(basic, max_pl, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  
  else if(strategy=="Long Straddle"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of two option contracts. To enter this strategy, purchase a put and a call both at the same strike price. You want the current spot to be between these strikes. You will enter this strategy when you expect large movements in the underlying") 
      max_pl <- glue("<b>Maxmium Profit:</b> If the underlying increases, profit is theoretically unlimited. and if the underlying decreases, profit is limited to strike - net premium paid  <br/> <b>Maxmium Loss:</b> Net premium paid")
      breakeven <- glue("<b>Breakeven Spot Price:</b> Either: Strike + net premium paid <b>or</b> strike - net premium paid")
      tip <- glue("<b>Tip:</b> This strategy is best entered when you are expecting a significant swing in the underlying in the extremely short term. Time decay is your worst enemy, but increased volatility will make it easier to break even. You should be wary about this strategy around earnings releases, because options suffer from what is known as 'volatility crush' following these planned releases.") 
      
      HTML(paste(basic, max_pl, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  
  
  else if(strategy=="Call Backspread"){
    react_help <- renderUI({
      basic <- glue("This strategy consists of one option contract. To enter this strategy, purchase a call. You will enter this strategy when you are bullish on the underlying") 
      max_pl <- glue("<b>Maxmium Profit:</b>   <br/> <b>Maxmium Loss:</b>")
      breakeven <- glue("<b>Breakeven Spot Price:</b>Strike price + Premium paid")
      tip <- glue("<b>Tip:</b>") 
      
      HTML(paste(basic, max_pl, breakeven, tip, sep="<br/> <br/>"))
    })
    return(react_help)
  }
  else{
    return(HTML("Uh oh, you shouldn't have gotten here! Please select a valid strategy and report this bug to the developer!"))
  }
}


  




