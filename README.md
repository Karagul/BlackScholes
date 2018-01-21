# BlackScholes

### Package Requirements
|Software|Package Requirements|
|:-:|:-:|
|R|shiny, shinyjs, tidyverse, pracma, plot3D, plotly, Hmisc, glue|
|Python|os, sys, numpy, pandas, matplotlib, sqlite3, pickle|
|Other||

### Summary:
BlackScholes is a dashboard designed to be used to aid the development of options trading strategies and algorithms. An understanding of all the types of risk your strategy exposes you to is crucial to building and fine-tuning your model. One often overlooked type of risk can be measured by what are colloquially called 'the Greeks'.

These option Greeks are simply the derivatives of the Black Scholes pricing model with respect to each of the parameters, but even first-order greeks can have complex surfaces, and second or third-order greeks quickly become difficult to visualize in your mind's eye.

### Shiny App Documentation
There are 4 main tabs on the interface.

##### Greek Surface:

3D surface plot of the selected Greek. There are also inputs for Risk-free Rate, Volatility, Maturity, Moneyness range, and contract type.

##### P/L Chart

Displays a simple chart depicting the profit and loss scenarios for your chosen position, with axes determined by inputs for the contract details.

Ties in with the Strategy Planner tab

##### Pricing

Calculates theoretical Black Scholes price for European options contracts.

Can also be used to calculate Implied Volatility by providing a price instead of Vol

Includes a volatility surface (functionality to switch between relative and absolute Vol. surface is in progress.)

##### Strategy Planner

Helps to develop multi-option or synthetic option strategies.

Provides an interface to select a number of different options to build a tailored strategy. Table display for important Greeks, other tabular info.

Ties in with the P/L Chart tab


### References
* [Plotly R API](https://plot.ly/r/)

* [Shiny API](https://shiny.rstudio.com/reference/shiny/1.0.5/)
*
Black, F., & Scholes, M. (1973). The Pricing of Options and Corporate Liabilities. The Journal of Political Economy, 81(3), 637–654. Retrieved from [http://www.jstor.org/stable/1831029](http://www.jstor.org/stable/1831029)

Corrado, C. J., & Miller, T. W. (1996). A note on a simple, accurate formula to compute implied standard deviations. Journal of Banking & Finance, 20(1996), 595–603. [https://doi.org/10.1016/0378-4266](https://doi.org/10.1016/0378-4266)(95)00014-3

Haug, E. G. (2003). The collector: Know your weapon part 2. Wilmott, 4(3), 141–59. [https://doi.org/10.1002/wilm.42820030313](https://doi.org/10.1002/wilm.42820030313)

Haug, E. (2003). Know your weapon, Part 1. Wilmott Magazine, May, 49–57. [https://doi.org/10.1002/wilm.42820030313](https://doi.org/10.1002/wilm.42820030313)


### Author
Written by: Tyler Roberts
