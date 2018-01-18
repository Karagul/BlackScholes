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

* Implied Voltility Approximations: [Corrado and Miller (1996)](http://quantlabs.net/academy/download/free_quant_instituitional_books_/%5BJournal%20of%20Banking%20Finance,%20Corrado%5D%20A%20note%20on%20a%20simple,%20accurate%20formula%20to%20compute%20implied%20standard%20deviations.pdf)

* Know Your Weapon I & II: [Haug, Epsen (2005)](http://www.espenhaug.com/articles.html)


### Author
Written by: Tyler Roberts
