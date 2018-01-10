# BlackScholes

### Package Requirements
|Software|Package Requirements|
|:-:|:-:|
|R|shiny, tidyverse, pracma, plot3D, plotly, Hmisc, gluc|
|Python|os, sys, numpy, pandas, matplotlib, sqlite3, pickle|
|Other||

### Summary:
BlackScholes is a dashboard designed to be used to aid the development of options trading strategies and algorithms. An understanding of all the types of risk your strategy exposes you to is crucial to building and fine-tuning your model. One often overlooked type of risk can be measured by what are colloquially called 'the Greeks'.

 These option Greeks are simply the derivatives of the Black Scholes pricing model with respect to each of the parameters, but even first-order greeks can have complex surfaces, and second or third-order greeks quickly become difficult to visualize in your mind's eye.

### Shiny App Documentation
There are 4 main tabs on the interface.

##### Greek Surface:

3D surface plot of the selected Greek. There are also inputs for Risk-free Rate, Volatility, Maturity, moneyness range, and contract type.

##### P/L Chart

##### Pricing

Calculates theoretical Black Scholes price for European options contracts.

Can also be used to calculate Implied Volatility by providing a price instead of Vol

##### Strategy Planner





### Author
Written by: Tyler Roberts
