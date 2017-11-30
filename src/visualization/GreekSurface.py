% matplotlib
notebook
import numpy as np
import pandas as pd
from scipy import stats
import sympy as sp
import math
import scipy
from matplotlib import style
from numpy import exp, arange
from pylab import meshgrid, cm, imshow, contour, clabel, colorbar, axis, title, show

import plotly
import plotly.plotly as py
import plotly.figure_factory as ff
import plotly.graph_objs as go

with open('C:\\Users\\Tyler\\plotlyapi.txt', 'r') as myfile:
    api = myfile.read()

plotly.tools.set_credentials_file(username='tcroberts', api_key=api)

style.use('fivethirtyeight')

class GreekSurface(object):
    """
    Container object for plotting Greeks of an Option and/or Synthetic position

    """
    def __init__(self, money_range=(0.5,1.5),
                 maturity_range=2, strike=1,vol=0.15,rfr=0.05):

        self.moneyness = np.linspace(strike*money_range[0],
                                     strike*money_range[1],num=100)
        self.maturities = [i+1 for i in range(maturity_range*365)]
        self.maturities = [(i/365) for i in self.maturities]
        self.maturity_label = [i*12 for i in self.maturities]
        self.strike = strike
        self.rfr = rfr
        self.vol = vol

        self.X,self.Y = meshgrid(self.moneyness, self.maturities)

        self.d1 = (1/(self.vol*np.sqrt(self.Y)))*\
                  (np.log((self.X)/self.strike) +
                  ((self.rfr + ((self.vol**2)/2))*self.Y))
        self.d2 = self.d1 - (self.vol*np.sqrt(self.Y))

    def plotFunc(self, Z, xlab, ylab, zlab):
        """
        
        :param Z: (numpy array) Contains values for z-axis for each (x,y) pair
        :param xlab: (str) Label for x-axis,
        :param ylab: (str) Label for y-axis,
        :param zlab: (str) Label for z-axis,
        :return: 
        """
        steps = list()
        data = go.Surface(z=Z)

        for i in range(len(data)):
            step = dict(method='restyle',
                        args = ['visible',[False]*len(data)])
            step['args'][1][i]=True #Toggle's the i-th trace to be visible
            steps.append(step)

        layout = go.Layout(
            xaxis=dict(
                title = xlab,
                range = [self.moneyness[0], self.moneyness[-1]],
                titlefont =dict(
                    family = 'Arial, sans-serif',
                    size = 18,
                    color = 'lightgrey'),
                showticklabels=True,
                tickangle=45,
                tickfont=dict(
                    family='Old Standard TT, serif',
                    size=14,
                    color='black'),
                exponentformat='e',
                showexponent='All'),
            yaxis=dict(
                title=ylab,
                range=[self.maturities[0],self.maturities[-1]],
                titlefont=dict(
                    family='Arial, sans-serif',
                    size=18,
                    color='lightgrey'),
                showticklabels=True,
                tickangle=45,
                tickfont=dict(
                    family='Old Standard TT, serif',
                    size = 14,
                    color='black'),
                exponentformat='e',
                showexponent='All'),
            sliders=[dict(
                active = self.vol,
                currentvalue={'prefix':'Volatility: '},
                pad = {"t":1},
                steps=steps),
                dict(active = self.rfr,
                    currentvalue={'prefix':'Risk Free Rate: '},
                    pad = {"t":1},
                    steps=steps)]

        )
        fig = go.Figure(data=data, layout=layout)

        return fig, data, layout


    def plot_surface(self, contract, greek, change_params = False):

        if not change_params:
            self.rfr = change_params[0]
            self.vol = change_params[1]
            self.d1 = (1 / (self.vol * np.sqrt(self.Y))) *\
                      (np.log((self.X) / self.strike) +
                      ((self.rfr + ((self.vol ** 2) / 2)) * self.Y))

            self.d2 = self.d1 - (self.vol * np.sqrt(self.Y))

        if greek == 'delta':
            if contract == 'call':
                Z = stats.norm.cdf(self.d1)
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            elif contract == 'put':
                Z = -(stats.norm.cdf(-self.d1))
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            else:
                return "contract type {} is not a valid input".format(contract)

        elif greek == 'vega':
            Z = self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2) * np.sqrt(self.Y)
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'theta':
            if contract == 'call':
                Z = -((self.X * scipy.stats.norm.pdf(self.d1) * self.vol) / (2 * self.Y)) - (
                self.rfr * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(self.d2))
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            if contract == 'put':
                Z = -((self.X * scipy.stats.norm.pdf(self.d1) * self.vol) / (2 * self.Y)) + (
                self.rfr * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(-self.d2))
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            else:
                return "contract type {} is not a valid input".format(contract)

        elif greek == 'rho':
            if contract == 'call':
                Z = self.strike * self.Y * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(self.d2)
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            elif contract == 'put':
                Z = -(self.strike * self.Y * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(-self.d2))
                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            else:
                return "contract type {} is not a valid input".format(contract)

        elif greek == 'psi':
            for i in self.moneyness:
                for j in self.maturities:
                    toplot = Option('TEST', [self.moneyness[i], 1, self.rfr, self.vol, self.maturities[j]])

        elif greek == 'gamma':
            Z = (self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2)) / (
            self.X * self.vol * np.sqrt(self.Y))
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'vanna':
            Z = scipy.stats.norm.pdf(self.d1) * (self.d2 / self.vol)
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'charm':
            Z = scipy.stats.norm.pdf(self.d1) * (((2 * self.rfr * self.Y) - (self.d2 * self.vol * np.sqrt(self.Y))) / (
            2 * self.Y * self.vol * np.sqrt(self.Y)))
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'thega':
            if contract == 'call':
                Z = -((self.X * self.vol * scipy.stats.norm.pdf(self.d1)) / (4 * self.Y * np.sqrt(self.Y))) * (
                1 + self.d1 * (((2 * (self.rfr) * self.Y) - (self.d2 * self.vol * np.sqrt(self.Y))) / (
                self.vol * np.sqrt(self.Y)))) - (
                    (self.rfr ** 2) * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(self.d2)) + (
                    (self.X * scipy.stats.norm.pdf(self.d1)) * (
                    (2 * (((self.rfr) ** 2) * self.Y) - (self.vol * np.sqrt(self.Y) * (self.rfr * self.d1))) / (
                    2 * self.vol * self.Y * np.sqrt(self.Y))))

                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            elif contract == 'put':
                Z = -((self.X * self.vol * scipy.stats.norm(self.d1)) / (4 * self.Y * np.sqrt(self.Y))) * (1 + (
                ((2 * (self.rfr) * self.Y) - (self.d2 * self.vol * np.sqrt(self.Y))) / (
                self.vol * np.sqrt(self.Y))) * self.d1) + (
                    (self.rfr ** 2) * self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.cdf(-self.d2)) + (
                    (self.X * scipy.stats.norm.pdf(self.d1)) * (
                    (2 * (((self.rfr) ** 2) * self.Y) - (self.vol * np.sqrt(self.Y) * (self.rfr * self.d1))) / (
                    2 * self.vol * self.Y * np.sqrt(self.Y))))

                fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

            else:
                return "contract type {} is not a valid input".format(contract)

        elif greek == 'speed':
            gamma = (self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2)) / (
            self.X * self.vol * np.sqrt(self.Y))
            Z = -(gamma / self.X) * (((self.d1) / (self.vol * np.sqrt(self.Y))) + 1)
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'zomma':
            gamma = (self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2)) / (
            self.X * self.vol * np.sqrt(self.Y))
            Z = gamma * ((self.d1 * self.d2 - 1) / self.vol)
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'colour':
            Z = -((scipy.stats.norm.pdf(self.d1)) / (2 * self.X * self.Y * self.vol * np.sqrt(self.Y))) * (1 + (
            ((2 * self.rfr * self.Y) - self.d2 * self.vol * np.sqrt(self.Y)) / (self.vol * np.sqrt(self.Y))) * self.d1)
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'veta':
            Z = self.X * scipy.stats.norm.pdf(self.d1) * np.sqrt(self.Y) * (
            ((self.rfr * self.d1) / (self.vol * np.sqrt(self.Y))) - ((1 + self.d1 * self.d2) / (2 * self.Y)))
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'vomma':
            vega = self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2) * np.sqrt(self.Y)
            Z = vega * ((self.d1 * self.d2) / (self.vol))
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        elif greek == 'ultima':
            vega = self.strike * np.exp((-self.rfr) * self.Y) * scipy.stats.norm.pdf(self.d2) * np.sqrt(self.Y)
            Z = (-vega / (self.vol ** 2)) * (
            (self.d1 * self.d2 * (1 - (self.d1 * self.d2))) + (self.d1 ** 2) + (self.d2 ** 2))
            fig, data, layout = self.plotFunc(Z, 'Spot', 'TTM', greek)

        else:
            return "{} is not a supported Greek surface".format(greek)

        return py.iplot(fig, filename='{}-surface plot'.format(greek))






