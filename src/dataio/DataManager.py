class DataManager(object):
    """
    Creates an object which contains the data used in the algo
    """
    def __init__(self):
        self.tickers = []
        self.dataframes = {}
        self.options = {}

    def __str__(self):
        return "DataManager currently contains data for: {}".format([i for i in tickerlist])

    def createDataframes(self, tickerlist, folder):
        """
        Initiates the Pandas dataframes for all tickers in tickerlist that can be found in folder

        :param tickerlist: list of strings: list of stock tickers whose data to import.
        :param folder: String: path to the folder where the price/volume data is saved.

        :return: No return value, instead changes the self.dataframes attribute to be a dicitonary of dataframes.
        The keys for this dictionary are the tickers, the values are the dataframes.
        """
        # example of folder = 'C:\Users\RobertsTy\Desktop\Documents\Misc Excel'
        self.tickers = tickerlist
        for ticker in tickerlist:
            # Reads the tickers into a pandas dataframe for futher analysis
            tickerpath = folder + "\\{}.csv".format(ticker)
            stockdata = pd.read_csv(tickerpath)

            # Adjusts the index of the dataframe to be date, adds in simple MA & crosses to df as well
            stockdata = stockdata.iloc[::-1]
            stockdata['20MA'] = stockdata['Adj Close'].rolling(center=False, window=20).mean()
            stockdata['50MA'] = stockdata['Adj Close'].rolling(center=False, window=50).mean()
            stockdata['Previous20MA'] = stockdata['20MA'].shift(1)
            stockdata['Previous50MA'] = stockdata['50MA'].shift(1)
            ##Where shortcrosslong is the 20MA risign above the 50MA, and longcrossshort is the 50MA rising above the 20MA
            shortcrosslong = (
            (stockdata['20MA'] >= stockdata["50MA"]) & stockdata['Previous20MA'] <= stockdata['Previous50MA'])
            longcrossshort = (
            (stockdata['20MA'] <= stockdata["50MA"]) & stockdata['Previous20MA'] >= stockdata['Previous50MA'])
            stockdata['shortcrosslong'] = shortcrosslong
            stockdata['longcrossshort'] = longcrossshort

            #             shortcrosslong_dates = ticker_price.loc[shortcrosslong, 'Date']
            #             longcrossshort_dates = ticker_price.loc[longcrossshort, 'Date']
            #             print(shortcrosslong_dates, longcrossshort_dates)

            # Save dataframes to self.dataframes with the ticker as a key
            self.dataframes[ticker] = stockdata

    def calcVol(self, tickerlist, n):
        """
        Calculate historical volatility for use in the Black-Scholes calculations later on.
        Stores these values in DataManager.dataframes

        :param tickerlist: list of strings: indicates the tickers to perform this operation on
        :param n: int: number of periods to use in calculating historical Volatility
        :return: No return value, but updates the self.dataframes attribute for the given tickers to include a "Vol" column

        """
        for ticker in tickerlist:
            df = self.dataframes[ticker]
            df['DailyRet'] = (df['Adj Close'] / df["Adj Close"].shift(1)) - 1
            df['RetDev'] = df['DailyRet'].rolling(center=False, window=n).std()
            df["{}Vol".format(n)] = df["RetDev"] * np.sqrt(252)

            self.dataframes[ticker]['{}Vol'.format(n)] = df['{}Vol'.format(n)]

        return self.dataframes[ticker]['{}Vol'.format(n)] # Why did i have this returning?

    def calcRSI(self, tickerlist, n=14):
        """
        Calculates RSI for a list of given tickers stored in the DataManager object.
        Stores these values in DataManager.dataframes

        :param tickerlist: list of strings: list of tickers to perform the RSI calculation on
        :param n: int: Period length for RSI calculation

        :return: No return value, simply updates the self.dataframes attribute to include an "RSI" column
        """
        # Takes a tickerlist as an argument (should be able to use self.tickers). Default is 14-period RSI.

        for ticker in tickerlist:
            if ticker in self.dataframes.keys():
                df = self.dataframes[ticker]

                # Calculates RSi
                delta = df['Adj Close'].diff()
                df['Change'] = delta
                dUp, dDown = delta.copy(), delta.copy()
                dUp[dUp < 0] = 0
                dDown[dDown > 0] = 0
                rollUp = dUp.rolling(center=False, window=n).mean()
                rollDown = dDown.rolling(center=False, window=n).mean()
                RS = rollUp / rollDown
                rsi = 100.0 - (100.0 / (1.0 + RS))
                df['RSI'] = rsi

                # Stores new dataframe back in self
                self.dataframes[ticker] = df
            else:
                print('{} dataframe not found in DataManager'.format(ticker))

        return self.dataframes

    def bollingerBands(self, tickerlist, n=20, k=2):
        """
        Calculates Bollinger Bands for the stocks in tickerlist.
        Stores these values in DataManager.dataframes

        :param tickerlist: list of strings: Tickers to perform the Bollinger Band calculation on
        :param n: int: period length for Bollinger Band updating. Default = 20
        :param k: int: number of standard deviations to be used to calculate Bollinger Band values. Default = 2

        :return: returns the updated self.dataframes attirbute.
        """

        for ticker in tickerlist:
            if ticker in self.dataframes.keys():
                self.dataframes[ticker] = df

                # Calculates top and bottom bollinger bands for n periods and k standard deviations
                df['20STD'] = df['Adj Close'].rolling(center=False, window=n).std()
                df['bollingertop'] = df['20MA'] + k * df['20STD']
                df['bollingerbottom'] = df['20MA'] - k * df['20STD']

                # Stores new dataframe back to self
                self.dataframes[ticker] = df

            else:
                print('{} dataframe not found in DataManager'.format(ticker))

        return self.dataframes

    def calcMACD(self, tickerlist, n1=26, n2=12):
        """
        Calculates the Moving Average Convergence Divergence indicator for the stocks in tickerlist
        Stores these values in DataManager.dataframes

        :param tickerlist: list of strings: Tickers to perform the above operation on.
        :param n1: int: 1st Period to be used in EMA calculation. Default = 26
        :param n2: int: 2nd Period to be used in EMA calculation. Default = 12

        :return: returns the self.dataframes attribute.
        Will still update the self.dataframes attribute even if return is supressed
        """
        # Takes tickerlist as an argument and defaults to 26 and 12 period EMAs
        for ticker in tickerlist:
            if ticker in self.dataframes.keys():
                self.dataframes[ticker] = df

                # First, calculates n1-period and n2-period EMAs
                df['{}EMA'.format(n1)] = df['Adj Close'].ewm(span=n1).mean()
                df['{}EMA'.format(n2)] = df['Adj Close'].ewm(span=n2).mean()

                # MACD and MACDSignal are what is generally plotted in technical charts. This is how we get buy/sell signals
                df['MACD'] = df['{}EMA'.format(n1)] - df['{}EMA'.format(n2)]
                df['MACDSignal'] = df['MACD'].ewm(span=9).mean()

                # Calculating buy/sell signals
                df['PreviousMACD'] = df['MACD'].shift(1)
                df['PreviousSignal'] = df['MACDSignal'].shift(1)
                df['MACDBuy'] = ((df['MACDSignal'] >= df['MACD']) & (df['PreviousSignal'] <= df['PreviousMACD']))
                df['MACDSell'] = ((df['MACDSignal'] <= df['MACD']) & (df['PreviousSignal'] >= df['PreviousMACD']))

                # Locates the dates for the above calculated buy/sell signals

                #             buysignal_dates = ticker_price.loc[buysignal, 'Date']
                #             sellsignal_dates = ticker_price.loc[sellsignal, 'Date']
                #             print(buysignal_dates, sellsignal_dates)

                # Stores new dataframe back in self
                self.dataframes[ticker] = df
            else:
                print("{} dataframe not found in DataManager".format(ticker))
        return self.dataframes

    def createOption(self, ticker, contract, strike, rfr=0.03, ttm, voln=20, divinfo=[False, False]):
        """
        Creates an Option object and stores it in the DataManager.options attribute.

        :param ticker: str: ticker to create option for
        :param contract: str: "Put" or "Call" are acceptable values
        :param strike: float: Strike price of the option to be calculated
        :param rfr: float: Prevailing risk free rate to discouting calculations
        :param ttm: int: Time to maturity
        :param voln: Number of periods to use for historical volatility.
        :param divinfo: Dividend yield/year

        :return: Updated self.options attribute which is a dictionary
        ticker is the key, Option object is the value, which contains option information.
        """
        # Input list to the Option class
        fundlist = [self.dataframes[ticker]['Adj Close'].iloc[-1:], strike, rfr,
                    self.dataframes[ticker]['{}Vol'.format(n)].iloc[-1:], ttm]

        option = Option(ticker, fundlist, contract, divinfo)
        # self.options is a dictionary of dictionaries, with tickers as keys.
        self.options[ticker] = {}

        # The internal dictionaries have information about the option as their keys, e.g. C15 is a call with strike = 15
        # The values in these dictionaries are the Option objects, which allows us to take advantage of the Option methods
        if contract == 'call':
            self.options[ticker]['C{}'.format(strike)] = option
        elif contract == 'put':
            self.options[ticker]['P{}'.format(strike)] = option

        return self.options