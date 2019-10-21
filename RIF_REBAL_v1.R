'''
Author: 7Leven
Purpose: Portfolio Optimization & Risk Analytics

Notes from last update:
Config area will be removed/made smaller in further updates..
'''
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyr)
source("C:\\Users\\patdj\\Documents\\RIF\\GUI\\v1\\SETTINGS_v1.R")
######################      MISC: CONFIG       #########################################

setwd(settings.directory) # sets the working directory
start_date <- settings.start #Format year-mo-da
end_date <- settings.end #gets current date
tar_return <- .1 #target return - 10%
tar_risk <- .05 #target risk variance - 5%
alp <- .01 #alpha
risk_free <- .0275 #risk free rate, treasury
risk_aversion <- settings.risk_av #risk aversion coefficient

tickers <- settings.tickers

weights <- c(settings.weights)

######################STEP ONE: Wrangling Data #########################################
portfolio <- NULL #normally you dont need to declare variables null before using them
portfolio.div <- NULL # R's scope requirements make this easier when binding within a loop..
for(ticker in tickers){ #looping through portfolio, gathering returns & dividends
  portfolio <- cbind(portfolio, getSymbols(ticker, from = start_date, to = end_date, auto.assign =  FALSE)[,6])
  portfolio.div <- cbind(portfolio.div, getDividends(ticker,from = start_date, to = end_date, auto.assign =  FALSE))
}
colnames(portfolio) <- tickers #making the dataframes look pretty
names(portfolio.div) <- gsub(".div", "", names(portfolio.div))

portfolio.yield <- colSums(portfolio.div, na.rm = TRUE) #calculating dividend yield
portfolio.yield <- as.data.frame(t(portfolio.yield)) #converting to row
portfolio.yield$MINT <- NULL #removing cash yield - placeholde

for(ticker in names(portfolio.yield)){ #adding dividends into total return. 
  portfolio[nrow(portfolio), ticker] <- as.double(portfolio[nrow(portfolio), ticker]) + as.double(portfolio.yield[ticker])
}

portfolio.returns <- CalculateReturns(portfolio) #calculating returns with dividends added
portfolio <- na.omit(portfolio) #removing empty/NA values
portfolio.returns <- as.timeSeries(portfolio.returns) #converting to time series, required below

meanReturns <- colMeans(portfolio.returns) #calculating mean return for each ticker 
covMat <- cov(portfolio.returns) #covariance matrix
portfolio.returns <- na.omit(portfolio.returns) # YOU MAY NEED THIS FOR ERRORS BELOW

######################STEP THREE: CONSTRUCTION #########################################

portSpec <- portfolioSpec( #this is where the risk aversion and other parameters are input
  model = list(type = "MV", optimize = "minRisk",
               estimator = "covEstimator", tailRisk = list(),
               params = list(alpha = alp, a = risk_aversion)), 
  portfolio = list(weights = NULL, targetReturn = tar_return,
                   targetRisk = tar_risk, riskFreeRate = risk_free, 
                   status = 0),
  optim = list(solver = "solveRquadprog", objective = NULL,
               params = list(meq = 2), control = list(), trace = FALSE))

######################STEP FOUR: PORTFOLIO GENERATION#########################################

#calculates efficient frontier, minimum variance portfolio, effiicent portfolio, minrisk
effFrontier <- portfolioFrontier(portfolio.returns, spec = portSpec, constraints = weights)
efmvPort <- minvariancePortfolio(portfolio.returns, spec = portSpec, constraints = weights)
efficientPort <- efficientPortfolio(portfolio.returns, spec = portSpec, constraints = weights)
minRiskPort <- minriskPortfolio(portfolio.returns, spec = portSpec, constraints = weights)
#the above functions work, but with new constraints added in, I need more memory to get more results
frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
frontierReturns <- getTargetReturn(effFrontier) #generates the return
frontier <- cbind(frontierWeights, frontierReturns) #formats return and weights into a dataframe
