# Jamie Hackett
# X16386576
# Data clean up and prepping file


#LOading pacakges
library(ggplot2)
library(scales)
library(dplyr)
library(data.table)
library(tidyquant)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(broom)
library(tidyr)
library(zoo)
library(tidyquant)
library(Amelia)
library(mice)

#Setting the working directory to main file 
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Loading the inital stock price file
stock_prices <- read.csv("historical_stock_prices.csv", header=T, na.strings=c(""), stringsAsFactors = T)
#The above file isn't really needed now as I have an API that produces the same data with the additional column for Annual returns, but it's still used in the process of creating a list of tickers for the API to run through.

#Summary of the dataset and it's properies
str(stock_prices)
summary(stock_prices)

#Changing date from a factor to a date. 
stock_prices$date <- as.Date(stock_prices$date, format = "%Y-%m-%d")

#Cutting the data to 2009 onwards to match ratios availble with the API
stock_prices <- subset(stock_prices, date > "2009-01-01")

#Checking for nulls
sapply(stock_prices,function(x) sum(is.na(x)))
#No nulls

#Renaming one column to adjusted to work better with Quantmod pacakage
names(stock_prices)[names(stock_prices) == "adj_close"] <- "adjusted"

#Extracting the annual returns for the stock prices.
multpl_stock_yearly_returns <- stock_prices %>%
  group_by(ticker) %>%                            # We are grouping the stocks by the stock symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'yearly',
               col_rename = 'returns')

#Wirting it to CSV s owe can export and import without runing the above query again. 
write.csv(multpl_stock_yearly_returns, "stock_returns_annual.csv")
write.csv(stock_prices, "stock_prices_subsetted.csv")

#Next is to group the stock prices by year and calcualte their mean values so I can combine it with the return values

stock_prices_annual <- stock_prices %>% group_by(date=floor_date(date, "year"), ticker) %>%
  summarize(open=mean(open), close=mean(close), adjusted=mean(adjusted), low=mean(low), high=mean(high), volume=mean(volume))

#Grabbing the years so I can match them with the Financial ratios
stock_prices_annual$year <- format(as.Date(stock_prices_annual$date, format="%d/%m/%Y"),"%Y")
multpl_stock_yearly_returns$year <- format(as.Date(multpl_stock_yearly_returns$date, format="%d/%m/%Y"),"%Y")

#Combining the two dataframes into one dataframe
stock_prices_ratios <- merge(multpl_stock_yearly_returns, stock_prices_annual, by=c("ticker", "year"))
#Extracting that data into a csv
write.csv(stock_prices_ratios, "stock_prices_ratios.csv")

stock_prices_ratios <- read.csv("stock_prices_ratios.csv")

#Bringing in the ratios and company descriptions 
#Switch directors to open that up first. 
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP/Financial_Ratios_Data_Extract")
ratios_description <- read.csv("ratios_description.csv")

#Switching back over to main directory 
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Change column name from symbol to ticker
names(ratios_description)[names(ratios_description) == "symbol"] <- "ticker"

#Merging this stock data
stock_ratios_desc <- merge(stock_prices_ratios,ratios_description , by=c("ticker", "year"))

#Checking for nulls
sapply(stock_ratios_desc,function(x) sum(is.na(x)))
#Creating a visulisation of this 
missmap(stock_ratios_desc, main="Missing values vs observed")

#Dropping the columns not needed
stock_ratios_desc <- select(stock_ratios_desc, -c(X.x, X.1, X.y, daysOfSalesOutstanding, daysOfInventoryOutstanding, operatingCycle, daysOfPayablesOutstanding, cashConversionCycle, totalDebtToCapitalization, cashFlowToDebtRatio, inventoryTurnover, operatingCashFlowSalesRatio, cashFlowCoverageRatios, shortTermCoverageRatios, dividendPaidAndCapexCoverageRatio, dividendPayoutRatio, dividendYield, price, beta, range, changes, dcfDiff, dcf))

write.csv(stock_ratios_desc, "stock_ratios_desc_reduced.csv")

#Running mice on various columns

imp1 <- parlmice(data = stock_ratios_desc[, !names(stock_ratios_desc) %in%
                                            c('ticker','year','date.x','returns','date.y','open','close','adjusted','low','high','volume','date','volAvg','mktCap','lastDiv','companyName','exchange','industry','website','description','ceo','sector')], m=5, method = 'rf', n.core = 4, n.imp.core = 5)
mice_output <- complete(imp1)

#Adding the mice output to the dataframe
stock_ratios_desc$currentRatio <- mice_output$currentRatio
stock_ratios_desc$quickRatio <- mice_output$quickRatio
stock_ratios_desc$cashRatio <- mice_output$cashRatio
stock_ratios_desc$grossProfitMargin <- mice_output$grossProfitMargin
stock_ratios_desc$operatingProfitMargin <- mice_output$operatingProfitMargin
stock_ratios_desc$pretaxProfitMargin <- mice_output$pretaxProfitMargin
stock_ratios_desc$netProfitMargin <- mice_output$netProfitMargin
stock_ratios_desc$effectiveTaxRate <- mice_output$effectiveTaxRate
stock_ratios_desc$returnOnAssets <- mice_output$returnOnAssets
stock_ratios_desc$returnOnEquity <- mice_output$returnOnEquity
stock_ratios_desc$returnOnCapitalEmployed <- mice_output$returnOnCapitalEmployed
stock_ratios_desc$netIncomePerEBT <- mice_output$netIncomePerEBT
stock_ratios_desc$ebtPerEbit <- mice_output$ebtPerEbit
stock_ratios_desc$debtRatio <- mice_output$debtRatio
stock_ratios_desc$debtEquityRatio <- mice_output$debtEquityRatio
stock_ratios_desc$longTermDebtToCapitalization <- mice_output$longTermDebtToCapitalization
stock_ratios_desc$interestCoverage <- mice_output$interestCoverage
stock_ratios_desc$companyEquityMultiplier <- mice_output$companyEquityMultiplier
stock_ratios_desc$receivablesTurnover <- mice_output$receivablesTurnover
stock_ratios_desc$payablesTurnover <- mice_output$payablesTurnover
stock_ratios_desc$fixedAssetTurnover <- mice_output$fixedAssetTurnover
stock_ratios_desc$assetTurnover <- mice_output$assetTurnover
stock_ratios_desc$operatingCashFlowPerShare <- mice_output$operatingCashFlowPerShare
stock_ratios_desc$freeCashFlowPerShare <- mice_output$freeCashFlowPerShare
stock_ratios_desc$cashPerShare <- mice_output$cashPerShare
stock_ratios_desc$payoutRatio <- mice_output$payoutRatio
stock_ratios_desc$freeCashFlowOperatingCashFlowRatio <- mice_output$freeCashFlowOperatingCashFlowRatio
stock_ratios_desc$capitalExpenditureCoverageRatio <- mice_output$capitalExpenditureCoverageRatio
stock_ratios_desc$priceBookValueRatio <- mice_output$priceBookValueRatio
stock_ratios_desc$priceToBookRatio <- mice_output$priceToBookRatio
stock_ratios_desc$priceToSalesRatio <- mice_output$priceToSalesRatio
stock_ratios_desc$priceEarningsRatio <- mice_output$priceEarningsRatio
stock_ratios_desc$priceToFreeCashFlowsRatio <- mice_output$priceToFreeCashFlowsRatio
stock_ratios_desc$priceToOperatingCashFlowsRatio <- mice_output$priceToOperatingCashFlowsRatio
stock_ratios_desc$priceCashFlowRatio <- mice_output$priceCashFlowRatio
stock_ratios_desc$priceEarningsToGrowthRatio <- mice_output$priceEarningsToGrowthRatio
stock_ratios_desc$priceSalesRatio <- mice_output$priceSalesRatio
stock_ratios_desc$enterpriseValueMultiple <- mice_output$enterpriseValueMultiple

#Recheck to see if there is any missing values 
sapply(stock_ratios_desc,function(x) sum(is.na(x)))

#Dropping the columns that Mice could not calcualte the values for
stock_ratios_desc <- select(stock_ratios_desc, -c(priceCashFlowRatio, priceSalesRatio, priceFairValue, ebitPerRevenue, priceToBookRatio))

#Recheck to see if there is any missing values 
sapply(stock_ratios_desc,function(x) sum(is.na(x)))

#Writing the data to a csv
write.csv(stock_ratios_desc, "stock_ratios_desc_final.csv")

#Removing outliers found
#Removing Sol-Gel Technologies Ltd, outlier 
stock_ratios_desc <- stock_ratios_desc[!stock_ratios_desc$companyName == "Sol-Gel Technologies Ltd.", ]

#Removing Company GrafTech International Ltd. outlier
stock_ratios_desc <- stock_ratios_desc[!stock_ratios_desc$companyName == "GrafTech International Ltd.", ]

write.csv(stock_ratios_desc, "stock_ratios_desc_final.csv")

