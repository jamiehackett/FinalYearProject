# Jamie Hackett
# X16386576
# Machine Learning
# Decision Trees
# Credit to Noel Cosgrave for Tutorial on DT

#Loading required packages
library(dplyr)
library(C50) 
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(ROCR) 
library(rattle) 

#Setting the working directory
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Loading the data required
stock_ratios <- read.csv("stock_ratios_desc_final.csv")

#Engineering a feature so that it measures if the stock has gone up or not gone up. 
stock_ratios$result <- ifelse(stock_ratios$returns < 0, "LOSS", "GAIN")

#Creating a sample 
set.seed(2002)
stock_ratios_sample <- sample_n(stock_ratios, 1000)

########## MACHINE LEARNING DECISION TREE ##########
DT.DF <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test split 80:20
set.seed(2002)
dt.train.index <- sample(1:nrow(DT.DF),ceiling(0.8*nrow(DT.DF)))
dt.train <- DT.DF[dt.train.index,]
dt.test <- DT.DF[-dt.train.index,]

#Create DT with C5.0 model. 
treeControl <- C5.0Control(
  subset = FALSE,
  bands = 0,
  winnow = FALSE,
  noGlobalPruning = FALSE,
  CF = 0.25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  earlyStopping = TRUE
)
fit.C5.0 <- C5.0(
  x = dt.train[, -34],
  y = dt.train$stock_ratios_sample.result,
  control = treeControl,
)

#Print summary of the model 
summary(fit.C5.0)
plot(fit.C5.0)

#Test the performance of the model
C5.0.predict <- predict(
  fit.C5.0,
  dt.test[, -34],
  type = "class"
)
confusionMatrix(C5.0.predict, dt.test[,34])

#Model with recursive partioning (rpart)
rpart.Control <- rpart.control(
  minsplit = 20,
  xval = 10
)
fit.rpart <- rpart(
  stock_ratios_sample.result ~ .,
  data = dt.train,
  method='class',
  control = rpart.Control
)
fit.rpart

prp(fit.rpart, faclen = 0, cex = 0.8, extra = 1)

#Performance of the model
rpart.predict <- predict(
  fit.rpart,
  dt.test[, -34],
  type = "class"
)

confusionMatrix(rpart.predict,dt.test[,34])

#Prune the tree
bestcp <- fit.rpart$cptable[
  which.min(fit.rpart$cptable[,"xerror"]),
  "CP"
  ]
fit.rpart.pruned <- prune(fit.rpart, cp = bestcp)
prp(fit.rpart.pruned, faclen = 0, cex = 0.7, extra = 1)
fancyRpartPlot(fit.rpart.pruned)

#Evaluate the performance of the model
rpart.predict.pruned <- predict(
  fit.rpart.pruned,
  dt.test[, -34],
  type = "class"
)
confusionMatrix(rpart.predict.pruned,dt.test[,34])

#Now trying with a larger sample. 

set.seed(2002)
stock_ratios_sample <- sample_n(stock_ratios, 10000)

########## MACHINE LEARNING DECISION TREE ##########
DT.DF.LARGE <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test split 80:20
set.seed(2002)
dt.train.index.large <- sample(1:nrow(DT.DF.LARGE),ceiling(0.8*nrow(DT.DF.LARGE)))
dt.train.large <- DT.DF.LARGE[dt.train.index.large,]
dt.test.large <- DT.DF.LARGE[-dt.train.index.large,]

#Create DT with C5.0 model. 
treeControl <- C5.0Control(
  subset = FALSE,
  bands = 0,
  winnow = FALSE,
  noGlobalPruning = FALSE,
  CF = 0.25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  earlyStopping = TRUE
)
fit.C5.0 <- C5.0(
  x = dt.train.large[, -34],
  y = dt.train.large$stock_ratios_sample.result,
  control = treeControl,
)

#Print summary of the model 
summary(fit.C5.0)
plot(fit.C5.0)

#Test the performance of the model
C5.0.predict <- predict(
  fit.C5.0,
  dt.train.large[, -34],
  type = "class"
)
confusionMatrix(C5.0.predict, dt.train.large[,34])

#Model with recursive partioning (rpart)
rpart.Control <- rpart.control(
  minsplit = 20,
  xval = 10
)
fit.rpart <- rpart(
  stock_ratios_sample.result ~ .,
  data = dt.train.large,
  method='class',
  control = rpart.Control
)
fit.rpart

prp(fit.rpart, faclen = 0, cex = 0.8, extra = 1)

#Performance of the model
rpart.predict <- predict(
  fit.rpart,
  dt.test.large[, -34],
  type = "class"
)
confusionMatrix(rpart.predict,dt.test.large[,34])

#Prune the tree
bestcp <- fit.rpart$cptable[
  which.min(fit.rpart$cptable[,"xerror"]),
  "CP"
  ]
fit.rpart.pruned <- prune(fit.rpart, cp = bestcp)
prp(fit.rpart.pruned, faclen = 0, cex = 0.7, extra = 1)
fancyRpartPlot(fit.rpart.pruned)

#Evaluate the performance of the model
rpart.predict.pruned <- predict(
  fit.rpart.pruned,
  dt.test.large[, -34],
  type = "class"
)
confusionMatrix(rpart.predict.pruned,dt.test.large[,34])

#Going rerun the models for each sector this time. 

#Healthcare sector
Healthcare <- stock_ratios[stock_ratios$sector == "Healthcare",]

set.seed(2002)
stock_ratios_sample <- sample_n(Healthcare, 1000)

########## MACHINE LEARNING SVM FOR EACH SECTOR ##########
DT.DF.HEALTHCARE <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test split 80:20
set.seed(2002)
dt.train.index.healthcare <- sample(1:nrow(DT.DF.HEALTHCARE),ceiling(0.8*nrow(DT.DF.HEALTHCARE)))
dt.train.healthcare <- DT.DF.HEALTHCARE[dt.train.index.healthcare,]
dt.test.healthcare <- DT.DF.HEALTHCARE[-dt.train.index.healthcare,]

#Create DT with C5.0 model. 
treeControl <- C5.0Control(
  subset = FALSE,
  bands = 0,
  winnow = FALSE,
  noGlobalPruning = FALSE,
  CF = 0.25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  earlyStopping = TRUE
)
fit.C5.0 <- C5.0(
  x = dt.train.healthcare[, -34],
  y = dt.train.healthcare$stock_ratios_sample.result,
  control = treeControl,
)

#Print summary of the model 
summary(fit.C5.0)
plot(fit.C5.0)

#Test the performance of the model
C5.0.predict <- predict(
  fit.C5.0,
  dt.test.healthcare[, -34],
  type = "class"
)
confusionMatrix(C5.0.predict, dt.test.healthcare[,34])

#Model with recursive partioning (rpart)
rpart.Control <- rpart.control(
  minsplit = 20,
  xval = 10
)
fit.rpart <- rpart(
  stock_ratios_sample.result ~ .,
  data = dt.train.healthcare,
  method='class',
  control = rpart.Control
)
fit.rpart

prp(fit.rpart, faclen = 0, cex = 0.8, extra = 1)

#Performance of the model
rpart.predict <- predict(
  fit.rpart,
  dt.test.healthcare[, -34],
  type = "class"
)
confusionMatrix(rpart.predict,dt.test.healthcare[,34])

#Prune the tree
bestcp <- fit.rpart$cptable[
  which.min(fit.rpart$cptable[,"xerror"]),
  "CP"
  ]
fit.rpart.pruned <- prune(fit.rpart, cp = bestcp)
prp(fit.rpart.pruned, faclen = 0, cex = 0.7, extra = 1)
fancyRpartPlot(fit.rpart.pruned)

#Evaluate the performance of the model
rpart.predict.pruned <- predict(
  fit.rpart.pruned,
  dt.test.healthcare[, -34],
  type = "class"
)
confusionMatrix(rpart.predict.pruned,dt.test.healthcare[,34])


#Energy sector
Energy <- stock_ratios[stock_ratios$sector == "Energy",]

set.seed(2002)
stock_ratios_sample <- sample_n(Energy, 1000)

########## MACHINE LEARNING SVM FOR EACH SECTOR ##########
DT.DF.ENERGY <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test split 80:20
set.seed(2002)
dt.train.index.energy <- sample(1:nrow(DT.DF.ENERGY),ceiling(0.8*nrow(DT.DF.ENERGY)))
dt.train.energy <- DT.DF.ENERGY[dt.train.index.energy,]
dt.test.energy <- DT.DF.ENERGY[-dt.train.index.energy,]

#Create DT with C5.0 model. 
treeControl <- C5.0Control(
  subset = FALSE,
  bands = 0,
  winnow = FALSE,
  noGlobalPruning = FALSE,
  CF = 0.25,
  minCases = 2,
  fuzzyThreshold = FALSE,
  earlyStopping = TRUE
)
fit.C5.0 <- C5.0(
  x = dt.train.energy[, -34],
  y = dt.train.energy$stock_ratios_sample.result,
  control = treeControl,
)

#Print summary of the model 
summary(fit.C5.0)
plot(fit.C5.0)

#Test the performance of the model
C5.0.predict <- predict(
  fit.C5.0,
  dt.test.energy[, -34],
  type = "class"
)
confusionMatrix(C5.0.predict, dt.test.energy[,34])

#Model with recursive partioning (rpart)
rpart.Control <- rpart.control(
  minsplit = 20,
  xval = 10
)
fit.rpart <- rpart(
  stock_ratios_sample.result ~ .,
  data = dt.train.energy,
  method='class',
  control = rpart.Control
)
fit.rpart

prp(fit.rpart, faclen = 0, cex = 0.8, extra = 1)
fancyRpartPlot(fit.rpart)


#Performance of the model
rpart.predict <- predict(
  fit.rpart,
  dt.test.energy[, -34],
  type = "class"
)
confusionMatrix(rpart.predict,dt.test.energy[,34])

#Prune the tree
bestcp <- fit.rpart$cptable[
  which.min(fit.rpart$cptable[,"xerror"]),
  "CP"
  ]
fit.rpart.pruned <- prune(fit.rpart, cp = bestcp)
prp(fit.rpart.pruned, faclen = 0, cex = 0.7, extra = 1)
fancyRpartPlot(fit.rpart.pruned)

#Evaluate the performance of the model
rpart.predict.pruned <- predict(
  fit.rpart.pruned,
  dt.test.energy[, -34],
  type = "class"
)
confusionMatrix(rpart.predict.pruned,dt.test.energy[,34])

