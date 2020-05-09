# Jamie Hackett
# X16386576
# Machine Learning
# Support Vector Machines

#Loading the required packages
library(dplyr)
library(HistogramTools)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyverse)
library(caret)
library(analogsea)
library(future)
library(ggplot2)
library(e1071)
library(randomForest)
library(rminer)

#Setting the working directory
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Loading the data required
stock_ratios <- read.csv("stock_ratios_desc_final.csv")

#Engineering a feature so that it measures if the stock has gone up or not gone up. 
stock_ratios$result <- ifelse(stock_ratios$returns < 0, "LOSS", "GAIN")

#Creating a sample 
set.seed(2002)
stock_ratios_sample <- sample_n(stock_ratios, 1000)

########## MACHINE LEARNING SVM ##########
SVM.DF <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)


#Creating training and test datasets with a 75% / 25% split
set.seed(2002)
svm.df.train.index <- createDataPartition(
  SVM.DF$stock_ratios_sample.result,
  p = .75,
  list = F
)

#Create the train datast using the indexs from the partitioning indices generated above
set.seed(2002)
svm.df.train <- SVM.DF[svm.df.train.index,]
set.seed(2002)
svm.df.test <- SVM.DF[-svm.df.train.index,]

Cost = 2^c(1:8)
print(Cost)

set.seed(2002)
svm.control = trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary
)

set.seed(2002)
svm.linear.grid <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit1 <- train( # Train a model model
  stock_ratios_sample.result ~ ., # with Class as the response variable, all
  # others as explanatory
  data = svm.df.train, # ionosphere.train as the training data
  method = "svmLinear", # using the linear kernel
  trControl = svm.control, # cross-validated as configured above
  preProc = c("center", "scale","nzv"), # centre and scale the data, removing near-zero
  # variance variables
  verbose = FALSE,
  tuneGrid = svm.linear.grid # use the tuning grid created above
)

set.seed(2002)
svm.fit1

#It looks like the optimum value for Cost is beterrn 32 and 63, so let's narrow that done.
Cost = c(32:64)
print(Cost)

set.seed(2002)
svm.linear.grid2 <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit2 <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train, 
  method = "svmLinear",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.linear.grid2
)

set.seed(2002)
svm.fit2

#Now we somewhat have a better idea of the optimum value

set.seed(2002)
table(svm.df.test$stock_ratios_sample.result)

set.seed(2002)
svm.linear.predict <- predict(
  svm.fit2,
  svm.df.test[,-34],
)

set.seed(2002)
svm.linear.confusionmatrix <- confusionMatrix(
  data = svm.linear.predict,
  reference = svm.df.test[,34],
)

set.seed(2002)
svm.linear.confusionmatrix

# SVM - Radial Basis Kernel with tuning grid
set.seed(2002)
svm.rbf.grid <- expand.grid(
  C = 2^seq(3,5,0.1),
  sigma = 2^c(-25, -20, -15,-10,-5, 0)
)

set.seed(2002)
svm.rbf.fit <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train, 
  method = "svmRadial",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.rbf.grid
)

set.seed(2002)
svm.rbf.fit$bestTune

set.seed(2002)
svm.rbf.predict <- predict(
  svm.rbf.fit,
  svm.df.test[,-34],
)

set.seed(2002)
svm.rbf.confusionmatrix <- confusionMatrix(
  data = svm.rbf.predict,
  reference = svm.df.test[,34],
  positive = "GAIN"
)

set.seed(2002)
svm.rbf.confusionmatrix

##################### SVM - Radial basis kernel with random search #####################
set.seed(2002)
svm.random.control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)


set.seed(2002)
svm.rbf.random.fit <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train,
  method = "svmRadial",
  trControl = svm.control,
  verbose = F,
  preProc = c("center", "scale", "nzv"),
  tuneLength = 60
)

set.seed(2002)
svm.rbf.random.fit$bestTune

set.seed(2002)
svm.rbf.random.predict <- predict(
  svm.rbf.random.fit,
  svm.df.train[,-34]
)

set.seed(2002)
svm.rbf.random.confusionmatrix <- confusionMatrix(
  data = svm.rbf.random.predict,
  reference = svm.df.train[,34],
  positive = "GAIN"
)
set.seed(2002)
svm.rbf.random.confusionmatrix
#Variable importance analysis
set.seed(2002)
varImp(svm.rbf.random.fit)


#Going rerun the models for each sector this time. 

#Healthcare sector
Healthcare <- stock_ratios[stock_ratios$sector == "Healthcare",]

set.seed(2002)
stock_ratios_sample <- sample_n(Healthcare, 1000)

########## MACHINE LEARNING SVM FOR EACH SECTOR ##########
SVM.DF.HEALTHCARE <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)


set.seed(2002)
svm.df.train.index.healthcare <- createDataPartition(
  SVM.DF.HEALTHCARE$stock_ratios_sample.result,
  p = .75,
  list = F
)

#Create the train datast using the indexs from the partitioning indices generated above
set.seed(2002)
svm.df.train.healthcare <- SVM.DF.HEALTHCARE[svm.df.train.index,]
set.seed(2002)
svm.df.test.healthcare <- SVM.DF.HEALTHCARE[-svm.df.train.index,]

Cost = 2^c(1:8)
print(Cost)

set.seed(2002)
svm.control = trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary
)

set.seed(2002)
svm.linear.grid <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit1.healthcare <- train( # Train a model model
  stock_ratios_sample.result ~ ., # with Class as the response variable, all
  # others as explanatory
  data = svm.df.train.healthcare, # ionosphere.train as the training data
  method = "svmLinear", # using the linear kernel
  trControl = svm.control, # cross-validated as configured above
  preProc = c("center", "scale","nzv"), # centre and scale the data, removing near-zero
  # variance variables
  verbose = FALSE,
  tuneGrid = svm.linear.grid # use the tuning grid created above
)

set.seed(2002)
svm.fit1.healthcare

#It looks like the optimum value for Cost is beterrn 2 and 4, so let's narrow that done.
Cost = c(2:4)
print(Cost)

set.seed(2002)
svm.linear.grid2 <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit2.healthcare <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train.healthcare, 
  method = "svmLinear",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.linear.grid2
)

set.seed(2002)
svm.fit2.healthcare

#Now we somewhat have a better idea of the optimum value
set.seed(2002)
table(svm.df.test.healthcare$stock_ratios_sample.result)

set.seed(2002)
svm.linear.predict <- predict(
  svm.fit2.healthcare,
  svm.df.test.healthcare[,-34],
)

set.seed(2002)
svm.linear.confusionmatrix.healthcare <- confusionMatrix(
  data = svm.linear.predict,
  reference = svm.df.test.healthcare[,34],
)

svm.linear.confusionmatrix.healthcare

# SVM - Radial Basis Kernel with tuning grid for healthcare
set.seed(2002)
svm.rbf.grid <- expand.grid(
  C = 2^seq(3,5,0.1),
  sigma = 2^c(-25, -20, -15,-10,-5, 0)
)

set.seed(2002)
svm.rbf.fit.heathcare <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train.healthcare, 
  method = "svmRadial",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.rbf.grid
)

set.seed(2002)
svm.rbf.fit.heathcare$bestTune

set.seed(2002)
svm.rbf.predict <- predict(
  svm.rbf.fit.heathcare,
  svm.df.test.healthcare[,-34],
)

set.seed(2002)
svm.rbf.confusionmatrix.healthcare <- confusionMatrix(
  data = svm.rbf.predict,
  reference = svm.df.test.healthcare[,34],
  positive = "GAIN"
)

set.seed(2002)
svm.rbf.confusionmatrix.healthcare

##################### SVM - Radial basis kernel with random search #####################
set.seed(2002)
svm.random.control.healthcare <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)

set.seed(2002)
svm.rbf.random.fit.healthcare <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train.healthcare,
  method = "svmRadial",
  trControl = svm.random.control.healthcare,
  verbose = F,
  preProc = c("center", "scale", "nzv"),
  tuneLength = 60
)

set.seed(2002)
svm.rbf.random.fit.healthcare$bestTune

set.seed(2002)
svm.rbf.random.predict <- predict(
  svm.rbf.random.fit.healthcare,
  svm.df.train.healthcare[,-34]
)

set.seed(2002)
svm.rbf.random.confusionmatrix.healthcare <- confusionMatrix(
  data = svm.rbf.random.predict,
  reference = svm.df.train.healthcare[,34],
  positive = "GAIN"
)

set.seed(2002)
svm.rbf.random.confusionmatrix.healthcare

#Variable importance analysis
varImp(svm.rbf.random.fit.healthcare)

#Energy sector
Energy <- stock_ratios[stock_ratios$sector == "Energy",]

set.seed(2002)
stock_ratios_sample <- sample_n(Energy, 1000)

########## MACHINE LEARNING SVM FOR EACH SECTOR ##########
SVM.DF.ENERGY <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)


set.seed(2002)
svm.df.train.index.ENERGY <- createDataPartition(
  SVM.DF.ENERGY$stock_ratios_sample.result,
  p = .75,
  list = F
)

#Create the train datast using the indexs from the partitioning indices generated above
set.seed(2002)
svm.df.train.energy <- SVM.DF.ENERGY[svm.df.train.index,]
set.seed(2002)
svm.df.test.energy <- SVM.DF.ENERGY[-svm.df.train.index,]

Cost = 2^c(1:8)
print(Cost)

set.seed(2002)
svm.control = trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary
)

set.seed(2002)
svm.linear.grid <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit1.energy <- train( # Train a model model
  stock_ratios_sample.result ~ ., # with Class as the response variable, all
  # others as explanatory
  data = svm.df.train.energy, # ionosphere.train as the training data
  method = "svmLinear", # using the linear kernel
  trControl = svm.control, # cross-validated as configured above
  preProc = c("center", "scale","nzv"), # centre and scale the data, removing near-zero
  # variance variables
  verbose = FALSE,
  tuneGrid = svm.linear.grid # use the tuning grid created above
)

set.seed(2002)
svm.fit1.energy

#It looks like the optimum value for Cost is beterrn 4 and 8, so let's narrow that done.
Cost = c(4:8)
print(Cost)

set.seed(2002)
svm.linear.grid2 <- expand.grid(
  C = Cost
)

set.seed(2002)
svm.fit2.energy <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train, 
  method = "svmLinear",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.linear.grid2
)

set.seed(2002)
svm.fit2.energy

#Now we somewhat have a better idea of the optimum value
set.seed(2002)
table(svm.df.test.energy$stock_ratios_sample.result)

set.seed(2002)
svm.linear.predict <- predict(
  svm.fit2.energy,
  svm.df.test.energy[,-34],
)

set.seed(2002)
svm.linear.confusionmatrix.energy <- confusionMatrix(
  data = svm.linear.predict,
  reference = svm.df.test.energy[,34],
)

svm.linear.confusionmatrix.energy

#Radial based with grid tune
set.seed(2002)
svm.rbf.grid <- expand.grid(
  C = 2^seq(3,5,0.1),
  sigma = 2^c(-25, -20, -15,-10,-5, 0)
)

set.seed(2002)
svm.rbf.fit.energy <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train.energy, 
  method = "svmRadial",
  trControl = svm.control,
  preProc = c("center", "scale", "nzv"),
  verbose = F,
  tuneGrid = svm.rbf.grid
)

set.seed(2002)
svm.rbf.fit.energy$bestTune

set.seed(2002)
svm.rbf.predict <- predict(
  svm.rbf.fit.energy,
  svm.df.test.energy[,-34],
)

set.seed(2002)
svm.rbf.confusionmatrix.energy <- confusionMatrix(
  data = svm.rbf.predict,
  reference = svm.df.test.energy[,34],
  positive = "GAIN"
)

set.seed(2002)
svm.rbf.confusionmatrix.energy

##################### SVM - Radial basis kernel with random search #####################
set.seed(2002)
svm.random.control.energy <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary,
  search = "random"
)

set.seed(2002)
svm.rbf.random.fit.energy <- train(
  stock_ratios_sample.result ~ .,
  data = svm.df.train.energy,
  method = "svmRadial",
  trControl = svm.random.control.energy,
  verbose = F,
  preProc = c("center", "scale", "nzv"),
  tuneLength = 60
)

set.seed(2002)
svm.rbf.random.fit.energy$bestTune

set.seed(2002)
svm.rbf.random.predict <- predict(
  svm.rbf.random.fit.energy,
  svm.df.train.energy[,-34]
)

set.seed(2002)
svm.rbf.random.confusionmatrix.energy <- confusionMatrix(
  data = svm.rbf.random.predict,
  reference = svm.df.train.energy[,34],
  positive = "GAIN"
)

set.seed(2002)
svm.rbf.random.confusionmatrix.energy

#Variable importance analysis
varImp(svm.rbf.random.fit.energy)

