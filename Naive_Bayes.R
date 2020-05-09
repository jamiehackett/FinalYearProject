# Jamie Hackett
# X16386576
# Machine Learning
# Naive Bayes

library(rsample)
library(dplyr)
library(ggplot2)
library(caret)
library(magrittr)
library(dplyr)

#Setting the working directory
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Loading the data required
stock_ratios <- read.csv("stock_ratios_desc_final.csv")

#Engineering a feature so that it measures if the stock has gone up or not gone up. 
stock_ratios$result <- ifelse(stock_ratios$returns < 0, "LOSS", "GAIN")

#Creating a sample 
set.seed(2002)
stock_ratios_sample <- sample_n(stock_ratios, 1000)

########## MACHINE LEARNING NB ##########
NB.DF <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test set of data
set.seed(2002)
nb.train.index <- createDataPartition(
  NB.DF$stock_ratios_sample.result,
  p = .70,
  list = FALSE
)
# create the train dataset using the indexes from the partitioning above
set.seed(2002)
nb.train<- NB.DF[nb.train.index,]
# create the test dataset using all but the indexes from the partitioning above
set.seed(2002)
nb.test <- NB.DF[-nb.train.index,]

#Checking the relative class balance in the two
table(NB.DF$stock_ratios_sample.result) %>% prop.table()

table(nb.train$stock_ratios_sample.result) %>% prop.table()
table(nb.test$stock_ratios_sample.result) %>% prop.table()

#Laplase estiamte
set.seed(2002)
train_control <- trainControl(
  method = "cv",
  number = 10
)

set.seed(2002)
nb.fit <- train(
  x = nb.train[,-34], # everything except
  y = nb.train[,34], # only Attrition
  method = "nb", # uses the NaiveBayes() function from the klaR library behind the scenes
  trControl = train_control # our train configuration
)
set.seed(2002)
caret::confusionMatrix(nb.fit)

set.seed(2002)
nb.predict <- predict(nb.fit,nb.test[,-34])
set.seed(2002)
confusionMatrix(nb.predict,nb.test[,34])

varImp(nb.fit)

#Configuring tuning parameters to fine tune it
set.seed(2002)
tuning_grid_kernel <- expand.grid(
  usekernel = TRUE,
  fL = 0:5,
  adjust = seq(0, 5, by = 0.5)
)
set.seed(2002)
tuning_grid_no_kernel <- expand.grid(
  usekernel = FALSE,
  fL = 0:5,
  adjust = 0
)
set.seed(2002)
tuning_grid = rbind(tuning_grid_kernel,tuning_grid_no_kernel)

#We can now train and hyperparameter optimise a model using the tuning grid created above. We will also centre, scale and Box-Cox transform any numeric variables.
set.seed(2002)
nb.fit.2 <- train(
  x = nb.train[,-34],
  y = nb.train[,34],
  method = "nb",
  trControl = ,
  tuneGrid = tuning_grid,
  preProc = c("BoxCox", "center", "scale")
)
set.seed(2002)
nb.fit.2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

set.seed(2002)
confusionMatrix(nb.fit.2)

set.seed(2002)
nb.predict.2 <- predict(nb.fit.2, newdata = nb.test[,-34])
set.seed(2002)
confusionMatrix(nb.predict.2, nb.test[,34])

#Going rerun the models for each sector this time. 

########## HEALTHCARE ########## 
#Healthcare sector
set.seed(2002)
Healthcare <- stock_ratios[stock_ratios$sector == "Healthcare",]

#Creating a sample 
set.seed(2002)
stock_ratios_sample <- sample_n(Healthcare, 1000)

########## MACHINE LEARNING NB ##########
NB.DF.HEALTHCARE <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test set of data
set.seed(2002)
nb.train.index.healthcare <- createDataPartition(
  NB.DF.HEALTHCARE$stock_ratios_sample.result,
  p = .70,
  list = FALSE
)
# create the train dataset using the indexes from the partitioning above
set.seed(2002)
nb.train.healthcare<- NB.DF.HEALTHCARE[nb.train.index.healthcare,]
# create the test dataset using all but the indexes from the partitioning above
set.seed(2002)
nb.test.healthcare <- NB.DF.HEALTHCARE[-nb.train.index.healthcare,]

#Checking the relative class balance in the two
table(NB.DF.HEALTHCARE$stock_ratios_sample.result) %>% prop.table()

table(nb.train.healthcare$stock_ratios_sample.result) %>% prop.table()
table(nb.test.healthcare$stock_ratios_sample.result) %>% prop.table()

#Laplase estiamte
set.seed(2002)
train_control <- trainControl(
  method = "cv",
  number = 10
)
set.seed(2002)
nb.fit <- train(
  x = nb.train.healthcare[,-34], # everything except Attrition
  y = nb.train.healthcare[,34], # only Attrition
  method = "nb", # uses the NaiveBayes() function from the klaR library behind the scenes
  trControl = train_control # our train configuration
)
set.seed(2002)
caret::confusionMatrix(nb.fit)

set.seed(2002)
nb.predict <- predict(nb.fit,nb.test.healthcare[,-34])
set.seed(2002)
confusionMatrix(nb.predict,nb.test.healthcare[,34])

varImp(nb.fit)

#Configuring tuning parameters to fine tune it
set.seed(2002)
tuning_grid_kernel <- expand.grid(
  usekernel = TRUE,
  fL = 0:5,
  adjust = seq(0, 5, by = 0.5)
)
set.seed(2002)
tuning_grid_no_kernel <- expand.grid(
  usekernel = FALSE,
  fL = 0:5,
  adjust = 0
)
set.seed(2002)
tuning_grid = rbind(tuning_grid_kernel,tuning_grid_no_kernel)

#We can now train and hyperparameter optimise a model using the tuning grid created above. We will also centre, scale and Box-Cox transform any numeric variables.
set.seed(2002)
nb.fit.2 <- train(
  x = nb.train.healthcare[,-34],
  y = nb.train.healthcare[,34],
  method = "nb",
  trControl = ,
  tuneGrid = tuning_grid,
  preProc = c("BoxCox", "center", "scale")
)
set.seed(2002)
nb.fit.2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

set.seed(2002)
confusionMatrix(nb.fit.2)

set.seed(2002)
nb.predict.2 <- predict(nb.fit.2, newdata = nb.test.healthcare[,-34])
set.seed(2002)
confusionMatrix(nb.predict.2, nb.test.healthcare[,34])


########## ENERGY ########## 
#Energy sector
Energy <- stock_ratios[stock_ratios$sector == "Energy",]

#Creating a sample 
set.seed(2002)
stock_ratios_sample <- sample_n(Energy, 1000)

########## MACHINE LEARNING NB ##########
NB.DF.ENERGY <- data.frame(stock_ratios_sample$volume, stock_ratios_sample$currentRatio, stock_ratios_sample$quickRatio, stock_ratios_sample$cashRatio, stock_ratios_sample$grossProfitMargin, stock_ratios_sample$operatingProfitMargin, stock_ratios_sample$pretaxProfitMargin, stock_ratios_sample$netProfitMargin, stock_ratios_sample$effectiveTaxRate, stock_ratios_sample$returnOnAssets, stock_ratios_sample$returnOnEquity, stock_ratios_sample$returnOnCapitalEmployed, stock_ratios_sample$netIncomePerEBT, stock_ratios_sample$ebtPerEbit, stock_ratios_sample$debtRatio, stock_ratios_sample$debtEquityRatio, stock_ratios_sample$longTermDebtToCapitalization, stock_ratios_sample$interestCoverage, stock_ratios_sample$companyEquityMultiplier, stock_ratios_sample$receivablesTurnover, stock_ratios_sample$payablesTurnover, stock_ratios_sample$fixedAssetTurnover, stock_ratios_sample$assetTurnover, stock_ratios_sample$operatingCashFlowPerShare, stock_ratios_sample$freeCashFlowPerShare, stock_ratios_sample$cashPerShare, stock_ratios_sample$payoutRatio, stock_ratios_sample$freeCashFlowOperatingCashFlowRatio, stock_ratios_sample$priceBookValueRatio, stock_ratios_sample$priceToSalesRatio, stock_ratios_sample$priceEarningsRatio,stock_ratios_sample$priceEarningsToGrowthRatio, stock_ratios_sample$enterpriseValueMultiple, stock_ratios_sample$result)

#Creating training and test set of data
set.seed(2002)
nb.train.index.energy <- createDataPartition(
  NB.DF.ENERGY$stock_ratios_sample.result,
  p = .70,
  list = FALSE
)
# create the train dataset using the indexes from the partitioning above
set.seed(2002)
nb.train.energy<- NB.DF.ENERGY[nb.train.index.energy,]
# create the test dataset using all but the indexes from the partitioning above
set.seed(2002)
nb.test.energy <- NB.DF.ENERGY[-nb.train.index.energy,]

#Checking the relative class balance in the two
table(NB.DF.ENERGY$stock_ratios_sample.result) %>% prop.table()

table(nb.train.energy$stock_ratios_sample.result) %>% prop.table()
table(nb.test.energy$stock_ratios_sample.result) %>% prop.table()

#Laplase estiamte
set.seed(2002)
train_control <- trainControl(
  method = "cv",
  number = 10
)

set.seed(2002)
nb.fit <- train(
  x = nb.train.energy[,-34], # everything except Attrition
  y = nb.train.energy[,34], # only Attrition
  method = "nb", # uses the NaiveBayes() function from the klaR library behind the scenes
  trControl = train_control # our train configuration
)
set.seed(2002)
caret::confusionMatrix(nb.fit)

set.seed(2002)
nb.predict <- predict(nb.fit,nb.test.energy[,-34])
set.seed(2002)
confusionMatrix(nb.predict,nb.test.energy[,34])

#Configuring tuning parameters to fine tune it
set.seed(2002)
tuning_grid_kernel <- expand.grid(
  usekernel = TRUE,
  fL = 0:5,
  adjust = seq(0, 5, by = 0.5)
)
set.seed(2002)
tuning_grid_no_kernel <- expand.grid(
  usekernel = FALSE,
  fL = 0:5,
  adjust = 0
)
set.seed(2002)
tuning_grid = rbind(tuning_grid_kernel,tuning_grid_no_kernel)

#We can now train and hyperparameter optimise a model using the tuning grid created above. We will also centre, scale and Box-Cox transform any numeric variables.
set.seed(2002)
nb.fit.2 <- train(
  x = nb.train.energy[,-34],
  y = nb.train.energy[,34],
  method = "nb",
  trControl = ,
  tuneGrid = tuning_grid,
  preProc = c("BoxCox", "center", "scale")
)
set.seed(2002)
nb.fit.2$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

set.seed(2002)
confusionMatrix(nb.fit.2)

set.seed(2002)
nb.predict.2 <- predict(nb.fit.2, newdata = nb.test.energy[,-34])
set.seed(2002)
confusionMatrix(nb.predict.2, nb.test.energy[,34])

varImp(nb.fit.2)
