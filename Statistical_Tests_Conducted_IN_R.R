# Jamie Hackett
# X16386576
# Statistical tests conducted in R/

#Installing packages
install.packages(c("ggplot2", "scales", "dplyr", "data.table", "tidyverse", "ggthemes", "lubridate", "broom", "tidyr", "zoo", "tidyquant", "Amelia", "mice", "caret", "HistogramTools", "cluster", "factoextra", "gridExtra"))

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
library(caret)
library(HistogramTools)
library(cluster)
library(factoextra)
library(gridExtra)
library(corrplot)

#Setting the working directory 
setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

#Loading in the data
stock_ratios_desc <- read.csv("stock_ratios_desc_final.csv")

#Summary of data to identify some outliers
summary(stock_ratios_desc)


######### Stat investigation 1 ######### 
# Checking to see if there is a significant difference in the mean market capitalization in the real estate and energy sector in the 2018

#First time to create a dataframe the has just the two sectors & year = 2018
energy_realestate <- subset(stock_ratios_desc, year == 2018) # Subsets via 2018
energy <- subset(energy_realestate, sector  %in% c("Energy")) # Subsets organizations belonging to Energy sector.
realestate <- subset(energy_realestate, sector  %in% c("Real Estate")) # Subsets organizations belonging to Real Estate sector.
energy_realestate <- rbind(energy, realestate) #Combines both together.

energy_realestate <- data.frame(energy_realestate$ticker, energy_realestate$mktCap, energy_realestate$year, energy_realestate$sector, energy_realestate$companyName) #Selecting variables I only want to work on.

#Removing duplicate values from the dataframe
energy_realestate<- energy_realestate[!duplicated(energy_realestate$energy_realestate.companyName), ]
energy<- energy[!duplicated(energy$companyName), ]
realestate<- realestate[!duplicated(realestate$companyName), ]

#Check to see if the data is normal with histograms
ggplot(energy_realestate, aes(x=energy_realestate.mktCap)) + geom_histogram(bins = 55, color="black", fill="orange") + labs(title="Distribution of market cap by sector (histogram)",x="Market Cap", y = "Count") + facet_grid(energy_realestate.sector ~ .)

#Boxplots to identify any outliers
ggplot(energy_realestate, aes(x=energy_realestate.sector, y=energy_realestate.mktCap)) + labs(title="Distribution of market cap by sector (boxplot)",x="Market Cap", y = "Count") +
  geom_boxplot(color="black", fill="orange")

#QQPlots for normality 
qqnorm(energy$mktCap, frame = F, main = "Normal Q-Q Plot for the Energy sector")
qqline(energy$mktCap, col = "orange")

qqnorm(realestate$mktCap, frame = F, main = "Normal Q-Q Plot for the Real Estate sector")
qqline(realestate$mktCap, col = "orange")

#statistical tests for normality Shapiro-Wilk
shapiro.test(energy$mktCap)
shapiro.test(realestate$mktCap)

#Mann Whitney U
wilcox.test(energy_realestate$energy_realestate.mktCap~energy_realestate$energy_realestate.sector)

#Export to verify in SPSS
write.csv(energy_realestate, "energy_realesate_final.csv")

######### Stat investigation 2 #########
# Checking to see if there is a significant difference in the mean market capitalization in all sector using A Kruskal Wallis test, will not need to run normality tests as already know two of the samples are not normally distributed.

#Subsetting to 2018
kruskal_wallis <- subset(stock_ratios_desc, year == 2018)

#Selecting the headings I want
kruskal_wallis <- data.frame(kruskal_wallis$ticker, kruskal_wallis$mktCap, kruskal_wallis$year, kruskal_wallis$sector, kruskal_wallis$companyName)

#Exporting it so I can load it into SPSS
write.csv(kruskal_wallis, "kruskal_wallis.csv")

#Time to use some boxplots to identify outliers lets do it by sector so we can group it
p <- ggplot(stock_ratios_desc, aes(x=sector, y=returns)) + 
  geom_boxplot()

p

######### K nearest neighbour ######### 
#Running some KNNs to identify any immediate outliers

#Subsetting the data to just numeric values
KNN1 <- stock_ratios_desc %>% select(3, 11:45)

#Scale the data
KNN1 <- scale(KNN1, center = T, scale = T)

#Creating a function for the sum within squares so we can see what the best number of clusters are. Credit to Noel Cosgrave for providing the function for this in his tutorials.
wssplot <- function(data, max_clusters=15) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (k in 2:max_clusters){
    wss[k] <- sum(kmeans(data, centers=k)$withinss)
  }
  plot(1:max_clusters, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
}

#Running the WSS plot.
wssplot(KNN1, 5)

#From the plot it looks like the best might be 3 or 4 clusters, looking for the elbow on the graph. 
K1.4 <- kmeans(KNN1, centers = 4, nstart = 25)
K1.5 <- kmeans(KNN1, centers = 5, nstart = 25)

#Plots
P1.4 <- fviz_cluster(K1.4, geom = "point", data = KNN1) + ggtitle('K=4')
P1.5 <- fviz_cluster(K1.5, geom = "point", data = KNN1) + ggtitle('K=5')
grid.arrange(P1.4, P1.5, nrow = 1)

######### Correlation Plot of Financial Ratios ######### 
#Subsetting the data to just numeric values same used in KNN
plot <- stock_ratios_desc %>% select(3, 11:45)

corrplot(cor(plot))

