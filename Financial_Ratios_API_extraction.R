# Jamie Hackett
# X16386576
# Extract the data for the Symbols and Finacial ratios

#Loading the packages
library(jsonlite)
library(dplyr)

#Setting the working directory
#setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP")

setwd("C:/Users/Jamie/OneDrive - National College of Ireland/Year 4/FYP/Financial_Ratios_Data_Extract")

#Attempting to see if I can get the Sector and Industry information in one API request

#Getting financial ratios
#I've already loaded the API key into a enviroment variable for security purposes.
#Loading in the list of tickers
tickers <- read.csv("Ticker_list.csv")

tickers <- read.csv("Ticker_list_edited.csv")

#Setting the base api for the for loop
base_url <- "https://fmpcloud.io/api/v3/ratios/"

base_url_profile <- "https://fmpcloud.io/api/v3/profile/"

#Begining the first on the lsit for the for loop
df_ratios <- fromJSON(paste(base_url,"SUN","?apikey=",Sys.getenv("apikey"), sep =""))
write.csv(df_ratios, "df_ratios.csv")
df_ratios <- read.csv("df_ratios.csv")

#Now removing that from the start of the list 
tickers <- dplyr::pull(tickers, ?..Ticker)

#Start the for loop
for(i in tickers){
  tryCatch({
    print(paste(base_url,i, sep =""))
    ratio <- fromJSON(paste(base_url,i,"?apikey=",Sys.getenv("apikey"), sep =""))
    ratiodf <- as.data.frame(ratio)
    write.csv(ratiodf, "ratio.csv")
    ratiodf <- read.csv("ratio.csv")
    df_ratios <- rbind(df_ratios, ratiodf)
    write.csv(df_ratios, "df_ratios.csv")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#Time to load in all the files that we're seperated and combine them together. #This won't have to be done if you rerun the script but this was just necessary while I had the non-premium access to the API

#Extracting profile information from the API
base_url_profile <- "https://fmpcloud.io/api/v3/profile/"

df_profiles <- fromJSON(paste(base_url_profile,"SUN","?apikey=",Sys.getenv("apikey"), sep =""))

for(i in tickers){
  tryCatch({
    print(paste(base_url,i, sep =""))
    profile <- fromJSON(paste(base_url_profile,i,"?apikey=",Sys.getenv("apikey"), sep =""))
    df_profiles <- rbind(df_profiles, profile)
  }, error=function(c){cat("ERROR :",conditionMessage(c), "\n")})
}


#Combining Ratios
df_ratios_1 <- read.csv("df_ratios_arch.csv")
df_ratios_2 <- read.csv("df_ratios.csv")

df_ratios_final <- rbind(df_ratios_1, df_ratios_2)

#Creating a column for the year so I can merge in the prepping data file.
df_ratios_final$year <- format(as.Date(df_ratios_final$date, format="%d/%m/%Y"),"%Y")

#Combining the company info and the stock prices
ratios_description <- merge(df_ratios_final, df_profiles, by=c("symbol"))

#Extracting this as a CSV that can be imported in the "Prepping the data file"
write.csv(ratios_description, "ratios_description.csv")
