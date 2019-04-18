# Stock-Price-Simulation
Simulating the price movements of stocks in the Dow Jones using Monte Carlo Simulation

getwd()
setwd("C:/Users/bendu/Desktop/Northeastern Level/Capstone/DowJonesStock_Capstone/DowJonesStock_Capstone/stock-data-dow-jones")

# install.packages("tidyverse")
# install.packages("zoo")
# install.packages("ggplot2")
# install.packages("here")
# 
library(tidyverse)
library(zoo)
library(ggplot2)
library(here)

######### Before contining with the below code ############

# Make sure to 

pathname <- "C:/Users/bendu/Desktop/Northeastern Level/Capstone/DowJonesStock_Capstone/DowJonesStock_Capstone/stock-data-dow-jones"
# Create a pathname of where all 30 stock files that you downloaded from my Github
# Yours will be different then mine
# You will need to create a file where you store all the stock excel files
# The stocks are in their own files due to that how I had to pull them
# Each stock must be pulled individually

chunk <- list.files(pathname, full.names = T)
# Create a variable with all full pathnames for the 30 stock files I want to merge

all <- map_dfr(.x = chunk, .f = read.csv)
# Create dataframe that combines all stock files
# Make sure your file only has the stock tickers

##### The files from my Github contain all 30 stocks in the Dow Jones for the timeframe of "2014-02-03" to 2019-01-31"
##### Which is a time range of about 5 years
##### The first 4 years will be used as the train dataset
##### The final year will be used as the test dataset

rm(chunk)
rm(pathname)
# Removes the uneccessary dataframes
# We do not need pathname and chunk anymore

unique(all$ticker)
length(unique(all$ticker))
# Sanity check that all files have been added and that the ticker names are correct
# There are 30 stocks in the Dow Jones and we can clearly see all 30 are in our "all" dataframe


as.Date(all$date)
# R is registered the dates incorrectly
# For example the date that we have is "02/03/2014". However R is reading it as "0002-03-20"
# R is reading it as "Year/Month/Day" when it is actuall "Month/Day/Year"
# Now we will convert the date to the format R understands correctly

all$date <- as.Date(all$date, "%m/%d/%Y")
# Converting the dates to the format R understands
# "%m/%d/%Y" tells R how the date is correctly formated so that it can convert it over to something it can recognize

as.Date(all$date)
# As you can see now R is reading the dates correctly

test <- all %>% 
  filter(date > "2018-01-01") %>% 
  group_by(ticker) %>% 
  select(ticker, date, close, changePercent)
# Create a dataframe for our test data
# In this case it is the dates from "2018-01-01" to "2019-01-31"
# A little more tha a year's worth of data

test <- test %>%
  group_by(ticker) %>% 
  mutate(day = row_number())
# Add a day number to the row
# This will allow me to plot the actual movement of the stock price and compare it to my future simulations

test <- test %>%
  group_by(ticker) %>% 
  mutate(day = row_number())
# Add a day number to the row
# This will allow me to compare the simulations to reality

train <- all %>% 
  group_by(ticker) %>% 
  filter(date < "2018-01-01")
# Create database train for my data to train my simulations

train$changePercent <- train$changePercent/100
# dividing changePercent by 100 to convert it to the decimal
# This is needed to run Monte Carlo simulation

risk <- train %>% 
  select(ticker, date, changePercent)
# Create a dataframe where I will calculate my mu and sig
# mu being the average changePercent
# sig being the standard deviation

risk <- risk %>% 
  group_by(ticker) %>% 
  mutate(sig = sqrt((var(changePercent, na.rm = T)*1257)/1258))
# Created the sig / volatility for each ticker

risk <- risk %>% 
  group_by(ticker) %>% 
  mutate(mu = mean(changePercent, na.rm = TRUE))
# Create mu column of the mean "growth" per day for each ticker

risk$month <- NULL
risk$year <- NULL
risk$changePercent <- NULL
risk$date <- NULL
# Eliminate columns so I am left with ticker, mu, and sig

risk <- dplyr::distinct(risk)
# Eliminate rows so I just have 1 for each ticker

risk$ticker <- toupper(risk$ticker)
# Turning all tickers to capitals which is better for visualizations


# Creating needed values / dataframes
test_value <- all %>% 
  filter(date == "2018-01-02") %>% 
  group_by(ticker) %>%
  select(ticker, close) %>% 
  ungroup()
# Create data frame with the test (start of 2018) values of each stock

colnames(test_value)[2] <- "test_value"
# Change name of column

test_value$ticker <- toupper(test_value$ticker)
# Need to capitalize "test_value" so that I can match to risk and add the start_values

risk <- risk %>% 
  left_join(test_value, by = "ticker")
# Left join with "start_value" so "risk" can have start_value
# Need to delete the unneccessary columns


# First, create databases for each stock

# This is the number of simulations I will run
msft <- train %>% 
  filter(ticker == "msft") %>% 
  select(ticker, date, changePercent)

unh <- train %>% 
  filter(ticker == "unh") %>% 
  select(ticker, date, changePercent)

aapl <- train %>% 
  filter(ticker == "aapl") %>% 
  select(ticker, date, changePercent)

dwdp <- train %>% 
  filter(ticker == "dwdd") %>% 
  select(ticker, date, changePercent)

ko <- train %>% 
  filter(ticker == "ko") %>% 
  select(ticker, date, changePercent)

n <- 500
# This is the number of simulations I will run

# Creating a matrix for the 100 simulations
msft_matrix <- matrix(nrow=271,ncol=n)

unh_matrix <- matrix(nrow=271,ncol=n)

aapl_matrix <- matrix(nrow=271,ncol=n)

dwdp_matrix <- matrix(nrow=271,ncol=n)

ko_matrix <- matrix(nrow=271,ncol=n)
# Creating a matrix for the 100 simulations

# adding the start price to beginning of 100 samples I will be running
msft_matrix[1,1:500] <- as.numeric(risk[(which(risk$ticker == "MSFT")), 4])

unh_matrix[1,1:500] <- as.numeric(risk[(which(risk$ticker == "UNH")), 4])

aapl_matrix[1,1:500] <- as.numeric(risk[(which(risk$ticker == "AAPL")), 4])

dwdp_matrix[1,1:500] <- as.numeric(risk[(which(risk$ticker == "DWDP")), 4])

ko_matrix[1,1:500] <- as.numeric(risk[(which(risk$ticker == "KO")), 4])
# adding the start price to beginning of 500 samples I will be running

set.seed(100)
# Setting a seed so that your simulations match mine

# adding the 500 simulations to the matrix
for(j in 1:ncol(msft_matrix)){
  msft_matrix[1,j]<-as.numeric(risk[(which(risk$ticker == "MSFT")), 4])
  for(i in 2:nrow(msft_matrix)){
    msft_matrix[i,j]<-msft_matrix[i-1,j]*exp(rnorm(1, as.numeric(risk[(which(risk$ticker == "MSFT")), 3]), as.numeric(risk[(which(risk$ticker == "MSFT")), 2])))
  }
}

for(j in 1:ncol(unh_matrix)){
  unh_matrix[1,j]<-as.numeric(risk[(which(risk$ticker == "UNH")), 4])
  for(i in 2:nrow(unh_matrix)){
    unh_matrix[i,j]<-unh_matrix[i-1,j]*exp(rnorm(1, as.numeric(risk[(which(risk$ticker == "UNH")), 3]), as.numeric(risk[(which(risk$ticker == "UNH")), 2])))
  }
}

for(j in 1:ncol(aapl_matrix)){
  aapl_matrix[1,j]<-as.numeric(risk[(which(risk$ticker == "AAPL")), 4])
  for(i in 2:nrow(aapl_matrix)){
    aapl_matrix[i,j]<-aapl_matrix[i-1,j]*exp(rnorm(1, as.numeric(risk[(which(risk$ticker == "AAPL")), 3]), as.numeric(risk[(which(risk$ticker == "AAPL")), 2])))
  }
}

for(j in 1:ncol(dwdp_matrix)){
  dwdp_matrix[1,j]<-as.numeric(risk[(which(risk$ticker == "DWDP")), 4])
  for(i in 2:nrow(dwdp_matrix)){
    dwdp_matrix[i,j]<-dwdp_matrix[i-1,j]*exp(rnorm(1, as.numeric(risk[(which(risk$ticker == "DWDP")), 3]), as.numeric(risk[(which(risk$ticker == "DWDP")), 2])))
  }
}

for(j in 1:ncol(ko_matrix)){
  ko_matrix[1,j]<-as.numeric(risk[(which(risk$ticker == "KO")), 4])
  for(i in 2:nrow(ko_matrix)){
    ko_matrix[i,j]<-ko_matrix[i-1,j]*exp(rnorm(1, as.numeric(risk[(which(risk$ticker == "KO")), 3]), as.numeric(risk[(which(risk$ticker == "KO")), 2])))
  }
}
# added the 100 simulations to the matrix

name<-str_c("Sim ",seq(1,500))
# Creates a vector for the names of the columns

name<-c("Day",name)
# Adds a "Day" to the vector name that will be used to create column names for the matrix

# Creates the final matrix to be used
msft_final_mat<-cbind(1:(271), msft_matrix)
msft_final_mat<-as.tibble(msft_final_mat)

unh_final_mat<-cbind(1:(271), unh_matrix)
unh_final_mat<-as.tibble(unh_final_mat)

aapl_final_mat<-cbind(1:(271), aapl_matrix)
aapl_final_mat<-as.tibble(aapl_final_mat)

dwdp_final_mat<-cbind(1:(271), dwdp_matrix)
dwdp_final_mat<-as.tibble(dwdp_final_mat)

ko_final_mat<-cbind(1:(271), ko_matrix)
ko_final_mat<-as.tibble(ko_final_mat)
# Creates the final matrix to be used

# adds the names to column names for final_mat
colnames(msft_final_mat) <- name

colnames(unh_final_mat) <- name

colnames(aapl_final_mat) <- name

colnames(dwdp_final_mat) <- name

colnames(ko_final_mat) <- name
# adds the names to column names for final_mat

# Checking dimensions of the final matrix
dim(msft_final_mat)

dim(unh_final_mat)

dim(aapl_final_mat)

dim(dwdp_final_mat)

dim(ko_final_mat)
# Checking dimensions of the final matrix
# They should all be 271 501


msft_actual <- test %>% 
  filter(ticker == "msft")

unh_actual <- test %>% 
  filter(ticker == "unh")

aapl_actual <- test %>% 
  filter(ticker == "aapl")

dwdp_actual <- test %>% 
  filter(ticker == "dwdp")

ko_actual <- test %>% 
  filter(ticker == "ko")
# Create a data frame for the actual growth of msft
# This actual data is our test data

msft_graph <- msft_final_mat%>%
  gather(Simulation, Price, 2:501) %>% 
  bind_rows(msft_actual) %>% 
  mutate(close = coalesce(Price, close),
         Day = coalesce(as.integer(Day), day),
         ticker = coalesce(Simulation, ticker))

msft_graph %>% 
  ggplot(aes(Day, close, group = ticker)) +
  geom_line(color = "grey", alpha = .5) +
  geom_line(data = filter(msft_graph, ticker == "msft"), color = "black", size = 1) +
  labs(title = "500 Monte Carlo Simulations of Microsoft", y= "Price") +
  theme_bw()

##########

unh_graph <- unh_final_mat%>%
  gather(Simulation, Price, 2:501) %>% 
  bind_rows(unh_actual) %>% 
  mutate(close = coalesce(Price, close),
         Day = coalesce(as.integer(Day), day),
         ticker = coalesce(Simulation, ticker))

unh_graph %>% 
  ggplot(aes(Day, close, group = ticker)) +
  geom_line(color = "grey", alpha = .5) +
  geom_line(data = filter(unh_graph, ticker == "unh"), color = "black", size = 1) +
  labs(title = "500 Monte Carlo Simulations of United Health", y= "Price") +
  theme_bw()

#############

aapl_graph <- aapl_final_mat%>%
  gather(Simulation, Price, 2:501) %>% 
  bind_rows(aapl_actual) %>% 
  mutate(close = coalesce(Price, close),
         Day = coalesce(as.integer(Day), day),
         ticker = coalesce(Simulation, ticker))

aapl_graph %>% 
  ggplot(aes(Day, close, group = ticker)) +
  geom_line(color = "grey", alpha = .5) +
  geom_line(data = filter(aapl_graph, ticker == "aapl"), color = "black", size = 1) +
  labs(title = "500 Monte Carlo Simulations of Apple", y= "Price") +
  theme_bw()

############

dwdp_graph <- dwdp_final_mat%>%
  gather(Simulation, Price, 2:501) %>% 
  bind_rows(dwdp_actual) %>% 
  mutate(close = coalesce(Price, close),
         Day = coalesce(as.integer(Day), day),
         ticker = coalesce(Simulation, ticker))

dwdp_graph %>% 
  ggplot(aes(Day, close, group = ticker)) +
  geom_line(color = "grey", alpha = .5) +
  geom_line(data = filter(dwdp_graph, ticker == "dwdp"), color = "black", size = 1) +
  labs(title = "500 Monte Carlo Simulations of DowDuPont", y= "Price") +
  theme_bw()

##########

ko_graph <- ko_final_mat%>%
  gather(Simulation, Price, 2:501) %>% 
  bind_rows(ko_actual) %>% 
  mutate(close = coalesce(Price, close),
         Day = coalesce(as.integer(Day), day),
         ticker = coalesce(Simulation, ticker))

ko_graph %>% 
  ggplot(aes(Day, close, group = ticker)) +
  geom_line(color = "grey", alpha = .5) +
  geom_line(data = filter(ko_graph, ticker == "ko"), color = "black", size = 1) +
  labs(title = "500 Monte Carlo Simulations of Coca-Cola", y= "Price") +
  theme_bw()
# The above plots each stock's simulation and actual

msft_final_mat[251,-1]%>%
  as.numeric()%>%
  quantile

unh_final_mat[251,-1]%>%
  as.numeric()%>%
  quantile

aapl_final_mat[251,-1]%>%
  as.numeric()%>%
  quantile

dwdp_final_mat[251,-1]%>%
  as.numeric()%>%
  quantile

ko_final_mat[251,-1]%>%
  as.numeric()%>%
  quantile
# gives the quantiles for the simulations

# Here I will calculate the growth if I bought the stock at 2017-12-29
start <- all %>% 
  filter(date == "2017-12-29") %>% 
  select(ticker, close, date)
# This gives me the close of each stock for the date of 2017-12-29

# Here I will figure out the percentile the actual fell compared to the simulations

msft_percentile <- as.numeric(msft_final_mat[271,])

unh_percentile <- as.numeric(unh_final_mat[271,])

aapl_percentile <- as.numeric(aapl_final_mat[271,])

dwdp_percentile <- as.numeric(dwdp_final_mat[271,])

ko_percentile <- as.numeric(ko_final_mat[271,])
# Create a vector with all end values of the simulations

length(which(msft_percentile < msft_actual$close[nrow(msft_actual)]))/500
# Should get a value of .346

length(which(unh_percentile < unh_actual$close[nrow(unh_actual)]))/500
# Should get a value of .246

length(which(aapl_percentile < aapl_actual$close[nrow(aapl_actual)]))/500
# Should get a value of .116

length(which(dwdp_percentile < dwdp_actual$close[nrow(dwdp_actual)]))/500
# Should get a value of 0

length(which(ko_percentile < ko_actual$close[nrow(ko_actual)]))/500
# Should get a value of .468

# Calculates the percentile of where the actual ended compared to the simulations
# These percentiles allows us to compare the actual price movement to the predicted
# We can see on at least these 5 stocks all have them fell in the bottom 50th percentile
# This indicates that our simulation overestimated stock growth
# To increase accuracy more variables need to be included

