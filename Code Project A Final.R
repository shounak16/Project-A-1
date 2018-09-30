###### Importing Libraries

library(tidyquant)
library(xts)
library(ggplot2)
library(readxl)
library(knitr)

############################## QUESTION 1
###### Read Excel
SP500_df <- read_excel("C:/Users/Laura/Desktop/MIT Fall 2018/Financial Data Science/SPX Historical Data.xlsx")
SP500_xts <- xts(SP500_df[,-1], order.by = SP500_df$Dates )

###### Part b

## Check if no data < 0 
summary(SP500_xts)

## Plot the data
p <- ggplot () + 
  geom_line (data = SP500_df, aes(x = SP500_df$Dates, y=SP500_df$PX_OPEN), color = "green") +
  geom_line (data = SP500_df, aes(x = SP500_df$Dates, y=SP500_df$PX_HIGH), color = "yellow") +
  geom_line (data = SP500_df, aes(x = SP500_df$Dates, y=SP500_df$PX_LOW), color = "red") +
  geom_line (data = SP500_df, aes(x = SP500_df$Dates, y=SP500_df$PX_LAST), color = "blue") + 
  xlab("Date") +
  ylab("Prices") +
  ggtitle("S&P500 Price Data")


print(p)  

## Check that Max is the Max Value and Min is the Min
min_Check <- sum(SP500_xts$PX_LOW > SP500_xts$PX_HIGH) + sum(SP500_xts$PX_LOW > SP500_xts$PX_OPEN) + sum(SP500_xts$PX_LOW > SP500_xts$PX_LAST)
min_Check

max_Check <- sum(SP500_xts$PX_HIGH < SP500_xts$PX_OPEN) + sum(SP500_xts$PX_HIGH < SP500_xts$PX_LAST)
max_Check

## Checking for same data
same_Data_Check <- sum(SP500_xts$PX_OPEN == SP500_xts$PX_HIGH & SP500_xts$PX_LOW == SP500_xts$PX_HIGH & SP500_xts$PX_LAST == SP500_xts$PX_HIGH)
same_Data_Check

## Sub Vector For Same Data
same_data_Diff = SP500_xts$PX_HIGH - SP500_xts$PX_LOW
same_data_Diff_df <- data.frame(coredata(same_data_Diff), date=index(same_data_Diff))


p1 <- ggplot () + 
  geom_line (data = same_data_Diff_df, aes(x = same_data_Diff_df$date, y=same_data_Diff_df$PX_HIGH), color = "blue") + 
  ylab("High - Low Range") + 
  xlab("Time") +
  ggtitle("High - Low Price")

print(p1)  


## Checkign for NA or characters
char_Check <- sum(is.na(as.numeric(SP500_xts$PX_OPEN))) + sum(is.na(as.numeric(SP500_xts$PX_HIGH))) + sum(is.na(as.numeric(SP500_xts$PX_LOW))) + sum(is.na(as.numeric(SP500_xts$PX_LAST)))

###### Part c

## Probabilities without data with issue
clean <- index(SP500_xts)[SP500_xts$PX_LOW != SP500_xts$PX_HIGH]
prob_H_O <- mean(SP500_xts[clean]$PX_HIGH == SP500_xts[clean]$PX_OPEN)
prob_H_L <- mean(SP500_xts[clean]$PX_HIGH == SP500_xts[clean]$PX_LAST)
prob_L_O <- mean(SP500_xts[clean]$PX_LOW == SP500_xts[clean]$PX_OPEN)
prob_L_L <- mean(SP500_xts[clean]$PX_LOW == SP500_xts[clean]$PX_LAST)

prob_H_O
prob_H_L
prob_L_O
prob_L_L

## Probabilities with data after 1980
SP500_Post1980 <- window (SP500_xts, start = '1982-04-20', end = '2018-09-07')

prob_H_O_Post1980 <- mean(SP500_Post1980$PX_HIGH == SP500_Post1980$PX_OPEN)
prob_H_L_Post1980 <- mean(SP500_Post1980$PX_HIGH == SP500_Post1980$PX_LAST)
prob_L_O_Post1980 <- mean(SP500_Post1980$PX_LOW == SP500_Post1980$PX_OPEN)
prob_L_L_Post1980 <- mean(SP500_Post1980$PX_LOW == SP500_Post1980$PX_LAST)

prob_H_O_Post1980
prob_H_L_Post1980
prob_L_O_Post1980
prob_L_L_Post1980

###### Part d
intraday_Range <- (SP500_xts$PX_HIGH - SP500_xts$PX_LOW) / (SP500_xts$PX_LOW)
intraday_Range_1980 <- window (intraday_Range, start = '1980-01-01', end = '2011-08-30') 
intraday_Range_1980_df <- data.frame(coredata(intraday_Range_1980), date=index(intraday_Range_1980))
intraday_Range_1980_df_sort <- intraday_Range_1980_df[order(-intraday_Range_1980_df$PX_HIGH),]
intraday_Range_top20 <- intraday_Range_1980_df_sort[1:20,]
intraday_Range_top20

###### Part e
overnight_Return <- diff(c(SP500_xts$PX_OPEN, SP500_xts$PX_LAST), lag = 1)/lag(SP500_xts$PX_LAST, k = 1)
overnight_Return <- overnight_Return[-1,]

overnight_Return_1980 <- window (overnight_Return, start = '1980-01-01', end = '2011-08-30') 
overnight_Return_1980_df <- data.frame(coredata(overnight_Return_1980), date=index(overnight_Return_1980))

overnight_Return_1980_df_sort_des <- overnight_Return_1980_df[order(-overnight_Return_1980_df$PX_OPEN),]
overnight_Return_1980_df_sort_asc <- overnight_Return_1980_df[order(overnight_Return_1980_df$PX_OPEN),]

overnight_Return_top20 <- overnight_Return_1980_df_sort_des[1:20,]
overnight_Return_top20

overnight_Return_low20 <- overnight_Return_1980_df_sort_asc[1:20,]
overnight_Return_low20 <- overnight_Return_low20[order(overnight_Return_low20$date),]
overnight_Return_low20

## Excluding data pre 4/20/1982
overnight_Return_1982 <- window (overnight_Return, start = '1982-04-20', end = '2011-08-30')

overnight_Return_1982_df <- data.frame(coredata(overnight_Return_1982), date=index(overnight_Return_1982))

overnight_Return_1982_df_sort_des <- overnight_Return_1982_df[order(-overnight_Return_1982_df$PX_OPEN),]
overnight_Return_1982_df_sort_asc <- overnight_Return_1982_df[order(overnight_Return_1982_df$PX_OPEN),]

overnight_Return_top20_1982 <- overnight_Return_1982_df_sort_des[1:20,]
overnight_Return_top20_1982

overnight_Return_low20_1982 <- overnight_Return_1982_df_sort_asc[1:20,]
overnight_Return_low20_1982 <- overnight_Return_low20_1982[order(overnight_Return_low20_1982$date),]
overnight_Return_low20_1982



###### Part f
log_Return <- log(SP500_xts$PX_LAST / lag(SP500_xts$PX_LAST, k = 1))
log_Return <- log_Return[-1,]

log_Return_std <- rollapply(log_Return, width = 63, FUN = sd)
jump <- abs(log_Return / lag(log_Return_std, n = 1))

jump_df <- data.frame(coredata(jump), date=index(jump))

jump_sort <- jump_df[order(-jump_df$PX_LAST),]
jump_sort_top20 <- jump_sort[1:20,]
jump_sort_top20



############################## QUESTION 3

DJI <- read_csv("C:/Users/Laura/Desktop/MIT Fall 2018/Financial Data Science/Assignment/^DJI_March.csv")
###Question 3A

#Get Close value of DOW on March6, 2015
DJI_March6 <- DJI$Close[2]
print(DJI_March6)

#Get stock prices of DOW components (with AT&T) on March6
March_06_Ticker_ATT <- tq_get(c("T", "MMM", "AXP", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP",
                                "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE",
                                "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "GE") 
                              , get = "stock.prices", from = "2015-03-06", to = "2015-03-07")

#Sum stock prices of DOW components
Stock_Prices_Sum_T <- sum(March_06_Ticker_ATT$close)

#Calculate Divisor as Stock Prices/DOW index Value
Divisor_March6_ATT <- Stock_Prices_Sum_T/DJI_March6
print(Divisor_March6_ATT)

#Get stock prices of DOW components (with AAPL) on March6
March_06_Ticker_AAPL <- tq_get(c("AAPL", "MMM", "AXP", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP",
                                 "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE",
                                 "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "GE") 
                               , get = "stock.prices", from = "2015-03-06", to = "2015-03-07")

#Sum Stock Prices of DOW Components (with AAPL)
Stock_Prices_Sum_AAPL <- sum(March_06_Ticker_AAPL$close)

#Calculate Divisor as Stock Prices/DOW index Value
Divisor_March6_AAPL <- Stock_Prices_Sum_AAPL/DJI_March6
print(Divisor_March6_AAPL)

###Question 3B
#Get ATT Close Price on Announcement Date
price_ATT_March6 <- March_06_Ticker_ATT$close[1]
#Get weight of ATT
weight_ATT <- price_ATT_March6/Stock_Prices_Sum_T
print(weight_ATT)

###Question 3C
#Get weight of remaining 29 companies (w/o ATT)
weight_remaining29_ATT <- 1-weight_ATT
price_AAPL_March6 <- March_06_Ticker_AAPL$close[1]
weight_AAPL <- price_AAPL_March6/Stock_Prices_Sum_AAPL
print(weight_AAPL)
# Weight of remaining 29 companies (w/o AAPL)
weight_remaining29_APPL <- 1-weight_AAPL
print(weight_remaining29_APPL)
#Change in weight of remaining 29 companies after AAPL replaces ATT
change_weight_remaining29 <- weight_remaining29_APPL-weight_remaining29_ATT
print(change_weight_remaining29)


###Question 3D
#Replace ATT with Amazon
March_06_Ticker_AMZN <- tq_get(c("AMZN", "MMM", "AXP", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP",
                                 "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE",
                                 "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "GE") 
                               , get = "stock.prices", from = "2015-03-06", to = "2015-03-07")

#Get sum of stock prices with Amazon in DOW
Stock_Prices_Sum_AMZN <- sum(March_06_Ticker_AMZN$close)

#Calculate Divisor as Stock Prices/DOW index Value
Divisor_March6_AMZN <- Stock_Prices_Sum_AMZN/DJI_March6
print(Divisor_March6_AMZN)


#Replace ATT with Berkshire Hathaway
March_06_Ticker_BRKA <- tq_get(c("BRK-A", "MMM", "AXP", "BA", "CAT", "CVX", "CSCO", "KO", "DIS", "DWDP",
                                 "XOM", "GS", "HD", "IBM", "INTC", "JNJ", "JPM", "MCD", "MRK", "MSFT", "NKE",
                                 "PFE", "PG", "TRV", "UTX", "UNH", "VZ", "V", "WMT", "GE") 
                               , get = "stock.prices", from = "2015-03-06", to = "2015-03-07")

March_06_Ticker_BRKA_AMX <- tq_get(c("BRK-A","AMZN") 
                                   , get = "stock.prices", from = "2015-03-06", to = "2015-03-07")


#Get sum of stock prices with BRKA in DOW
Stock_Prices_Sum_BRKA <- sum(March_06_Ticker_BRKA$close)

#Calculate Divisor as Stock Prices/DOW index Value
Divisor_March6_BRKA <- Stock_Prices_Sum_BRKA/DJI_March6
print(Divisor_March6_BRKA)