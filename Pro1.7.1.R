library(gapminder)
library(dplyr)
library(ggplot2)
library(xlsx)
#upload the data first
NASDAQ_100 <- read.csv("First.csv")

#Obtain NASDAQ-100 constituent list
NASDAQ_100 <- NASDAQ_100 %>% 
  filter(conm == "Nasdaq 100", is.na(thru) | thru > 20220101 )

#Form in 2021
NASDAQ_100_2021 <- NASDAQ_100 %>% 
  filter(from > 20210101 )
count(NASDAQ_100_2021)
#there are 8 stocks are constituents only part of 2021

#output the gvkey code from first file
write.csv(NASDAQ_100, file="G-code.csv",row.names = F)

#upload ID cross-walk
ID_cross_walk <- read.csv("ID cross-walk.csv")



#upload Daily Stock Price
Daily_Stock_Price <- read.csv("Daily Stock Price.csv")

#Creat a DATA FRAME to store the weighted average of daily return

summary(Daily_Stock_Price)

#Calculate the Market cap as weight
Daily_Stock_Price$MARKETSHARE =  Daily_Stock_Price$PRC * Daily_Stock_Price$SHROUT

Daily_Data <- group_by(Daily_Stock_Price, date)
Daily_Avg <- summarise(Daily_Data,
                       weighted_return = weighted.mean(RET,MARKETSHARE))

#transfer the date from character to Date
Daily_Avg <- Daily_Avg %>%
  mutate(date = as.Date(as.character(date), format = "%Y %m %d"))


#plot the result
ggplot(Daily_Avg , aes(x =date, y = weighted_return)) +
  geom_line() +
  scale_y_continuous(
    limits = c(-0.04,0.04),
    breaks = seq(-0.04,0.04,0.01)) +
  geom_hline(yintercept = 0) +
labs(
  title = "Weighted Average of Daily Return",
  x = "Date",
  y = "Weighted Return"
)


Official_NASDAQ_100 <- read.csv("^NDX.csv")

Official_NASDAQ_100 <- Official_NASDAQ_100 %>%
  mutate(Date = as.Date(Date), format = "%Y %m %d")

Official_NASDAQ_100$Daily_Return <-
  ((Official_NASDAQ_100$Close-Official_NASDAQ_100$Open)*100/Official_NASDAQ_100$Open)


ggplot(Official_NASDAQ_100 , aes(x = Date, y = Daily_Return)) +
  geom_line() +
  labs(
    title = "Official_NASDAQ_100",
    x = "Date",
    y = "Daily_Return"
  )
