rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)#時間序列
library(tidyquant)

#remotes::install_github("joshuaulrich/quantmod@358-getsymbols-new.session")

#計算漲跌幅之函數
RiseFall <- function(x){
  up <- c(NA)
  down <- c(NA)
  for (i in (1:(length(x) - 1))) {
    if ((x[[i+1]] - x[[i]]) <= 0) {
      up[(length(up)+1)] <- 0
      down[(length(down)+1)] <- x[[i]] - x[[i+1]]
    }else{
      up[(length(up)+1)] <- x[[i+1]] - x[[i]]
      down[(length(down)+1)] <- 0
    }
  }
  return(list(up,down))
}

#滾動平均
RollingAver <- function(x,n){filter(x,rep(1/n,n),side = 1)}

#判斷買賣訊號
BuyOrSell <- function(x,lower,upper){
  BuySell <- c()
  for (BS in x) {
    if (BS <= lower & is.na(BS) == FALSE) {
      BuySell[(length(BuySell)+1)] <- 1
    }else if (BS >= upper & is.na(BS) == FALSE) {
      BuySell[(length(BuySell)+1)] <- -1
    }else if(is.na(BS) == TRUE){
      BuySell[(length(BuySell)+1)] <- NA
    }else{
      BuySell[(length(BuySell)+1)] <- 0
    }
  }
  return(BuySell)
}

#創建持有部位函數
part <- function(x,min,max){
  x[is.na(x)] <- 0
  acc <- x[[1]]
  pos  <- c(0,0)
  for (p in (1:(length(x) - 1))) {
    acc <- acc + x[[p+1]]
    if (acc >= min & acc <= max) {
      pos[(length(pos)+1)] <- acc
    }else if (acc < min) {
      acc <- min
      pos[(length(pos)+1)] <- acc
    }else if (acc > max) {
      acc <- max
      pos[(length(pos)+1)] <- acc
    }
  }
  return(pos[-length(pos)])
}


#stockData <- tq_get("0050.TW", get = "stock.prices", from = "2021-05-26", to = "2022-05-26")
#stockData <- stockData[,c('symbol','date','open','close')] %>% arrange(date)

#paste(number,".TW",sep = '')

#回測函數
RSI_BackTest <- function(number,startday,endday,Tday,lower,upper,posmin = -1000,posmax = 1000){
  stockData <- tq_get(number, get = "stock.prices", from = startday, to = endday)
  stockData <- stockData[,c('symbol','date','open','close')] %>% arrange(date)
  
  #隔k當k價差、漲跌幅、平均漲跌幅、RSI
  ShiftClose <- c(0,stockData$close[1:(nrow(stockData)-1)])
  stockData <- stockData %>% mutate(隔K價差 = open - ShiftClose)
  stockData <- stockData %>% mutate(當K價差 = open - close)
  stockData$漲幅 <- RiseFall(stockData$close)[[1]]
  stockData$跌幅 <- RiseFall(stockData$close)[[2]]
  stockData$平均漲幅 <- rollmean(stockData$漲幅,Tday,fill = NA,align = "right")
  stockData$平均跌幅 <- rollmean(stockData$跌幅,Tday,fill = NA,align = "right")
  stockData <- stockData %>%  mutate(RSI = (平均漲幅/(平均漲幅 + 平均跌幅)) * 100)
  
  #買賣訊號
  stockData$買賣 <- BuyOrSell(stockData$RSI,lower,upper)
  
  #持有部位
  stockData$部位 <- part(stockData$買賣,posmin,posmax)
  
  #當期損益
  ShiftPos <- c(0,stockData$部位[1:(nrow(stockData)-1)])
  stockData <- stockData %>% mutate(當期損益 = (ShiftPos * 隔K價差 + 部位 * 當K價差))
  
  
  #累積損益
  acc_profit <- stockData$當期損益[[1]]
  profit <- c(0)
  
  for (prof in (1:(length(stockData$當期損益) - 1))) {
    acc_profit <- acc_profit + stockData$當期損益[[prof + 1]]
    profit[(length(profit)+1)] <- acc_profit
  }
  
  stockData$累積損益 <- profit
  
  return(stockData)
}

#----------------------以上為建構函數-----------------------#

#參數為股票代號、回測開始日、回測結束日、天數設定、買賣訊號下限、買賣訊號上限、持有部位下限、持有部位上限
Result <- RSI_BackTest('NVDA','2020-05-31','2022-05-31',6,20,80,-5,5)

#股票代號格式:'美股代號'、'台股代號.TW'
#回測日期格式:'yyyy-mm-dd'

ggplot(Result) +
  geom_line(aes(x = date, y = RSI),colour = 'red') +
  labs(title = 'RSI指標', x = '日期', y = 'RSI') +
  theme_tq() + 
  scale_color_tq()

#---------------------以下為最佳化--------------------------#
income <- c()
t <- c()
RSILower <- c()
RSIUpper <- c()
Posimin <- c()
Posimax <- c()

for (a in c(6,14,30,60)) {
  for (b in c(20,30)) {
    for (c in c(70,80)) {
      for (d in c(-5:0)) {
        for (e in c(1:5)) {
          df <- RSI_BackTest('2330.TW','2020-05-31','2022-05-31',a,b,c,d,e)
          income[(length(income)+1)] <- df$累積損益[nrow(df)]
          t[(length(t)+1)] <- a
          RSILower[(length(RSILower)+1)] <- b
          RSIUpper[(length(RSIUpper)+1)] <- c
          Posimin[(length(Posimin)+1)] <- d
          Posimax[(length(Posimax)+1)] <- e

        }

      }
    }

  }
}

df_2 <- data.frame(天數設定 = t,
                   買訊 = RSILower,
                   賣訊 = RSIUpper,
                   部位下限 = Posimin,
                   部位上限 = Posimax,
                   累積損益 = income)

df_3 <- df_2 %>% arrange(desc(累積損益))
df_3[1,]













