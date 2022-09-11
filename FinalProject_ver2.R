rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)#�ɶ��ǦC
library(tidyquant)

#remotes::install_github("joshuaulrich/quantmod@358-getsymbols-new.session")

#�p�⺦�^�T�����
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

#�u�ʥ���
RollingAver <- function(x,n){filter(x,rep(1/n,n),side = 1)}

#�P�_�R��T��
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

#�Ыث���������
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

#�^�����
RSI_BackTest <- function(number,startday,endday,Tday,lower,upper,posmin = -1000,posmax = 1000){
  stockData <- tq_get(number, get = "stock.prices", from = startday, to = endday)
  stockData <- stockData[,c('symbol','date','open','close')] %>% arrange(date)
  
  #�jk��k���t�B���^�T�B�������^�T�BRSI
  ShiftClose <- c(0,stockData$close[1:(nrow(stockData)-1)])
  stockData <- stockData %>% mutate(�jK���t = open - ShiftClose)
  stockData <- stockData %>% mutate(��K���t = open - close)
  stockData$���T <- RiseFall(stockData$close)[[1]]
  stockData$�^�T <- RiseFall(stockData$close)[[2]]
  stockData$�������T <- rollmean(stockData$���T,Tday,fill = NA,align = "right")
  stockData$�����^�T <- rollmean(stockData$�^�T,Tday,fill = NA,align = "right")
  stockData <- stockData %>%  mutate(RSI = (�������T/(�������T + �����^�T)) * 100)
  
  #�R��T��
  stockData$�R�� <- BuyOrSell(stockData$RSI,lower,upper)
  
  #��������
  stockData$���� <- part(stockData$�R��,posmin,posmax)
  
  #�����l�q
  ShiftPos <- c(0,stockData$����[1:(nrow(stockData)-1)])
  stockData <- stockData %>% mutate(�����l�q = (ShiftPos * �jK���t + ���� * ��K���t))
  
  
  #�ֿn�l�q
  acc_profit <- stockData$�����l�q[[1]]
  profit <- c(0)
  
  for (prof in (1:(length(stockData$�����l�q) - 1))) {
    acc_profit <- acc_profit + stockData$�����l�q[[prof + 1]]
    profit[(length(profit)+1)] <- acc_profit
  }
  
  stockData$�ֿn�l�q <- profit
  
  return(stockData)
}

#----------------------�H�W���غc���-----------------------#

#�ѼƬ��Ѳ��N���B�^���}�l��B�^��������B�ѼƳ]�w�B�R��T���U���B�R��T���W���B��������U���B��������W��
Result <- RSI_BackTest('NVDA','2020-05-31','2022-05-31',6,20,80,-5,5)

#�Ѳ��N���榡:'���ѥN��'�B'�x�ѥN��.TW'
#�^������榡:'yyyy-mm-dd'

ggplot(Result) +
  geom_line(aes(x = date, y = RSI),colour = 'red') +
  labs(title = 'RSI����', x = '���', y = 'RSI') +
  theme_tq() + 
  scale_color_tq()

#---------------------�H�U���̨Τ�--------------------------#
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
          income[(length(income)+1)] <- df$�ֿn�l�q[nrow(df)]
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

df_2 <- data.frame(�ѼƳ]�w = t,
                   �R�T = RSILower,
                   ��T = RSIUpper,
                   ����U�� = Posimin,
                   ����W�� = Posimax,
                   �ֿn�l�q = income)

df_3 <- df_2 %>% arrange(desc(�ֿn�l�q))
df_3[1,]












