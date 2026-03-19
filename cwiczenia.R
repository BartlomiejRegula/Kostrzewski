library(quantmod)

nazwy_cen <- c("BTC-USD", "ETH-USD", "XRP-USD","^DJI")

prices <- NULL

for (stock_index in 1:length(nazwy_cen)) {
  dane <- Ad(getSymbols(nazwy_cen[stock_index], 
                        from = "2021-01-01",
                        to = "2024-12-31",
                        auto.assign = FALSE))
  
  if (is.null(prices)) {
    prices <- dane
  } else {
    prices <- merge(prices, dane)
  }
}

colnames(prices) <- nazwy_cen

library(psych)
library(TSA)
library(MTS)
dane <- read.csv2("C:/Users/s-A121-34/Desktop/BR/Wig20_2000do2007.csv",
                 sep = ";",
                 dec = ",",
                 header = TRUE)
dane <- ts(dane[,4])
plot(dane)
t(describe(dane))
plot()
#dowiedzieć sie jak jest liczona kurtoza, i pewnie inne rzeczy tez

#AUTOKORELACJA
par(mfrow = c(1,1))

lnrdane<-diff(log(dane)) #ln(s_t/s_(t-1))



par(mfrow=c(4,1))
acf(lnrdane)
acf(dane)
acf(lnrdane*lnrdane)
acf(dane*dane)
# jakaś babka poweidziała że jak masz kwadraty chyba tych stóp zwrotu, to
# jest tam biały szum, i można stosować arch i garch chyba tak pan powiedział


#BOX TEST
#Box.test(lnrdane)
#archTest(lnrdane)
#(McLeod.Li.test(y=(lnrdane)))

Box.test(lnrdane, lag = 1, type ="Ljung")
Box.test(lnrdane^2, lag = 1, type = "Ljung")
McLeod.Li.test((y = (lnrdane)))
test = McLeod.Li.test(y=dane)


