################################################################################
# ZADANIE 2
################################################################################

# WCZYTANIE DANYCH + PRZEKSZTAŁCENIA
dane <- read.csv2(file = "Wig20_2000do2007.csv", dec = ',')
head(dane)
dane<-ts(dane[,4])
lnrdane<- diff(log(dane))#logarytmiczne stopy zwrotu

# STATYSTYKI OPISOWE
#install.packages("psych")
library(psych)
t(describe(dane))
t(describe(lnrdane))

# WYKRESY POZIOMÓW I LOGARYTMICZNYCH STÓP ZWROTU
par(mfrow = c(2,1))
plot(dane) # widać I(1)
plot(lnrdane) # wygląda białoszumowo

# WYKRESY FUNKCJI ACF
par(mfrow = c(3,1))
acf(dane)
acf(lnrdane)
acf(lnrdane^2)

#install.packages("TSA")
library(TSA)#zmiana pakietu do rysowania acf
par(mfrow = c(3,1))
TSA::acf(dane)
TSA::acf(lnrdane)
TSA::acf(lnrdane^2)

# TESTY LJUNG BOXA
Box.test(dane) # autokorelacja
Box.test(lnrdane) # brak
Box.test(lnrdane^2) # autokorelacja

# TESTY MC LEOD
mcleod <- McLeod.Li.test(y = dane)
mcleod
mcleod <- McLeod.Li.test(y = lnrdane)
mcleod
#mcleod <- McLeod.Li.test(y = lnrdane^2)
#mcleod

# TEST EFEKTU ARCH
#install.packages("MTS")
library(MTS)
archTest(dane) #ARCH
archTest(lnrdane) #ARCH
#archTest(lnrdane^2) #ARCH

################################################################################
# ZADANIE 3
################################################################################

# WCZYTANIE DANYCH + PRZEKSZTAŁCENIA
dane <- read.csv2(file = "DAX_2006_2016_ost.csv", dec = ',')
head(dane)
dane<-ts(dane[,2])
lnrdane<- diff(log(dane))#logarytmiczne stopy zwrotu

# STATYSTYKI OPISOWE
#install.packages("psych")
library(psych)
t(describe(dane))
t(describe(lnrdane))

# WYKRESY POZIOMÓW I LOGARYTMICZNYCH STÓP ZWROTU
par(mfrow = c(2,1))
plot(dane) # widać I(1)
plot(lnrdane) # wygląda białoszumowo

# WYKRESY FUNKCJI ACF
par(mfrow = c(3,1))
acf(dane)
acf(lnrdane)
acf(lnrdane^2)

#install.packages("TSA")
library(TSA)#zmiana pakietu do rysowania acf
par(mfrow = c(3,1))
TSA::acf(dane)
TSA::acf(lnrdane)
TSA::acf(lnrdane^2)

# TESTY LJUNG BOXA
Box.test(dane) # autokorelacja
Box.test(lnrdane) # brak
Box.test(lnrdane^2) # autokorelacja

# TESTY MC LEOD
par(mfrow = c(2,1))
mcleod <- McLeod.Li.test(y = dane)
mcleod
mcleod <- McLeod.Li.test(y = lnrdane)
mcleod
#mcleod <- McLeod.Li.test(y = lnrdane^2)
#mcleod

# TEST EFEKTU ARCH
#install.packages("MTS")
library(MTS)
archTest(dane) #ARCH
archTest(lnrdane) #ARCH
#archTest(lnrdane^2) #ARCH

#install.packages("rugarch")
library(rugarch)

# Model 1: sGARCH + normalny
spec_norm <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

# Model 2: sGARCH + Student
spec_std <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)

# Model 3: GJR-GARCH + skośny Student
spec_sstd <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)

# Estymacja modeli
fit_norm <- ugarchfit(spec = spec_norm, data = lnrdane)
fit_std <- ugarchfit(spec = spec_std, data = lnrdane)
fit_sstd <- ugarchfit(spec = spec_sstd, data = lrndane)

infocriteria(fit_norm)
infocriteria(fit_std)
infocriteria(fit_sstd)

coef(fit_norm)
coef(fit_std)
coef(fit_sstd)

fit_norm@fit$matcoef
fit_std@fit$matcoef
fit_sstd@fit$matcoef

par(mfrow = c(3,1))
reszty_norm <- residuals(fit_norm, standardize = FALSE)
plot(reszty_norm, type = "l", main = "Surowe reszty")
reszty_std <- residuals(fit_std, standardize = FALSE)
plot(reszty_std, type = "l", main = "Surowe reszty")
reszty_sstd <- residuals(fit_sstd, standardize = FALSE)
plot(reszty_sstd, type = "l", main = "Surowe reszty")

par(mfrow = c(3,1))
wariancja_warunkowa_norm <- sigma(fit_norm)^2
plot(wariancja_warunkowa_norm, type = "l", main = "Wariancje warunkowe")
wariancja_warunkowa_std <- sigma(fit_std)^2
plot(wariancja_warunkowa_std, type = "l", main = "Wariancje warunkowe")
wariancja_warunkowa_sstd <- sigma(fit_sstd)^2
plot(wariancja_warunkowa_sstd, type = "l", main = "Wariancje warunkowe")

par(mfrow = c(3,1))
odchylenie_warunkowa_norm <- sigma(fit_norm)
plot(odchylenie_warunkowa_norm, type = "l", main = "Odchylenia warunkowe")
odchylenie_warunkowa_std <- sigma(fit_std)
plot(odchylenie_warunkowa_std, type = "l", main = "Odchylenia warunkowe")
odchylenie_warunkowa_sstd <- sigma(fit_sstd)
plot(odchylenie_warunkowa_sstd, type = "l", main = "Odchylenia warunkowe")


reszty_std_norm <- residuals(fit_norm, standardize = TRUE)
par(mfrow = c(2,1))
acf(reszty_std_norm, main = "ACF standaryzowanych reszt")
acf(reszty_std_norm^2, main = "ACF kwadratów standaryzowanych reszt")
Box.test(reszty_std_norm, lag = 10, type = "Ljung-Box")

reszty_std_std <- residuals(fit_std, standardize = TRUE)
par(mfrow = c(2,1))
acf(reszty_std_std, main = "ACF standaryzowanych reszt")
acf(reszty_std_std^2, main = "ACF kwadratów standaryzowanych reszt")
Box.test(reszty_std_std, lag = 10, type = "Ljung-Box")

reszty_std_sstd <- residuals(fit_sstd, standardize = TRUE)
par(mfrow = c(2,1))
acf(reszty_std_sstd, main = "ACF standaryzowanych reszt")
acf(reszty_std_sstd^2, main = "ACF kwadratów standaryzowanych reszt")
Box.test(reszty_std_sstd, lag = 10, type = "Ljung-Box")

# NAJLEPSZY MODEL - GJR-GARCH + skośny Student
prognoza <- ugarchforecast(fit_sstd, n.ahead = 10)
par(mfrow = c(1, 1))
plot(forecast, which = 3)
plot(forecast, which = 1)

