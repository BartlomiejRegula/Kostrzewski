library(rmgarch)
data(dji30retw)

#View(dji30retw)
Dat <-dji30retw[, 1:3]
y<-Dat#na tym wektorze pracujemy

dim(Dat)
names(y)

specyfikacja <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model="norm")

dcc_specyfikacja<-dccspec(uspec = multispec( replicate(3, specyfikacja)), VAR = TRUE, lag =1, dccOrder = c(1,1), distribution = "mvnorm")

fit_dcc <- dccfit(spec = dcc_specyfikacja, data = y)

?dccfit

names(fit_dcc@mfit)

class(fit_dcc)

slotNames(fit_dcc)
names(fit_dcc@model)
names(fit_dcc@mfit)

fit_dcc@model$varcoef

names(y)
