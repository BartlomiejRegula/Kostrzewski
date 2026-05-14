getSymbols("AAPL", from="1990-01-01", src="yahoo") 
lnrdane<-diff(log(dane))

par(mfrow=c(4,1))


library(TSA)
acf(lnrdane,drop.lag.0 = T,lag.max=40)

library(MTS)

archTest(lnrdane)

(McLeod.Li.test(y=(lnrdane)))

#############################################3
26.03.2026

library(car)
res_n<-fit_n@fit$residuals/fit_n@fit$sigma#Garch residuals (normal)
res_t<-fit_t@fit$residuals/fit_t@fit$sigma#Garch residuals (student)
plot(res_n-res_t)
par(mfrow=c(2,1))
qqPlot(res_n,distribution="norm")
qqPlot(res_t,distribution="t", df=fit_t@fit$coef["shape"])


##############################################
Dat <-dji30retw[, 1:3]
y<-Dat#na tym wektorze pracujemy
specyfikacja <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model="norm")

dcc_specyfikacja<-dccspec(uspec = multispec( replicate(3, specyfikacja)), VAR = TRUE, lag =1, dccOrder = c(1,1), distribution = "mvnorm")

Zad1 dokończone w domu

###########################################
09.04.2026
names(fit_dcc@mfit$coef)
(fit_dcc@mfit$matcoef)
fit_dcc@mfit$Q
names(fit_dcc@mfit)

simulation<-dccsim(fit_dcc,n.sim=3000,n.start=1000,m.sim=1, rseed=1)
slotNames(simulation)
names(simulation@msim)
(simulation@msim$simQ)
simulation@msim$rseed
names(simulation@msim)
(simulation@msim$simX)
simulation@msim$simH[[1]][,,111]
simulation@msim$simH[[1]][1,1,111]
simulation@msim$simR[[1]][,,111]
simulation@msim$simR[[1]][1,2,111]


par(mfrow=c(3,1))
plot(simulation@msim$simX[[1]][,1],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simX[[1]][,2],type="l")
plot(simulation@msim$simX[[1]][,3],type="l")

par(mfrow=c(3,1))
plot(simulation@msim$simH[[1]][1,1,],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simH[[1]][2,2,],type="l")
plot(simulation@msim$simH[[1]][3,3,],type="l")


par(mfrow=c(3,1))
plot(simulation@msim$simR[[1]][1,2,],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simR[[1]][1,3,],type="l")
plot(simulation@msim$simR[[1]][2,3,],type="l")
#####################################################
Dat = dji30retw[, 1:3, drop = FALSE]

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                   distribution.model = "norm")
spec1 = cgarchspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE, robust = FALSE, lag = 0, lag.max = NULL,
                   lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                   robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500),
                   dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1],
                                                                                    method = c("Kendall", "ML")[2], time.varying = FALSE,
                                                                                    transformation = c("parametric", "empirical", "spd")[1]))
#CCC-MGARCH
fit1 = cgarchfit(spec1, data = Dat, cluster = cluster, fit.control = list(eval.se=FALSE))
(fit1)
#Teraz generujemy z modelu CCC-MGARCH, który wcześniej został dopasowany do rzeczywistych obserwacji, dzięki temu mamy realne wartości parametrów
simulationCCC = cgarchsim(fit1, n.sim = 1000, m.sim = 1, startMethod = "sample", cluster=cluster)

#Do danych z CCC dopasuje DCC

specyfikacja = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model="norm")
dcc_specyfikacja<-dccspec(uspec = multispec( replicate(3, specyfikacja)), VAR = TRUE, lag =0, dccOrder = c(1,1), distribution = "mvnorm")
fit_dcc_symulacja<-dccfit(spec = dcc_specyfikacja, data = simulationCCC@msim$simX[[1]])

#generujemy dane z modelu DCC:
simulation<-dccsim(fit_dcc_symulacja,n.sim=3000,n.start=1000,m.sim=1, rseed=1)



#Wykresy wygenerowanych szereg?w z modelu DCC
par(mfrow=c(3,1))
plot(simulation@msim$simX[[1]][,1],type="l")#simulated time series with DCC-GARCH process
plot(simulation@msim$simX[[1]][,2],type="l")
plot(simulation@msim$simX[[1]][,3],type="l")


par(mfrow=c(3,1))
plot(simulation@msim$simR[[1]][1,2,],type="l",ylim=c(-1,1))
plot(simulation@msim$simR[[1]][1,3,],type="l",ylim=c(-1,1))
plot(simulation@msim$simR[[1]][2,3,],type="l",ylim=c(-1,1))

par(mfrow=c(3,1))
plot(simulation@msim$simR[[1]][1,2,],type="l",ylim=c(0.2,0.6))
abline(h=median(simulation@msim$simR[[1]][1,2,]),col='red')
plot(simulation@msim$simR[[1]][1,3,],type="l",ylim=c(0.2,0.6))
abline(h=median(simulation@msim$simR[[1]][1,3,]),col='red')
plot(simulation@msim$simR[[1]][2,3,],type="l",ylim=c(0.2,0.6))
abline(h=median(simulation@msim$simR[[1]][2,3,]),col='red')

DCCtest(Dat, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)
#####################################################3
16.04.2026

fit1@mfit$convergence

simulationCCC <- cgarchsim(fit1, n.sim = 3000, m.sim = 1, startMethod = "sample", cluster=cluster,rseed=1)
y<-simulationCCC@msim$simX[[1]][,1:3]
DCCtest(y, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)


simulationDCC<-dccsim(fit_dcc_symulacja,n.sim=3000,n.start=1000,m.sim=1, rseed=1)
y<-simulationDCC@msim$simX[[1]][,1:3]
DCCtest(y, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)

####################################################

simulationDCC<-dccsim(fit_dcc_symulacja,n.sim=3000,n.start=1000,m.sim=1, rseed=1)

fit_dcc_symulacja@mfit$coef[13]=0.4
fit_dcc_symulacja@mfit$coef[14]=0.2
fit_dcc_symulacja@mfit$coef
fit_dcc_symulacja@mfit$matcoef
fit_dcc_symulacja

simulationDCC<-dccsim(fit_dcc_symulacja,n.sim=3000,n.start=1000,m.sim=1, rseed=1)
y<-simulationDCC@msim$simX[[1]][,1:3]
DCCtest(y, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)

#####################################################
simulationDCC<-dccsim(fit_dcc,n.sim=3000,n.start=1000,m.sim=1, rseed=9)

p

lot(fit_dcc)

plot(fit_dcc, which = 4, series = c(1, 3))
plot(fit_dcc, which = 4, series = c(2, 3))
######################################################
30.04.2026
y1spec <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model="sstd")
y2spec <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(1,0), include.mean = FALSE), distribution.model="sstd")

yspec = multispec(c(y1spec,y2spec))
spec_dcc<-dccspec(uspec = yspec, VAR = TRUE, lag =1, dccOrder = c(1,1), distribution = "mvt")
yfit_DCC<-dccfit(spec = spec_dcc, data = y)

infocriteria(yfit_DCC)

dane1<-read.csv2(file="Dane/WIG20_2008_2016_ost.csv",dec=",")
dane2<-read.csv2(file="Dane/DAX_2008_2016_ost.csv", dec=",")
dane3<-read.csv2(file="Dane/DJI_2008_2016_ost.csv", dec=",")
acwi_us_poczatkowe<-read.csv2(file="Dane/acwi_us_d_2008_2016_ost.csv",dec=",")

#############################################
dane_wstepne<-merge(dane1,dane2, by="Date",all =T,sort=T)
dane_wstepne<-merge(dane_wstepne,dane3, by="Date",all =T,sort=T)
dane_wstepne<-merge(dane_wstepne,acwi_us_poczatkowe, by="Date",all =T,sort=T)

names(dane_wstepne)<-c("Date","WIG20","DAX","DJI","ACWI")
brakujace1<-which(is.na(dane_wstepne[,2])==TRUE)


missing_data_ind_min<-unique(c(brakujace1,brakujace2,brakujace3,brakujace4))

dane<-dane_wstepne[-(missing_data_ind_min),]



dim(dane_wstepne[complete.cases(dane_wstepne), ])

write.table(dane,file="WIG20_DAX_DJI_ACWI_20082016_usunieteBraki.txt",col.names = c("Date","WIG20","DAX","DJI","ACWI"))
dane<-read.table(file="WIG20_DAX_DJI_ACWI_20082016_usunieteBraki.txt",header = TRUE)l



acwi_us<-dane[,5]

y<-matrix(0,(dim(dane)[1]-1),2)
y[,1]<-log(dane[-1,2]/dane[-(dim(dane)[1]),2])
y[,2]<-log(dane[-1,3]/dane[-(dim(dane)[1]),3])

yspec1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model="sstd")
yspec <- multispec(replicate(2, yspec1))
#Dopasowywuje do
multf <- multifit(yspec, data = y,cluster=NULL)
spec_dcc<-dccspec(uspec = yspec, VAR = FALSE, lag =0, dccOrder = c(1,1), distribution = "mvnorm")
yfit_DCC <- dccfit(spec_dcc, data = y, fit.control = list(eval.se=TRUE),fit = multf, cluster = NULL)



plot(yfit_DCC, which = 4, series = c(1, 2))

Dygresja: plot(yfit_DCC, which = 4, series = c(1, 2,3))

par(mfrow=c(1,1))
plot(rcor(yfit_DCC)[1,2,],type="l",ylim=c(-1,1))

DCCtest(y, garchOrder = c(1,1), n.lags = 2, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)

###########################

uspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                    distribution.model = "norm")
spec1 <- cgarchspec(uspec = multispec( replicate(2, uspec) ), VAR = FALSE, robust = FALSE, lag = 0, lag.max = NULL,
                    lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL,
                    robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500),
                    dccOrder = c(1,1), asymmetric = FALSE, distribution.model = list(copula = c("mvnorm", "mvt")[1],
                                                                                     method = c("Kendall", "ML")[2], time.varying = FALSE,#Je?li FALSE to CCC, w przeciwnym razie DCC
                                                                                     transformation = c("parametric", "empirical", "spd")[1]))
dim(y)
yfit_CCC <- cgarchfit(spec1, data = y, cluster = NULL, fit.control = list(eval.se=FALSE))

#################
par(mfrow=c(3,1), las=2)
plot(rcor(yfit_DCC)[1,2,],type="l", axes=FALSE,xlab="",ylab="")
abline(h=yfit_CCC@mfit$Rt[1,2],col="red")
skala<-seq(from=1, to=(dim(dane)[1]-1),by=252)
axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])
axis(2)
plot(rcov(yfit_DCC)[1,1,],type="l", axes=FALSE,xlab="",ylab="")
skala<-seq(from=1, to=(dim(dane)[1]-1),by=252)
axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])
axis(2)
plot(rcov(yfit_DCC)[2,2,],type="l", axes=FALSE,xlab="",ylab="")
skala<-seq(from=1, to=(dim(dane)[1]-1),by=252)
axis(side=1,cex.axis=0.8,at=skala,padj=1,labels=dane$Date[skala+1])
axis(2)



DCCtest(y,garchOrder = c(1,1), n.lags = 5, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)

############
#Dygresja
simulationDCC<-dccsim(yfit_DCC,n.sim=3000,n.start=1000,m.sim=1, rseed=9)
ysimDCC<-simulationDCC@msim$simX[[1]][,1:2]
DCCtest(ysimDCC, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
        solver.control = list(), cluster = NULL, Z = NULL)


acwi<-log(acwi_us[-1]/acwi_us[-length(acwi_us)])
y_resztyACWI<-matrix(0,length(acwi),2)

pom1<-lm(y[,1]~1+acwi)
y_resztyACWI[,1]<-pom1$residuals

pom2<-lm(y[,2]~1+acwi)
y_resztyACWI[,2]<-pom2$residuals


DOM: wykonać to co wcześniej dla innych par zmiennych; dodatkowo dlla AA, AXP,BE
##########################################################
14.05.2026

se=FALSE))#Dopasowanie CCC

par(mfrow=c(1,1))
plot(rcor(z_DCC_bezGlobalny)[1,2,],type="l",ylim=c(-1,1))
abline(h=z_CCC_bezGlobalny@mfit$Rt[1,2],col="red")
z_DCC_bezGlobalny@mfit$matcoef#

z_DCC_bezGlobalny <- dccfit(spec_dcc, data = y_resztyACWI, fit.control = list(eval.se=TRUE),fit = multf, cluster = NULL)#dopasowanie DCC
z_CCC_bezGlobalny <- cgarchfit(spec1, data = y_resztyACWI, cluster = NULL, fit.control = list(eval.
                                                                                              
                                                                                              DCCtest(y_resztyACWI, garchOrder = c(1,1), n.lags = 2, solver = "solnp",
                                                                                                      solver.control = list(), cluster = NULL, Z = NULL)
                                                                                              
                                                                                              DCCtest(y_resztyACWI,garchOrder = c(1,1), n.lags = 3, solver = "solnp",solver.control = list(), cluster = NULL, Z = z_CCC_bezGlobalny@mfit$stdresid)
                                                                                              DCCtest(y_resztyACWI,garchOrder = c(1,1), n.lags = 6, solver = "solnp",solver.control = list(), cluster = NULL, Z = NULL)
                                                                                              DCCtest(y_resztyACWI,garchOrder = c(3,3), n.lags = 6, solver = "nlminb",solver.control = list(), cluster = NULL, Z = NULL)
                                                                                              