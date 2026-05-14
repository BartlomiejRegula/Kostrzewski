dane1<- read.csv2(file="./Dane/pkn_20152021.csv",dec=".",sep=",")

dane1<-as.data.frame(dane1[,c(1,5)])
names(dane1)[2]<-"pkn"
dane2<-read.csv(file="./Dane/kgh_20152021.csv", sep=",", dec=".")
dane2<-as.data.frame(dane2[,c(1,5)])
names(dane2)[2]<-"kgh"

dane3<-read.csv(file="./Dane/pko_d.csv", dec=".")
dane3<-as.data.frame(dane3[,c(1,5)])
names(dane3)[2]<-"PKOBP"
