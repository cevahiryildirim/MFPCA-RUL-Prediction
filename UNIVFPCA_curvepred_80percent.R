

# load package
library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(devtools)
library(philentropy)




##########################################################################################################
#######################      Our Data      ###############################################################
##########################################################################################################


argvalsforsmooth<-read.csv("C:/Users/cevah/Desktop/data_for_registration/intervals-for-smoothing.csv", header = FALSE)
argvalsforsmooth[1,1] <- 0
argvalsforsmooth<-as.matrix(argvalsforsmooth)
argvalsforsmooth<-as.numeric(argvalsforsmooth)
argvalsforsmooth<-matrix(argvalsforsmooth,362,100)
engineheader <- seq(c(1:100))
colnames(argvalsforsmooth) <- engineheader
argvalsforsmooth




T24traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/1-T24.csv", header = FALSE)
T24traindata[1,1] <- 641.82
T24traindata<-as.matrix(T24traindata)
T24traindata<-as.numeric(T24traindata)
T24traindata<-matrix(T24traindata,100,362)
colnames(T24traindata) <- newheaders
T24traindata

T30traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/2-T30.csv", header = FALSE)
T30traindata[1,1] <- 1589.7
T30traindata<-as.matrix(T30traindata)
T30traindata<-as.numeric(T30traindata)
T30traindata<-matrix(T30traindata,100,362)
colnames(T30traindata) <- newheaders
T30traindata

T50traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/3-T50.csv", header = FALSE)
T50traindata[1,1] <- 1400.6
T50traindata<-as.matrix(T50traindata)
T50traindata<-as.numeric(T50traindata)
T50traindata<-matrix(T50traindata,100,362)
colnames(T50traindata) <- newheaders
T50traindata

P30traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/4-P30.csv", header = FALSE)
P30traindata[1,1] <- 554.36
P30traindata<-as.matrix(P30traindata)
P30traindata<-as.numeric(P30traindata)
P30traindata<-matrix(P30traindata,100,362)
colnames(P30traindata) <- newheaders
P30traindata

ps30traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/5-ps30.csv", header = FALSE)
ps30traindata[1,1] <- 47.47
ps30traindata<-as.matrix(ps30traindata)
ps30traindata<-as.numeric(ps30traindata)
ps30traindata<-matrix(ps30traindata,100,362)
colnames(ps30traindata) <- newheaders
ps30traindata

phitraindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/6-phi.csv", header = FALSE)
phitraindata[1,1] <- 521.66
phitraindata<-as.matrix(phitraindata)
phitraindata<-as.numeric(phitraindata)
phitraindata<-matrix(phitraindata,100,362)
colnames(phitraindata) <- newheaders
phitraindata

BPRtraindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/7-BPR.csv", header = FALSE)
BPRtraindata[1,1] <- 8.4195
BPRtraindata<-as.matrix(BPRtraindata)
BPRtraindata<-as.numeric(BPRtraindata)
BPRtraindata<-matrix(BPRtraindata,100,362)
colnames(BPRtraindata) <- newheaders
BPRtraindata

W31traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/8-W31.csv", header = FALSE)
W31traindata[1,1] <- 39.06
W31traindata<-as.matrix(W31traindata)
W31traindata<-as.numeric(W31traindata)
W31traindata<-matrix(W31traindata,100,362)
colnames(W31traindata) <- newheaders
W31traindata

W32traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/9-W32.csv", header = FALSE)
W32traindata[1,1] <- 23.419
W32traindata<-as.matrix(W32traindata)
W32traindata<-as.numeric(W32traindata)
W32traindata<-matrix(W32traindata,100,362)
colnames(W32traindata) <- newheaders
W32traindata



T24traindata
tT24traindata <-  t(T24traindata)
dim(tT24traindata)
T30traindata
tT30traindata <-  t(T30traindata)
dim(tT30traindata)
T50traindata
tT50traindata <-  t(T50traindata)
dim(tT50traindata)
P30traindata
tP30traindata <-  t(P30traindata)
dim(tP30traindata)
ps30traindata
tps30traindata <-  t(ps30traindata)
dim(tps30traindata)
phitraindata
tphitraindata <-  t(phitraindata)
dim(tphitraindata)
BPRtraindata
tBPRtraindata <-  t(BPRtraindata)
dim(tBPRtraindata)
W31traindata
tW31traindata <-  t(W31traindata)
dim(tW31traindata)
W32traindata
tW32traindata <-  t(W32traindata)
dim(tW32traindata)



plot(argvalsforsmooth,tT24traindata, 
     ylab="T30 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tT30traindata, 
     ylab="T30 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tT50traindata,
     ylab="T50 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tP30traindata,
     ylab="P30 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tps30traindata,
     ylab="ps30 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tphitraindata,
     ylab="phi Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tBPRtraindata,
     ylab="BPR Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tW31traindata,
     ylab="W31 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(argvalsforsmooth,tW32traindata,
     ylab="W32 Sensor Value", 
     xlab="All 100 Engines observations registered between [0-1] interval")

dim(argvalsforsmooth)
is.numeric(argvalsforsmooth)

bsplinebasis2<- create.bspline.basis(c(0,1), 8)
bsplinebasis2
plot(bsplinebasis2)
bsplinebasis

smoothallT24 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT24traindata[,i])))
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT24traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallT30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT30traindata[,i])))
  smoothallT30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT30traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallT50 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT50traindata[,i])))
  smoothallT50[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT50traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallP30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tP30traindata[,i])))
  smoothallP30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tP30traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallps30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tps30traindata[,i])))
  smoothallps30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tps30traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallphi <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tphitraindata[,i])))
  smoothallphi[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tphitraindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallBPR <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tBPRtraindata[,i])))
  smoothallBPR[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tBPRtraindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW31 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW31traindata[,i])))
  smoothallW31[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW31traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW32 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW32traindata[,i])))
  smoothallW32[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW32traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}

#burada datayı fd ye çevireceğiz
fdsmoothallT24<-Data2fd(argvals = newargvals, y=smoothallT24)
fdsmoothallT24
class(fdsmoothallT24)

###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallT24$coefs<-smoothallT24
fdsmoothallT24$basis$nbasis<-8
fdsmoothallT24$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallT24$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallT24

fdsmoothallT30<-Data2fd(argvals = newargvals, y=smoothallT30)
fdsmoothallT30
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallT30$coefs<-smoothallT30
fdsmoothallT30$basis$nbasis<-8
fdsmoothallT30$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallT30$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallT30

fdsmoothallT50<-Data2fd(argvals = newargvals, y=smoothallT50)
fdsmoothallT50
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallT50$coefs<-smoothallT50
fdsmoothallT50$basis$nbasis<-8
fdsmoothallT50$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallT50$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallT50

fdsmoothallP30<-Data2fd(argvals = newargvals, y=smoothallP30)
fdsmoothallP30
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallP30$coefs<-smoothallP30
fdsmoothallP30$basis$nbasis<-8
fdsmoothallP30$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallP30$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallP30
smoothallps30
fdsmoothallps30<-Data2fd(argvals = newargvals, y=smoothallps30)
fdsmoothallps30
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallps30$coefs<-smoothallps30
fdsmoothallps30$basis$nbasis<-8
fdsmoothallps30$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallps30$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallps30

fdsmoothallphi<-Data2fd(argvals = newargvals, y=smoothallphi)
fdsmoothallphi
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallphi$coefs<-smoothallphi
fdsmoothallphi$basis$nbasis<-8
fdsmoothallphi$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallphi$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallphi

fdsmoothallBPR<-Data2fd(argvals = newargvals, y=smoothallBPR)
fdsmoothallBPR
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallBPR$coefs<-smoothallBPR
fdsmoothallBPR$basis$nbasis<-8
fdsmoothallBPR$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallBPR$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallBPR

fdsmoothallW31<-Data2fd(argvals = newargvals, y=smoothallW31)
fdsmoothallW31
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallW31$coefs<-smoothallW31
fdsmoothallW31$basis$nbasis<-8
fdsmoothallW31$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallW31$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallW31

fdsmoothallW32<-Data2fd(argvals = newargvals, y=smoothallW32)
fdsmoothallW32
###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothallW32$coefs<-smoothallW32
fdsmoothallW32$basis$nbasis<-8
fdsmoothallW32$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallW32$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallW32


plot(fdsmoothallT24,
     ylab="T24 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallT30,
     ylab="T30 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallT50,
     ylab="T50 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallP30,
     ylab="P30 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallps30,
     ylab="ps30 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallphi,
     ylab="phi / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallBPR,
     ylab="BPR / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallW31,
     ylab="W31 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")
plot(fdsmoothallW32,
     ylab="W32 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")


newargvals3<- seq(0,1, length.out=200)
funDatasmoothallT24<-fd2funData(fdobj=fdsmoothallT24,argvals =newargvals3)
funDatasmoothallT24
autoplot(funDatasmoothallT24)
class(funDatasmoothallT24)
class(registeredargvals[[1]])

###copare is ok !!
plot(fdsmoothallT24, main = "fd object")
plot(funDatasmoothallT24, main = "funData object")


funDatasmoothallT24<-fd2funData(fdobj=fdsmoothallT24,argvals =newargvals3)
funDatasmoothallT24
autoplot(funDatasmoothallT24)
class(funDatasmoothallT24)
class(registeredargvals[[1]])
funDatasmoothallT30<-fd2funData(fdsmoothallT30,argvals = newargvals3)
funDatasmoothallT30
funDatasmoothallT50<-fd2funData(fdsmoothallT50,argvals = newargvals3)
funDatasmoothallT50
funDatasmoothallP30<-fd2funData(fdsmoothallP30,argvals = newargvals3)
funDatasmoothallP30
funDatasmoothallps30<-fd2funData(fdsmoothallps30,argvals = newargvals3)
funDatasmoothallps30
funDatasmoothallphi<-fd2funData(fdsmoothallphi,argvals = newargvals3)
funDatasmoothallphi
funDatasmoothallBPR<-fd2funData(fdsmoothallBPR,argvals = newargvals3)
funDatasmoothallBPR
funDatasmoothallW31<-fd2funData(fdsmoothallW31,argvals = newargvals3)
funDatasmoothallW31
funDatasmoothallW32<-fd2funData(fdsmoothallW32,argvals = newargvals3)
funDatasmoothallW32
######3
#bsplinebasis
bsplinebasis2
fdsmoothallT24


#fdparbsplines<-fdPar(bsplinebasis)
fdparbsplines2<-fdPar(bsplinebasis2)
UniPcaT24<-pca.fd(fdsmoothallT24, nharm = 3, fdparbsplines2)
UniPcaT24varmx <- varmx.pca.fd(UniPcaT24)

UniPcaT30<-pca.fd(fdsmoothallT30, nharm = 3, fdparbsplines2)
UniPcaT30varmx <- varmx.pca.fd(UniPcaT30)

UniPcaT50<-pca.fd(fdsmoothallT50, nharm = 3, fdparbsplines2)
UniPcaT50varmx <- varmx.pca.fd(UniPcaT50)

UniPcaP30<-pca.fd(fdsmoothallP30, nharm = 3, fdparbsplines2)
UniPcaP30varmx <- varmx.pca.fd(UniPcaP30)

UniPcaps30<-pca.fd(fdsmoothallps30, nharm = 3, fdparbsplines2)
UniPcaps30varmx <- varmx.pca.fd(UniPcaps30)

UniPcaphi<-pca.fd(fdsmoothallphi, nharm = 3, fdparbsplines2)
UniPcaphivarmx <- varmx.pca.fd(UniPcaphi)

UniPcaBPR<-pca.fd(fdsmoothallBPR, nharm = 3, fdparbsplines2)
UniPcaBPRvarmx <- varmx.pca.fd(UniPcaBPR)

UniPcaW31<-pca.fd(fdsmoothallW31, nharm = 3, fdparbsplines2)
UniPcaW31varmx <- varmx.pca.fd(UniPcaW31)

UniPcaW32<-pca.fd(fdsmoothallW32, nharm = 3, fdparbsplines2)
UniPcaW32varmx <- varmx.pca.fd(UniPcaW32)


#  plot harmonics
op <- par(mfrow=c(2,3))

plot.pca.fd(UniPcaT24, cex.main=0.9)
plot.pca.fd(UniPcaT24varmx, cex.main=0.9)

plot.pca.fd(UniPcaT30, cex.main=0.9)
plot.pca.fd(UniPcaT30varmx, cex.main=0.9)

plot.pca.fd(UniPcaT50, cex.main=0.9)
plot.pca.fd(UniPcaT50varmx, cex.main=0.9)

plot.pca.fd(UniPcaP30, cex.main=0.9)
plot.pca.fd(UniPcaP30varmx, cex.main=0.9)

plot.pca.fd(UniPcaps30, cex.main=0.9)
plot.pca.fd(UniPcaps30varmx, cex.main=0.9)

plot.pca.fd(UniPcaphi, cex.main=0.9)
plot.pca.fd(UniPcaphivarmx, cex.main=0.9)

plot.pca.fd(UniPcaBPR, cex.main=0.9)
plot.pca.fd(UniPcaBPRvarmx, cex.main=0.9)


plot.pca.fd(UniPcaW31, cex.main=0.9)
plot.pca.fd(UniPcaW31varmx, cex.main=0.9)

plot.pca.fd(UniPcaW32, cex.main=0.9)
plot.pca.fd(UniPcaW32varmx, cex.main=0.9)
par(op)


par(mfrow=c(1,1))
plot(UniPcaT24$harmonics)
plot(UniPcaT24varmx$harmonics)


par(mfrow=c(1,1))
plot(UniPcaT24$scores[,1], UniPcaT24$scores[,2])


uniFPCAT24scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAT30scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAT50scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAP30scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAps30scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAphiscores<-BsplineRegisteredsmoothedMFPCA
uniFPCABPRscores<-BsplineRegisteredsmoothedMFPCA
uniFPCAW31scores<-BsplineRegisteredsmoothedMFPCA
uniFPCAW32scores<-BsplineRegisteredsmoothedMFPCA

uniFPCAT24scores$scores<-UniPcaT24$scores
uniFPCAT30scores$scores<-UniPcaT30$scores
uniFPCAT50scores$scores<-UniPcaT50$scores
uniFPCAP30scores$scores<-UniPcaP30$scores
uniFPCAps30scores$scores<-UniPcaps30$scores
uniFPCAphiscores$scores<-UniPcaphi$scores
uniFPCABPRscores$scores<-UniPcaBPR$scores
uniFPCAW31scores$scores<-UniPcaW31$scores
uniFPCAW32scores$scores<-UniPcaW32$scores

scoreplot(uniFPCAT24scores)
scoreplot(uniFPCAT30scores)
scoreplot(uniFPCAT50scores)
scoreplot(uniFPCAP30scores)
scoreplot(uniFPCAps30scores)
scoreplot(uniFPCAphiscores)
scoreplot(uniFPCABPRscores)
scoreplot(uniFPCAW31scores)
scoreplot(uniFPCAW32scores)



scoreplot(BsplineRegisteredsmoothedMFPCA)

UniPcaT24$scores[which(UniPcaT24$scores[,1]<0)]
unifpcalowscoresT24<-which(UniPcaT24$scores[,1]<0)

UniPcaT24$scores[which(UniPcaT24$scores[,1]>0)]
unifpcabigscoresT24<-which(UniPcaT24$scores[,1]>0)

############################################################
UniPcaT30$scores[which(UniPcaT30$scores[,1]<0)]
unifpcalowscoresT30<-which(UniPcaT30$scores[,1]<0)
unifpcalowscoresT30

UniPcaT30$scores[which(UniPcaT30$scores[,1]>0)]
unifpcabigscoresT30<-which(UniPcaT30$scores[,1]>0)
unifpcabigscoresT30

############################################################
UniPcaT50$scores[which(UniPcaT50$scores[,1]<0)]
unifpcalowscoresT50<-which(UniPcaT50$scores[,1]<0)
unifpcalowscoresT50

UniPcaT50$scores[which(UniPcaT50$scores[,1]>0)]
unifpcabigscoresT50<-which(UniPcaT50$scores[,1]>0)
unifpcabigscoresT50

############################################################
UniPcaP30$scores[which(UniPcaP30$scores[,1]<0)]
unifpcalowscoresP30<-which(UniPcaP30$scores[,1]<0)
unifpcalowscoresP30

UniPcaP30$scores[which(UniPcaP30$scores[,1]>0)]
unifpcabigscoresP30<-which(UniPcaP30$scores[,1]>0)
unifpcabigscoresP30

############################################################
UniPcaps30$scores[which(UniPcaps30$scores[,1]<0)]
unifpcalowscoresps30<-which(UniPcaps30$scores[,1]<0)
unifpcalowscoresps30

UniPcaps30$scores[which(UniPcaps30$scores[,1]>0)]
unifpcabigscoresps30<-which(UniPcaps30$scores[,1]>0)
unifpcabigscoresps30

############################################################
UniPcaphi$scores[which(UniPcaphi$scores[,1]<0)]
unifpcalowscoresphi<-which(UniPcaphi$scores[,1]<0)
unifpcalowscoresphi

UniPcaphi$scores[which(UniPcaphi$scores[,1]>0)]
unifpcabigscoresphi<-which(UniPcaphi$scores[,1]>0)
unifpcabigscoresphi

############################################################
UniPcaBPR$scores[which(UniPcaBPR$scores[,1]<0)]
unifpcalowscoresBPR<-which(UniPcaBPR$scores[,1]<0)
unifpcalowscoresBPR

UniPcaBPR$scores[which(UniPcaBPR$scores[,1]>0)]
unifpcabigscoresBPR<-which(UniPcaBPR$scores[,1]>0)
unifpcabigscoresBPR

############################################################
UniPcaW31$scores[which(UniPcaW31$scores[,1]<0)]
unifpcalowscoresW31<-which(UniPcaW31$scores[,1]<0)
unifpcalowscoresW31

UniPcaW31$scores[which(UniPcaW31$scores[,1]>0)]
unifpcabigscoresW31<-which(UniPcaW31$scores[,1]>0)
unifpcabigscoresW31

############################################################
UniPcaW32$scores[which(UniPcaW32$scores[,1]<0)]
unifpcalowscoresW32<-which(UniPcaW32$scores[,1]<0)
unifpcalowscoresW32

UniPcaW32$scores[which(UniPcaW32$scores[,1]>0)]
unifpcabigscoresW32<-which(UniPcaW32$scores[,1]>0)
unifpcabigscoresW32



###################T24 uniFPCA plots
plot(listeT24[[1]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(unifpcalowscoresT24)) {
  lines(listeT24[[i]], col="black" ) }
for(j in c(unifpcabigscoresT24)){
  lines(listeT24[[j]], col="red")
}

i=1
plot(funDatasmoothallT24@argvals[[1]], xlim=c(0,1), ylim=c(641.7,644))
for (i in c(unifpcalowscoresT24)) {
  lines(funDatasmoothallT24@argvals[[1]],funDatasmoothallT24@X[i,], col="black" ) }
for(j in c(unifpcabigscoresT24)){
  lines(funDatasmoothallT24@argvals[[1]],funDatasmoothallT24@X[j,], col="red")
}



###################T30 uniFPCA plots
plot(listeP30[[1]],xlim=c(0,330), ylim=c(550.5,555.2))
for (i in c(unifpcabigscoresP30)) {
  lines(listeP30[[i]], col="black" ) }
for(j in c(unifpcalowscoresP30)){
  lines(listeP30[[j]], col="red")
}


plot(funDatasmoothallP30@argvals[[1]], xlim=c(0,1),  ylim=c(550.5,555.2))
for (i in c(unifpcabigscoresP30)) {
  lines(funDatasmoothallP30@argvals[[1]],funDatasmoothallP30@X[i,], col="black" ) }
for(j in c(unifpcalowscoresP30)){
  lines(funDatasmoothallP30@argvals[[1]],funDatasmoothallP30@X[j,], col="red")
}



###################################################

unifpcabigscoresT24
unifpcabigscoresT30
unifpcabigscoresT50
unifpcabigscoresP30
unifpcabigscoresps30
unifpcabigscoresphi
unifpcabigscoresBPR
unifpcabigscoresW31
unifpcabigscoresW32

unifpcalowscoresT24
unifpcalowscoresT30
unifpcalowscoresT50
unifpcalowscoresP30
unifpcalowscoresps30
unifpcalowscoresphi
unifpcalowscoresBPR
unifpcalowscoresW31
unifpcalowscoresW32


#################################################
#################################################
#################################################
#################################################
#################################################

T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)
T24_all_train_test[120,149:185]<-NA
T24_all_train_test[131,159:197]<-NA
T24_all_train_test[134,164:204]<-NA
T24_all_train_test[135,160:199]<-NA
T24_all_train_test[142,127:157]<-NA
T24_all_train_test[149,244:304]<-NA
T24_all_train_test[168,152:188]<-NA
T24_all_train_test[176,166:206]<-NA
T24_all_train_test[181,172:214]<-NA
T24_all_train_test[182,132:163]<-NA


T30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/2-T30train_test_all.csv", header = TRUE,row.names = 1)
T30_all_train_test
T30_all_train_test<-as.matrix(T30_all_train_test)
T30_all_train_test[120,149:185]<-NA
T30_all_train_test[131,159:197]<-NA
T30_all_train_test[134,164:204]<-NA
T30_all_train_test[135,160:199]<-NA
T30_all_train_test[142,127:157]<-NA
T30_all_train_test[149,244:304]<-NA
T30_all_train_test[168,152:188]<-NA
T30_all_train_test[176,166:206]<-NA
T30_all_train_test[181,172:214]<-NA
T30_all_train_test[182,132:163]<-NA


T50_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/3-T50train_test_all.csv", header = TRUE,row.names = 1)
T50_all_train_test
T50_all_train_test<-as.matrix(T50_all_train_test)
T50_all_train_test[120,149:185]<-NA
T50_all_train_test[131,159:197]<-NA
T50_all_train_test[134,164:204]<-NA
T50_all_train_test[135,160:199]<-NA
T50_all_train_test[142,127:157]<-NA
T50_all_train_test[149,244:304]<-NA
T50_all_train_test[168,152:188]<-NA
T50_all_train_test[176,166:206]<-NA
T50_all_train_test[181,172:214]<-NA
T50_all_train_test[182,132:163]<-NA


P30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/4-P30train_test_all.csv", header = TRUE,row.names = 1)
P30_all_train_test
P30_all_train_test<-as.matrix(P30_all_train_test)
P30_all_train_test[120,149:185]<-NA
P30_all_train_test[131,159:197]<-NA
P30_all_train_test[134,164:204]<-NA
P30_all_train_test[135,160:199]<-NA
P30_all_train_test[142,127:157]<-NA
P30_all_train_test[149,244:304]<-NA
P30_all_train_test[168,152:188]<-NA
P30_all_train_test[176,166:206]<-NA
P30_all_train_test[181,172:214]<-NA
P30_all_train_test[182,132:163]<-NA


ps30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/5-ps30train_test_all.csv", header = TRUE,row.names = 1)
ps30_all_train_test
ps30_all_train_test<-as.matrix(ps30_all_train_test)
ps30_all_train_test[120,149:185]<-NA
ps30_all_train_test[131,159:197]<-NA
ps30_all_train_test[134,164:204]<-NA
ps30_all_train_test[135,160:199]<-NA
ps30_all_train_test[142,127:157]<-NA
ps30_all_train_test[149,244:304]<-NA
ps30_all_train_test[168,152:188]<-NA
ps30_all_train_test[176,166:206]<-NA
ps30_all_train_test[181,172:214]<-NA
ps30_all_train_test[182,132:163]<-NA


phi_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/6-phitrain_test_all.csv", header = TRUE,row.names = 1)
phi_all_train_test
phi_all_train_test<-as.matrix(phi_all_train_test)
phi_all_train_test[120,149:185]<-NA
phi_all_train_test[131,159:197]<-NA
phi_all_train_test[134,164:204]<-NA
phi_all_train_test[135,160:199]<-NA
phi_all_train_test[142,127:157]<-NA
phi_all_train_test[149,244:304]<-NA
phi_all_train_test[168,152:188]<-NA
phi_all_train_test[176,166:206]<-NA
phi_all_train_test[181,172:214]<-NA
phi_all_train_test[182,132:163]<-NA


BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
BPR_all_train_test
BPR_all_train_test<-as.matrix(BPR_all_train_test)
BPR_all_train_test[120,149:185]<-NA
BPR_all_train_test[131,159:197]<-NA
BPR_all_train_test[134,164:204]<-NA
BPR_all_train_test[135,160:199]<-NA
BPR_all_train_test[142,127:157]<-NA
BPR_all_train_test[149,244:304]<-NA
BPR_all_train_test[168,152:188]<-NA
BPR_all_train_test[176,166:206]<-NA
BPR_all_train_test[181,172:214]<-NA
BPR_all_train_test[182,132:163]<-NA


W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
W31_all_train_test
W31_all_train_test<-as.matrix(W31_all_train_test)
W31_all_train_test[120,149:185]<-NA
W31_all_train_test[131,159:197]<-NA
W31_all_train_test[134,164:204]<-NA
W31_all_train_test[135,160:199]<-NA
W31_all_train_test[142,127:157]<-NA
W31_all_train_test[149,244:304]<-NA
W31_all_train_test[168,152:188]<-NA
W31_all_train_test[176,166:206]<-NA
W31_all_train_test[181,172:214]<-NA
W31_all_train_test[182,132:163]<-NA


W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
W32_all_train_test
W32_all_train_test<-as.matrix(W32_all_train_test)
W32_all_train_test[120,149:185]<-NA
W32_all_train_test[131,159:197]<-NA
W32_all_train_test[134,164:204]<-NA
W32_all_train_test[135,160:199]<-NA
W32_all_train_test[142,127:157]<-NA
W32_all_train_test[149,244:304]<-NA
W32_all_train_test[168,152:188]<-NA
W32_all_train_test[176,166:206]<-NA
W32_all_train_test[181,172:214]<-NA
W32_all_train_test[182,132:163]<-NA


#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix[[i]]<-T24_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.

TrainLowScoresUniT24<-unifpcabigscoresT24
TrainBigScoresUniT24<-unifpcalowscoresT24

TrainLowScoresUniT30<-unifpcabigscoresT24
TrainBigScoresUniT30<-unifpcalowscoresT24

TrainLowScoresUniT50<-unifpcabigscoresT24
TrainBigScoresUniT50<-unifpcalowscoresT24

TrainLowScoresUniP30<-unifpcabigscoresT24
TrainBigScoresUniP30<-unifpcalowscoresT24

TrainLowScoresUnips30<-unifpcabigscoresT24
TrainBigScoresUnips30<-unifpcalowscoresT24

TrainLowScoresUniphi<-unifpcabigscoresT24
TrainBigScoresUniphi<-unifpcalowscoresT24

TrainLowScoresUniBPR<-unifpcabigscoresT24
TrainBigScoresUniBPR<-unifpcalowscoresT24

TrainLowScoresUniW31<-unifpcabigscoresT24
TrainBigScoresUniW31<-unifpcalowscoresT24

TrainLowScoresUniW32<-unifpcabigscoresT24
TrainBigScoresUniW32<-unifpcalowscoresT24

# TrainLowScoresUniT30<-unifpcabigscoresT30
# TrainBigScoresUniT30<-unifpcalowscoresT30
# 
# TrainLowScoresUniT50<-unifpcabigscoresT50
# TrainBigScoresUniT50<-unifpcalowscoresT50
# 
# TrainLowScoresUniP30<-unifpcalowscoresP30
# TrainBigScoresUniP30<-unifpcabigscoresP30
# 
# TrainLowScoresUnips30<-unifpcabigscoresps30
# TrainBigScoresUnips30<-unifpcalowscoresps30
# 
# TrainLowScoresUniphi<-unifpcalowscoresphi
# TrainBigScoresUniphi<-unifpcabigscoresphi
# 
# TrainLowScoresUniBPR<-unifpcabigscoresBPR
# TrainBigScoresUniBPR<-unifpcalowscoresBPR
# 
# TrainLowScoresUniW31<-unifpcalowscoresW31
# TrainBigScoresUniW31<-unifpcabigscoresW31
# 
# TrainLowScoresUniW32<-unifpcalowscoresW32
# TrainBigScoresUniW32<-unifpcabigscoresW32

TestLowClassEng
TestBigClassEng
i=1
##low and big matrixes
list_test_matrix_T24_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniT24)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_T24_SCORE_UNIFPCA[[i]]<-T24_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniT24)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_T24_SCORE_UNIFPCA[[i]]<-T24_test_matrix_length
}

list_test_matrix_T30_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  T30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniT30)+1), ncol=length(na.omit(T30_all_train_test[100+i,])))
  list_test_matrix_T30_SCORE_UNIFPCA[[i]]<-T30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniT30)+1), ncol=length(na.omit(T30_all_train_test[100+i,])))
  list_test_matrix_T30_SCORE_UNIFPCA[[i]]<-T30_test_matrix_length
}

list_test_matrix_T50_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  T50_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniT50)+1), ncol=length(na.omit(T50_all_train_test[100+i,])))
  list_test_matrix_T50_SCORE_UNIFPCA[[i]]<-T50_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T50_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniT50)+1), ncol=length(na.omit(T50_all_train_test[100+i,])))
  list_test_matrix_T50_SCORE_UNIFPCA[[i]]<-T50_test_matrix_length
}

list_test_matrix_P30_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  P30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniP30)+1), ncol=length(na.omit(P30_all_train_test[100+i,])))
  list_test_matrix_P30_SCORE_UNIFPCA[[i]]<-P30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  P30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniP30)+1), ncol=length(na.omit(P30_all_train_test[100+i,])))
  list_test_matrix_P30_SCORE_UNIFPCA[[i]]<-P30_test_matrix_length
}

list_test_matrix_ps30_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  ps30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUnips30)+1), ncol=length(na.omit(ps30_all_train_test[100+i,])))
  list_test_matrix_ps30_SCORE_UNIFPCA[[i]]<-ps30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  ps30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUnips30)+1), ncol=length(na.omit(ps30_all_train_test[100+i,])))
  list_test_matrix_ps30_SCORE_UNIFPCA[[i]]<-ps30_test_matrix_length
}

list_test_matrix_phi_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  phi_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniphi)+1), ncol=length(na.omit(phi_all_train_test[100+i,])))
  list_test_matrix_phi_SCORE_UNIFPCA[[i]]<-phi_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  phi_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniphi)+1), ncol=length(na.omit(phi_all_train_test[100+i,])))
  list_test_matrix_phi_SCORE_UNIFPCA[[i]]<-phi_test_matrix_length
}

list_test_matrix_BPR_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniBPR)+1), ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_matrix_BPR_SCORE_UNIFPCA[[i]]<-BPR_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniBPR)+1), ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_matrix_BPR_SCORE_UNIFPCA[[i]]<-BPR_test_matrix_length
}

list_test_matrix_W31_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniW31)+1), ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_matrix_W31_SCORE_UNIFPCA[[i]]<-W31_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniW31)+1), ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_matrix_W31_SCORE_UNIFPCA[[i]]<-W31_test_matrix_length
}

list_test_matrix_W32_SCORE_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScoresUniW32)+1), ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_matrix_W32_SCORE_UNIFPCA[[i]]<-W32_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScoresUniW32)+1), ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_matrix_W32_SCORE_UNIFPCA[[i]]<-W32_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T24_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniT24)+1), ncol=length(as.matrix(list_test_matrix_T24_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniT24)+1)) {
    T24_test_matrix[j,]<-T24_all_train_test[TrainLowScoresUniT24[j],1:length(as.matrix(list_test_matrix_T24_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T24_SCORE_UNIFPCA[[i]][j,]<-T24_test_matrix[j,]
  }
  list_test_matrix_T24_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT24)+1),]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T24_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniT24)+1), ncol=length(as.matrix(list_test_matrix_T24_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniT24)+1)) {
    T24_test_matrix[j,]<-T24_all_train_test[TrainBigScoresUniT24[j],1:length(as.matrix(list_test_matrix_T24_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T24_SCORE_UNIFPCA[[i]][j,]<-T24_test_matrix[j,]
  }
  list_test_matrix_T24_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT24)+1),]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniT30)+1), ncol=length(as.matrix(list_test_matrix_T30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniT30)+1)) {
    T30_test_matrix[j,]<-T30_all_train_test[TrainLowScoresUniT30[j],1:length(as.matrix(list_test_matrix_T30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T30_SCORE_UNIFPCA[[i]][j,]<-T30_test_matrix[j,]
  }
  list_test_matrix_T30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT30)+1),]<-T30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniT30)+1), ncol=length(as.matrix(list_test_matrix_T30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniT30)+1)) {
    T30_test_matrix[j,]<-T30_all_train_test[TrainBigScoresUniT30[j],1:length(as.matrix(list_test_matrix_T30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T30_SCORE_UNIFPCA[[i]][j,]<-T30_test_matrix[j,]
  }
  list_test_matrix_T30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT30)+1),]<-T30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T50_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniT50)+1), ncol=length(as.matrix(list_test_matrix_T50_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniT50)+1)) {
    T50_test_matrix[j,]<-T50_all_train_test[TrainLowScoresUniT50[j],1:length(as.matrix(list_test_matrix_T50_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T50_SCORE_UNIFPCA[[i]][j,]<-T50_test_matrix[j,]
  }
  list_test_matrix_T50_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT50)+1),]<-T50_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T50_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniT50)+1), ncol=length(as.matrix(list_test_matrix_T50_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniT50)+1)) {
    T50_test_matrix[j,]<-T50_all_train_test[TrainBigScoresUniT50[j],1:length(as.matrix(list_test_matrix_T50_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_T50_SCORE_UNIFPCA[[i]][j,]<-T50_test_matrix[j,]
  }
  list_test_matrix_T50_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT50)+1),]<-T50_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  P30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniP30)+1), ncol=length(as.matrix(list_test_matrix_P30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniP30)+1)) {
    P30_test_matrix[j,]<-P30_all_train_test[TrainLowScoresUniP30[j],1:length(as.matrix(list_test_matrix_P30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_P30_SCORE_UNIFPCA[[i]][j,]<-P30_test_matrix[j,]
  }
  list_test_matrix_P30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniP30)+1),]<-P30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  P30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniP30)+1), ncol=length(as.matrix(list_test_matrix_P30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniP30)+1)) {
    P30_test_matrix[j,]<-P30_all_train_test[TrainBigScoresUniP30[j],1:length(as.matrix(list_test_matrix_P30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_P30_SCORE_UNIFPCA[[i]][j,]<-P30_test_matrix[j,]
  }
  list_test_matrix_P30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniP30)+1),]<-P30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  ps30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUnips30)+1), ncol=length(as.matrix(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUnips30)+1)) {
    ps30_test_matrix[j,]<-ps30_all_train_test[TrainLowScoresUnips30[j],1:length(as.matrix(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_ps30_SCORE_UNIFPCA[[i]][j,]<-ps30_test_matrix[j,]
  }
  list_test_matrix_ps30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUnips30)+1),]<-ps30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  ps30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUnips30)+1), ncol=length(as.matrix(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUnips30)+1)) {
    ps30_test_matrix[j,]<-ps30_all_train_test[TrainBigScoresUnips30[j],1:length(as.matrix(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_ps30_SCORE_UNIFPCA[[i]][j,]<-ps30_test_matrix[j,]
  }
  list_test_matrix_ps30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUnips30)+1),]<-ps30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  phi_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniphi)+1), ncol=length(as.matrix(list_test_matrix_phi_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniphi)+1)) {
    phi_test_matrix[j,]<-phi_all_train_test[TrainLowScoresUniphi[j],1:length(as.matrix(list_test_matrix_phi_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_phi_SCORE_UNIFPCA[[i]][j,]<-phi_test_matrix[j,]
  }
  list_test_matrix_phi_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniphi)+1),]<-phi_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  phi_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniphi)+1), ncol=length(as.matrix(list_test_matrix_phi_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniphi)+1)) {
    phi_test_matrix[j,]<-phi_all_train_test[TrainBigScoresUniphi[j],1:length(as.matrix(list_test_matrix_phi_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_phi_SCORE_UNIFPCA[[i]][j,]<-phi_test_matrix[j,]
  }
  list_test_matrix_phi_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniphi)+1),]<-phi_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  BPR_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniBPR)+1), ncol=length(as.matrix(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniBPR)+1)) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainLowScoresUniBPR[j],1:length(as.matrix(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_BPR_SCORE_UNIFPCA[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniBPR)+1),]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  BPR_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniBPR)+1), ncol=length(as.matrix(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniBPR)+1)) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainBigScoresUniBPR[j],1:length(as.matrix(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_BPR_SCORE_UNIFPCA[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniBPR)+1),]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  W31_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniW31)+1), ncol=length(as.matrix(list_test_matrix_W31_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniW31)+1)) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainLowScoresUniW31[j],1:length(as.matrix(list_test_matrix_W31_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_W31_SCORE_UNIFPCA[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniW31)+1),]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W31_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniW31)+1), ncol=length(as.matrix(list_test_matrix_W31_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniW31)+1)) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainBigScoresUniW31[j],1:length(as.matrix(list_test_matrix_W31_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_W31_SCORE_UNIFPCA[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniW31)+1),]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  W32_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScoresUniW32)+1), ncol=length(as.matrix(list_test_matrix_W32_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainLowScoresUniW32)+1)) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainLowScoresUniW32[j],1:length(as.matrix(list_test_matrix_W32_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_W32_SCORE_UNIFPCA[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniW32)+1),]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W32_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScoresUniW32)+1), ncol=length(as.matrix(list_test_matrix_W32_SCORE_UNIFPCA[[i]])[1,]))
  for (j in 1:(length(TrainBigScoresUniW32)+1)) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainBigScoresUniW32[j],1:length(as.matrix(list_test_matrix_W32_SCORE_UNIFPCA[[i]])[1,])]
    list_test_matrix_W32_SCORE_UNIFPCA[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniW32)+1),]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}


test3<-list_test_matrix_T24_SCORE_UNIFPCA[[4]]
test3<-list_test_matrix_T24_SCORE_UNIFPCA[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_T24_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=(length(TrainLowScoresUniT24)+1), ncol=length(na.omit(list_test_matrix_T24_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT24)+1),])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_T24_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T24_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T24
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=(length(TrainBigScoresUniT24)+1), ncol=length(na.omit(list_test_matrix_T24_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT24)+1),])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_T24_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T24_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T24
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_T24_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_T30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T30 <- matrix(data = NA, nrow=(length(TrainLowScoresUniT30)+1), ncol=length(na.omit(list_test_matrix_T30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT30)+1),])))
  noNA_test_matrix_T30 <-na.omit(list_test_matrix_T30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T30 <- matrix(data = NA, nrow=(length(TrainBigScoresUniT30)+1), ncol=length(na.omit(list_test_matrix_T30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT30)+1),])))
  noNA_test_matrix_T30 <-na.omit(list_test_matrix_T30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T30
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_T30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_T50_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T50 <- matrix(data = NA, nrow=(length(TrainLowScoresUniT50)+1), ncol=length(na.omit(list_test_matrix_T50_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniT50)+1),])))
  noNA_test_matrix_T50 <-na.omit(list_test_matrix_T50_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T50_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T50
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T50 <- matrix(data = NA, nrow=(length(TrainBigScoresUniT50)+1), ncol=length(na.omit(list_test_matrix_T50_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniT50)+1),])))
  noNA_test_matrix_T50 <-na.omit(list_test_matrix_T50_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_T50_noNA_UNIFPCA[[i]]<-noNA_test_matrix_T50
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_T50_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_P30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_P30 <- matrix(data = NA, nrow=(length(TrainLowScoresUniP30)+1), ncol=length(na.omit(list_test_matrix_P30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniP30)+1),])))
  noNA_test_matrix_P30 <-na.omit(list_test_matrix_P30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_P30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_P30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_P30 <- matrix(data = NA, nrow=(length(TrainBigScoresUniP30)+1), ncol=length(na.omit(list_test_matrix_P30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniP30)+1),])))
  noNA_test_matrix_P30 <-na.omit(list_test_matrix_P30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_P30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_P30
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_P30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_ps30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_ps30 <- matrix(data = NA, nrow=(length(TrainLowScoresUnips30)+1), ncol=length(na.omit(list_test_matrix_ps30_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUnips30)+1),])))
  noNA_test_matrix_ps30 <-na.omit(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_ps30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_ps30 <- matrix(data = NA, nrow=(length(TrainBigScoresUnips30)+1), ncol=length(na.omit(list_test_matrix_ps30_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUnips30)+1),])))
  noNA_test_matrix_ps30 <-na.omit(list_test_matrix_ps30_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[i]]<-noNA_test_matrix_ps30
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_phi_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_phi <- matrix(data = NA, nrow=(length(TrainLowScoresUniphi)+1), ncol=length(na.omit(list_test_matrix_phi_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniphi)+1),])))
  noNA_test_matrix_phi <-na.omit(list_test_matrix_phi_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_phi_noNA_UNIFPCA[[i]]<-noNA_test_matrix_phi
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_phi <- matrix(data = NA, nrow=(length(TrainBigScoresUniphi)+1), ncol=length(na.omit(list_test_matrix_phi_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniphi)+1),])))
  noNA_test_matrix_phi <-na.omit(list_test_matrix_phi_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_phi_noNA_UNIFPCA[[i]]<-noNA_test_matrix_phi
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_phi_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_BPR_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=(length(TrainLowScoresUniBPR)+1), ncol=length(na.omit(list_test_matrix_BPR_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniBPR)+1),])))
  noNA_test_matrix_BPR <-na.omit(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[i]]<-noNA_test_matrix_BPR
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=(length(TrainBigScoresUniBPR)+1), ncol=length(na.omit(list_test_matrix_BPR_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniBPR)+1),])))
  noNA_test_matrix_BPR <-na.omit(list_test_matrix_BPR_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[i]]<-noNA_test_matrix_BPR
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_W31_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=(length(TrainLowScoresUniW31)+1), ncol=length(na.omit(list_test_matrix_W31_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniW31)+1),])))
  noNA_test_matrix_W31 <-na.omit(list_test_matrix_W31_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]]<-noNA_test_matrix_W31
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=(length(TrainBigScoresUniW31)+1), ncol=length(na.omit(list_test_matrix_W31_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniW31)+1),])))
  noNA_test_matrix_W31 <-na.omit(list_test_matrix_W31_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]]<-noNA_test_matrix_W31
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_W31_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]])
}

#### ignore NAs for all
#####list with ignoring train data with observation less than test data

list_test_matrix_SCORE_W32_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=(length(TrainLowScoresUniW32)+1), ncol=length(na.omit(list_test_matrix_W32_SCORE_UNIFPCA[[i]][(length(TrainLowScoresUniW32)+1),])))
  noNA_test_matrix_W32 <-na.omit(list_test_matrix_W32_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_W32_noNA_UNIFPCA[[i]]<-noNA_test_matrix_W32
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=(length(TrainBigScoresUniW32)+1), ncol=length(na.omit(list_test_matrix_W32_SCORE_UNIFPCA[[i]][(length(TrainBigScoresUniW32)+1),])))
  noNA_test_matrix_W32 <-na.omit(list_test_matrix_W32_SCORE_UNIFPCA[[i]])
  list_test_matrix_SCORE_W32_noNA_UNIFPCA[[i]]<-noNA_test_matrix_W32
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_W32_noNA_UNIFPCA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[i]]<-t(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[i]])
}

######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_T24_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T24_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T24_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_T24_Big_scores_UNIFPCA
}


list_test_all_smooth_T24_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T24_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T24_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_T24_Low_scores_UNIFPCA
}


######smoothing for each engine
list_test_all_smooth_T30_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T30_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T30_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T30_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_T30_Big_scores_UNIFPCA
}


list_test_all_smooth_T30_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T30_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T30_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T30_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_T30_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_T50_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T50_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T50_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T50_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_T50_Big_scores_UNIFPCA
}


list_test_all_smooth_T50_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T50_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T50_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_T50_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_T50_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_P30_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_P30_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_P30_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_P30_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_P30_Big_scores_UNIFPCA
}


list_test_all_smooth_P30_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_P30_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_P30_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_P30_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_P30_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_ps30_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_ps30_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_ps30_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_ps30_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_ps30_Big_scores_UNIFPCA
}


list_test_all_smooth_ps30_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_ps30_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_ps30_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_ps30_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_ps30_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_phi_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_phi_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_phi_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_phi_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_phi_Big_scores_UNIFPCA
}


list_test_all_smooth_phi_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_phi_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_phi_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_phi_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_phi_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_BPR_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_BPR_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_BPR_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_BPR_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_BPR_Big_scores_UNIFPCA
}


list_test_all_smooth_BPR_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_BPR_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_BPR_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_BPR_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_BPR_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_W31_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_W31_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W31_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_W31_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_W31_Big_scores_UNIFPCA
}


list_test_all_smooth_W31_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_W31_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W31_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_W31_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_W31_Low_scores_UNIFPCA
}

######smoothing for each engine
list_test_all_smooth_W32_Big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_W32_Big_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W32_Big_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_W32_Big_scores_UNIFPCA[[j]]<-list_test_smoothing_W32_Big_scores_UNIFPCA
}


list_test_all_smooth_W32_Low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_W32_Low_scores_UNIFPCA<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA_UNIFPCA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W32_Low_scores_UNIFPCA[[i]]<-Smooth
  }
  list_test_all_smooth_W32_Low_scores_UNIFPCA[[j]]<-list_test_smoothing_W32_Low_scores_UNIFPCA
}



TestLowClassEng
TestBigClassEng
###burası BİG VE LOW lık smoothlar
###plot for cutted at observation
testengine=7
testengine=8

#lows cutted
plot(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]]))
{
  lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[i]], col="black")
}

lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]]))
{
  lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[testengine]])]], col="red")


#lows cutted DECREASİNG
plot(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]]))
{
  lines(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[i]], col="black")
}

lines(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]]))
{
  lines(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in as.vector(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]][,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in as.vector(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]][,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")




j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

FUNDATAT24LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAT24LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T24_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T24_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT24LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT24LIST_UNIFPCA[[j]]<-FUNDATAT24LIST_UNIFPCA_LIST
}

FUNDATAT24LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT24LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T24_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T24_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT24LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT24LIST_UNIFPCA_BIG[[j]]<-FUNDATAT24LIST_UNIFPCA_LIST_BIG
}


FUNDATAT30LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAT30LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T30_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T30_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT30LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT30LIST_UNIFPCA[[j]]<-FUNDATAT30LIST_UNIFPCA_LIST
}

FUNDATAT30LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT30LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T30_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T30_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT30LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT30LIST_UNIFPCA_BIG[[j]]<-FUNDATAT30LIST_UNIFPCA_LIST_BIG
}

FUNDATAT50LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAT50LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T50_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T50_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT50LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT50LIST_UNIFPCA[[j]]<-FUNDATAT50LIST_UNIFPCA_LIST
}

FUNDATAT50LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT50LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T50_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T50_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT50LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT50LIST_UNIFPCA_BIG[[j]]<-FUNDATAT50LIST_UNIFPCA_LIST_BIG
}

FUNDATAP30LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAP30LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_P30_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_P30_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAP30LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAP30LIST_UNIFPCA[[j]]<-FUNDATAP30LIST_UNIFPCA_LIST
}

FUNDATAP30LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAP30LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_P30_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_P30_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAP30LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAP30LIST_UNIFPCA_BIG[[j]]<-FUNDATAP30LIST_UNIFPCA_LIST_BIG
}

FUNDATAps30LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAps30LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_ps30_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_ps30_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAps30LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAps30LIST_UNIFPCA[[j]]<-FUNDATAps30LIST_UNIFPCA_LIST
}

FUNDATAps30LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAps30LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_ps30_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_ps30_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAps30LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAps30LIST_UNIFPCA_BIG[[j]]<-FUNDATAps30LIST_UNIFPCA_LIST_BIG
}

FUNDATAphiLIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAphiLIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_phi_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_phi_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAphiLIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAphiLIST_UNIFPCA[[j]]<-FUNDATAphiLIST_UNIFPCA_LIST
}

FUNDATAphiLIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAphiLIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_phi_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_phi_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAphiLIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAphiLIST_UNIFPCA_BIG[[j]]<-FUNDATAphiLIST_UNIFPCA_LIST_BIG
}

FUNDATABPRLIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATABPRLIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_BPR_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_BPR_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATABPRLIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATABPRLIST_UNIFPCA[[j]]<-FUNDATABPRLIST_UNIFPCA_LIST
}

FUNDATABPRLIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATABPRLIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_BPR_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_BPR_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATABPRLIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATABPRLIST_UNIFPCA_BIG[[j]]<-FUNDATABPRLIST_UNIFPCA_LIST_BIG
}

FUNDATAW31LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAW31LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W31_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W31_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW31LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAW31LIST_UNIFPCA[[j]]<-FUNDATAW31LIST_UNIFPCA_LIST
}

FUNDATAW31LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAW31LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W31_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W31_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW31LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAW31LIST_UNIFPCA_BIG[[j]]<-FUNDATAW31LIST_UNIFPCA_LIST_BIG
}

FUNDATAW32LIST_UNIFPCA<-list()
for (j in TestLowClassEng) {
  FUNDATAW32LIST_UNIFPCA_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W32_Low_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W32_Low_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW32LIST_UNIFPCA_LIST[[i]]<-funDatasmooth
  }
  FUNDATAW32LIST_UNIFPCA[[j]]<-FUNDATAW32LIST_UNIFPCA_LIST
}

FUNDATAW32LIST_UNIFPCA_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAW32LIST_UNIFPCA_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W32_Big_scores_UNIFPCA[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W32_Big_scores_UNIFPCA[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW32LIST_UNIFPCA_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAW32LIST_UNIFPCA_BIG[[j]]<-FUNDATAW32LIST_UNIFPCA_LIST_BIG
}


#funDatasmooth<-fd2funData(list_test_all_smooth_T24_Big_scores_UNIFPCA[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_T24_Big_scores_UNIFPCA[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(T24_all_train_test)

vectorT24<-as.vector(T24_all_train_test[101:200,2:363])
vectorT24<-na.omit(vectorT24)
mean(na.omit(vectorT24))
meanT24<-mean(na.omit(vectorT24))

vectorT30<-as.vector(T30_all_train_test[101:200,2:363])
vectorT30<-na.omit(vectorT30)
meanT30<-mean(na.omit(vectorT30))

vectorT50<-as.vector(T50_all_train_test[101:200,2:363])
vectorT50<-na.omit(vectorT50)
meanT50<-mean(na.omit(vectorT50))

vectorP30<-as.vector(P30_all_train_test[101:200,2:363])
vectorP30<-na.omit(vectorP30)
meanP30<-mean(na.omit(vectorP30))

vectorps30<-as.vector(ps30_all_train_test[101:200,2:363])
vectorps30<-na.omit(vectorps30)
meanps30<-mean(na.omit(vectorps30))

vectorphi<-as.vector(phi_all_train_test[101:200,2:363])
vectorphi<-na.omit(vectorphi)
meanphi<-mean(na.omit(vectorphi))

vectorBPR<-as.vector(BPR_all_train_test[101:200,2:363])
vectorBPR<-na.omit(vectorBPR)
meanBPR<-mean(na.omit(vectorBPR))

vectorW31<-as.vector(W31_all_train_test[101:200,2:363])
vectorW31<-na.omit(vectorW31)
meanW31<-mean(na.omit(vectorW31))

vectorW32<-as.vector(W32_all_train_test[101:200,2:363])
vectorW32<-na.omit(vectorW32)
meanW32<-mean(na.omit(vectorW32))


#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T24_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]])) {
    distancematrixT24reg[i,]<-FUNDATAT24LIST_UNIFPCA_BIG[[j]][[i]]@X/meanT24
  }
  list_for_distance_T24_reg_big_scores_UNIFPCA[[j]]<-distancematrixT24reg
}

list_for_distance_T24_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T24_noNA_UNIFPCA[[j]])) {
    distancematrixT24reg[i,]<-FUNDATAT24LIST_UNIFPCA[[j]][[i]]@X/meanT24
  }
  list_for_distance_T24_reg_low_scores_UNIFPCA[[j]]<-distancematrixT24reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T24_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T24_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T24_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_T24_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T24_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T24_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}


#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T30_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixT30reg<-matrix(NA,nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]])) {
    distancematrixT30reg[i,]<-FUNDATAT30LIST_UNIFPCA_BIG[[j]][[i]]@X/meanT30
  }
  list_for_distance_T30_reg_big_scores_UNIFPCA[[j]]<-distancematrixT30reg
}

list_for_distance_T30_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixT30reg<-matrix(NA,nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T30_noNA_UNIFPCA[[j]])) {
    distancematrixT30reg[i,]<-FUNDATAT30LIST_UNIFPCA[[j]][[i]]@X/meanT30
  }
  list_for_distance_T30_reg_low_scores_UNIFPCA[[j]]<-distancematrixT30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T30_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T30_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T30_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_T30_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T30_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T30_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T50_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixT50reg<-matrix(NA,nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]])) {
    distancematrixT50reg[i,]<-FUNDATAT50LIST_UNIFPCA_BIG[[j]][[i]]@X/meanT50
  }
  list_for_distance_T50_reg_big_scores_UNIFPCA[[j]]<-distancematrixT50reg
}

list_for_distance_T50_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixT50reg<-matrix(NA,nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T50_noNA_UNIFPCA[[j]])) {
    distancematrixT50reg[i,]<-FUNDATAT50LIST_UNIFPCA[[j]][[i]]@X/meanT50
  }
  list_for_distance_T50_reg_low_scores_UNIFPCA[[j]]<-distancematrixT50reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T50_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T50_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T50_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_T50_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T50_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_T50_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_P30_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixP30reg<-matrix(NA,nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]])) {
    distancematrixP30reg[i,]<-FUNDATAP30LIST_UNIFPCA_BIG[[j]][[i]]@X/meanP30
  }
  list_for_distance_P30_reg_big_scores_UNIFPCA[[j]]<-distancematrixP30reg
}

list_for_distance_P30_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixP30reg<-matrix(NA,nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_P30_noNA_UNIFPCA[[j]])) {
    distancematrixP30reg[i,]<-FUNDATAP30LIST_UNIFPCA[[j]][[i]]@X/meanP30
  }
  list_for_distance_P30_reg_low_scores_UNIFPCA[[j]]<-distancematrixP30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_P30_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_P30_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_P30_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_P30_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_P30_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_P30_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_ps30_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixps30reg<-matrix(NA,nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]])) {
    distancematrixps30reg[i,]<-FUNDATAps30LIST_UNIFPCA_BIG[[j]][[i]]@X/meanps30
  }
  list_for_distance_ps30_reg_big_scores_UNIFPCA[[j]]<-distancematrixps30reg
}

list_for_distance_ps30_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixps30reg<-matrix(NA,nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_ps30_noNA_UNIFPCA[[j]])) {
    distancematrixps30reg[i,]<-FUNDATAps30LIST_UNIFPCA[[j]][[i]]@X/meanps30
  }
  list_for_distance_ps30_reg_low_scores_UNIFPCA[[j]]<-distancematrixps30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_ps30_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_ps30_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_ps30_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_ps30_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_ps30_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_ps30_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_phi_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixphireg<-matrix(NA,nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]])) {
    distancematrixphireg[i,]<-FUNDATAphiLIST_UNIFPCA_BIG[[j]][[i]]@X/meanphi
  }
  list_for_distance_phi_reg_big_scores_UNIFPCA[[j]]<-distancematrixphireg
}

list_for_distance_phi_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixphireg<-matrix(NA,nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_phi_noNA_UNIFPCA[[j]])) {
    distancematrixphireg[i,]<-FUNDATAphiLIST_UNIFPCA[[j]][[i]]@X/meanphi
  }
  list_for_distance_phi_reg_low_scores_UNIFPCA[[j]]<-distancematrixphireg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_phi_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_phi_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_phi_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_phi_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_phi_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_phi_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_BPR_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixBPRreg<-matrix(NA,nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]])) {
    distancematrixBPRreg[i,]<-FUNDATABPRLIST_UNIFPCA_BIG[[j]][[i]]@X/meanBPR
  }
  list_for_distance_BPR_reg_big_scores_UNIFPCA[[j]]<-distancematrixBPRreg
}

list_for_distance_BPR_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixBPRreg<-matrix(NA,nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_BPR_noNA_UNIFPCA[[j]])) {
    distancematrixBPRreg[i,]<-FUNDATABPRLIST_UNIFPCA[[j]][[i]]@X/meanBPR
  }
  list_for_distance_BPR_reg_low_scores_UNIFPCA[[j]]<-distancematrixBPRreg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_BPR_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_BPR_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_BPR_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_BPR_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_BPR_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_BPR_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_W31_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixW31reg<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]])) {
    distancematrixW31reg[i,]<-FUNDATAW31LIST_UNIFPCA_BIG[[j]][[i]]@X/meanW31
  }
  list_for_distance_W31_reg_big_scores_UNIFPCA[[j]]<-distancematrixW31reg
}

list_for_distance_W31_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixW31reg<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[j]])) {
    distancematrixW31reg[i,]<-FUNDATAW31LIST_UNIFPCA[[j]][[i]]@X/meanW31
  }
  list_for_distance_W31_reg_low_scores_UNIFPCA[[j]]<-distancematrixW31reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_W31_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_W31_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_W31_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_W31_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_W31_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_W31_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_W32_reg_big_scores_UNIFPCA<-list()
for (j in TestBigClassEng) {
  distancematrixW32reg<-matrix(NA,nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]])) {
    distancematrixW32reg[i,]<-FUNDATAW32LIST_UNIFPCA_BIG[[j]][[i]]@X/meanW32
  }
  list_for_distance_W32_reg_big_scores_UNIFPCA[[j]]<-distancematrixW32reg
}

list_for_distance_W32_reg_low_scores_UNIFPCA<-list()
for (j in TestLowClassEng) {
  distancematrixW32reg<-matrix(NA,nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[j]])) {
    distancematrixW32reg[i,]<-FUNDATAW32LIST_UNIFPCA[[j]][[i]]@X/meanW32
  }
  list_for_distance_W32_reg_low_scores_UNIFPCA[[j]]<-distancematrixW32reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_W32_reg_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_W32_reg_low_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_W32_reg_low_scores_UNIFPCA[[i]]<-DISTANCE
}

list_distance_W32_reg_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_W32_reg_big_scores_UNIFPCA[[i]], method = "euclidean" )
  list_distance_W32_reg_big_scores_UNIFPCA[[i]]<-DISTANCE
}


list_all_distance_merged_low_scores_UNIFPCA<-list()
for (i in TestLowClassEng) {
  
  a<-list_distance_T24_reg_low_scores_UNIFPCA[[i]]+
    list_distance_T30_reg_low_scores_UNIFPCA[[i]]+
    list_distance_T50_reg_low_scores_UNIFPCA[[i]]+
    list_distance_P30_reg_low_scores_UNIFPCA[[i]]+
    list_distance_ps30_reg_low_scores_UNIFPCA[[i]]+
    list_distance_phi_reg_low_scores_UNIFPCA[[i]]+
    list_distance_BPR_reg_low_scores_UNIFPCA[[i]]+
    list_distance_W31_reg_low_scores_UNIFPCA[[i]]+
    list_distance_W32_reg_low_scores_UNIFPCA[[i]]
  list_all_distance_merged_low_scores_UNIFPCA[[i]]<-a
}

list_all_distance_merged_big_scores_UNIFPCA<-list()
for (i in TestBigClassEng) {
  
  a<-list_distance_T24_reg_big_scores_UNIFPCA[[i]]+
    list_distance_T30_reg_big_scores_UNIFPCA[[i]]+
    list_distance_T50_reg_big_scores_UNIFPCA[[i]]+
    list_distance_P30_reg_big_scores_UNIFPCA[[i]]+
    list_distance_ps30_reg_big_scores_UNIFPCA[[i]]+
    list_distance_phi_reg_big_scores_UNIFPCA[[i]]+
    list_distance_BPR_reg_big_scores_UNIFPCA[[i]]+
    list_distance_W31_reg_big_scores_UNIFPCA[[i]]+
    list_distance_W32_reg_big_scores_UNIFPCA[[i]]
  list_all_distance_merged_big_scores_UNIFPCA[[i]]<-a
}

TestBigClassEngwithout49<-TestBigClassEng
TestBigClassEngwithout49[27]<-NA
TestBigClassEngwithout49<- as.vector(na.omit(TestBigClassEngwithout49))




############## SORTED DISTANCES FOR ALL ENGINES
list_dist_all_sorted_UNIFPCA<-list()
for (i in TestLowClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_low_scores_UNIFPCA[[i]][,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted_UNIFPCA[[i]]<-DISTANCE
}



for (i in TestBigClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_big_scores_UNIFPCA[[i]][,nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted_UNIFPCA[[i]]<-DISTANCE
}



######################PLOTS

TestBigClassEng
TestLowClassEng
no_of_closest=5
testengine=93

#lows
par(mfrow=c(3,3))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
plot(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]], col="red")
}
lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="blue")
text(70, 643, "Test Engine no:70", col="blue")
text(200, 642.5, "5 closest train engines (low score group)", col="red")

plot(list_test_all_smooth_T30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_T50_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_P30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_ps30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_phi_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_BPR_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


plot(list_test_all_smooth_W31_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


testengine=81
#Bigs
par(mfrow=c(3,3))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
plot(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]])
}
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="blue")
text(60, 642.5, "Test Engine no:81", col="blue")

text(220, 642.3, "5 closest train engines (big score group)", col="black")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="blue")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted_UNIFPCA[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")



#bigs cutted
#plot(list_test_all_smooth_W32_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
#for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]]))
#{
#  lines(list_test_all_smooth_W32_Big_scores[[testengine]][[i]], col="black")
#}
#lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA_UNIFPCA[[testengine]])]], col="red")


####TRUE RULimport
RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
RUL_test
RUL_test<-as.matrix(RUL_test)

RUL_test[20,2]<-147
RUL_test[31,2]<-157
RUL_test[34,2]<-162
RUL_test[35,2]<-158
RUL_test[42,2]<-125
RUL_test[49,2]<-242
RUL_test[68,2]<-150
RUL_test[76,2]<-164
RUL_test[81,2]<-170
RUL_test[82,2]<-130

RUL_test[20,3]<-RUL_test[20,4]-RUL_test[20,2]
RUL_test[31,3]<-RUL_test[31,4]-RUL_test[24,2]
RUL_test[34,3]<-RUL_test[34,4]-RUL_test[34,2]
RUL_test[35,3]<-RUL_test[35,4]-RUL_test[35,2]
RUL_test[42,3]<-RUL_test[42,4]-RUL_test[42,2]
RUL_test[49,3]<-RUL_test[49,4]-RUL_test[49,2]
RUL_test[68,3]<-RUL_test[68,4]-RUL_test[68,2]
RUL_test[76,3]<-RUL_test[76,4]-RUL_test[76,2]
RUL_test[81,3]<-RUL_test[81,4]-RUL_test[81,2]
RUL_test[82,3]<-RUL_test[82,4]-RUL_test[82,2]


TRUE_RUL_DECREASING<-RUL_test[order(RUL_test[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING[,3])

RUL_test_bigscore<- matrix(NA,ncol=4, nrow= length(TestBigClassEng))
for (i in 1:length(TestBigClassEng)) {
  RUL_test_bigscore[i,]<- RUL_test[TestBigClassEng[i],]
}

TRUE_RUL_DECREASING_bigscore<-RUL_test_bigscore[order(RUL_test_bigscore[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING_bigscore[,3])

RUL_test_lowscore<- matrix(NA,ncol=4, nrow= length(TestLowClassEng))
for (i in 1:length(TestLowClassEng)) {
  RUL_test_lowscore[i,]<- RUL_test[TestLowClassEng[i],]
}

TRUE_RUL_DECREASING_lowscore<-RUL_test_lowscore[order(RUL_test_lowscore[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING_lowscore[,3])


####RUL Prediction

par(mfrow=c(1,1))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
no_of_closest_engine=6

LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
LIFE_TRAIN<-as.matrix(LIFE_TRAIN)
LIFE_TRAIN


list_closest_X_lowscore_UNIFPCA<-list()
for (i in TestLowClassEng ) {
  close<-list_dist_all_sorted_UNIFPCA[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_lowscore_UNIFPCA[[i]]<-close
}

#27
TestBigClassEngexcept49<-TestBigClassEng
TestBigClassEngexcept49[27]<-NA
TestBigClassEngexcept49<-na.omit(TestBigClassEngexcept49)

list_closest_X_bigcore_UNIFPCA<-list()
for (i in TestBigClassEngexcept49 ) {
  close<-list_dist_all_sorted_UNIFPCA[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_bigcore_UNIFPCA[[i]]<-close
}
list_closest_X_bigcore_UNIFPCA[[49]]<-c(96,69,92,67)



# FOR ENGINE 49 - 96,69,92,67
#1 NO OF ENGINE
#2 OBS Test
#3 RUL TEST
#4 LIFE TEST
#5 PRED LIFE
#6 PRED RUL
#7 MEAN RUL ERROR
#8 PRED LIFE MEDIAN
#9 PRED RUL MEDIAN
#9 MEDIAN RUL ERROR

RUL_PREDICTION_lowscore_UNIFPCA<- matrix(data = NA, nrow=length(TestLowClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_lowscore_UNIFPCA) <- newheaderstest
RUL_PREDICTION_lowscore_UNIFPCA[,1]<-RUL_test_lowscore[,1]
RUL_PREDICTION_lowscore_UNIFPCA[,2]<-RUL_test_lowscore[,2]
RUL_PREDICTION_lowscore_UNIFPCA[,3]<-RUL_test_lowscore[,3]
RUL_PREDICTION_lowscore_UNIFPCA[,4]<-RUL_test_lowscore[,4]
i=1
for (i in 1:length(TestLowClassEng)) {
  RUL_PREDICTION_lowscore_UNIFPCA[i,5]<-mean(LIFE_TRAIN[list_closest_X_lowscore_UNIFPCA[[TestLowClassEng[i]]],2])
  RUL_PREDICTION_lowscore_UNIFPCA[i,6]<-RUL_PREDICTION_lowscore_UNIFPCA[i,5]-RUL_PREDICTION_lowscore_UNIFPCA[i,2]
  RUL_PREDICTION_lowscore_UNIFPCA[i,7]<-RUL_PREDICTION_lowscore_UNIFPCA[i,6]-RUL_PREDICTION_lowscore_UNIFPCA[i,3]
  RUL_PREDICTION_lowscore_UNIFPCA[i,8]<-median(LIFE_TRAIN[list_closest_X_lowscore_UNIFPCA[[TestLowClassEng[i]]],2]) 
  RUL_PREDICTION_lowscore_UNIFPCA[i,9]<-RUL_PREDICTION_lowscore_UNIFPCA[i,8]-RUL_PREDICTION_lowscore_UNIFPCA[i,2]
  RUL_PREDICTION_lowscore_UNIFPCA[i,10]<-RUL_PREDICTION_lowscore_UNIFPCA[i,9]-RUL_PREDICTION_lowscore_UNIFPCA[i,3]
}

RUL_PREDICTION_bigscore_UNIFPCA<- matrix(data = NA, nrow=length(TestBigClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_bigscore_UNIFPCA) <- newheaderstest
RUL_PREDICTION_bigscore_UNIFPCA[,1]<-RUL_test_bigscore[,1]
RUL_PREDICTION_bigscore_UNIFPCA[,2]<-RUL_test_bigscore[,2]
RUL_PREDICTION_bigscore_UNIFPCA[,3]<-RUL_test_bigscore[,3]
RUL_PREDICTION_bigscore_UNIFPCA[,4]<-RUL_test_bigscore[,4]

for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_bigscore_UNIFPCA[i,5]<-mean(LIFE_TRAIN[list_closest_X_bigcore_UNIFPCA[[TestBigClassEng[i]]],2])
  RUL_PREDICTION_bigscore_UNIFPCA[i,6]<-RUL_PREDICTION_bigscore_UNIFPCA[i,5]-RUL_PREDICTION_bigscore_UNIFPCA[i,2]
  RUL_PREDICTION_bigscore_UNIFPCA[i,7]<-RUL_PREDICTION_bigscore_UNIFPCA[i,6]-RUL_PREDICTION_bigscore_UNIFPCA[i,3]
  RUL_PREDICTION_bigscore_UNIFPCA[i,8]<-median(LIFE_TRAIN[list_closest_X_bigcore_UNIFPCA[[TestBigClassEng[i]]],2]) 
  RUL_PREDICTION_bigscore_UNIFPCA[i,9]<-RUL_PREDICTION_bigscore_UNIFPCA[i,8]-RUL_PREDICTION_bigscore_UNIFPCA[i,2]
  RUL_PREDICTION_bigscore_UNIFPCA[i,10]<-RUL_PREDICTION_bigscore_UNIFPCA[i,9]-RUL_PREDICTION_bigscore_UNIFPCA[i,3]
}

TRUE_RUL_DECREASING_lowscore
ordervector_mean<- as.vector(TRUE_RUL_DECREASING_lowscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_lowscore_UNIFPCA_SORTED <- RUL_PREDICTION_lowscore_UNIFPCA[match(ordervector_mean, RUL_PREDICTION_lowscore_UNIFPCA),]##SORT increasing RUL

ordervector_mean<- as.vector(TRUE_RUL_DECREASING_bigscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_bigscore_UNIFPCA_SORTED <- RUL_PREDICTION_bigscore_UNIFPCA[match(ordervector_mean, RUL_PREDICTION_bigscore_UNIFPCA),]##SORT increasing RUL


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_lowscore_UNIFPCA_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_lowscore_UNIFPCA_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUARE))

max(RUL_PREDICTION_lowscore_UNIFPCA[,7])
min(RUL_PREDICTION_lowscore_UNIFPCA[,7])
max(RUL_PREDICTION_lowscore_UNIFPCA[,10])
min(RUL_PREDICTION_lowscore_UNIFPCA[,10])

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(0)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,7]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,7]> (0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore_UNIFPCA[,10]==(0)) )


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_bigscore_UNIFPCA_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_bigscore_UNIFPCA_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUAREMED))

max(RUL_PREDICTION_bigscore_UNIFPCA[,7])
min(RUL_PREDICTION_bigscore_UNIFPCA[,7])
max(RUL_PREDICTION_bigscore_UNIFPCA[,10])
min(RUL_PREDICTION_bigscore_UNIFPCA[,10])

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(0)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,7]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,7]> (0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore_UNIFPCA[,10]==(0)) )



RUL_PREDICTION_lowscore_UNIFPCA
RUL_PREDICTION_bigscore_UNIFPCA

total<-rbind(RUL_PREDICTION_lowscore_UNIFPCA,RUL_PREDICTION_bigscore_UNIFPCA)

ERRORSQUARE <- total[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- total[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)





############################# classificationsız


no_of_closest_engine=5

list_closest_X<-list()
for (i in 1:48) {
  close<-list_dist_all_sorted_UNIFPCA[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}
list_closest_X[[49]]<-c(96,69,92,67)
for (i in 50:100) {
  close<-list_dist_all_sorted_UNIFPCA[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}

length(TestBigClassEng)+length(TestLowClassEng)

RUL_PREDICTION<- matrix(data = NA, nrow=100, 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION) <- newheaderstest
RUL_PREDICTION[,1]<-RUL_test[,1]
RUL_PREDICTION[,2]<-RUL_test[,2]
RUL_PREDICTION[,3]<-RUL_test[,3]
RUL_PREDICTION[,4]<-RUL_test[,4]
for (i in 1:100) {
  RUL_PREDICTION[i,5]<-mean(LIFE_TRAIN[list_closest_X[[i]],2])
  RUL_PREDICTION[i,6]<-RUL_PREDICTION[i,5]-RUL_PREDICTION[i,2]
  RUL_PREDICTION[i,7]<-RUL_PREDICTION[i,6]-RUL_PREDICTION[i,3]
  RUL_PREDICTION[i,8]<-median(LIFE_TRAIN[list_closest_X[[i]],2]) 
  RUL_PREDICTION[i,9]<-RUL_PREDICTION[i,8]-RUL_PREDICTION[i,2]
  RUL_PREDICTION[i,10]<-RUL_PREDICTION[i,9]-RUL_PREDICTION[i,3]
}

ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL

###PLOT for TRUE RUL and PRED RUL
plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200),ylab="RUL", xlab="Mean - 100 total Test Engines")
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,6], col="red")
}
legend(75, -50, legend=c("Predicted RUL", "True RUL"),
       col=c("red", "black"), pch=1:1, cex=0.8)

###PLOT for TRUE RUL and PRED MEDIAN
ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL

plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200), ylab="RUL", xlab="Median - 100 total Test Engines")
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,9], col="red")
}
legend(75, -50, legend=c("Predicted RUL", "True RUL"),
       col=c("red", "black"), pch=1:1, cex=0.8)
RUL_PREDICTION


#plot(RUL_PREDICTION[,2],abs(RUL_PREDICTION[,10]))



######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)

max(RUL_PREDICTION[,7])
min(RUL_PREDICTION[,7])
max(RUL_PREDICTION[,10])
min(RUL_PREDICTION[,10])

length(which(RUL_PREDICTION[,10]< (-0.5)) )

length(which(RUL_PREDICTION[,10]> (0.5)) )

length(which(RUL_PREDICTION[,10]==(-0.5)) )
length(which(RUL_PREDICTION[,10]==(0.5)) )
length(which(RUL_PREDICTION[,10]==(0)) )

length(which(RUL_PREDICTION[,7]< (-0.5)) )

length(which(RUL_PREDICTION[,7]> (0.5)) )

length(which(RUL_PREDICTION[,10]< (-0.5)) )

length(which(RUL_PREDICTION[,10]> (0.5)) )

length(which(RUL_PREDICTION[,10]==(-0.5)) )
length(which(RUL_PREDICTION[,10]==(0.5)) )
length(which(RUL_PREDICTION[,10]==(0)) )


########################################################
########################################################

RUL_PREDICTION_lowscore_UNIFPCA
RUL_PREDICTION_bigscore_UNIFPCA
TestBigClassEngexcept49
TestLowClassEng
TestBigClassEng
####curve prediction
class(fdsmoothallT30)

RUL_PREDICTION_low_big_merged_UNIFPCA<-matrix(NA, nrow = 100, ncol=10)

for (i in 1:length(TestLowClassEng)) {
  RUL_PREDICTION_low_big_merged_UNIFPCA[TestLowClassEng[i],]<-RUL_PREDICTION_lowscore_UNIFPCA[i,]
}
for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_low_big_merged_UNIFPCA[TestBigClassEng[i],]<-RUL_PREDICTION_bigscore_UNIFPCA[i,]
}

RUL_PREDICTION_low_big_merged_UNIFPCA<-round(RUL_PREDICTION_low_big_merged_UNIFPCA,0)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_low_big_merged_UNIFPCA) <- newheaderstest


funDatasmoothallT24_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT24,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallT24_list_UNIFPCA[[i]]<-funDatasmoothallT24
}
funDatasmoothallT30_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallT30_list_UNIFPCA[[i]]<-funDatasmoothallT24
}
funDatasmoothallT50_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallT50<-fd2funData(fdsmoothallT50,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallT50_list_UNIFPCA[[i]]<-funDatasmoothallT50
}
funDatasmoothallP30_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallP30<-fd2funData(fdsmoothallP30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallP30_list_UNIFPCA[[i]]<-funDatasmoothallP30
}
funDatasmoothallps30_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallps30<-fd2funData(fdsmoothallps30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallps30_list_UNIFPCA[[i]]<-funDatasmoothallps30
}
funDatasmoothallphi_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallphi<-fd2funData(fdsmoothallphi,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallphi_list_UNIFPCA[[i]]<-funDatasmoothallphi
}
funDatasmoothallBPR_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallBPR<-fd2funData(fdsmoothallBPR,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallBPR_list_UNIFPCA[[i]]<-funDatasmoothallBPR
}
funDatasmoothallW31_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallW31<-fd2funData(fdsmoothallW31,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallW31_list_UNIFPCA[[i]]<-funDatasmoothallW31
}
funDatasmoothallW32_list_UNIFPCA<-list()
for (i in 1:100) {
  funDatasmoothallW32<-fd2funData(fdsmoothallW32,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged_UNIFPCA[i,5]))
  funDatasmoothallW32_list_UNIFPCA[[i]]<-funDatasmoothallW32
}


#curve 20'ye en yakın 10 train engine 
as.vector(list_dist_all_sorted_UNIFPCA[[20]][2:6,1])

VALUEST24<-funDatasmoothallT24@X
VALUEST30<-funDatasmoothallT30@X
VALUEST50<-funDatasmoothallT50@X
VALUESP30<-funDatasmoothallP30@X
VALUESps30<-funDatasmoothallps30@X
VALUESphi<-funDatasmoothallphi@X
VALUESBPR<-funDatasmoothallBPR@X
VALUESW31<-funDatasmoothallW31@X
VALUESW32<-funDatasmoothallW32@X

predictedcurves_UNIFPCA<-list
curve_prediction_list_UNIFPCA<-list()  
for (j in 1:48) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]))
  predictedcurve<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]))
  for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list_UNIFPCA[[j]]@X[list_dist_all_sorted_UNIFPCA[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]]
  }
  curve_prediction_list_UNIFPCA[[j]]<-CURVETAHMIN
  tahmin<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]) )  
  for (k in 1:(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2])) {
    tahmin[,k] <- mean(curve_prediction_list_UNIFPCA[[j]][,k])
  }
  predictedcurves_UNIFPCA[[j]]<- tahmin
}

k=1
j=1

for (j in 50:100) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]))
  predictedcurve<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]))
  for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list_UNIFPCA[[j]]@X[list_dist_all_sorted[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]]
  }
  curve_prediction_list_UNIFPCA[[j]]<-CURVETAHMIN
  tahmin<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2]) )  
  for (k in 1:(RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]-RUL_PREDICTION_low_big_merged_UNIFPCA[j,2])) {
    tahmin[,k]<-mean(curve_prediction_list_UNIFPCA[[j]][,k])
  }
  predictedcurves_UNIFPCA[[j]]<- tahmin
}

predictedcurves_UNIFPCA
## predicted curve list for each test engine

#T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
#T24_all_train_test
#T24_all_train_test<-as.matrix(T24_all_train_test)

T24_TEST_OBSERVED_UNIFPCA<-T24_all_train_test[101:200, 2:363]
dim(T24_TEST_OBSERVED_UNIFPCA)

T24_TEST_PREDICTED_UNIFPCA<-list()
for (i in 1:48) {
  T24_TEST_PREDICTED_UNIFPCA[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED_UNIFPCA[i,]))),as.matrix(predictedcurves_UNIFPCA[[i]])) 
}
for (i in 50:100) {
  T24_TEST_PREDICTED_UNIFPCA[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED_UNIFPCA[i,]))),as.matrix(predictedcurves_UNIFPCA[[i]])) 
}



bsplinebasis2<- create.bspline.basis(c(0,1), 8)
bsplinebasis2
plot(bsplinebasis2)
bsplinebasis
################# separately smoothing and merging.############

#A<-smooth.basis(ENGINEallArgvals,as.vector(na.omit(K[,i])),bsplinebasis2)
#ENGINEallArgvals
#K


####buradaki for ile her bir datayı ayrı ayrı bspile ile smoot yapıyoruz
smoothallT24 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(K[,i])))
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(K[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}


predictedcurves_smooth_matrix_T24_UNIFPCA<-matrix(data = NA, nrow=8, ncol=100)

predictedcurves_smooth_list_T24_UNIFPCA<-list()
for (j in 1:48) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]),
                         y= as.vector(T24_TEST_PREDICTED_UNIFPCA[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]),no_of_splines))
  predictedcurves_smooth_list_T24_UNIFPCA[[j]]<-Smooth
}
for (j in 50:100) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]),
                         y= as.vector(T24_TEST_PREDICTED_UNIFPCA[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged_UNIFPCA[j,5]),no_of_splines))
  predictedcurves_smooth_list_T24_UNIFPCA[[j]]<-Smooth
}

TestLowClassEng
TestBigClassEngexcept49
#10,
testengine=20
T24_TEST_PREDICTED_UNIFPCA[[testengine]]
plot(predictedcurves_smooth_list_T24_UNIFPCA[[20]] , xlim=c(0,370), ylim=c(641.7,644), col="red")
for (i in TestLowClassEng) {
  lines(predictedcurves_smooth_list_T24_UNIFPCA[[i]],col="red")
}
for (i in TestBigClassEngexcept49) {
  lines(predictedcurves_smooth_list_T24_UNIFPCA[[i]], col="black")
}
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")


T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)


TestBigClassEng
testengine=20
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

testengine=31
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

testengine=34
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

testengine=35
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

testengine=42
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

testengine=49
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")


testengine=68
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")


testengine=76
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")


testengine=81
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")


testengine=82
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24_UNIFPCA[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores_UNIFPCA[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA_UNIFPCA[[testengine]])]], col="blue")

T24_all_train_test[182,]

T24_all_train_test[(testengine+100),(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]]

RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]

########################################
###tahmin edilen %90 a kadar olan aralık
c((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5])
true90percent_PRED_T24<-T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,5]+1)]
true90percent_PRED_T24<-na.omit(true90percent_PRED_T24)
length(true90percent_PRED_T24)
true90percent_PRED_T24<-as.vector(true90percent_PRED_T24)
true90percent_PRED_T24

RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]

predicted90percent_PRED_T24<-predictedcurves_smooth_list_T24_UNIFPCA[[testengine]]$y[(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+1):(RUL_PREDICTION_low_big_merged_UNIFPCA[testengine,2]+length(true90percent_PRED_T24))]
predicted90percent_PRED_T24

ERRORcurve<-predicted90percent_PRED_T24-true90percent_PRED_T24
ERRORSQ<-ERRORcurve^2
SUMERRORSQ<-sum(ERRORSQ)
SUMERRORSQmean<-SUMERRORSQ/length(predicted90percent_PRED_T24)
sqrt(SUMERRORSQmean)

