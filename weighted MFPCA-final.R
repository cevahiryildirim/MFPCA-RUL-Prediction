

# load package
library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(devtools)



########################################################################################################
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

newheaders2 <- c(1:100)
K <-  t(T24traindata)
colnames(K) <- newheaders2
K <- as.matrix(K)
K
class(K)
dim(K)
str(K)
ncol(K)

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
dim(K)
is.numeric(argvalsforsmooth)

bsplinebasis2<- create.bspline.basis(c(0,1), 8)
bsplinebasis2
plot(bsplinebasis2)
bsplinebasis

smoothallT24 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT24traindata[,i])))
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT24traindata[,i]))/meanT24,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallT30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT30traindata[,i])))
  smoothallT30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT30traindata[,i]))/meanT30,bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallT50 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT50traindata[,i])))
  smoothallT50[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT50traindata[,i]))/meanT50,bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallP30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tP30traindata[,i])))
  smoothallP30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tP30traindata[,i]))/meanP30,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallps30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tps30traindata[,i])))
  smoothallps30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tps30traindata[,i]))/meanps30,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallphi <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tphitraindata[,i])))
  smoothallphi[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tphitraindata[,i]))/meanphi,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallBPR <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tBPRtraindata[,i])))
  smoothallBPR[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tBPRtraindata[,i]))/meanBPR,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW31 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW31traindata[,i])))
  smoothallW31[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW31traindata[,i]))/meanW31,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW32 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW32traindata[,i])))
  smoothallW32[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW32traindata[,i]))/meanW32,bsplinebasis2)[["fd"]][["coefs"]]
}

newargvals <- seq(0,1,length=8)

fdsmoothallT24<-Data2fd(argvals = newargvals, y=smoothallT24)
fdsmoothallT24
fdsmoothallT24$coefs<-smoothallT24
fdsmoothallT24$basis$nbasis<-8
fdsmoothallT24$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothallT24$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothallT24

plot(fdsmoothallT24)

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

newargvals3 <- seq(0,1,length=200) ##tryig because  fd has 12 coefs

funDatasmoothallT24<-fd2funData(fdobj=fdsmoothallT24,argvals =newargvals3)
funDatasmoothallT24
autoplot(funDatasmoothallT24)
class(funDatasmoothallT24)
class(registeredargvals[[1]])

###copare is ok !!
plot(fdsmoothallT24, main = "fd object")
plot(funDatasmoothallT24, main = "funData object")


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

##check all
autoplot(funDatasmoothallT30)
autoplot(funDatasmoothallT50)
autoplot(funDatasmoothallP30)
autoplot(funDatasmoothallps30)
autoplot(funDatasmoothallBPR)
autoplot(funDatasmoothallW31)
autoplot(funDatasmoothallW32)


multifunallsensors<-multiFunData(funDatasmoothallT24,funDatasmoothallT30,funDatasmoothallT50,
                                 funDatasmoothallP30,funDatasmoothallps30,funDatasmoothallphi,
                                 funDatasmoothallBPR,funDatasmoothallW31,funDatasmoothallW32)

#artık yeni fonksiyonlarla MFPCA yapılabilir.


multifunallsensors
dimSupp(multifunallsensors)

## Mean
MultifunMean<-meanFunction(multifunallsensors)
GRAPHMultifunMean1 <- plot(MultifunMean[[1]], 
                           xlab="Observations in [0-1]",
                           ylab="T24 Sensor Value")
GRAPHMultifunMean1

GRAPHMultifunMean2 <- plot(MultifunMean[[2]], 
                           xlab="Observations in [0-1]",
                           ylab="T30 Sensor Value")
GRAPHMultifunMean3 <- plot(MultifunMean[[3]], 
                           xlab="Observations in [0-1]",
                           ylab="T50 Sensor Value")
GRAPHMultifunMean4 <- plot(MultifunMean[[4]], 
                           xlab="Observations in [0-1]",
                           ylab="P30 Sensor Value")
GRAPHMultifunMean5 <- plot(MultifunMean[[5]], 
                           xlab="Observations in [0-1]",
                           ylab="ps30 Sensor Value")
GRAPHMultifunMean6 <- plot(MultifunMean[[6]], 
                           xlab="Observations in [0-1]",
                           ylab="phi Sensor Value")
GRAPHMultifunMean7 <- plot(MultifunMean[[7]], 
                           xlab="Observations in [0-1]",
                           ylab="BPR Sensor Value")
GRAPHMultifunMean8 <- plot(MultifunMean[[8]], 
                           xlab="Observations in [0-1]",
                           ylab="W31 Sensor Value")
GRAPHMultifunMean9 <- plot(MultifunMean[[9]], 
                           xlab="Observations in [0-1]",
                           ylab="W32 Sensor Value")

uniExpansions <- list(list(type = "uFPCA", npc = 3), 
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3),
                      list(type = "uFPCA", npc = 3)) 
uniExpansions

BSplineExpansions <- list(list(type = "splines1D"), 
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"),
                          list(type = "splines1D"))
BSplineExpansions


UnivRegisteredsmoothedMFPCA <- MFPCA(multifunallsensors, M = 3, 
                                     uniExpansions = uniExpansions, fit = TRUE)
UnivRegisteredsmoothedMFPCA

BsplineRegisteredsmoothedMFPCA <- MFPCA(multifunallsensors, M = 3, 
                                        uniExpansions = BSplineExpansions, fit = TRUE)
BsplineRegisteredsmoothedMFPCA


univMeanFunctions <- autoplot(UnivRegisteredsmoothedMFPCA$meanFunction)
univMeanFunctions
univMeanFunctions[[1]]
univMeanFunctions[[2]]
univMeanFunctions[[3]]
univMeanFunctions[[4]]
univMeanFunctions[[5]]
univMeanFunctions[[6]]
univMeanFunctions[[7]]
univMeanFunctions[[8]]
univMeanFunctions[[9]]



BsplineMeanFunctions <- autoplot(BsplineRegisteredsmoothedMFPCA$meanFunction)
BsplineMeanFunctions
BsplineMeanFunctions[[1]]
BsplineMeanFunctions[[2]]
BsplineMeanFunctions[[3]]
BsplineMeanFunctions[[4]]
BsplineMeanFunctions[[5]]
BsplineMeanFunctions[[6]]
BsplineMeanFunctions[[7]]
BsplineMeanFunctions[[8]]
BsplineMeanFunctions[[9]]

screeplot(UnivRegisteredsmoothedMFPCA, main = "Screeplot - lines")
screeplot(UnivRegisteredsmoothedMFPCA, type = "barplot", main = "Screeplot - barplot")

screeplot(BsplineRegisteredsmoothedMFPCA, main = "Screeplot - lines")
screeplot(BsplineRegisteredsmoothedMFPCA, type = "barplot", main = "Screeplot - barplot")



T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g

##bspline compare
T24g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g
##weighted compare
T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g


T30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[2]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T30g

T50g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[3]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T50g

P30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[4]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
P30g

ps30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[5]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
ps30g

phig <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[6]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
phig

BPRg <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[7]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
BPRg

W31g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[8]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
W31g

W32g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[9]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
W32g


get_legend<-function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


gridExtra::grid.arrange(T24g + theme(legend.position = "none"), 
                        T30g + theme(legend.position = "none"), 
                        T50g + theme(legend.position = "none"), 
                        P30g + theme(legend.position = "none"), 
                        ps30g + theme(legend.position = "none"), 
                        phig + theme(legend.position = "none"),
                        BPRg + theme(legend.position = "none"), 
                        W31g + theme(legend.position = "none"), 
                        W32g + theme(legend.position = "none"),
                        nrow= 3, ncol = 3, widths = c(1, 1, 1))


dev.off()
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par(mar=c(5.1,4.1,4.1,2.1))

plot(UnivRegisteredsmoothedMFPCA, plotPCs = 1, combined = TRUE, 
     xlab = c("Cycle Time"))
plot(UnivRegisteredsmoothedMFPCA, plotPCs = 2, combined = TRUE, 
     xlab = c("Cycle Time"))
plot(UnivRegisteredsmoothedMFPCA, plotPCs = 3, combined = TRUE, 
     xlab = c("Cycle Time"))



df <- rbind(data.frame(PC = 1, val = UnivRegisteredsmoothedMFPCA$scores[, 1]), 
            data.frame(PC = 2, val = UnivRegisteredsmoothedMFPCA$scores[, 2]), 
            data.frame(PC = 3, val = UnivRegisteredsmoothedMFPCA$scores[, 3]))

df

UnivRegisteredsmoothedMFPCA$scores


ggplot2::ggplot(data = df, aes(x = PC, y = val)) +
  geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + # grey line first!
  geom_boxplot(alpha = 0.5, lwd = 1) +
  facet_grid(.~PC, scales = "free_x", labeller = label_both) +
  labs(y = "Score values", x = "PCs") +
  theme(strip.text = element_text(size = 16)) + coord_flip()


dev.off()
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par(mar=c(5.1,4.1,4.1,2.1))
univscore <- scoreplot(UnivRegisteredsmoothedMFPCA, cex = 0.8, main = "Scoreplot", col = rgb(0, 0, 0, alpha = 0.7))
bspscore <- scoreplot(BsplineRegisteredsmoothedMFPCA, cex = 0.8, main = "Scoreplot", col = rgb(0, 0, 0, alpha = 0.7))

df <- rbind(data.frame(PC = 1, val = BsplineRegisteredsmoothedMFPCA$scores[, 1]), 
            data.frame(PC = 2, val = BsplineRegisteredsmoothedMFPCA$scores[, 2]), 
            data.frame(PC = 3, val = BsplineRegisteredsmoothedMFPCA$scores[, 3]))


dfpc1 <- df[,2]
dfpc1 <- dfpc1[1:100]
dfpc1
transformeddfpc1<- (1/(dfpc1_100+1000))
transformeddfpc1

hist(dfpc1)
hist(transformeddfpc1)

##### histogram of first PC score & density find using MLE ####
hist(dfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1/(FPC1+1000)",
     main = "")
lines(density(dfpc1), # density plot
      lwd = 2, # thickness of line
      col = "red")


library(multimode)
dfpc1
modetest(dfpc1)

####  In the above analysis, the p-value 
####  is virtually 0 — supporting the alternative 
####  hypothesis of more than one mode present in the 
####  data at the 5% level of significance.

y<-locmodes(dfpc1,mod0=2,display=TRUE)
y
dfpc1

modetest(transformeddfpc1)
####  In the above analysis, the p-value 
####  is virtually 0 — supporting the alternative 
####  hypothesis of more than one mode present in the 
####  data at the 5% level of significance.

y<-locmodes(transformeddfpc1,mod0=2,display=TRUE)
y

length(bigscoredata)

lowscoredata<-dfpc1[which(dfpc1<0.002501255 )]
bigscoredata<-dfpc1[which(dfpc1>0.002501255 )]

lowscoredata<-dfpc1[which(transformeddfpc1>0.0009983719 )]
bigscoredata<-dfpc1[which(transformeddfpc1<0.0009983719 )]


library(PredictionR)
lowtest<-bestfit(lowscoredata, "norm") ##using transformed data because we need positives
lowtest

bigtest<-bestfit(bigscoredata, "norm") ##using transformed data because we need positives
bigtest

normallow<-rnorm(100, -2.874058 ,2.649711 )
normallow

normalbig <-rnorm(100, 4.893666 ,1.197366)
normalbig

hist(dfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1st FPC",
     main = "",
    )
lines(density(bigscoredata), # density plot
      lwd = 2, # thickness of line
      col = "red")
lines(density(lowscoredata), # density plot
      lwd = 2, # thickness of line
      col = "red")


ks.test(bigscoredata,"pnorm",4.893666,1.197366)

ks.test(lowscoredata,"pnorm",-2.874058,2.649711)


bigscoredata
lowscoredata

pmixture=function(x){
  0.63*pnorm(x,-2.874058,2.649711)+0.37*pnorm(x,4.893666,1.197366)}

ks.test(dfpc1,"pmixture")

dmixture=function(x){
  0.63*dnorm(x,-2.874058,2.649711)+0.37*dnorm(x,4.893666,1.197366)}


hist(dfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1st FPC",
     main = "",
   )
curve(dmixture(x),add=TRUE,col="blue")


modetest(transformeddfpc1)
####  In the above analysis, the p-value 
####  is virtually 0 — supporting the alternative 
####  hypothesis of more than one mode present in the 
####  data at the 5% level of significance.

y<-locmodes(transformeddfpc1,mod0=2,display=TRUE)
y

lowscoredist<-transformeddfpc1[which(transformeddfpc1>0.0009983719)]
bigscoredist<-transformeddfpc1[which(transformeddfpc1<0.0009983719)]

bigscoredist
lowscoredist
library(PredictionR)
bigtest<-bestfit(bigscoredist, "gamma") ##using transformed data because we need positives
bigtest

lowtest<-bestfit(lowscoredist, "gamma") ##using transformed data because we need positives
lowtest

gammabig<-rgamma(1000, 555223.3 ,557852045.6 )
gammabig

gammalow <-rgamma(1000, 152870.7 ,152406917.3)
gammalow

hist(transformeddfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1st FPC",
     main = "",ylim=c(0,300000))
lines(density(gammabig), # density plot
      lwd = 2, # thickness of line
      col = "red")
lines(density(gammalow), # density plot
      lwd = 2, # thickness of line
      col = "red")

ks.test(dfpc1, "pnorm", 3.345562e-15, 4.361003e+00) ### doublecheck

####According to the above, we can see that 
####the two modes peak at 0.07808114 and 0.09358918, 
####with the antimode identified at 0.04766974.

library(LaplacesDemon)
library(diptest)

is.unimodal(dfpc1)
is.multimodal(dfpc1)
is.bimodal(dfpc1)
is.trimodal(dfpc1)

library(mousetrap)
bimodality_coefficient(dfpc1)

test<-Modes(dfpc1)


library(devtools)
install_github("choisy/cutoff")
library(cutoff)
install.packages("bbmle")
library(bbmle)

plot(density(dfpc1))
plot(density(transformeddfpc1))
x<-em(transformeddfpc1,"normal","normal")
x



set.seed(12345)
data <- c(rnorm(100,10,2), rgamma(100,20,2))
data
is.multimodal(data)
plot(density(data))
x<-em(data,"normal","gamma")
confint(x)
cutoff(x)