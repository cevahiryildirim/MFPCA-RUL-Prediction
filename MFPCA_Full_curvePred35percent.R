


# load package
library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(devtools)
library(philentropy)
library(multimode)
library(PredictionR)
library(LaplacesDemon)
library(diptest)
library(mousetrap)
library(devtools)
library(cutoff)
library(bbmle)




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


# uniExpansions <- list(list(type = "uFPCA", npc = 3), 
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3),
#                       list(type = "uFPCA", npc = 3)) 
# uniExpansions


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


# UnivRegisteredsmoothedMFPCA <- MFPCA(multifunallsensors, M = 3, 
#                                     uniExpansions = uniExpansions, fit = TRUE)
# UnivRegisteredsmoothedMFPCA

BsplineRegisteredsmoothedMFPCA <- MFPCA(multifunallsensors, M = 3, 
                                        uniExpansions = BSplineExpansions, fit = TRUE)
BsplineRegisteredsmoothedMFPCA

# univMeanFunctions <- autoplot(UnivRegisteredsmoothedMFPCA$meanFunction)
# univMeanFunctions
# univMeanFunctions[[1]]
# univMeanFunctions[[2]]
# univMeanFunctions[[3]]
# univMeanFunctions[[4]]
# univMeanFunctions[[5]]
# univMeanFunctions[[6]]
# univMeanFunctions[[7]]
# univMeanFunctions[[8]]
# univMeanFunctions[[9]]



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

# screeplot(UnivRegisteredsmoothedMFPCA, main = "Screeplot - lines")
# screeplot(UnivRegisteredsmoothedMFPCA, type = "barplot", main = "Screeplot - barplot")

screeplot(BsplineRegisteredsmoothedMFPCA, main = "Screeplot - lines")
screeplot(BsplineRegisteredsmoothedMFPCA, type = "barplot", main = "Screeplot - barplot")




# T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
# T24g

##bspline compare
T24g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g
# ##weighted compare
# T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
# T24g


T30g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[2]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T30g

T50g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[3]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T50g

P30g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[4]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
P30g

ps30g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[5]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
ps30g

phig <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[6]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
phig

BPRg <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[7]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
BPRg

W31g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[8]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
W31g

W32g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[9]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
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

plot(BsplineRegisteredsmoothedMFPCA, plotPCs = 1, combined = TRUE, 
     xlab = c("Cycle Time"))
plot(BsplineRegisteredsmoothedMFPCA, plotPCs = 2, combined = TRUE, 
     xlab = c("Cycle Time"))
plot(BsplineRegisteredsmoothedMFPCA, plotPCs = 3, combined = TRUE, 
     xlab = c("Cycle Time"))



df <- rbind(data.frame(PC = 1, val = BsplineRegisteredsmoothedMFPCA$scores[, 1]), 
            data.frame(PC = 2, val = BsplineRegisteredsmoothedMFPCA$scores[, 2]), 
            data.frame(PC = 3, val = BsplineRegisteredsmoothedMFPCA$scores[, 3]))

df



BsplineRegisteredsmoothedMFPCA$scores


ggplot2::ggplot(data = df, aes(x = PC, y = val)) +
  geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + # grey line first!
  geom_boxplot(alpha = 0.5, lwd = 1) +
  facet_grid(.~PC, scales = "free_x", labeller = label_both) +
  labs(y = "Score values", x = "PCs") +
  theme(strip.text = element_text(size = 16)) + coord_flip()





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

##### histogram of first PC score & density find using MLE ####
hist(transformeddfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1/(FPC1+1000)",
     main = "")
lines(density(transformeddfpc1), # density plot
      lwd = 2, # thickness of line
      col = "red")



# dfpc1
# modetest(dfpc1)

####  In the above analysis, the p-value 
####  is virtually 0 — supporting the alternative 
####  hypothesis of more than one mode present in the 
####  data at the 5% level of significance.

# y<-locmodes(dfpc1,mod0=2,display=TRUE)
# y
# dfpc1

modetest(transformeddfpc1)
####  In the above analysis, the p-value 
####  is virtually 0 — supporting the alternative 
####  hypothesis of more than one mode present in the 
####  data at the 5% level of significance.

antimode<-locmodes(transformeddfpc1,mod0=2,display=TRUE)
antimode<-antimode$locations[2]
antimode


# lowscoredata<-dfpc1[which(dfpc1<0.002501448 )]
# bigscoredata<-dfpc1[which(dfpc1>0.002501448 )]

lowscoredata<-transformeddfpc1[which(transformeddfpc1>antimode )]
bigscoredata<-transformeddfpc1[which(transformeddfpc1<antimode )]
length(bigscoredata)


lowtest<-bestfit(lowscoredata, "gamma", conf = 0.90) ##using transformed data because we need positives
lowtest

bigtest<-bestfit(bigscoredata, "gamma") ##using transformed data because we need positives
bigtest

normallow<-rgamma(100, 152870.7  ,152406917.3 )
normallow

normalbig <-rgamma(100, 555223.3  ,557852045.6 )
normalbig

hist(transformeddfpc1, # histogram
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

ks.test(bigscoredata,"pgamma",555223.3  ,557852045.6)

ks.test(lowscoredata,"pgamma",152870.7, 152406917.3)


pmixture=function(x){
  0.61*pgamma(x,152870.7,152406917.3)+0.39*pgamma(x,555223.3,557852045.6)}

ks.test(dfpc1,"pmixture")

dmixture=function(x){
  0.61*dgamma(x,152870.7,152406917.3)+0.39*dgamma(x,555223.3,557852045.6)}

hist(transformeddfpc1, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "1st FPC",
     main = "",
)
curve(dmixture(x),add=TRUE,col="blue")


# normallow<-rnorm(100, -2.874058 ,2.649711 )
# normallow
# 
# normalbig <-rnorm(100, 4.893666 ,1.197366)
# normalbig
# 
# hist(dfpc1, # histogram
#      col="peachpuff", # column color
#      border="black",
#      prob = TRUE, # show densities instead of frequencies
#      xlab = "1st FPC",
#      main = "",
# )
# lines(density(bigscoredata), # density plot
#       lwd = 2, # thickness of line
#       col = "red")
# lines(density(lowscoredata), # density plot
#       lwd = 2, # thickness of line
#       col = "red")

# 
# ks.test(bigscoredata,"pnorm",4.893666,1.197366)
# 
# ks.test(lowscoredata,"pnorm",-2.874058,2.649711)
# 

bigscoredata
lowscoredata

# pmixture=function(x){
#   0.63*pnorm(x,-2.874058,2.649711)+0.37*pnorm(x,4.893666,1.197366)}
# 
# ks.test(dfpc1,"pmixture")
# 
# dmixture=function(x){
#   0.63*dnorm(x,-2.874058,2.649711)+0.37*dnorm(x,4.893666,1.197366)}
# 

# hist(dfpc1, # histogram
#      col="peachpuff", # column color
#      border="black",
#      prob = TRUE, # show densities instead of frequencies
#      xlab = "1st FPC",
#      main = "",
# )
# curve(dmixture(x),add=TRUE,col="blue")
# 
# 
# modetest(transformeddfpc1)
# ####  In the above analysis, the p-value 
# ####  is virtually 0 — supporting the alternative 
# ####  hypothesis of more than one mode present in the 
# ####  data at the 5% level of significance.
# 
# y<-locmodes(transformeddfpc1,mod0=2,display=TRUE)
# y
# 
# lowscoredist<-transformeddfpc1[which(transformeddfpc1>0.0009983719)]
# bigscoredist<-transformeddfpc1[which(transformeddfpc1<0.0009983719)]
# 
# bigscoredist
# lowscoredist
# library(PredictionR)
# bigtest<-bestfit(bigscoredist, "gamma") ##using transformed data because we need positives
# bigtest
# 
# lowtest<-bestfit(lowscoredist, "gamma") ##using transformed data because we need positives
# lowtest
# 
# gammabig<-rgamma(1000, 555223.3 ,557852045.6 )
# gammabig
# 
# gammalow <-rgamma(1000, 152870.7 ,152406917.3)
# gammalow
# 
# hist(transformeddfpc1, # histogram
#      col="peachpuff", # column color
#      border="black",
#      prob = TRUE, # show densities instead of frequencies
#      xlab = "1st FPC",
#      main = "",ylim=c(0,300000))
# lines(density(gammabig), # density plot
#       lwd = 2, # thickness of line
#       col = "red")
# lines(density(gammalow), # density plot
#       lwd = 2, # thickness of line
#       col = "red")
# 
# ks.test(dfpc1, "pnorm", 3.345562e-15, 4.361003e+00) ### doublecheck

####According to the above, we can see that 
####the two modes peak at 0.07808114 and 0.09358918, 
####with the antimode identified at 0.04766974.
###According to the above, we can see that 
####the two modes peak at 0.07808114 and 0.09358918, 
####with the antimode identified at 0.04766974.

is.unimodal(transformeddfpc1)
is.multimodal(transformeddfpc1)
is.bimodal(transformeddfpc1)
is.trimodal(transformeddfpc1)

bimodality_coefficient(transformeddfpc1)

test<-Modes(transformeddfpc1)
test
plot(density(dfpc1))
plot(density(transformeddfpc1))
x<-em(transformeddfpc1,"gamma","gamma")
x


varT24 <- funData(argvals = multifunallsensors[[1]]@argvals, 
                  X = matrix(apply(multifunallsensors[[1]]@X, 2, var), nrow = 1))
a2<-varT24
a2
plot(a2)

a5<-integrate(varT24)
a5

b2<-multifunallsensors[[1]]@argvals
b2
c2<-multifunallsensors[[1]]@X
c2

varT30 <- funData(argvals = multifunallsensors[[2]]@argvals, 
                  X = matrix(apply(multifunallsensors[[2]]@X, 2, var), nrow = 1))


varT50 <- funData(argvals = multifunallsensors[[3]]@argvals, 
                  X = matrix(apply(multifunallsensors[[3]]@X, 2, var), nrow = 1))

varP30 <- funData(argvals = multifunallsensors[[4]]@argvals, 
                  X = matrix(apply(multifunallsensors[[4]]@X, 2, var), nrow = 1))

varps30 <- funData(argvals = multifunallsensors[[5]]@argvals, 
                   X = matrix(apply(multifunallsensors[[5]]@X, 2, var), nrow = 1))

varphi <- funData(argvals = multifunallsensors[[6]]@argvals, 
                  X = matrix(apply(multifunallsensors[[6]]@X, 2, var), nrow = 1))

varBPR<- funData(argvals = multifunallsensors[[7]]@argvals, 
                 X = matrix(apply(multifunallsensors[[7]]@X, 2, var), nrow = 1))

varW31 <- funData(argvals = multifunallsensors[[8]]@argvals, 
                  X = matrix(apply(multifunallsensors[[8]]@X, 2, var), nrow = 1))

varW32 <- funData(argvals = multifunallsensors[[9]]@argvals, 
                  X = matrix(apply(multifunallsensors[[9]]@X, 2, var), nrow = 1))


MFPCAscore <- scoreplot(BsplineRegisteredsmoothedMFPCA, cex = 0.8, main = "Scoreplot", col = rgb(0, 0, 0, alpha = 0.7))
MFPCAscore


obs <-1:4
obs <-1:100
obs <-64:66
obs <-49:49
obs <-20:20
obs <-31:31
obs <-34:34
obs <-35:35
obs <-42:42
obs <-49:49
obs <-68:68
obs <-76:76
obs <-91:91
obs <-82:82

class(obs)

length(obs)

graphT24 <- autoplot(multifunallsensors[[1]], obs = obs) + 
  labs(x = "Cycle Time", y = "T24 - Temperature in °C", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[1]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[1]]@X[obs, ])) +
  geom_line(aes(colour = "red")) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[1]], obs = obs1, lty = 4, 
            col = "black", 
            each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[1]]))+
  geom_line(aes(colour = "blue")) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[1]], obs = obs2, lty = 4, 
            col = "blue", 
            each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[1]]))
graphT24


graphps30 <- autoplot(multifunallsensors[[5]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[5]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[5]]@X[obs, ])) +
  geom_line(aes(colour = obs)) + scale_colour_manual(values="black", label="81", name="Engine")+
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[5]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[5]])))
graphps30

graphW31<- autoplot(multifunallsensors[[8]], obs = obs) + 
  labs(x = "Cycle Time", y = "W31 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[8]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[8]]@X[obs, ])) +
  geom_line(aes(colour = obs)) + scale_colour_manual(values="black", label="20", name="Engine")+
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[8]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[8]])))
graphW31

graphW32 <- autoplot(multifunallsensors[[8]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[8]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[8]]@X[obs, ])) +
  geom_line(aes(colour = obs)) + scale_colour_manual(values="black", label="91", name="Engine")+
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[8]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[8]])))
graphW32

graphps30 <- autoplot(multifunallsensors[[9]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[9]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[9]]@X[obs, ])) +
  geom_line(aes(colour = obs)) + scale_colour_manual(values="black", label="82", name="Engine")+
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[9]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[9]])))
graphps30

scale_colour_manual(values="red", label="New Color Labsl", name="New Series Label")

graphBPR <- autoplot(multifunallsensors[[7]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[7]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[7]]@X[obs, ])) +
  geom_line(aes(colour = obs)) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[7]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[7]])))
graphBPR

graphW31 <- autoplot(multifunallsensors[[8]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[8]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[8]]@X[obs, ])) +
  geom_line(aes(colour = obs)) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[8]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[8]])))
graphW31

graphW32 <- autoplot(multifunallsensors[[9]], obs = obs) + 
  labs(x = "Cycle Time", y = "ps30 Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[9]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[9]]@X[obs, ])) +
  geom_line(aes(colour = obs)) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[9]], obs = obs, lty = 2, 
            col = rep(scales::hue_pal()(length(obs)), 
                      each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[9]])))
graphW32

graphW32scored <- autoplot(multifunallsensors[[9]], obs = obs) + 
  labs(x = "Cycle Time", y = "W32 - Sensor Value", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[9]]@X[obs, ], 
                                    BsplineRegisteredsmoothedMFPCA$fit[[9]]@X[obs, ])) +
  geom_line(aes(colour = "red")) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[9]], obs = obs1, lty = 4, 
            col = "black", 
            each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[9]]))+
  geom_line(aes(colour = "blue")) +
  autolayer(BsplineRegisteredsmoothedMFPCA$fit[[9]], obs = obs2, lty = 4, 
            col = "blue", 
            each = nObsPoints(BsplineRegisteredsmoothedMFPCA$fit[[9]]))
graphW32scored




#######smooth registered back T24
###########################################################
#####################################################
#####################################################

seq(1,length(na.omit(tW32traindata[,1])), length.out= length(na.omit(tW32traindata[,i])))######## 1. motorun argvalleri
as.vector(na.omit(T24traindata[1,])) ####1. motorun valueları
create.bspline.basis(c(1,length(na.omit(tW32traindata[,1]))),8) ##### 1. motor için splinelar
#####for döngüsü ile tüm motorlar için smoothing yapılarak bir listeye eklendi


listeT24<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(T24traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeT24[[i]]<-SmoothtENG
}


#######smooth registered back T30
listeT30<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(T30traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeT30[[i]]<-SmoothtENG
}


#######smooth registered back T50
listeT50<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(T50traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeT50[[i]]<-SmoothtENG
}


#######smooth registered back P30
listeP30<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(P30traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeP30[[i]]<-SmoothtENG
}


#######smooth registered back ps30
listeps30<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(ps30traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeps30[[i]]<-SmoothtENG
}
listeps30


#######smooth registered back phi
listephi<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(phitraindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listephi[[i]]<-SmoothtENG
}


#######smooth registered back BPR
listeBPR<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(BPRtraindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeBPR[[i]]<-SmoothtENG
}


#######smooth registered back W31
listeW31<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(W31traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeW31[[i]]<-SmoothtENG
}


#######smooth registered back W32
listeW32<-list()
for (i in 1:100)
{
  SmoothtENG <- smooth.basis( argvals = seq(1,length(na.omit(tW32traindata[,i])), length.out= length(na.omit(tW32traindata[,i]))),
                              y= as.vector(na.omit(W32traindata[i,])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tW32traindata[,i]))),8))
  listeW32[[i]]<-SmoothtENG
}




plot(listeT24[[1]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeT24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeT24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')


plot(listeT30[[1]],xlim=c(0,370), ylim=c(1580,1610))
for (i in 2:100) {
  lines(listeT30[[i]])}

plot(listeT50[[1]],xlim=c(0,370), ylim=c(1390,1435))
for (i in 2:100) {
  lines(listeT50[[i]])}

plot(listeP30[[1]],xlim=c(0,370), ylim=c(550.5,555.2))
for (i in 2:100) {
  lines(listeP30[[i]])
}
plot(listeps30[[1]],xlim=c(0,370), ylim=c(47.05,48.4))
for (i in 2:100) {
  lines(listeps30[[i]])}

plot(listephi[[1]],xlim=c(0,370), ylim=c(519.1,522.8))
for (i in 2:100) {
  lines(listephi[[i]])}

plot(listeBPR[[1]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in 2:100) {
  lines(listeBPR[[i]])}

plot(listeW31[[1]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in 2:100) {
  lines(listeW31[[i]])}

plot(listeW32[[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in 2:100) {
  lines(listeW32[[i]])}

plot(listeW32[[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeW32[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeW32[[j]], col="red")
}
text(50, 23.2, 'negative MFPC scores', col='red')
text(220, 23.38, 'positive MFPC scores', col='black')

class(listeP30[[1]]$fd)

listeP30[[1]]$fd

T24_1stDeriv <- deriv.fd(listeP30[[1]]$fd)##1st deriv of T24
plot.fd(T24_1stDeriv, col=black)##1st deriv of T24


##2nd Derivatives of smoot original scale
T24_2ndDeriv <-deriv.fd(T24_1stDeriv)##1st deriv of T24
plot.fd(T24_2ndDeriv)
#####trial for only one curve above###############



listeDer_1_T24<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeT24[[i]]$fd)
  listeDer_1_T24[[i]]<-DERIV}
plot(listeDer_1_T24[[1]],xlim=c(0,370), ylim=c(-0.03,0.1))
for (i in c(2:100)) {
  lines(listeDer_1_T24[[i]], col="black" ) }


listeDer_1_T30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeT30[[i]]$fd)
  listeDer_1_T30[[i]]<-DERIV}
plot(listeDer_1_T30[[1]],xlim=c(0,370),ylim=c(-0.60,1.2))
for (i in c(2:100)) {
  lines(listeDer_1_T30[[i]], col="black" ) }

listeDer_1_T50<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeT50[[i]]$fd)
  listeDer_1_T50[[i]]<-DERIV}
plot(listeDer_1_T50[[1]],xlim=c(0,370),ylim=c(-0.40,1.9))
for (i in c(2:100)) {
  lines(listeDer_1_T50[[i]], col="black" ) }

listeDer_1_P30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeP30[[i]]$fd)
  listeDer_1_P30[[i]]<-DERIV}
plot(listeDer_1_P30[[1]],xlim=c(0,370),ylim=c(-0.15,0.04))
for (i in c(2:100)) {
  lines(listeDer_1_P30[[i]], col="black" ) }


listeDer_1_ps30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeps30[[i]]$fd)
  listeDer_1_ps30[[i]]<-DERIV}
plot(listeDer_1_ps30[[1]],xlim=c(0,370),ylim=c(-0.007,0.05))
for (i in c(2:100)) {
  lines(listeDer_1_ps30[[i]], col="black" ) }

listeDer_1_phi<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listephi[[i]]$fd)
  listeDer_1_phi[[i]]<-DERIV}
plot(listeDer_1_phi[[1]],xlim=c(0,370), ylim=c(-0.12,0.02))
for (i in c(2:100)) {
  lines(listeDer_1_phi[[i]], col="black" ) }

listeDer_1_BPR<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeBPR[[i]]$fd)
  listeDer_1_BPR[[i]]<-DERIV}
plot(listeDer_1_BPR[[1]],xlim=c(0,370),ylim=c(-0.001,0.007))
for (i in c(2:100)) {
  lines(listeDer_1_BPR[[i]], col="black" ) }

listeDer_1_W31<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeW31[[i]]$fd)
  listeDer_1_W31[[i]]<-DERIV}
plot(listeDer_1_W31[[1]],xlim=c(0,370),ylim=c(-0.032,0.015))
for (i in c(2:100)) {
  lines(listeDer_1_W31[[i]], col="black" ) }

listeDer_1_W32<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd( listeW32[[i]]$fd)
  listeDer_1_W32[[i]]<-DERIV}
plot(listeDer_1_W32[[1]],xlim=c(0,370),ylim=c(-0.02,0.01))
for (i in c(2:100)) {
  lines(listeDer_1_W32[[i]], col="black" ) }

##########plots high and low scores
plot(listeDer_1_T24[[1]],xlim=c(0,370),ylim=c(-0.03,0.1))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_1_T24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_1_T24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')

plot(listeDer_1_W32[[1]],xlim=c(0,370),ylim=c(-0.02,0.01))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_1_W32[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_1_W32[[j]], col="red")
}


#########original scale 2nd derivs
#########original scale 2nd derivs
#########original scale 2nd derivs

listeDer_2_T24<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_T24[[i]] )
  listeDer_2_T24[[i]]<-DERIV}
plot(listeDer_2_T24[[1]],xlim=c(0,370),ylim=c(-0.007,0.008))
for (i in c(2:100)) {
  lines(listeDer_2_T24[[i]], col="black" ) }

listeDer_2_T30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_T30[[i]] )
  listeDer_2_T30[[i]]<-DERIV}
plot(listeDer_2_T30[[1]],xlim=c(0,370),ylim=c(-0.08,0.08))
for (i in c(2:100)) {
  lines(listeDer_2_T30[[i]], col="black" ) }

listeDer_2_T50<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_T50[[i]] )
  listeDer_2_T50[[i]]<-DERIV}
plot(listeDer_2_T50[[1]],xlim=c(0,370),ylim=c(-0.1,0.1))
for (i in c(2:100)) {
  lines(listeDer_2_T50[[i]], col="black" ) }

listeDer_2_P30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_P30[[i]] )
  listeDer_2_P30[[i]]<-DERIV}
plot(listeDer_2_P30[[1]],xlim=c(0,370),ylim=c(-0.014,0.007))
for (i in c(2:100)) {
  lines(listeDer_2_P30[[i]], col="black" ) }

listeDer_2_ps30<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_P30[[i]] )
  listeDer_2_ps30[[i]]<-DERIV}
plot(listeDer_2_ps30[[1]],xlim=c(0,370),ylim=c(-0.01,0.006))
for (i in c(2:100)) {
  lines(listeDer_2_ps30[[i]], col="black" ) }

listeDer_2_phi<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_phi[[i]] )
  listeDer_2_phi[[i]]<-DERIV}
plot(listeDer_2_phi[[1]],xlim=c(0,370),ylim=c(-0.01,0.006))
for (i in c(2:100)) {
  lines(listeDer_2_phi[[i]], col="black" ) }

listeDer_2_BPR<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_BPR[[i]] )
  listeDer_2_BPR[[i]]<-DERIV}
plot(listeDer_2_BPR[[1]],xlim=c(0,370),ylim=c(-0.0005,0.0006))
for (i in c(2:100)) {
  lines(listeDer_2_BPR[[i]], col="black" ) }

listeDer_2_W31<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_W31[[i]] )
  listeDer_2_W31[[i]]<-DERIV}
plot(listeDer_2_W31[[1]],xlim=c(0,370),ylim=c(-0.003,0.002))
for (i in c(2:100)) {
  lines(listeDer_2_W31[[i]], col="black" ) }

listeDer_2_W32<-list()
for (i in 1:100)
{
  DERIV <- deriv.fd(listeDer_1_W32[[i]] )
  listeDer_2_W32[[i]]<-DERIV}
plot(listeDer_2_W32[[1]],xlim=c(0,370),ylim=c(-0.002,0.002))
for (i in c(2:100)) {
  lines(listeDer_2_W32[[i]], col="black" ) }

##########plots high and low scores
plot(listeDer_2_T24[[1]],xlim=c(0,370),ylim=c(-0.007,0.008))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_2_T24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_2_T24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')

plot(listeDer_2_W32[[1]],xlim=c(0,370),ylim=c(-0.002,0.002))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_2_W32[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_2_W32[[j]], col="red")
}



obs1 <-c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)## temperature delta changes from 
obs2 <-c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)## temperature delta changes from 
## left to right in score plot                      
## left to right in score plot
obs <- c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95,
         6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)


scoredifgraphT24 <- autoplot(multifunallsensors[[1]], obs = obs) + 
  labs(x = "Cycle Time", y = "T24 - Temperature in °C", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[1]]@X[obs, ], 
                                    UnivRegisteredsmoothedMFPCA$fit[[1]]@X[obs, ])) +
  geom_line(aes(colour = "red")) +
  autolayer(UnivRegisteredsmoothedMFPCA$fit[[1]], obs = obs1, lty = 4, 
            col = "black", 
            each = nObsPoints(UnivRegisteredsmoothedMFPCA$fit[[1]]))+
  geom_line(aes(colour = "blue")) +
  autolayer(UnivRegisteredsmoothedMFPCA$fit[[1]], obs = obs2, lty = 4, 
            col = "blue", 
            each = nObsPoints(UnivRegisteredsmoothedMFPCA$fit[[1]]))+
  autolayer(MultifunMean[[1]], col="red")
scoredifgraphT24


scoredifgraphW32 <- autoplot(multifunallsensors[[9]], obs = obs) + 
  labs(x = "Cycle Time", y = "T24 - Temperature in °C", col = "Engine") +
  scale_y_continuous(limits = range(multifunallsensors[[9]]@X[obs, ], 
                                    UnivRegisteredsmoothedMFPCA$fit[[9]]@X[obs, ])) +
  geom_line(aes(colour = "red")) +
  autolayer(UnivRegisteredsmoothedMFPCA$fit[[9]], obs = obs1, lty = 4, 
            col = "black", 
            each = nObsPoints(UnivRegisteredsmoothedMFPCA$fit[[9]]))+
  geom_line(aes(colour = "blue")) +
  autolayer(UnivRegisteredsmoothedMFPCA$fit[[9]], obs = obs2, lty = 4, 
            col = "blue", 
            each = nObsPoints(UnivRegisteredsmoothedMFPCA$fit[[9]]))+
  autolayer(MultifunMean[[9]], col="red")
scoredifgraphW32



fdsmoothallT24
plot(fdsmoothallT24)

derivfdsmoothallT24 <- deriv.fd(fdsmoothallT24)
plot(derivfdsmoothallT24)
FDmeanT24<-funData2fd(MultifunMean[[1]])
derivFDmeanT24 <- deriv.fd(FDmeanT24)
plot.fd(derivfdsmoothallT24, col=black)
lines(derivFDmeanT24, col="red")


derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)
DmeanT30<-funData2fd(MultifunMean[[2]])
derivFDmeanT30 <- deriv.fd(FDmeanT30)
plot.fd(derivfdsmoothallT30, col=black)
lines(derivFDmeanT30, col="red")

derivfdsmoothallT50 <- deriv.fd(fdsmoothallT50)
FDmeanT50<-funData2fd(MultifunMean[[3]])
derivFDmeanT50 <- deriv.fd(FDmeanT50)
plot.fd(derivfdsmoothallT50, col=black)
lines(derivFDmeanT50, col="red")

secderivmeanfdsmoothallT50 <- deriv.fd(derivFDmeanT50)
plot(secderivmeanfdsmoothallT50)

secderivfdsmoothallT50 <- deriv.fd(derivfdsmoothallT50)
plot(secderivfdsmoothallT50)
lines(secderivmeanfdsmoothallT50, col="red")
##go to original curve here

FDmeanT50<-funData2fd(MultifunMean[[3]])
derivFDmeanT50 <- deriv.fd(FDmeanT50)
plot.fd(derivfdsmoothallT50, col=black)
lines(derivFDmeanT50, col="red")



derivfdsmoothallP30 <- deriv.fd(fdsmoothallP30)
FDmeanP30<-funData2fd(MultifunMean[[4]])
derivFDmeanP30 <- deriv.fd(FDmeanP30)
plot.fd(derivfdsmoothallP30, col=black)
lines(derivFDmeanP30, col="red")

derivfdsmoothallps30 <- deriv.fd(fdsmoothallps30)
FDmeanps30<-funData2fd(MultifunMean[[5]])
derivFDmeanps30 <- deriv.fd(FDmeanps30)
plot.fd(derivfdsmoothallps30, col=black)
lines(derivFDmeanps30, col="red")


##arası dolacak


derivfdsmoothallW32 <- deriv.fd(fdsmoothallW32)
FDmeanW32<-funData2fd(MultifunMean[[9]])
derivFDmeanW32 <- deriv.fd(FDmeanW32)
plot.fd(derivfdsmoothallW32, col=black)
lines(derivFDmeanW32, col="red")







derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)





derivfdsmoothallT50 <- deriv.fd(fdsmoothallT50)
plot(derivfdsmoothallT50)

derivfdsmoothallP30 <- deriv.fd(fdsmoothallP30)
plot(derivfdsmoothallP30)
derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)
derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)
derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)

derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)
derivfdsmoothallT30 <- deriv.fd(fdsmoothallT30)
plot(derivfdsmoothallT30)
smoothallT50
plot(fdsmoothallT24)

smoothallP30
plot(fdsmoothallT24)
smoothallps30
plot(fdsmoothallT24)

smoothallphi
plot(fdsmoothallT24)

smoothallBPR
plot(fdsmoothallT24)

smoothallW31
plot(fdsmoothallT24)

smoothallW32
plot(fdsmoothallT24)

MultifunMean[[2]]

plot(listeT24[[1]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeT24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeT24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')

lines(listeT24[[2]], col="blue")
lines(listeT24[[49]], col="green")

plot(listeDer_1_T24[[1]],xlim=c(0,370), ylim=c(-0.03,0.1))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_1_T24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_1_T24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')

lines(listeDer_1_T24[[2]], col="blue")
lines(listeDer_1_T24[[49]], col="green")


plot(listeDer_2_T24[[1]],xlim=c(0,370),ylim=c(-0.007,0.008))
for (i in c(2,3,5,9,10,11,15,18,28,30,31,33,46,48,51,54,59,61,65,78,79,82,83,95)) {
  lines(listeDer_2_T24[[i]], col="black" ) }
for(j in c(6,8,12,14,16,19,22,26,39,40,49,56,62,66,73,75,76,80,84,91,94)){
  lines(listeDer_2_T24[[j]], col="red")
}
text(50, 643.3, 'negative MFPC scores', col='red')
text(200, 642.2, 'positive MFPC scores', col='black')

lines(listeDer_2_T24[[2]], col="blue")
lines(listeDer_2_T24[[49]], col="green")



listeDer_1_T24



##########first and seccond group comparisons

smoothallT24
smoothallT30
smoothallT50
smoothallP30
smoothallps30
smoothallphi
smoothallBPR
smoothallW31
smoothallW32

dfpc1
transformeddfpc1
antimode
lowscoredata<-transformeddfpc1[which(transformeddfpc1>antimode)]
bigscoredata<-transformeddfpc1[which(transformeddfpc1<antimode)]

lowscoredata<-dfpc1[which(dfpc1<2.137284)]
bigscoredata<-dfpc1[which(dfpc1>2.137284)]

smallscore<- which(dfpc1<2.137284)
bigscore<-which(dfpc1>2.137284)

smallscore<- which(transformeddfpc1>antimode)
bigscore<-which(transformeddfpc1<antimode)

smoothallT24
smallscoresT24<-smoothallT24[1:8,c(smallscore)]
bigscoresT24<-smoothallT24[1:8,c(bigscore)]

fdsmoothsmallT24<-Data2fd(argvals = newargvals, y=smallscoresT24)
fdsmoothbigT24<-Data2fd(argvals = newargvals, y=bigscoresT24)

###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothsmallT24$coefs<-smallscoresT24
fdsmoothsmallT24$basis$nbasis<-8
fdsmoothsmallT24$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothsmallT24$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothsmallT24
fdsmoothbigT24$coefs<-bigscoresT24
fdsmoothbigT24$basis$nbasis<-8
fdsmoothbigT24$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothbigT24$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothbigT24
plot(fdsmoothbigT24)
lines(fdsmoothsmallT24, col="red")
lines(fdsmoothbigT24, col="black")



################## fundata part (not necessary)########
################## fundata part (not necessary)########
funDatasmoothsmallT24<-fd2funData(fdsmoothsmallT24,argvals = newargvals3)
funDatasmoothsmallT24
funDatasmoothbigT24<-fd2funData(fdsmoothbigT24,argvals = newargvals3)
funDatasmoothbigT24

fundataT24bigMean<-meanFunction(funDatasmoothbigT24)
fundataT24smallMean<-meanFunction(funDatasmoothsmallT24)

autoplot(fundataT24bigMean)
plot(fundataT24smallMean)
################## fundata part (not necessary)########
################## fundata part (not necessary)########


##########Mean functions##############
T24smallMean<-mean.fd(fdsmoothsmallT24)
T24bigMean<-mean.fd(fdsmoothbigT24)

plot(T24bigMean)
lines(T24smallMean, col="red")
text(0.4, 643.0, 'Mean function (small scores)', col="red")
text(0.8, 642.35, 'Mean function (big scores)', col='black')

####T24 small / big comparison plot###
plot(fdsmoothbigT24)
lines(fdsmoothsmallT24, col="red")
lines(fdsmoothbigT24, col="black")
text(0.2, 643.3, 'small MFPC scores', col='red')
text(0.8, 642.2, 'big MFPC scores', col='black')
lines(T24smallMean, col="green")
lines(T24bigMean, col="yellow")
text(0.6, 643.2, 'Mean (small scores)', col='green')
text(0.9, 642.35, 'Mean (big scores)', col='orange')



##########first derivative of small and big scores
deriv1stT24big<-deriv.fd(fdsmoothbigT24)
deriv1stT24small<-deriv.fd(fdsmoothsmallT24)
plot(deriv1stT24big)
lines(deriv1stT24small,col="red")
lines(deriv1stT24big,col="black")

##########first derivative of mean functions
deriv1stT24bigMean<-deriv.fd(T24bigMean)
deriv1stT24smallMean<-deriv.fd(T24smallMean)
plot(deriv1stT24bigMean)
lines(deriv1stT24smallMean,col="red")
text(0.8, 0.2, '1st derivative Mean function (small scores)', col="red")
text(0.4, 2, '1st derivative Mean function (big scores)', col='black')

####1st derivatives - T24 small / big comparison plot###
plot(deriv1stT24big)
lines(deriv1stT24small,col="red")
lines(deriv1stT24big,col="black")
text(0.6, -5, 'negative MFPC scores', col='red')
text(0.4, 5, 'positive MFPC scores', col='black')
lines(deriv1stT24smallMean, col="green")
lines(deriv1stT24bigMean, col="yellow")
text(0.8, -2, '1st deriv Mean (smallscores)', col='green')
text(0.7, 6, '1st deriv Mean (bigscores)', col='orange')


##########second derivative of small and big scores
deriv2ndT24big<-deriv.fd(deriv1stT24big)
deriv2ndT24small<-deriv.fd(deriv1stT24small)

plot(deriv2ndT24big)
lines(deriv2ndT24small,col="red")
lines(deriv2ndT24big,col="black")

############second derivative of mean functions
deriv2ndT24bigMean<-deriv.fd(deriv1stT24bigMean)
deriv2ndT24smallMean<-deriv.fd(deriv1stT24smallMean)

plot(deriv2ndT24bigMean)
lines(deriv2ndT24smallMean,col="red")
text(0.8, 1.5, '2nd der. Mean function (small scores)', col="red")
text(0.4, 10, '2nd der. Mean function (big scores)', col='black')

####2nd derivatives - T24 small / big comparison plot###
plot(deriv2ndT24big)
lines(deriv2ndT24small,col="red")
lines(deriv2ndT24big,col="black")
text(0.6, -30, 'negative MFPC scores', col='red')
text(0.4, 40, 'positive MFPC scores', col='black')
lines(deriv2ndT24smallMean, col="green")
lines(deriv2ndT24bigMean, col="yellow")
text(0.7, -70, '2nd deriv Mean (smallscores)', col='green')
text(0.7, 100, '2nd deriv Mean (bigscores)', col='orange')

######################W32 the same one

smoothallW32
smallscoresW32<-smoothallW32[1:8,c(smallscore)]
bigscoresW32<-smoothallW32[1:8,c(bigscore)]

fdsmoothsmallW32<-Data2fd(argvals = newargvals, y=smallscoresW32)
fdsmoothbigW32<-Data2fd(argvals = newargvals, y=bigscoresW32)

###çok önemli smoothingi fd ye çevirdikten sonra düzeltme gerekiyor!!!!
fdsmoothsmallW32$coefs<-smallscoresW32
fdsmoothsmallW32$basis$nbasis<-8
fdsmoothsmallW32$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothsmallW32$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothsmallW32
fdsmoothbigW32$coefs<-bigscoresW32
fdsmoothbigW32$basis$nbasis<-8
fdsmoothbigW32$basis$params<- c(0.2,0.4,0.6,0.8)
fdsmoothbigW32$basis$names<-c("bspl4.1","bspl4.2","bspl4.3","bspl4.4","bspl4.5","bspl4.6","bspl4.7","bspl4.8")
fdsmoothbigW32
plot(fdsmoothbigW32)
lines(fdsmoothsmallW32, col="red")
lines(fdsmoothbigW32, col="black")


################## fundata part (not necessary)########
################## fundata part (not necessary)########
funDatasmoothsmallW32<-fd2funData(fdsmoothsmallW32,argvals = newargvals3)
funDatasmoothsmallW32
funDatasmoothbigW32<-fd2funData(fdsmoothbigW32,argvals = newargvals3)
funDatasmoothbigW32

fundataW32bigMean<-meanFunction(funDatasmoothbigW32)
fundataW32smallMean<-meanFunction(funDatasmoothsmallW32)

autoplot(fundataW32bigMean)
plot(fundataW32smallMean)
################## fundata part (not necessary)########
################## fundata part (not necessary)########


##########Mean functions##############
W32smallMean<-mean.fd(fdsmoothsmallW32)
W32bigMean<-mean.fd(fdsmoothbigW32)

plot(W32bigMean)
lines(W32smallMean, col="red")
text(0.6, 23.18, 'Mean (small scores)', col='red')
text(0.9, 23.35, 'Mean (big scores)', col='black')

####W32 small / big comparison plot###
plot(fdsmoothbigW32)
lines(fdsmoothsmallW32, col="red")
lines(fdsmoothbigW32, col="black")
text(0.2, 23.2, 'small MFPC scores', col='red')
text(0.8, 23.4, 'big MFPC scores', col='black')
lines(W32smallMean, col="green")
lines(W32bigMean, col="yellow")
text(0.6, 23.18, 'Mean (small scores)', col='green')
text(0.9, 23.35, 'Mean (big scores)', col='orange')



##########first derivative of small and big scores
deriv1stW32big<-deriv.fd(fdsmoothbigW32)
deriv1stW32small<-deriv.fd(fdsmoothsmallW32)
plot(deriv1stW32big)
lines(deriv1stW32small,col="red")
lines(deriv1stW32big,col="black")

##########first derivative of mean functions
deriv1stW32bigMean<-deriv.fd(W32bigMean)
deriv1stW32smallMean<-deriv.fd(W32smallMean)
plot(deriv1stW32bigMean)
lines(deriv1stW32smallMean,col="red")
text(0.8, -0.2, '1st deriv Mean (smallscores)', col='red')
text(0.6, -0.5, '1st deriv Mean (bigscores)', col='black')
####1st derivatives - T24 small / big comparison plot###
plot(deriv1stW32big)
lines(deriv1stW32small,col="red")
lines(deriv1stW32big,col="black")
text(0.5, 0.5, 'negative MFPC scores', col='red')
text(0.4, -1, 'positive MFPC scores', col='black')
lines(deriv1stW32smallMean, col="green")
lines(deriv1stW32bigMean, col="yellow")
text(0.7, 0.2, '1st deriv Mean (smallscores)', col='green')
text(0.6, -1.5, '1st deriv Mean (bigscores)', col='orange')


##########second derivative of small and big scores
deriv2ndW32big<-deriv.fd(deriv1stW32big)
deriv2ndW32small<-deriv.fd(deriv1stW32small)

plot(deriv2ndW32big)
lines(deriv2ndW32small,col="red")
lines(deriv2ndW32big,col="black")

############second derivative of mean functions
deriv2ndW32bigMean<-deriv.fd(deriv1stW32bigMean)
deriv2ndW32smallMean<-deriv.fd(deriv1stW32smallMean)

plot(deriv2ndW32bigMean)
lines(deriv2ndW32smallMean,col="red")
text(0.86, -1, '2nd deriv Mean (smallscores)', col='red')
text(0.6, -2, '2nd deriv Mean (bigscores)', col='black')

####2nd derivatives - T24 small / big comparison plot###
plot(deriv2ndW32big)
lines(deriv2ndW32small,col="red")
lines(deriv2ndW32big,col="black")
text(0.5, 10, 'negative MFPC scores', col='red')
text(0.4, -10, 'positive MFPC scores', col='black')
lines(deriv2ndW32smallMean, col="green")
lines(deriv2ndW32bigMean, col="yellow")
text(0.7, 20, '2nd deriv Mean (smallscores)', col='green')
text(0.7, -15, '2nd deriv Mean (bigscores)', col='orange')

#######################MFPCA SECTION DONE HERE#####################
#######################MFPCA SECTION DONE HERE#####################
#######################MFPCA SECTION DONE HERE#####################
#######################MFPCA SECTION DONE HERE#####################
#######################MFPCA SECTION DONE HERE#####################



#DATA1<-read.csv("C:/Users/albig/Dropbox/Tesis/Cevahir/YI/train initials.csv", header = TRUE, row.names = 1)
#DATA2<-read.csv("C:/Users/albig/Dropbox/Tesis/Cevahir/YI/test initials.csv", header = TRUE, row.names = 1)
#DATA3<-read.csv("C:/Users/albig/Dropbox/Tesis/Cevahir/YI/train mfpc scores.csv", header = TRUE, row.names = 1)


DATAT24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_T24.csv", header = TRUE, row.names = 1)

x<-DATAT24$Initial.value.of.Sensor.1[DATAT24$Classification..MFPC.Scores.=="Low"]
y<-DATAT24$Initial.value.of.Sensor.1[DATAT24$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(G-F)
which((G-F)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((G-F)==J)
c=gridxy[k[1]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

T24seperation<-c

T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
T24TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
T24TESTinit[,2]<-as.vector(T24_all_train_test[101:200,2])
T24TESTinit[,1]<-seq(1:100)

T24TESTinitlow<-T24TESTinit[,1][which(T24TESTinit[,2]>T24seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
T24TESTinitbig<-T24TESTinit[,1][which(T24TESTinit[,2]<T24seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAT30<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_T30.csv", header = TRUE, row.names = 1)

x<-DATAT30$Initial.value.of.Sensor.1[DATAT30$Classification..MFPC.Scores.=="Low"]
y<-DATAT30$Initial.value.of.Sensor.1[DATAT30$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(G-F)
which((G-F)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((G-F)==J)
c=gridxy[k[20]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

T30seperation<-c

T30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/2-T30train_test_all.csv", header = TRUE,row.names = 1)
T30_all_train_test
T30_all_train_test<-as.matrix(T30_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
T30TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
T30TESTinit[,2]<-as.vector(T30_all_train_test[101:200,2])
T30TESTinit[,1]<-seq(1:100)

T30TESTinitlow<-T30TESTinit[,1][which(T30TESTinit[,2]>T30seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
T30TESTinitbig<-T30TESTinit[,1][which(T30TESTinit[,2]<T30seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAT50<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_T50.csv", header = TRUE, row.names = 1)

x<-DATAT50$Initial.value.of.Sensor.1[DATAT50$Classification..MFPC.Scores.=="Low"]
y<-DATAT50$Initial.value.of.Sensor.1[DATAT50$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(G-F)
which((G-F)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((G-F)==J)
c=gridxy[k[10]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

T50seperation<-c

T50_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/3-T50train_test_all.csv", header = TRUE,row.names = 1)
T50_all_train_test
T50_all_train_test<-as.matrix(T50_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
T50TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
T50TESTinit[,2]<-as.vector(T50_all_train_test[101:200,2])
T50TESTinit[,1]<-seq(1:100)

T50TESTinitlow<-T50TESTinit[,1][which(T50TESTinit[,2]>T50seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
T50TESTinitbig<-T50TESTinit[,1][which(T50TESTinit[,2]<T50seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAP30<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_P30.csv", header = TRUE, row.names = 1)

x<-DATAP30$Initial.value.of.Sensor.1[DATAP30$Classification..MFPC.Scores.=="Low"]
y<-DATAP30$Initial.value.of.Sensor.1[DATAP30$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(F-G)
which((F-G)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((F-G)==J)
c=gridxy[k[1]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

P30seperation<-c

P30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/4-P30train_test_all.csv", header = TRUE,row.names = 1)
P30_all_train_test
P30_all_train_test<-as.matrix(P30_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
P30TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
P30TESTinit[,2]<-as.vector(P30_all_train_test[101:200,2])
P30TESTinit[,1]<-seq(1:100)

P30TESTinitlow<-P30TESTinit[,1][which(P30TESTinit[,2]<P30seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
P30TESTinitbig<-P30TESTinit[,1][which(P30TESTinit[,2]>P30seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAps30<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_ps30.csv", header = TRUE, row.names = 1)

x<-DATAps30$Initial.value.of.Sensor.1[DATAps30$Classification..MFPC.Scores.=="Low"]
y<-DATAps30$Initial.value.of.Sensor.1[DATAps30$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(G-F)
which((G-F)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((G-F)==J)
c=gridxy[k[35]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

ps30seperation<-c

ps30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/5-ps30train_test_all.csv", header = TRUE,row.names = 1)
ps30_all_train_test
ps30_all_train_test<-as.matrix(ps30_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
ps30TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
ps30TESTinit[,2]<-as.vector(ps30_all_train_test[101:200,2])
ps30TESTinit[,1]<-seq(1:100)

ps30TESTinitlow<-ps30TESTinit[,1][which(ps30TESTinit[,2]>ps30seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
ps30TESTinitbig<-ps30TESTinit[,1][which(ps30TESTinit[,2]<ps30seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.


#############
#############
#############
#############

DATAphi<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_phi.csv", header = TRUE, row.names = 1)

x<-DATAphi$Initial.value.of.Sensor.1[DATAphi$Classification..MFPC.Scores.=="Low"]
y<-DATAphi$Initial.value.of.Sensor.1[DATAphi$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(F-G)
which((F-G)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((F-G)==J)
c=gridxy[k[5]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

phiseperation<-c

phi_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/6-phitrain_test_all.csv", header = TRUE,row.names = 1)
phi_all_train_test
phi_all_train_test<-as.matrix(phi_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
phiTESTinit<-matrix(NA, nrow = 100 , ncol = 2)
phiTESTinit[,2]<-as.vector(phi_all_train_test[101:200,2])
phiTESTinit[,1]<-seq(1:100)

phiTESTinitlow<-phiTESTinit[,1][which(phiTESTinit[,2]<phiseperation)] #test datasında albanın bulduğu low scoreları ayırıdm
phiTESTinitbig<-phiTESTinit[,1][which(phiTESTinit[,2]>phiseperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATABPR<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_BPR.csv", header = TRUE, row.names = 1)

x<-DATABPR$Initial.value.of.Sensor.1[DATABPR$Classification..MFPC.Scores.=="Low"]
y<-DATABPR$Initial.value.of.Sensor.1[DATABPR$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(G-F)
which((G-F)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((G-F)==J)
c=gridxy[k[6]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

BPRseperation<-c

BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
BPR_all_train_test
BPR_all_train_test<-as.matrix(BPR_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
BPRTESTinit<-matrix(NA, nrow = 100 , ncol = 2)
BPRTESTinit[,2]<-as.vector(BPR_all_train_test[101:200,2])
BPRTESTinit[,1]<-seq(1:100)

BPRTESTinitlow<-BPRTESTinit[,1][which(BPRTESTinit[,2]>BPRseperation)] #test datasında albanın bulduğu low scoreları ayırıdm
BPRTESTinitbig<-BPRTESTinit[,1][which(BPRTESTinit[,2]<BPRseperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAW31<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_W31.csv", header = TRUE, row.names = 1)

x<-DATAW31$Initial.value.of.Sensor.1[DATAW31$Classification..MFPC.Scores.=="Low"]
y<-DATAW31$Initial.value.of.Sensor.1[DATAW31$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(F-G)
which((F-G)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((F-G)==J)
c=gridxy[k[5]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

W31seperation<-c

W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
W31_all_train_test
W31_all_train_test<-as.matrix(W31_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
W31TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
W31TESTinit[,2]<-as.vector(W31_all_train_test[101:200,2])
W31TESTinit[,1]<-seq(1:100)

W31TESTinitlow<-W31TESTinit[,1][which(W31TESTinit[,2]<W31seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
W31TESTinitbig<-W31TESTinit[,1][which(W31TESTinit[,2]>W31seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.

#############
#############
#############
#############

DATAW32<-read.csv("C:/Users/cevah/Desktop/data_for_registration/initials/train initials_new_W32.csv", header = TRUE, row.names = 1)

x<-DATAW32$Initial.value.of.Sensor.1[DATAW32$Classification..MFPC.Scores.=="Low"]
y<-DATAW32$Initial.value.of.Sensor.1[DATAW32$Classification..MFPC.Scores.=="Big"]

#Compute the empirical distribution function for both separately in a grid:
gridxy=seq(min(x,y),max(x,y),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(x<=gridxy[i])
  G[i]=mean(y<=gridxy[i])
}

#The Youden index is the maximum difference between these two functions: J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")
J=max(F-G)
which((F-G)==J)

#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((F-G)==J)
c=gridxy[k[5]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise
abline(v=c,col="blue",lty=3)

p1=hist(x)
p2=hist(y)

plot(p1, col=rgb(0,0,1,1/4), xlim=c(min(gridxy),max(gridxy)))  # first histogram
plot(p2, col=rgb(1,0,0,1/4), xlim=c(min(gridxy),max(gridxy)), add=T)
abline(v=c,col="blue",lty=3)

W32seperation<-c

W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
W32_all_train_test
W32_all_train_test<-as.matrix(W32_all_train_test)
#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
W32TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
W32TESTinit[,2]<-as.vector(W32_all_train_test[101:200,2])
W32TESTinit[,1]<-seq(1:100)

W32TESTinitlow<-W32TESTinit[,1][which(W32TESTinit[,2]<W32seperation)] #test datasında albanın bulduğu low scoreları ayırıdm
W32TESTinitbig<-W32TESTinit[,1][which(W32TESTinit[,2]>W32seperation)] #test datasında albanın bulduğu high scoreları ayırıdm.


T24TESTinitlow
T30TESTinitlow
T50TESTinitlow
P30TESTinitlow
ps30TESTinitlow
phiTESTinitlow
BPRTESTinitlow
W31TESTinitlow
W32TESTinitlow

T24TESTinitbig
T30TESTinitbig
T50TESTinitbig
P30TESTinitbig
ps30TESTinitbig
phiTESTinitbig
BPRTESTinitbig
W31TESTinitbig
W32TESTinitbig

allclassified<- matrix(NA,  ncol= 9, nrow = 100)
###############

for (i in 1:100) 
{
  
  if(T24TESTinit[i,2]>T24seperation)
  {allclassified[i,1]<- "low"}
  else{allclassified[i,1]<- "big"
  }
  if(T30TESTinit[i,2]>T30seperation)
  {allclassified[i,2]<- "low"}
  else{allclassified[i,2]<- "big"
  }
  if(T50TESTinit[i,2]>T50seperation)
  {allclassified[i,3]<- "low"}
  else{allclassified[i,3]<- "big"
  }
  if(P30TESTinit[i,2]<P30seperation)
  {allclassified[i,4]<- "low"}
  else{allclassified[i,4]<- "big"
  }
  if(ps30TESTinit[i,2]>ps30seperation)
  {allclassified[i,5]<- "low"}
  else{allclassified[i,5]<- "big"
  }
  if(phiTESTinit[i,2]<phiseperation)
  {allclassified[i,6]<- "low"}
  else{allclassified[i,6]<- "big"
  }
  if(BPRTESTinit[i,2]>BPRseperation)
  {allclassified[i,7]<- "low"}
  else{allclassified[i,7]<- "big"
  }
  if(W31TESTinit[i,2]<W31seperation)
  {allclassified[i,8]<- "low"}
  else{allclassified[i,8]<- "big"
  }
  if(W32TESTinit[i,2]<W32seperation)
  {allclassified[i,9]<- "low"}
  else{allclassified[i,9]<- "big"
  }
}


TESTclassified<- matrix(NA,  ncol= 2, nrow = 100)
TESTclassified[,1]<-seq(1:100)
for (i in 1:100) {
  if(length(allclassified[i,][which(allclassified[i,]=="low")])>4)
  {TESTclassified[i,2]<-"low"}
  else{TESTclassified[i,2]<-"big"}
}

TestLowClassEng<-as.numeric(TESTclassified[,1][which(TESTclassified[,2]=="low")])
TestBigClassEng<-as.numeric(TESTclassified[,1][which(TESTclassified[,2]=="big")])

######
TESTclassifieddeneme<- matrix(NA,  ncol= 2, nrow = 100)
TESTclassifieddeneme[,1]<-seq(1:100)
for (i in 1:100) {
  if(length(allclassified[i,][which(allclassified[i,]=="low")])>5)
  {TESTclassifieddeneme[i,2]<-"low"}
  else if(length(allclassified[i,][which(allclassified[i,]=="big")])>5)
  {TESTclassifieddeneme[i,2]<-"big"}
  else {TESTclassifieddeneme[i,2]<-"5 yada 4"}
}

TestLowClassEngdeneme<-as.numeric(TESTclassified[,1][which(TESTclassifieddeneme[,2]=="low")])
TestBigClassEngdeneme<-as.numeric(TESTclassified[,1][which(TESTclassifieddeneme[,2]=="big")])

length(TestLowClassEngdeneme)
length(TestBigClassEngdeneme)
length(TestLowClassEng)
length(TestBigClassEng)
#TestBigClassEng<-TestBigClassEngdeneme
#TestLowClassEng<-TestLowClassEngdeneme
#######

##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################
##############TEST DATA CLASSIFICATION DONE HERE (YOUDEN INDEX)  ################




###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION#########################


T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)
T24_all_train_test[120,66:185]<-NA
T24_all_train_test[131,71:197]<-NA
T24_all_train_test[134,73:204]<-NA
T24_all_train_test[135,71:199]<-NA
T24_all_train_test[142,56:157]<-NA
T24_all_train_test[149,108:304]<-NA
T24_all_train_test[168,67:188]<-NA
T24_all_train_test[176,74:206]<-NA
T24_all_train_test[181,77:214]<-NA
T24_all_train_test[182,59:163]<-NA


T30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/2-T30train_test_all.csv", header = TRUE,row.names = 1)
T30_all_train_test
T30_all_train_test<-as.matrix(T30_all_train_test)
T30_all_train_test[120,66:185]<-NA
T30_all_train_test[131,71:197]<-NA
T30_all_train_test[134,73:204]<-NA
T30_all_train_test[135,71:199]<-NA
T30_all_train_test[142,56:157]<-NA
T30_all_train_test[149,108:304]<-NA
T30_all_train_test[168,67:188]<-NA
T30_all_train_test[176,74:206]<-NA
T30_all_train_test[181,77:214]<-NA
T30_all_train_test[182,59:163]<-NA


T50_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/3-T50train_test_all.csv", header = TRUE,row.names = 1)
T50_all_train_test
T50_all_train_test<-as.matrix(T50_all_train_test)
T50_all_train_test[120,66:185]<-NA
T50_all_train_test[131,71:197]<-NA
T50_all_train_test[134,73:204]<-NA
T50_all_train_test[135,71:199]<-NA
T50_all_train_test[142,56:157]<-NA
T50_all_train_test[149,108:304]<-NA
T50_all_train_test[168,67:188]<-NA
T50_all_train_test[176,74:206]<-NA
T50_all_train_test[181,77:214]<-NA
T50_all_train_test[182,59:163]<-NA


P30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/4-P30train_test_all.csv", header = TRUE,row.names = 1)
P30_all_train_test
P30_all_train_test<-as.matrix(P30_all_train_test)
P30_all_train_test[120,66:185]<-NA
P30_all_train_test[131,71:197]<-NA
P30_all_train_test[134,73:204]<-NA
P30_all_train_test[135,71:199]<-NA
P30_all_train_test[142,56:157]<-NA
P30_all_train_test[149,108:304]<-NA
P30_all_train_test[168,67:188]<-NA
P30_all_train_test[176,74:206]<-NA
P30_all_train_test[181,77:214]<-NA
P30_all_train_test[182,59:163]<-NA


ps30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/5-ps30train_test_all.csv", header = TRUE,row.names = 1)
ps30_all_train_test
ps30_all_train_test<-as.matrix(ps30_all_train_test)
ps30_all_train_test[120,66:185]<-NA
ps30_all_train_test[131,71:197]<-NA
ps30_all_train_test[134,73:204]<-NA
ps30_all_train_test[135,71:199]<-NA
ps30_all_train_test[142,56:157]<-NA
ps30_all_train_test[149,108:304]<-NA
ps30_all_train_test[168,67:188]<-NA
ps30_all_train_test[176,74:206]<-NA
ps30_all_train_test[181,77:214]<-NA
ps30_all_train_test[182,59:163]<-NA


phi_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/6-phitrain_test_all.csv", header = TRUE,row.names = 1)
phi_all_train_test
phi_all_train_test<-as.matrix(phi_all_train_test)
phi_all_train_test[120,66:185]<-NA
phi_all_train_test[131,71:197]<-NA
phi_all_train_test[134,73:204]<-NA
phi_all_train_test[135,71:199]<-NA
phi_all_train_test[142,56:157]<-NA
phi_all_train_test[149,108:304]<-NA
phi_all_train_test[168,67:188]<-NA
phi_all_train_test[176,74:206]<-NA
phi_all_train_test[181,77:214]<-NA
phi_all_train_test[182,59:163]<-NA


BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
BPR_all_train_test
BPR_all_train_test<-as.matrix(BPR_all_train_test)
BPR_all_train_test[120,66:185]<-NA
BPR_all_train_test[131,71:197]<-NA
BPR_all_train_test[134,73:204]<-NA
BPR_all_train_test[135,71:199]<-NA
BPR_all_train_test[142,56:157]<-NA
BPR_all_train_test[149,108:304]<-NA
BPR_all_train_test[168,67:188]<-NA
BPR_all_train_test[176,74:206]<-NA
BPR_all_train_test[181,77:214]<-NA
BPR_all_train_test[182,59:163]<-NA


W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
W31_all_train_test
W31_all_train_test<-as.matrix(W31_all_train_test)
W31_all_train_test[120,66:185]<-NA
W31_all_train_test[131,71:197]<-NA
W31_all_train_test[134,73:204]<-NA
W31_all_train_test[135,71:199]<-NA
W31_all_train_test[142,56:157]<-NA
W31_all_train_test[149,108:304]<-NA
W31_all_train_test[168,67:188]<-NA
W31_all_train_test[176,74:206]<-NA
W31_all_train_test[181,77:214]<-NA
W31_all_train_test[182,59:163]<-NA


W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
W32_all_train_test
W32_all_train_test<-as.matrix(W32_all_train_test)
W32_all_train_test[120,66:185]<-NA
W32_all_train_test[131,71:197]<-NA
W32_all_train_test[134,73:204]<-NA
W32_all_train_test[135,71:199]<-NA
W32_all_train_test[142,56:157]<-NA
W32_all_train_test[149,108:304]<-NA
W32_all_train_test[168,67:188]<-NA
W32_all_train_test[176,74:206]<-NA
W32_all_train_test[181,77:214]<-NA
W32_all_train_test[182,59:163]<-NA



#### list for 100 test engine - all 101 x number of observation matrix


#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix[[i]]<-T24_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
T24TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
T24TrainScoresmatrix<-matrix(NA,100,3)
T24TrainScoresmatrix[,1]<-seq(1:100)
T24TrainScoresmatrix[,2]<-T24TrainScores[,1]
T24TrainScoresmatrix[,3]<-T24TrainScores[,2]

TrainLowScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_T24_SCORE<-list()
for (i in TestLowClassEng)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_T24_SCORE[[i]]<-T24_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_T24_SCORE[[i]]<-T24_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T24_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_T24_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainLowScores)+1)) {
    T24_test_matrix[j,]<-T24_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_T24_SCORE[[i]])[1,])]
    list_test_matrix_T24_SCORE[[i]][j,]<-T24_test_matrix[j,]
  }
  list_test_matrix_T24_SCORE[[i]][(length(TrainLowScores)+1),]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T24_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_T24_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainBigScores)+1)) {
    T24_test_matrix[j,]<-T24_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_T24_SCORE[[i]])[1,])]
    list_test_matrix_T24_SCORE[[i]][j,]<-T24_test_matrix[j,]
  }
  list_test_matrix_T24_SCORE[[i]][(length(TrainBigScores)+1),]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_T24_SCORE[[4]]
test3<-list_test_matrix_T24_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_T24_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(list_test_matrix_T24_SCORE[[i]][(length(TrainLowScores)+1),])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_T24_SCORE[[i]])
  list_test_matrix_SCORE_T24_noNA[[i]]<-noNA_test_matrix_T24
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(list_test_matrix_T24_SCORE[[i]][(length(TrainBigScores)+1),])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_T24_SCORE[[i]])
  list_test_matrix_SCORE_T24_noNA[[i]]<-noNA_test_matrix_T24
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_T24_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_T24_noNA[[i]]<-t(list_test_matrix_SCORE_T24_noNA[[i]])
#}

list_test_matrix_t_SCORE_T24_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T24_noNA[[i]]<-t(list_test_matrix_SCORE_T24_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T24_noNA[[i]]<-t(list_test_matrix_SCORE_T24_noNA[[i]])
}


######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_T24_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T24_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T24_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T24_Big_scores[[j]]<-list_test_smoothing_T24_Big_scores
}


list_test_all_smooth_T24_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T24_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T24_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T24_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T24_Low_scores[[j]]<-list_test_smoothing_T24_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=7

#lows cutted
plot(list_test_all_smooth_T24_Low_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T24_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_T24_Big_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T24_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_SCORE_T24_noNA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_SCORE_T24_noNA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T24_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_T24_noNA[[j]])

FUNDATAT24LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAT24LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T24_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T24_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT24LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT24LIST[[j]]<-FUNDATAT24LIST_LIST
}

FUNDATAT24LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT24LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T24_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T24_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT24LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT24LIST_BIG[[j]]<-FUNDATAT24LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_T24_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_T24_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(T24_all_train_test)

vectorT24<-as.vector(T24_all_train_test[101:200,2:363])
vectorT24<-na.omit(vectorT24)
mean(na.omit(vectorT24))
meanT24<-mean(na.omit(vectorT24))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T24_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_T24_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]])) {
    distancematrixT24reg[i,]<-FUNDATAT24LIST_BIG[[j]][[i]]@X/meanT24
  }
  list_for_distance_T24_reg_big_scores[[j]]<-distancematrixT24reg
}

list_for_distance_T24_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_T24_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T24_noNA[[j]])) {
    distancematrixT24reg[i,]<-FUNDATAT24LIST[[j]][[i]]@X/meanT24
  }
  list_for_distance_T24_reg_low_scores[[j]]<-distancematrixT24reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T24_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T24_reg_low_scores[[i]], method = "euclidean" )
  list_distance_T24_reg_low_scores[[i]]<-DISTANCE
}

list_distance_T24_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T24_reg_big_scores[[i]], method = "euclidean" )
  list_distance_T24_reg_big_scores[[i]]<-DISTANCE
}


###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T24 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################


###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
# T30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/2-T30train_test_all.csv", header = TRUE,row.names = 1)
# T30_all_train_test
# T30_all_train_test<-as.matrix(T30_all_train_test)
# 

#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  T30_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(T30_all_train_test[100+i,])))
  list_test_matrix[[i]]<-T30_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
T30TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
T30TrainScoresmatrix<-matrix(NA,100,3)
T30TrainScoresmatrix[,1]<-seq(1:100)
T30TrainScoresmatrix[,2]<-T30TrainScores[,1]
T30TrainScoresmatrix[,3]<-T30TrainScores[,2]

TrainLowScores<-as.numeric(T30TrainScoresmatrix[,1][which(T30TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(T30TrainScoresmatrix[,1][which(T30TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_T30_SCORE<-list()
for (i in TestLowClassEng)
{ 
  T30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(T30_all_train_test[100+i,])))
  list_test_matrix_T30_SCORE[[i]]<-T30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(T30_all_train_test[100+i,])))
  list_test_matrix_T30_SCORE[[i]]<-T30_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_T30_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainLowScores)+1)) {
    T30_test_matrix[j,]<-T30_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_T30_SCORE[[i]])[1,])]
    list_test_matrix_T30_SCORE[[i]][j,]<-T30_test_matrix[j,]
  }
  list_test_matrix_T30_SCORE[[i]][(length(TrainLowScores)+1),]<-T30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_T30_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainBigScores)+1)) {
    T30_test_matrix[j,]<-T30_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_T30_SCORE[[i]])[1,])]
    list_test_matrix_T30_SCORE[[i]][j,]<-T30_test_matrix[j,]
  }
  list_test_matrix_T30_SCORE[[i]][(length(TrainBigScores)+1),]<-T30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_T30_SCORE[[4]]
test3<-list_test_matrix_T30_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_T30_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T30 <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(list_test_matrix_T30_SCORE[[i]][(length(TrainLowScores)+1),])))
  noNA_test_matrix_T30 <-na.omit(list_test_matrix_T30_SCORE[[i]])
  list_test_matrix_SCORE_T30_noNA[[i]]<-noNA_test_matrix_T30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T30 <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(list_test_matrix_T30_SCORE[[i]][(length(TrainBigScores)+1),])))
  noNA_test_matrix_T30 <-na.omit(list_test_matrix_T30_SCORE[[i]])
  list_test_matrix_SCORE_T30_noNA[[i]]<-noNA_test_matrix_T30
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_T30_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_T30_noNA[[i]]<-t(list_test_matrix_SCORE_T30_noNA[[i]])
#}

list_test_matrix_t_SCORE_T30_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T30_noNA[[i]]<-t(list_test_matrix_SCORE_T30_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T30_noNA[[i]]<-t(list_test_matrix_SCORE_T30_noNA[[i]])
}


######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_T30_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T30_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T30_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T30_Big_scores[[j]]<-list_test_smoothing_T30_Big_scores
}


list_test_all_smooth_T30_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T30_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T30_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T30_Low_scores[[j]]<-list_test_smoothing_T30_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=7

#lows cutted
plot(list_test_all_smooth_T30_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(1580,1610))
for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T30_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_T30_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(1580,1610))
for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T30_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_T30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610))
for (i in as.vector(list_test_matrix_SCORE_T30_noNA[[testengine]][,1])) {
  lines(listeT30[[i]])
} 
lines(list_test_all_smooth_T30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_T30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610))
for (i in as.vector(list_test_matrix_SCORE_T30_noNA[[testengine]][,1])) {
  lines(listeT30[[i]])
} 
lines(list_test_all_smooth_T30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T30_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_T30_noNA[[j]])

FUNDATAT30LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAT30LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T30_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T30_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT30LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT30LIST[[j]]<-FUNDATAT30LIST_LIST
}

FUNDATAT30LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT30LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T30_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T30_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT30LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT30LIST_BIG[[j]]<-FUNDATAT30LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_T30_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_T30_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(T30_all_train_test)

vectorT30<-as.vector(T30_all_train_test[101:200,2:363])
vectorT30<-na.omit(vectorT30)
mean(na.omit(vectorT30))
meanT30<-mean(na.omit(vectorT30))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T30_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixT30reg<-matrix(NA,nrow(list_test_matrix_SCORE_T30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]])) {
    distancematrixT30reg[i,]<-FUNDATAT30LIST_BIG[[j]][[i]]@X/meanT30
  }
  list_for_distance_T30_reg_big_scores[[j]]<-distancematrixT30reg
}

list_for_distance_T30_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixT30reg<-matrix(NA,nrow(list_test_matrix_SCORE_T30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T30_noNA[[j]])) {
    distancematrixT30reg[i,]<-FUNDATAT30LIST[[j]][[i]]@X/meanT30
  }
  list_for_distance_T30_reg_low_scores[[j]]<-distancematrixT30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T30_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T30_reg_low_scores[[i]], method = "euclidean" )
  list_distance_T30_reg_low_scores[[i]]<-DISTANCE
}

list_distance_T30_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T30_reg_big_scores[[i]], method = "euclidean" )
  list_distance_T30_reg_big_scores[[i]]<-DISTANCE
}

###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################



###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################

# T50_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/3-T50train_test_all.csv", header = TRUE,row.names = 1)
# T50_all_train_test
# T50_all_train_test<-as.matrix(T50_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  T50_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(T50_all_train_test[100+i,])))
  list_test_matrix[[i]]<-T50_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
T50TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
T50TrainScoresmatrix<-matrix(NA,100,3)
T50TrainScoresmatrix[,1]<-seq(1:100)
T50TrainScoresmatrix[,2]<-T50TrainScores[,1]
T50TrainScoresmatrix[,3]<-T50TrainScores[,2]

TrainLowScores<-as.numeric(T50TrainScoresmatrix[,1][which(T50TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(T50TrainScoresmatrix[,1][which(T50TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_T50_SCORE<-list()
for (i in TestLowClassEng)
{ 
  T50_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(T50_all_train_test[100+i,])))
  list_test_matrix_T50_SCORE[[i]]<-T50_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  T50_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(T50_all_train_test[100+i,])))
  list_test_matrix_T50_SCORE[[i]]<-T50_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  T50_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_T50_SCORE[[i]])[1,]))
  for (j in 1:66) {
    T50_test_matrix[j,]<-T50_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_T50_SCORE[[i]])[1,])]
    list_test_matrix_T50_SCORE[[i]][j,]<-T50_test_matrix[j,]
  }
  list_test_matrix_T50_SCORE[[i]][66,]<-T50_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  T50_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_T50_SCORE[[i]])[1,]))
  for (j in 1:36) {
    T50_test_matrix[j,]<-T50_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_T50_SCORE[[i]])[1,])]
    list_test_matrix_T50_SCORE[[i]][j,]<-T50_test_matrix[j,]
  }
  list_test_matrix_T50_SCORE[[i]][36,]<-T50_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_T50_SCORE[[4]]
test3<-list_test_matrix_T50_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_T50_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_T50 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_T50_SCORE[[i]][66,])))
  noNA_test_matrix_T50 <-na.omit(list_test_matrix_T50_SCORE[[i]])
  list_test_matrix_SCORE_T50_noNA[[i]]<-noNA_test_matrix_T50
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_T50 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_T50_SCORE[[i]][36,])))
  noNA_test_matrix_T50 <-na.omit(list_test_matrix_T50_SCORE[[i]])
  list_test_matrix_SCORE_T50_noNA[[i]]<-noNA_test_matrix_T50
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_T50_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_T50_noNA[[i]]<-t(list_test_matrix_SCORE_T50_noNA[[i]])
#}

list_test_matrix_t_SCORE_T50_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_T50_noNA[[i]]<-t(list_test_matrix_SCORE_T50_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_T50_noNA[[i]]<-t(list_test_matrix_SCORE_T50_noNA[[i]])
}




######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_T50_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_T50_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T50_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T50_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T50_Big_scores[[j]]<-list_test_smoothing_T50_Big_scores
}


list_test_all_smooth_T50_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_T50_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_T50_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_T50_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T50_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T50_Low_scores[[j]]<-list_test_smoothing_T50_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=7

#lows cutted
plot(list_test_all_smooth_T50_Low_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(1390,1435))
for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T50_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T50_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_T50_Big_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(1390,1435))
for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T50_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_T50_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_T50_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]],xlim=c(0,330), ylim=c(1390,1435))
for (i in as.vector(list_test_matrix_SCORE_T50_noNA[[testengine]][,1])) {
  lines(listeT50[[i]])
} 
lines(list_test_all_smooth_T50_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_T50_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]],xlim=c(0,330), ylim=c(1390,1435))
for (i in as.vector(list_test_matrix_SCORE_T50_noNA[[testengine]][,1])) {
  lines(listeT50[[i]])
} 
lines(list_test_all_smooth_T50_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_T50_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_T50_noNA[[j]])

FUNDATAT50LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAT50LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T50_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T50_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT50LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAT50LIST[[j]]<-FUNDATAT50LIST_LIST
}

FUNDATAT50LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAT50LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_T50_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_T50_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAT50LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAT50LIST_BIG[[j]]<-FUNDATAT50LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_T50_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_T50_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(T50_all_train_test)

vectorT50<-as.vector(T50_all_train_test[101:200,2:363])
vectorT50<-na.omit(vectorT50)
mean(na.omit(vectorT50))
meanT50<-mean(na.omit(vectorT50))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_T50_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixT50reg<-matrix(NA,nrow(list_test_matrix_SCORE_T50_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]])) {
    distancematrixT50reg[i,]<-FUNDATAT50LIST_BIG[[j]][[i]]@X/meanT50
  }
  list_for_distance_T50_reg_big_scores[[j]]<-distancematrixT50reg
}

list_for_distance_T50_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixT50reg<-matrix(NA,nrow(list_test_matrix_SCORE_T50_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_T50_noNA[[j]])) {
    distancematrixT50reg[i,]<-FUNDATAT50LIST[[j]][[i]]@X/meanT50
  }
  list_for_distance_T50_reg_low_scores[[j]]<-distancematrixT50reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_T50_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_T50_reg_low_scores[[i]], method = "euclidean" )
  list_distance_T50_reg_low_scores[[i]]<-DISTANCE
}

list_distance_T50_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_T50_reg_big_scores[[i]], method = "euclidean" )
  list_distance_T50_reg_big_scores[[i]]<-DISTANCE
}


###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################T50 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################


###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################


# P30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/4-P30train_test_all.csv", header = TRUE,row.names = 1)
# P30_all_train_test
# P30_all_train_test<-as.matrix(P30_all_train_test)



#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  P30_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(P30_all_train_test[100+i,])))
  list_test_matrix[[i]]<-P30_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
P30TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
P30TrainScoresmatrix<-matrix(NA,100,3)
P30TrainScoresmatrix[,1]<-seq(1:100)
P30TrainScoresmatrix[,2]<-P30TrainScores[,1]
P30TrainScoresmatrix[,3]<-P30TrainScores[,2]

TrainLowScores<-as.numeric(P30TrainScoresmatrix[,1][which(P30TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(P30TrainScoresmatrix[,1][which(P30TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_P30_SCORE<-list()
for (i in TestLowClassEng)
{ 
  P30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(P30_all_train_test[100+i,])))
  list_test_matrix_P30_SCORE[[i]]<-P30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  P30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(P30_all_train_test[100+i,])))
  list_test_matrix_P30_SCORE[[i]]<-P30_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  P30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_P30_SCORE[[i]])[1,]))
  for (j in 1:66) {
    P30_test_matrix[j,]<-P30_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_P30_SCORE[[i]])[1,])]
    list_test_matrix_P30_SCORE[[i]][j,]<-P30_test_matrix[j,]
  }
  list_test_matrix_P30_SCORE[[i]][66,]<-P30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  P30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_P30_SCORE[[i]])[1,]))
  for (j in 1:36) {
    P30_test_matrix[j,]<-P30_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_P30_SCORE[[i]])[1,])]
    list_test_matrix_P30_SCORE[[i]][j,]<-P30_test_matrix[j,]
  }
  list_test_matrix_P30_SCORE[[i]][36,]<-P30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_P30_SCORE[[4]]
test3<-list_test_matrix_P30_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_P30_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_P30 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_P30_SCORE[[i]][66,])))
  noNA_test_matrix_P30 <-na.omit(list_test_matrix_P30_SCORE[[i]])
  list_test_matrix_SCORE_P30_noNA[[i]]<-noNA_test_matrix_P30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_P30 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_P30_SCORE[[i]][36,])))
  noNA_test_matrix_P30 <-na.omit(list_test_matrix_P30_SCORE[[i]])
  list_test_matrix_SCORE_P30_noNA[[i]]<-noNA_test_matrix_P30
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_P30_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_P30_noNA[[i]]<-t(list_test_matrix_SCORE_P30_noNA[[i]])
#}

list_test_matrix_t_SCORE_P30_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_P30_noNA[[i]]<-t(list_test_matrix_SCORE_P30_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_P30_noNA[[i]]<-t(list_test_matrix_SCORE_P30_noNA[[i]])
}




######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_P30_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_P30_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_P30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_P30_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_P30_Big_scores[[j]]<-list_test_smoothing_P30_Big_scores
}


list_test_all_smooth_P30_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_P30_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_P30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_P30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_P30_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_P30_Low_scores[[j]]<-list_test_smoothing_P30_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=7

#lows cutted
plot(list_test_all_smooth_P30_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(550.5,555.2))
for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_P30_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_P30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_P30_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(550.5,555.2))
for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_P30_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_P30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_P30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2))
for (i in as.vector(list_test_matrix_SCORE_P30_noNA[[testengine]][,1])) {
  lines(listeP30[[i]])
} 
lines(list_test_all_smooth_P30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_P30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2))
for (i in as.vector(list_test_matrix_SCORE_P30_noNA[[testengine]][,1])) {
  lines(listeP30[[i]])
} 
lines(list_test_all_smooth_P30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_P30_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_P30_noNA[[j]])

FUNDATAP30LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAP30LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_P30_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_P30_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAP30LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAP30LIST[[j]]<-FUNDATAP30LIST_LIST
}

FUNDATAP30LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAP30LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_P30_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_P30_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAP30LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAP30LIST_BIG[[j]]<-FUNDATAP30LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_P30_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_P30_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(P30_all_train_test)

vectorP30<-as.vector(P30_all_train_test[101:200,2:363])
vectorP30<-na.omit(vectorP30)
mean(na.omit(vectorP30))
meanP30<-mean(na.omit(vectorP30))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_P30_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixP30reg<-matrix(NA,nrow(list_test_matrix_SCORE_P30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]])) {
    distancematrixP30reg[i,]<-FUNDATAP30LIST_BIG[[j]][[i]]@X/meanP30
  }
  list_for_distance_P30_reg_big_scores[[j]]<-distancematrixP30reg
}

list_for_distance_P30_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixP30reg<-matrix(NA,nrow(list_test_matrix_SCORE_P30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_P30_noNA[[j]])) {
    distancematrixP30reg[i,]<-FUNDATAP30LIST[[j]][[i]]@X/meanP30
  }
  list_for_distance_P30_reg_low_scores[[j]]<-distancematrixP30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_P30_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_P30_reg_low_scores[[i]], method = "euclidean" )
  list_distance_P30_reg_low_scores[[i]]<-DISTANCE
}

list_distance_P30_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_P30_reg_big_scores[[i]], method = "euclidean" )
  list_distance_P30_reg_big_scores[[i]]<-DISTANCE
}


###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################P30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################

# 
# ps30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/5-ps30train_test_all.csv", header = TRUE,row.names = 1)
# ps30_all_train_test
# ps30_all_train_test<-as.matrix(ps30_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  ps30_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(ps30_all_train_test[100+i,])))
  list_test_matrix[[i]]<-ps30_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
ps30TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
ps30TrainScoresmatrix<-matrix(NA,100,3)
ps30TrainScoresmatrix[,1]<-seq(1:100)
ps30TrainScoresmatrix[,2]<-ps30TrainScores[,1]
ps30TrainScoresmatrix[,3]<-ps30TrainScores[,2]

TrainLowScores<-as.numeric(ps30TrainScoresmatrix[,1][which(ps30TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(ps30TrainScoresmatrix[,1][which(ps30TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_ps30_SCORE<-list()
for (i in TestLowClassEng)
{ 
  ps30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(ps30_all_train_test[100+i,])))
  list_test_matrix_ps30_SCORE[[i]]<-ps30_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  ps30_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(ps30_all_train_test[100+i,])))
  list_test_matrix_ps30_SCORE[[i]]<-ps30_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  ps30_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_ps30_SCORE[[i]])[1,]))
  for (j in 1:66) {
    ps30_test_matrix[j,]<-ps30_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_ps30_SCORE[[i]])[1,])]
    list_test_matrix_ps30_SCORE[[i]][j,]<-ps30_test_matrix[j,]
  }
  list_test_matrix_ps30_SCORE[[i]][66,]<-ps30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  ps30_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_ps30_SCORE[[i]])[1,]))
  for (j in 1:36) {
    ps30_test_matrix[j,]<-ps30_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_ps30_SCORE[[i]])[1,])]
    list_test_matrix_ps30_SCORE[[i]][j,]<-ps30_test_matrix[j,]
  }
  list_test_matrix_ps30_SCORE[[i]][36,]<-ps30_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_ps30_SCORE[[4]]
test3<-list_test_matrix_ps30_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_ps30_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_ps30 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_ps30_SCORE[[i]][66,])))
  noNA_test_matrix_ps30 <-na.omit(list_test_matrix_ps30_SCORE[[i]])
  list_test_matrix_SCORE_ps30_noNA[[i]]<-noNA_test_matrix_ps30
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_ps30 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_ps30_SCORE[[i]][36,])))
  noNA_test_matrix_ps30 <-na.omit(list_test_matrix_ps30_SCORE[[i]])
  list_test_matrix_SCORE_ps30_noNA[[i]]<-noNA_test_matrix_ps30
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_ps30_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_ps30_noNA[[i]]<-t(list_test_matrix_SCORE_ps30_noNA[[i]])
#}

list_test_matrix_t_SCORE_ps30_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_ps30_noNA[[i]]<-t(list_test_matrix_SCORE_ps30_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_ps30_noNA[[i]]<-t(list_test_matrix_SCORE_ps30_noNA[[i]])
}




######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_ps30_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_ps30_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_ps30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_ps30_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_ps30_Big_scores[[j]]<-list_test_smoothing_ps30_Big_scores
}


list_test_all_smooth_ps30_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_ps30_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_ps30_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_ps30_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_ps30_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_ps30_Low_scores[[j]]<-list_test_smoothing_ps30_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=8

#lows cutted
plot(list_test_all_smooth_ps30_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(47.05,48.4))
for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_ps30_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_ps30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_ps30_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(47.05,48.4))
for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]]))
{
  lines(list_test_all_smooth_ps30_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_ps30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_ps30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4))
for (i in as.vector(list_test_matrix_SCORE_ps30_noNA[[testengine]][,1])) {
  lines(listeps30[[i]])
} 
lines(list_test_all_smooth_ps30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_ps30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4))
for (i in as.vector(list_test_matrix_SCORE_ps30_noNA[[testengine]][,1])) {
  lines(listeps30[[i]])
} 
lines(list_test_all_smooth_ps30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_ps30_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]])

FUNDATAps30LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAps30LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_ps30_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_ps30_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAps30LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAps30LIST[[j]]<-FUNDATAps30LIST_LIST
}

FUNDATAps30LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAps30LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_ps30_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_ps30_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAps30LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAps30LIST_BIG[[j]]<-FUNDATAps30LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_ps30_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_ps30_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(ps30_all_train_test)

vectorps30<-as.vector(ps30_all_train_test[101:200,2:363])
vectorps30<-na.omit(vectorps30)
mean(na.omit(vectorps30))
meanps30<-mean(na.omit(vectorps30))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_ps30_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixps30reg<-matrix(NA,nrow(list_test_matrix_SCORE_ps30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]])) {
    distancematrixps30reg[i,]<-FUNDATAps30LIST_BIG[[j]][[i]]@X/meanps30
  }
  list_for_distance_ps30_reg_big_scores[[j]]<-distancematrixps30reg
}

list_for_distance_ps30_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixps30reg<-matrix(NA,nrow(list_test_matrix_SCORE_ps30_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_ps30_noNA[[j]])) {
    distancematrixps30reg[i,]<-FUNDATAps30LIST[[j]][[i]]@X/meanps30
  }
  list_for_distance_ps30_reg_low_scores[[j]]<-distancematrixps30reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_ps30_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_ps30_reg_low_scores[[i]], method = "euclidean" )
  list_distance_ps30_reg_low_scores[[i]]<-DISTANCE
}

list_distance_ps30_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_ps30_reg_big_scores[[i]], method = "euclidean" )
  list_distance_ps30_reg_big_scores[[i]]<-DISTANCE
}



###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################ps30 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################

# 
# phi_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/6-phitrain_test_all.csv", header = TRUE,row.names = 1)
# phi_all_train_test
# phi_all_train_test<-as.matrix(phi_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix

#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  phi_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(phi_all_train_test[100+i,])))
  list_test_matrix[[i]]<-phi_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
phiTrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
phiTrainScoresmatrix<-matrix(NA,100,3)
phiTrainScoresmatrix[,1]<-seq(1:100)
phiTrainScoresmatrix[,2]<-phiTrainScores[,1]
phiTrainScoresmatrix[,3]<-phiTrainScores[,2]

TrainLowScores<-as.numeric(phiTrainScoresmatrix[,1][which(phiTrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(phiTrainScoresmatrix[,1][which(phiTrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_phi_SCORE<-list()
for (i in TestLowClassEng)
{ 
  phi_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(phi_all_train_test[100+i,])))
  list_test_matrix_phi_SCORE[[i]]<-phi_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  phi_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(phi_all_train_test[100+i,])))
  list_test_matrix_phi_SCORE[[i]]<-phi_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  phi_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_phi_SCORE[[i]])[1,]))
  for (j in 1:66) {
    phi_test_matrix[j,]<-phi_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_phi_SCORE[[i]])[1,])]
    list_test_matrix_phi_SCORE[[i]][j,]<-phi_test_matrix[j,]
  }
  list_test_matrix_phi_SCORE[[i]][66,]<-phi_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  phi_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_phi_SCORE[[i]])[1,]))
  for (j in 1:36) {
    phi_test_matrix[j,]<-phi_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_phi_SCORE[[i]])[1,])]
    list_test_matrix_phi_SCORE[[i]][j,]<-phi_test_matrix[j,]
  }
  list_test_matrix_phi_SCORE[[i]][36,]<-phi_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_phi_SCORE[[4]]
test3<-list_test_matrix_phi_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_phi_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_phi <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_phi_SCORE[[i]][66,])))
  noNA_test_matrix_phi <-na.omit(list_test_matrix_phi_SCORE[[i]])
  list_test_matrix_SCORE_phi_noNA[[i]]<-noNA_test_matrix_phi
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_phi <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_phi_SCORE[[i]][36,])))
  noNA_test_matrix_phi <-na.omit(list_test_matrix_phi_SCORE[[i]])
  list_test_matrix_SCORE_phi_noNA[[i]]<-noNA_test_matrix_phi
}
#####list with transpose of each matrix

list_test_matrix_t_SCORE_phi_noNA<-list()
for (i in 1:100)
{ 
  list_test_matrix_t_SCORE_phi_noNA[[i]]<-t(list_test_matrix_SCORE_phi_noNA[[i]])
}



######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_phi_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_phi_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_phi_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_phi_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_phi_Big_scores[[j]]<-list_test_smoothing_phi_Big_scores
}


list_test_all_smooth_phi_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_phi_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_phi_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_phi_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_phi_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_phi_Low_scores[[j]]<-list_test_smoothing_phi_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=8

#lows cutted
plot(list_test_all_smooth_phi_Low_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(519.1,522.8))
for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[testengine]]))
{
  lines(list_test_all_smooth_phi_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_phi_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_phi_Big_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(519.1,522.8))
for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[testengine]]))
{
  lines(list_test_all_smooth_phi_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_phi_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_phi_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]],xlim=c(0,330), ylim=c(519.1,522.8))
for (i in as.vector(list_test_matrix_SCORE_phi_noNA[[testengine]][,1])) {
  lines(listephi[[i]])
} 
lines(list_test_all_smooth_phi_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_phi_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]],xlim=c(0,330), ylim=c(519.1,522.8))
for (i in as.vector(list_test_matrix_SCORE_phi_noNA[[testengine]][,1])) {
  lines(listephi[[i]])
} 
lines(list_test_all_smooth_phi_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_phi_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_phi_noNA[[j]])

FUNDATAphiLIST<-list()
for (j in TestLowClassEng) {
  FUNDATAphiLIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_phi_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_phi_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAphiLIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAphiLIST[[j]]<-FUNDATAphiLIST_LIST
}

FUNDATAphiLIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAphiLIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_phi_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_phi_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAphiLIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAphiLIST_BIG[[j]]<-FUNDATAphiLIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_phi_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_phi_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(phi_all_train_test)

vectorphi<-as.vector(phi_all_train_test[101:200,2:363])
vectorphi<-na.omit(vectorphi)
mean(na.omit(vectorphi))
meanphi<-mean(na.omit(vectorphi))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_phi_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixphireg<-matrix(NA,nrow(list_test_matrix_SCORE_phi_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]])) {
    distancematrixphireg[i,]<-FUNDATAphiLIST_BIG[[j]][[i]]@X/meanphi
  }
  list_for_distance_phi_reg_big_scores[[j]]<-distancematrixphireg
}

list_for_distance_phi_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixphireg<-matrix(NA,nrow(list_test_matrix_SCORE_phi_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_phi_noNA[[j]])) {
    distancematrixphireg[i,]<-FUNDATAphiLIST[[j]][[i]]@X/meanphi
  }
  list_for_distance_phi_reg_low_scores[[j]]<-distancematrixphireg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_phi_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_phi_reg_low_scores[[i]], method = "euclidean" )
  list_distance_phi_reg_low_scores[[i]]<-DISTANCE
}

list_distance_phi_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_phi_reg_big_scores[[i]], method = "euclidean" )
  list_distance_phi_reg_big_scores[[i]]<-DISTANCE
}


###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################phi DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
# 
# BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
# BPR_all_train_test
# BPR_all_train_test<-as.matrix(BPR_all_train_test)


#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_matrix[[i]]<-BPR_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
BPRTrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
BPRTrainScoresmatrix<-matrix(NA,100,3)
BPRTrainScoresmatrix[,1]<-seq(1:100)
BPRTrainScoresmatrix[,2]<-BPRTrainScores[,1]
BPRTrainScoresmatrix[,3]<-BPRTrainScores[,2]

TrainLowScores<-as.numeric(BPRTrainScoresmatrix[,1][which(BPRTrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(BPRTrainScoresmatrix[,1][which(BPRTrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_BPR_SCORE<-list()
for (i in TestLowClassEng)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_matrix_BPR_SCORE[[i]]<-BPR_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_matrix_BPR_SCORE[[i]]<-BPR_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  BPR_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainLowScores)+1)) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,])]
    list_test_matrix_BPR_SCORE[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE[[i]][(length(TrainLowScores)+1),]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  BPR_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainBigScores)+1)) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,])]
    list_test_matrix_BPR_SCORE[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE[[i]][(length(TrainBigScores)+1),]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_BPR_SCORE[[4]]
test3<-list_test_matrix_BPR_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_BPR_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(list_test_matrix_BPR_SCORE[[i]][(length(TrainLowScores)+1),])))
  noNA_test_matrix_BPR <-na.omit(list_test_matrix_BPR_SCORE[[i]])
  list_test_matrix_SCORE_BPR_noNA[[i]]<-noNA_test_matrix_BPR
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(list_test_matrix_BPR_SCORE[[i]][(length(TrainBigScores)+1),])))
  noNA_test_matrix_BPR <-na.omit(list_test_matrix_BPR_SCORE[[i]])
  list_test_matrix_SCORE_BPR_noNA[[i]]<-noNA_test_matrix_BPR
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_BPR_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_BPR_noNA[[i]]<-t(list_test_matrix_SCORE_BPR_noNA[[i]])
#}

list_test_matrix_t_SCORE_BPR_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_BPR_noNA[[i]]<-t(list_test_matrix_SCORE_BPR_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_BPR_noNA[[i]]<-t(list_test_matrix_SCORE_BPR_noNA[[i]])
}



######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_BPR_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_BPR_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_BPR_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_BPR_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_BPR_Big_scores[[j]]<-list_test_smoothing_BPR_Big_scores
}


list_test_all_smooth_BPR_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_BPR_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_BPR_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_BPR_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_BPR_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_BPR_Low_scores[[j]]<-list_test_smoothing_BPR_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=8

#lows cutted
plot(list_test_all_smooth_BPR_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]]))
{
  lines(list_test_all_smooth_BPR_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_BPR_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_BPR_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]]))
{
  lines(list_test_all_smooth_BPR_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_BPR_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_BPR_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in as.vector(list_test_matrix_SCORE_BPR_noNA[[testengine]][,1])) {
  lines(listeBPR[[i]])
} 
lines(list_test_all_smooth_BPR_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_BPR_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in as.vector(list_test_matrix_SCORE_BPR_noNA[[testengine]][,1])) {
  lines(listeBPR[[i]])
} 
lines(list_test_all_smooth_BPR_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_BPR_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]])

FUNDATABPRLIST<-list()
for (j in TestLowClassEng) {
  FUNDATABPRLIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_BPR_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_BPR_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATABPRLIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATABPRLIST[[j]]<-FUNDATABPRLIST_LIST
}

FUNDATABPRLIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATABPRLIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_BPR_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_BPR_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATABPRLIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATABPRLIST_BIG[[j]]<-FUNDATABPRLIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_BPR_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_BPR_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(BPR_all_train_test)

vectorBPR<-as.vector(BPR_all_train_test[101:200,2:363])
vectorBPR<-na.omit(vectorBPR)
mean(na.omit(vectorBPR))
meanBPR<-mean(na.omit(vectorBPR))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_BPR_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixBPRreg<-matrix(NA,nrow(list_test_matrix_SCORE_BPR_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]])) {
    distancematrixBPRreg[i,]<-FUNDATABPRLIST_BIG[[j]][[i]]@X/meanBPR
  }
  list_for_distance_BPR_reg_big_scores[[j]]<-distancematrixBPRreg
}

list_for_distance_BPR_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixBPRreg<-matrix(NA,nrow(list_test_matrix_SCORE_BPR_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_BPR_noNA[[j]])) {
    distancematrixBPRreg[i,]<-FUNDATABPRLIST[[j]][[i]]@X/meanBPR
  }
  list_for_distance_BPR_reg_low_scores[[j]]<-distancematrixBPRreg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_BPR_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_BPR_reg_low_scores[[i]], method = "euclidean" )
  list_distance_BPR_reg_low_scores[[i]]<-DISTANCE
}

list_distance_BPR_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_BPR_reg_big_scores[[i]], method = "euclidean" )
  list_distance_BPR_reg_big_scores[[i]]<-DISTANCE
}


###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################BPR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################

# 
# W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
# W31_all_train_test
# W31_all_train_test<-as.matrix(W31_all_train_test)


#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_matrix[[i]]<-W31_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
W31TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
W31TrainScoresmatrix<-matrix(NA,100,3)
W31TrainScoresmatrix[,1]<-seq(1:100)
W31TrainScoresmatrix[,2]<-W31TrainScores[,1]
W31TrainScoresmatrix[,3]<-W31TrainScores[,2]

TrainLowScores<-as.numeric(W31TrainScoresmatrix[,1][which(W31TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(W31TrainScoresmatrix[,1][which(W31TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_W31_SCORE<-list()
for (i in TestLowClassEng)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_matrix_W31_SCORE[[i]]<-W31_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_matrix_W31_SCORE[[i]]<-W31_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  W31_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainLowScores)+1)) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,])]
    list_test_matrix_W31_SCORE[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE[[i]][(length(TrainLowScores)+1),]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W31_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainBigScores)+1)) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,])]
    list_test_matrix_W31_SCORE[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE[[i]][(length(TrainBigScores)+1),]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_W31_SCORE[[4]]
test3<-list_test_matrix_W31_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_W31_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(list_test_matrix_W31_SCORE[[i]][(length(TrainLowScores)+1),])))
  noNA_test_matrix_W31 <-na.omit(list_test_matrix_W31_SCORE[[i]])
  list_test_matrix_SCORE_W31_noNA[[i]]<-noNA_test_matrix_W31
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(list_test_matrix_W31_SCORE[[i]][(length(TrainBigScores)+1),])))
  noNA_test_matrix_W31 <-na.omit(list_test_matrix_W31_SCORE[[i]])
  list_test_matrix_SCORE_W31_noNA[[i]]<-noNA_test_matrix_W31
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_W31_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_W31_noNA[[i]]<-t(list_test_matrix_SCORE_W31_noNA[[i]])
#}

list_test_matrix_t_SCORE_W31_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_W31_noNA[[i]]<-t(list_test_matrix_SCORE_W31_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_W31_noNA[[i]]<-t(list_test_matrix_SCORE_W31_noNA[[i]])
}



######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_W31_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_W31_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W31_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W31_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_W31_Big_scores[[j]]<-list_test_smoothing_W31_Big_scores
}


list_test_all_smooth_W31_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_W31_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W31_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W31_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W31_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_W31_Low_scores[[j]]<-list_test_smoothing_W31_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=1

#lows cutted
plot(list_test_all_smooth_W31_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W31_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_W31_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_W31_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W31_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_W31_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_W31_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in as.vector(list_test_matrix_SCORE_W31_noNA[[testengine]][,1])) {
  lines(listeW31[[i]])
} 
lines(list_test_all_smooth_W31_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_W31_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in as.vector(list_test_matrix_SCORE_W31_noNA[[testengine]][,1])) {
  lines(listeW31[[i]])
} 
lines(list_test_all_smooth_W31_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_W31_noNA[[j]])

FUNDATAW31LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAW31LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W31_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W31_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW31LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAW31LIST[[j]]<-FUNDATAW31LIST_LIST
}

FUNDATAW31LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAW31LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W31_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W31_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW31LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAW31LIST_BIG[[j]]<-FUNDATAW31LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_W31_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_W31_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(W31_all_train_test)

vectorW31<-as.vector(W31_all_train_test[101:200,2:363])
vectorW31<-na.omit(vectorW31)
mean(na.omit(vectorW31))
meanW31<-mean(na.omit(vectorW31))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_W31_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixW31reg<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]])) {
    distancematrixW31reg[i,]<-FUNDATAW31LIST_BIG[[j]][[i]]@X/meanW31
  }
  list_for_distance_W31_reg_big_scores[[j]]<-distancematrixW31reg
}

list_for_distance_W31_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixW31reg<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W31_noNA[[j]])) {
    distancematrixW31reg[i,]<-FUNDATAW31LIST[[j]][[i]]@X/meanW31
  }
  list_for_distance_W31_reg_low_scores[[j]]<-distancematrixW31reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_W31_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_W31_reg_low_scores[[i]], method = "euclidean" )
  list_distance_W31_reg_low_scores[[i]]<-DISTANCE
}

list_distance_W31_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_W31_reg_big_scores[[i]], method = "euclidean" )
  list_distance_W31_reg_big_scores[[i]]<-DISTANCE
}



###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W31 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################


# W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
# W32_all_train_test
# W32_all_train_test<-as.matrix(W32_all_train_test)


#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_matrix[[i]]<-W32_test_matrix_length
}

####train datasında low ve high scoreları ayırdım.
W32TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
W32TrainScoresmatrix<-matrix(NA,100,3)
W32TrainScoresmatrix[,1]<-seq(1:100)
W32TrainScoresmatrix[,2]<-W32TrainScores[,1]
W32TrainScoresmatrix[,3]<-W32TrainScores[,2]

TrainLowScores<-as.numeric(W32TrainScoresmatrix[,1][which(W32TrainScoresmatrix[,3]=="Low")])
TrainBigScores<-as.numeric(W32TrainScoresmatrix[,1][which(W32TrainScoresmatrix[,3]=="Big")])

TestLowClassEng
TestBigClassEng
list_test_matrix_W32_SCORE<-list()
for (i in TestLowClassEng)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_matrix_W32_SCORE[[i]]<-W32_test_matrix_length
}

for (i in TestBigClassEng)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_matrix_W32_SCORE[[i]]<-W32_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in TestLowClassEng) { 
  W32_test_matrix<- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainLowScores)+1)) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,])]
    list_test_matrix_W32_SCORE[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE[[i]][(length(TrainLowScores)+1),]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W32_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,]))
  for (j in 1:(length(TrainBigScores)+1)) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,])]
    list_test_matrix_W32_SCORE[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE[[i]][(length(TrainBigScores)+1),]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_W32_SCORE[[4]]
test3<-list_test_matrix_W32_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_W32_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=(length(TrainLowScores)+1), ncol=length(na.omit(list_test_matrix_W32_SCORE[[i]][(length(TrainLowScores)+1),])))
  noNA_test_matrix_W32 <-na.omit(list_test_matrix_W32_SCORE[[i]])
  list_test_matrix_SCORE_W32_noNA[[i]]<-noNA_test_matrix_W32
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(na.omit(list_test_matrix_W32_SCORE[[i]][(length(TrainBigScores)+1),])))
  noNA_test_matrix_W32 <-na.omit(list_test_matrix_W32_SCORE[[i]])
  list_test_matrix_SCORE_W32_noNA[[i]]<-noNA_test_matrix_W32
}
#####list with transpose of each matrix

#list_test_matrix_t_SCORE_W32_noNA<-list()
#for (i in 1:100)
#{ 
#  list_test_matrix_t_SCORE_W32_noNA[[i]]<-t(list_test_matrix_SCORE_W32_noNA[[i]])
#}

list_test_matrix_t_SCORE_W32_noNA<-list()
for (i in TestLowClassEng)
{ 
  list_test_matrix_t_SCORE_W32_noNA[[i]]<-t(list_test_matrix_SCORE_W32_noNA[[i]])
}
for (i in TestBigClassEng)
{ 
  list_test_matrix_t_SCORE_W32_noNA[[i]]<-t(list_test_matrix_SCORE_W32_noNA[[i]])
}



######smoothing for each engine
no_of_splines=5
j=10
i=2
list_test_all_smooth_W32_Big_scores<-list()
for (j in TestBigClassEng) {
  list_test_smoothing_W32_Big_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W32_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W32_Big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_W32_Big_scores[[j]]<-list_test_smoothing_W32_Big_scores
}


list_test_all_smooth_W32_Low_scores<-list()
for (j in TestLowClassEng) {
  list_test_smoothing_W32_Low_scores<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_W32_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_W32_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W32_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_W32_Low_scores[[j]]<-list_test_smoothing_W32_Low_scores
}



TestLowClassEng
TestBigClassEng
###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=8

#lows cutted
plot(list_test_all_smooth_W32_Low_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W32_Low_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

#bigs cutted
plot(list_test_all_smooth_W32_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W32_Big_scores[[testengine]][[i]], col="black")
}
lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")



###plot for (full observation trains - cutted test)

#lows full
plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in as.vector(list_test_matrix_SCORE_W32_noNA[[testengine]][,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

#bigs full
plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in as.vector(list_test_matrix_SCORE_W32_noNA[[testengine]][,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")



j
TestLowClassEng
TestBigClassEng

#################DISTANCE########################
#########################REGISTRATION OF SMOOTHED
#####Distance için 20 noktadan observation alınıyor

i=1
j=8

1:nrow(list_test_matrix_SCORE_W32_noNA[[j]])

FUNDATAW32LIST<-list()
for (j in TestLowClassEng) {
  FUNDATAW32LIST_LIST<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W32_Low_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W32_Low_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW32LIST_LIST[[i]]<-funDatasmooth
  }
  FUNDATAW32LIST[[j]]<-FUNDATAW32LIST_LIST
}

FUNDATAW32LIST_BIG<-list()
for (j in TestBigClassEng) {
  FUNDATAW32LIST_LIST_BIG<-list()
  for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]])) {
    funDatasmooth<-fd2funData(list_test_all_smooth_W32_Big_scores[[j]][[i]]$fd,
                              argvals = seq(from=1, to=length(list_test_all_smooth_W32_Big_scores[[j]][[i]]$argvals), length.out= 20))
    FUNDATAW32LIST_LIST_BIG[[i]]<-funDatasmooth
  }
  FUNDATAW32LIST_BIG[[j]]<-FUNDATAW32LIST_LIST_BIG
}

#funDatasmooth<-fd2funData(list_test_all_smooth_W32_Big_scores[[j]][[1]]$fd,
#                          argvals = seq(from=1, to=length(list_test_all_smooth_W32_Big_scores[[j]][[1]]$argvals), length.out= 20))
#funDatasmooth@X


#####DISTANCE Calculation
##Bu observationlardan matrix oluşturuluyor.

dim(W32_all_train_test)

vectorW32<-as.vector(W32_all_train_test[101:200,2:363])
vectorW32<-na.omit(vectorW32)
mean(na.omit(vectorW32))
meanW32<-mean(na.omit(vectorW32))

#####BURADA DISTANCE HESABI ICIN MATRIXLER LISTEYE ALINDI
list_for_distance_W32_reg_big_scores<-list()
for (j in TestBigClassEng) {
  distancematrixW32reg<-matrix(NA,nrow(list_test_matrix_SCORE_W32_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]])) {
    distancematrixW32reg[i,]<-FUNDATAW32LIST_BIG[[j]][[i]]@X/meanW32
  }
  list_for_distance_W32_reg_big_scores[[j]]<-distancematrixW32reg
}

list_for_distance_W32_reg_low_scores<-list()
for (j in TestLowClassEng) {
  distancematrixW32reg<-matrix(NA,nrow(list_test_matrix_SCORE_W32_noNA[[j]]),20) 
  for(i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[j]])) {
    distancematrixW32reg[i,]<-FUNDATAW32LIST[[j]][[i]]@X/meanW32
  }
  list_for_distance_W32_reg_low_scores[[j]]<-distancematrixW32reg
}

############# DISTANCE CALCUALTION YAPILDI
list_distance_W32_reg_low_scores<-list()
for (i in TestLowClassEng) {
  DISTANCE<-distance(list_for_distance_W32_reg_low_scores[[i]], method = "euclidean" )
  list_distance_W32_reg_low_scores[[i]]<-DISTANCE
}

list_distance_W32_reg_big_scores<-list()
for (i in TestBigClassEng) {
  DISTANCE<-distance(list_for_distance_W32_reg_big_scores[[i]], method = "euclidean" )
  list_distance_W32_reg_big_scores[[i]]<-DISTANCE
}


###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################W32 DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################

###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - START#########################
#Multivariate Distance calculation
list_all_distance_merged_low_scores<-list()
for (i in TestLowClassEng) {
  
  a<-list_distance_T24_reg_low_scores[[i]]+
    list_distance_T30_reg_low_scores[[i]]+
    list_distance_T50_reg_low_scores[[i]]+
    list_distance_P30_reg_low_scores[[i]]+
    list_distance_ps30_reg_low_scores[[i]]+
    list_distance_phi_reg_low_scores[[i]]+
    list_distance_BPR_reg_low_scores[[i]]+
    list_distance_W31_reg_low_scores[[i]]+
    list_distance_W32_reg_low_scores[[i]]
  list_all_distance_merged_low_scores[[i]]<-a
}

list_all_distance_merged_big_scores<-list()
for (i in TestBigClassEng) {
  
  a<-list_distance_T24_reg_big_scores[[i]]+
    list_distance_T30_reg_big_scores[[i]]+
    list_distance_T50_reg_big_scores[[i]]+
    list_distance_P30_reg_big_scores[[i]]+
    list_distance_ps30_reg_big_scores[[i]]+
    list_distance_phi_reg_big_scores[[i]]+
    list_distance_BPR_reg_big_scores[[i]]+
    list_distance_W31_reg_big_scores[[i]]+
    list_distance_W32_reg_big_scores[[i]]
  list_all_distance_merged_big_scores[[i]]<-a
}

############## SORTED DISTANCES FOR ALL ENGINES
list_dist_all_sorted<-list()
for (i in TestLowClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_low_scores[[i]][,nrow(list_test_matrix_SCORE_W31_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted[[i]]<-DISTANCE
}

for (i in TestBigClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_big_scores[[i]][,nrow(list_test_matrix_SCORE_W31_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted[[i]]<-DISTANCE
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
plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]], col="red")
}
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")
text(70, 643, "Test Engine no:70", col="blue")
text(200, 642.5, "5 closest train engines (low score group)", col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


testengine=83
#Bigs
par(mfrow=c(3,3))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]])
}
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")
text(60, 642.5, "Test Engine no:81", col="blue")

text(220, 642.3, "5 closest train engines (big score group)", col="black")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")



#bigs cutted
#plot(list_test_all_smooth_W32_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
#for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[testengine]]))
#{
#  lines(list_test_all_smooth_W32_Big_scores[[testengine]][[i]], col="black")
#}
#lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################MULTIVAR DISTANCE CALCULATION AFTER MFPCA CLASSIFICATION - FINISH#########################



###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - START#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - START#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - START#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - START#########################

####TRUE RULimport
RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
RUL_test
RUL_test<-as.matrix(RUL_test)


RUL_test[20,2]<-64
RUL_test[31,2]<-69
RUL_test[34,2]<-71
RUL_test[35,2]<-69
RUL_test[42,2]<-54
RUL_test[49,2]<-106
RUL_test[68,2]<-65
RUL_test[76,2]<-72
RUL_test[81,2]<-75
RUL_test[82,2]<-57

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



par(mfrow=c(1,1))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
no_of_closest_engine=6
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
no_of_closest_engine=8

LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
LIFE_TRAIN<-as.matrix(LIFE_TRAIN)
LIFE_TRAIN

list_closest_X_lowscore<-list()
for (i in TestLowClassEng ) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_lowscore[[i]]<-close
}

#19 eğer 75 lik yaptıysan
TestBigClassEngexcept49<-TestBigClassEng
TestBigClassEngexcept49[27]<-NA
TestBigClassEngexcept49<-na.omit(TestBigClassEngexcept49)

list_closest_X_bigcore<-list()
for (i in TestBigClassEngexcept49 ) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_bigcore[[i]]<-close
}
list_closest_X_bigcore[[49]]<-c(96,69,92,67)



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

RUL_PREDICTION_lowscore<- matrix(data = NA, nrow=length(TestLowClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_lowscore) <- newheaderstest
RUL_PREDICTION_lowscore[,1]<-RUL_test_lowscore[,1]
RUL_PREDICTION_lowscore[,2]<-RUL_test_lowscore[,2]
RUL_PREDICTION_lowscore[,3]<-RUL_test_lowscore[,3]
RUL_PREDICTION_lowscore[,4]<-RUL_test_lowscore[,4]
i=1
for (i in 1:length(TestLowClassEng)) {
  RUL_PREDICTION_lowscore[i,5]<-mean(LIFE_TRAIN[list_closest_X_lowscore[[TestLowClassEng[i]]],2])
  RUL_PREDICTION_lowscore[i,6]<-RUL_PREDICTION_lowscore[i,5]-RUL_PREDICTION_lowscore[i,2]
  RUL_PREDICTION_lowscore[i,7]<-RUL_PREDICTION_lowscore[i,6]-RUL_PREDICTION_lowscore[i,3]
  RUL_PREDICTION_lowscore[i,8]<-median(LIFE_TRAIN[list_closest_X_lowscore[[TestLowClassEng[i]]],2]) 
  RUL_PREDICTION_lowscore[i,9]<-RUL_PREDICTION_lowscore[i,8]-RUL_PREDICTION_lowscore[i,2]
  RUL_PREDICTION_lowscore[i,10]<-RUL_PREDICTION_lowscore[i,9]-RUL_PREDICTION_lowscore[i,3]
}

RUL_PREDICTION_bigscore<- matrix(data = NA, nrow=length(TestBigClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_bigscore) <- newheaderstest
RUL_PREDICTION_bigscore[,1]<-RUL_test_bigscore[,1]
RUL_PREDICTION_bigscore[,2]<-RUL_test_bigscore[,2]
RUL_PREDICTION_bigscore[,3]<-RUL_test_bigscore[,3]
RUL_PREDICTION_bigscore[,4]<-RUL_test_bigscore[,4]

for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_bigscore[i,5]<-mean(LIFE_TRAIN[list_closest_X_bigcore[[TestBigClassEng[i]]],2])
  RUL_PREDICTION_bigscore[i,6]<-RUL_PREDICTION_bigscore[i,5]-RUL_PREDICTION_bigscore[i,2]
  RUL_PREDICTION_bigscore[i,7]<-RUL_PREDICTION_bigscore[i,6]-RUL_PREDICTION_bigscore[i,3]
  RUL_PREDICTION_bigscore[i,8]<-median(LIFE_TRAIN[list_closest_X_bigcore[[TestBigClassEng[i]]],2]) 
  RUL_PREDICTION_bigscore[i,9]<-RUL_PREDICTION_bigscore[i,8]-RUL_PREDICTION_bigscore[i,2]
  RUL_PREDICTION_bigscore[i,10]<-RUL_PREDICTION_bigscore[i,9]-RUL_PREDICTION_bigscore[i,3]
}

TRUE_RUL_DECREASING_lowscore
ordervector_mean<- as.vector(TRUE_RUL_DECREASING_lowscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_lowscore_SORTED <- RUL_PREDICTION_lowscore[match(ordervector_mean, RUL_PREDICTION_lowscore),]##SORT increasing RUL

ordervector_mean<- as.vector(TRUE_RUL_DECREASING_bigscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_bigscore_SORTED <- RUL_PREDICTION_bigscore[match(ordervector_mean, RUL_PREDICTION_bigscore),]##SORT increasing RUL


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_lowscore_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_lowscore_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUARE))

max(RUL_PREDICTION_lowscore[,7])
min(RUL_PREDICTION_lowscore[,7])
max(RUL_PREDICTION_lowscore[,10])
min(RUL_PREDICTION_lowscore[,10])

length(which(RUL_PREDICTION_lowscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0)) )

length(which(RUL_PREDICTION_lowscore[,7]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,7]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0)) )


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_bigscore_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_bigscore_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUAREMED))

max(RUL_PREDICTION_bigscore[,7])
min(RUL_PREDICTION_bigscore[,7])
max(RUL_PREDICTION_bigscore[,10])
min(RUL_PREDICTION_bigscore[,10])

length(which(RUL_PREDICTION_bigscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0)) )

length(which(RUL_PREDICTION_bigscore[,7]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,7]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0)) )




RUL_PREDICTION_lowscore
RUL_PREDICTION_bigscore

total<-rbind(RUL_PREDICTION_lowscore,RUL_PREDICTION_bigscore)

ERRORSQUARE <- total[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- total[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)


###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - FINISH#########################
###################RUL PREDICTION AFTER MFPCA CLASSIFICATION - FINISH#########################

############ Curve Prediction starts ##############


RUL_PREDICTION_lowscore
RUL_PREDICTION_bigscore
TestBigClassEngexcept49
TestLowClassEng
TestBigClassEng
####curve prediction
class(fdsmoothallT30)

RUL_PREDICTION_low_big_merged<-matrix(NA, nrow = 100, ncol=10)

for (i in 1:length(TestLowClassEng)) {
  RUL_PREDICTION_low_big_merged[TestLowClassEng[i],]<-RUL_PREDICTION_lowscore[i,]
}
for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_low_big_merged[TestBigClassEng[i],]<-RUL_PREDICTION_bigscore[i,]
}

RUL_PREDICTION_low_big_merged<-round(RUL_PREDICTION_low_big_merged,0)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_low_big_merged) <- newheaderstest



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

funDatasmoothallT24_list<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT24,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT24_list[[i]]<-funDatasmoothallT24
}
funDatasmoothallT30_list<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT30_list[[i]]<-funDatasmoothallT24
}
funDatasmoothallT50_list<-list()
for (i in 1:100) {
  funDatasmoothallT50<-fd2funData(fdsmoothallT50,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT50_list[[i]]<-funDatasmoothallT50
}
funDatasmoothallP30_list<-list()
for (i in 1:100) {
  funDatasmoothallP30<-fd2funData(fdsmoothallP30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallP30_list[[i]]<-funDatasmoothallP30
}
funDatasmoothallps30_list<-list()
for (i in 1:100) {
  funDatasmoothallps30<-fd2funData(fdsmoothallps30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallps30_list[[i]]<-funDatasmoothallps30
}
funDatasmoothallphi_list<-list()
for (i in 1:100) {
  funDatasmoothallphi<-fd2funData(fdsmoothallphi,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallphi_list[[i]]<-funDatasmoothallphi
}
funDatasmoothallBPR_list<-list()
for (i in 1:100) {
  funDatasmoothallBPR<-fd2funData(fdsmoothallBPR,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallBPR_list[[i]]<-funDatasmoothallBPR
}
funDatasmoothallW31_list<-list()
for (i in 1:100) {
  funDatasmoothallW31<-fd2funData(fdsmoothallW31,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallW31_list[[i]]<-funDatasmoothallW31
}
funDatasmoothallW32_list<-list()
for (i in 1:100) {
  funDatasmoothallW32<-fd2funData(fdsmoothallW32,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallW32_list[[i]]<-funDatasmoothallW32
}


#curve 20'ye en yakın 10 train engine 
as.vector(list_dist_all_sorted[[20]][2:6,1])

VALUEST24<-funDatasmoothallT24@X
VALUEST30<-funDatasmoothallT30@X
VALUEST50<-funDatasmoothallT50@X
VALUESP30<-funDatasmoothallP30@X
VALUESps30<-funDatasmoothallps30@X
VALUESphi<-funDatasmoothallphi@X
VALUESBPR<-funDatasmoothallBPR@X
VALUESW31<-funDatasmoothallW31@X
VALUESW32<-funDatasmoothallW32@X



predictedcurves<-list()
curve_prediction_list<-list()  
for (j in 1:48) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]))
  predictedcurve<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]))
  for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list[[j]]@X[list_dist_all_sorted[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged[j,2]+1):RUL_PREDICTION_low_big_merged[j,5]]
  }
  curve_prediction_list[[j]]<-CURVETAHMIN
  tahmin<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]) )  
  for (k in 1:(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2])) {
    tahmin[,k] <- mean(curve_prediction_list[[j]][,k])
  }
  predictedcurves[[j]]<- tahmin
}

k=1
j=1

for (j in 50:100) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]))
  predictedcurve<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]))
  for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list[[j]]@X[list_dist_all_sorted[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged[j,2]+1):RUL_PREDICTION_low_big_merged[j,5]]
  }
  curve_prediction_list[[j]]<-CURVETAHMIN
  tahmin<-matrix(NA,nrow = 1, ncol =(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2]) )  
  for (k in 1:(RUL_PREDICTION_low_big_merged[j,5]-RUL_PREDICTION_low_big_merged[j,2])) {
    tahmin[,k]<-mean(curve_prediction_list[[j]][,k])
  }
  predictedcurves[[j]]<- tahmin
}

predictedcurves
## predicted curve list for each test engine

#T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
#T24_all_train_test
#T24_all_train_test<-as.matrix(T24_all_train_test)

T24_TEST_OBSERVED<-T24_all_train_test[101:200, 2:363]
dim(T24_TEST_OBSERVED)

T24_TEST_PREDICTED<-list()
for (i in 1:48) {
  T24_TEST_PREDICTED[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED[i,]))),as.matrix(predictedcurves[[i]])) 
}
for (i in 50:100) {
  T24_TEST_PREDICTED[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED[i,]))),as.matrix(predictedcurves[[i]])) 
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


predictedcurves_smooth_matrix_T24<-matrix(data = NA, nrow=8, ncol=100)

predictedcurves_smooth_list_T24<-list()
for (j in 1:48) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged[j,5]),
                         y= as.vector(T24_TEST_PREDICTED[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged[j,5]),no_of_splines))
  predictedcurves_smooth_list_T24[[j]]<-Smooth
}
for (j in 50:100) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged[j,5]),
                         y= as.vector(T24_TEST_PREDICTED[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged[j,5]),no_of_splines))
  predictedcurves_smooth_list_T24[[j]]<-Smooth
}

TestLowClassEng
TestBigClassEngexcept49
#10,
testengine=20
T24_TEST_PREDICTED[[testengine]]
plot(predictedcurves_smooth_list_T24[[20]] , xlim=c(0,370), ylim=c(641.7,644), col="red")
for (i in TestLowClassEng) {
  lines(predictedcurves_smooth_list_T24[[i]],col="red")
}
for (i in TestBigClassEngexcept49) {
  lines(predictedcurves_smooth_list_T24[[i]], col="black")
}
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")


T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)

par(mfrow=c(1,1))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=1.5
TestBigClassEng
testengine=76
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black",
     xlab="Cycle Time" , ylab="T24 / Sensor values", main="MFPCA Prediction (%35 of observation)")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

testengine=31
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

testengine=34
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

testengine=68
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black",
     xlab="Cycle Time" , ylab="T24 / Sensor values", main="MFPCA Prediction (%35 of observation)")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

testengine=42
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

testengine=49
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")


testengine=68
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")


testengine=76
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")


testengine=81
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")


testengine=82
#bigscore individual curves
plot(c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]),T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)] , xlim=c(0,370), ylim=c(641.7,644), col="black")
lines(predictedcurves_smooth_list_T24[[testengine]], col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

T24_all_train_test[182,]

T24_all_train_test[(testengine+100),(RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5]]

RUL_PREDICTION_low_big_merged[testengine,5]

########################################
###tahmin edilen %90 a kadar olan aralık
c((RUL_PREDICTION_low_big_merged[testengine,2]+1):RUL_PREDICTION_low_big_merged[testengine,5])
true90percent_PRED_T24<-T24_all_train_test[(testengine+100),((RUL_PREDICTION_low_big_merged[testengine,2]+1)+1):(RUL_PREDICTION_low_big_merged[testengine,5]+1)]
true90percent_PRED_T24<-na.omit(true90percent_PRED_T24)
length(true90percent_PRED_T24)
true90percent_PRED_T24<-as.vector(true90percent_PRED_T24)
true90percent_PRED_T24

RUL_PREDICTION_low_big_merged[testengine,2]

testengine=68
predicted90percent_PRED_T24<-predictedcurves_smooth_list_T24[[testengine]]$y[(RUL_PREDICTION_low_big_merged[testengine,2]+1):(RUL_PREDICTION_low_big_merged[testengine,2]+length(true90percent_PRED_T24))]
predicted90percent_PRED_T24

ERRORcurve<-predicted90percent_PRED_T24-true90percent_PRED_T24
ERRORSQ<-ERRORcurve^2
SUMERRORSQ<-sum(ERRORSQ)
SUMERRORSQmean<-SUMERRORSQ/length(predicted90percent_PRED_T24)
sqrt(SUMERRORSQmean)
SUMERRORSQmean

