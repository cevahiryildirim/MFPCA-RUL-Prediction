

# load package
library(funData)
library(ggplot2)
library(fda)
library(MFPCA)



##########################################################################################################
#######################      Our Data      ###############################################################
##########################################################################################################

##### Load Data for only Engine 1 and Sensor 1(T24) #######
Engine1Sensor1<-read.csv("C:/Users/cevah/Desktop/eng1.csv", header = TRUE, row.names = 1)
head(Engine1Sensor1)
Engine1Sensor1

#convert into matrix
Engine1Sensor1<-data.matrix(Engine1Sensor1)
Engine1Sensor1

#### Egine1 lived for 192 cycle and failed ###
dim(Engine1Sensor1)

##### define the registration interval for 192 observation #####
ENG1allArgvals <- seq(0, 1, length.out = 192)

#####funData for first 2 Engine
fdENG1 <- funData(argvals = ENG1allArgvals,
                  X = t(arrE1S1[, 1, ]))
##### Plot for interval 0-1 #####
plot(fdENG1,xlab='cycle time',ylab='sensor value',cex.lab=1.5,cex.axis=1, xlim=c(0,1))

###########################################################
##### Load Data for only Engine 2 and Sensor 1(T24) #######
Engine2Sensor1<-read.csv("C:/Users/cevah/Desktop/eng2.csv", header = TRUE, row.names = 1)
head(Engine2Sensor1)

#convert into matrix
Engine2Sensor1<-data.matrix(Engine2Sensor1)
Engine2Sensor1

#### Egine2 lived for 287 cycle and failed ###
dim(Engine2Sensor1)

###Registred intervals###
ENG2allArgvals <- seq(0, 1, length.out = 287)

#####funData for first 2 Engine
fdENG2 <- funData(argvals = ENG2allArgvals,
                  X = t(arrE2S1[, 1, ]))
##### Plot for interval 0-1 #####
plot(fdENG2,xlab='cycle time',ylab='sensor value',cex.lab=1.5,cex.axis=1, xlim=c(0,1))



############SMOOTHING SPLINES##################

#NUMERIC OLARAK

ENG2allArgvals<-ENG2allArgvals
ENG2allArgvals<-as.numeric(ENG2allArgvals)
length(ENG2allArgvals)

ENG2values<-fdENG2@X
ENG2values<-as.numeric(ENG2values)
length(ENG2values)

plot(ENG2allArgvals,ENG2values, ylab="T24 Sensor Value", xlab="Engine '2' observations in [0-1] interval")

##### cubic splines by default - number of basis 10 )
bsplinebasis<- create.bspline.basis(c(0,1),13)
class(bsplinebasis)

plot(bsplinebasis)

#### smoth data #####
resultENG2 <- smooth.basis(ENG2allArgvals, ENG2values, bsplinebasis)
class(resultENG2)
plot(resultENG2)
resultENG2

plot(resultENG2)
plot(ENG2allArgvals,ENG2values)
ENG2smooth  <- resultENG2$fd
class(ENG2smooth)
plot(ENG2smooth)
ENG2smooth

class(resultENG2)
A<-resultENG2$fd
A



#####smoth fd######
ENG2smoothfdPar <- fdPar(ENG2smooth, 2, 1e-4)
smoothENG2fd <- smooth.fd(ENG2smooth, yfd2Par)
smoothENG2fd 
plot(smoothENG2fd)
class(smoothENG2fd)


################################

##fd2funData(smoothENG2fd)

##multiFunData(fundata1,fundata2)




plot(ENG2allArgvals,ENG2values)           # plot the data
lines(ENG2smooth, lty=1)  #  add moderately penalized smooth

plot(fdENG2)           # plot the data
lines(ENG2smooth, lty=1)  #  add moderately penalized smooth

plotfit.fd(ENG2values, ENG2allArgvals, ENG2smooth, ylab="T24 Sensor Value", xlab="Engine '2' observations in [0-1] interval")  # plot data and smooth


########### now we are able to convert from "fd" to "funData" using this
funDataSmoothfd2 <- fd2funData(ENG2smooth, ENG2allArgvals)
plot(funDataSmoothfd2)
class(funDataSmoothfd2)


########### now we can create multifundata using

multiFunData(funDataSmoothfd2,otherfundatahere)

########### now we can perform MFPCA with multifundata.



###########when Trial with multiple curves################

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


T24traindata
tT24traindata <-  t(T24traindata)
dim(tT24traindata)

plot(argvalsforsmooth,tT24traindata, 
     ylab="T30 Sensor Value", 
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
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT24traindata[,i])),bsplinebasis2)[["fd"]][["coefs"]]
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

plot(fdsmoothallT24,
     ylab="T24 / Smoothed Spline functions",
     xlab="All 100 Engines observations registered between [0-1] interval")

funDatasmoothallT24<-fd2funData(fdobj=fdsmoothallT24,argvals =newargvals3)
funDatasmoothallT24
autoplot(funDatasmoothallT24)
class(funDatasmoothallT24)
class(registeredargvals[[1]])


############


daybasis65 <- create.fourier.basis(c(0, 365), nbasis=65, period=365)

harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)
daytempfd <- smooth.basis(day.5, CanadianWeather$dailyAv[,,"Temperature.C"],
                          daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd

daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)
daytemppcaVarmx <- varmx.pca.fd(daytemppcaobj)
#  plot harmonics
op <- par(mfrow=c(2,2))
plot.pca.fd(daytemppcaobj, cex.main=0.9)

plot.pca.fd(daytemppcaVarmx, cex.main=0.9)
par(op)

plot(daytemppcaobj$harmonics)
plot(daytemppcaVarmx$harmonics)



######3
bsplinebasis
bsplinebasis2
fdsmoothallT24


fdparbsplines<-fdPar(bsplinebasis)
fdparbsplines2<-fdPar(bsplinebasis2)
testpca<-pca.fd(fdsmoothallT24, nharm = 3, fdparbsplines2)
testpcavarmx <- varmx.pca.fd(testpca)

#  plot harmonics
op <- par(mfrow=c(2,3))

plot.pca.fd(testpca, cex.main=0.9)
plot.pca.fd(testpcavarmx, cex.main=0.9)
par(op)

plot(testpca$harmonics)
plot(testpcavarmx$harmonics)

scoreplot(testpca$scores)

par(mfrow=c(1,1))
plot(testpca$scores[,1], testpca$scores[,2])


testBsplineRegisteredsmoothedMFPCA<-BsplineRegisteredsmoothedMFPCA

testBsplineRegisteredsmoothedMFPCA$scores<-testpca$scores

scoreplot(testBsplineRegisteredsmoothedMFPCA)
scoreplot(BsplineRegisteredsmoothedMFPCA)

testpca$scores[which(testpca$scores[,1]<0)]
unifpcalowscores<-which(testpca$scores[,1]<0)

testpca$scores[which(testpca$scores[,1]>0)]
unifpcabigscores<-which(testpca$scores[,1]>0)

plot(listeT24[[1]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(unifpcabigscores)) {
  lines(listeT24[[i]], col="black" ) }
for(j in c(unifpcalowscores)){
  lines(listeT24[[j]], col="red")
}


plot(funDatasmoothallT24@argvals[[1]],funDatasmoothallT24@X[i,], xlim=c(0,1), ylim=c(641.7,644))
for (i in c(unifpcabigscores)) {
  lines(funDatasmoothallT24@argvals[[1]],funDatasmoothallT24@X[i,], col="black" ) }
for(j in c(unifpcalowscores)){
  lines(funDatasmoothallT24@argvals[[1]],funDatasmoothallT24@X[j,], col="red")
}



