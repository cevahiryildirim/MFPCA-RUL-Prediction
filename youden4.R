

# install package
install.packages("funData")
install.packages("fda")
install.packages("MFPCA")
install.packages("devtools")
install.packages("ggplot2")
install.packages("fields")

# load package
library(funData)
library(fda)
library(MFPACA)
library(devtools)
library(ggplot2)
library(fields)

#load left truncated data
lefttrun<-read.csv("C:/Users/cevah/Desktop/first10_seperated_lefttruncated.csv", header = TRUE)
head(lefttrun)


#convert it into matrix
lefttrunmtrx<-data.matrix(lefttrun)
lefttrunmtrx

#fourier basis expansion
cyclebasis = create.fourier.basis(c(0,287),287)

# harmonic acceleration differential operator
harmLfd = vec2Lfd(c(0,(2*pi/287)^2,0), c(0, 287))
sensorfdPar = fdPar(cyclebasis,harmLfd,1e4)

t24fd = smooth.basis(1:287,lefttrunmtrx,sensorfdPar)

fd<-t24fd$fd
coef<-fd$coefs
coef

#bspline basis expansion
cyclebasis = create.bspline.basis(norder = 4)

plot(cyclebasis)
# harmonic acceleration differential operator
harmLfd = vec2Lfd(c(0,(2*pi/287)^2,0), c(0, 287))
sensorfdPar = fdPar(cyclebasis,harmLfd,1e4)

harmfd

t24fd = smooth.basis(1:287,lefttrunmtrx,sensorfdPar)

fd<-t24fd$fd
coef<-fd$coefs


par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(fd,xlab='cycle time',ylab='sensor value',cex.lab=1.5,cex.axis=1.5, xlim=c(0,300), ylim=c(641,644))

install.packages("locfit")
library(locfit)
# compute left and right smooths
data(penny)
xev <- (1945:1988)+0.5
fitl <- locfit(thickness~left(year,h=10,deg=1), ev=xev, data=penny)
fitr <- locfit(thickness~right(year,h=10,deg=1),ev=xev, data=penny)
xev
fitl$terms
# plot the squared difference, to show the change points.
plot( xev, (predict(fitr,where="ev") - predict(fitl,where="ev"))^2 )

