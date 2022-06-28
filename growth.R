#  ---------------------------------------------------------------------
#            Register the velocity curves for the girls
#  ---------------------------------------------------------------------

#  load the data
install.packages("fda")
library(fda)

load("growthfd")

hgtffdPar <- growthfd$hgtffdPar
hgtffd    <- hgtffdPar$fd

age  <- c( seq(1, 2, 0.25), seq(3, 8, 1), seq(8.5, 18, 0.5))
nage <- length(age)

ncasef <- 54

#  set up the basis for function Wfd

rng     <- c(1,18)
nbasisw <- 15
norder  <- 5
basisw  <- create.bspline.basis(rng, nbasisw, norder)

# set up the mean velocity curve as the preliminary target for
#  registration

hgtfmeanfd <- mean(hgtffd)
y0fd <- deriv(hgtfmeanfd,  1)

#  curves to be registered

yfd  <- deriv(hgtffd, 1)

#  set up functional parameter object for function Wfd

coef0  <- matrix(0,nbasisw,ncasef)
Wfd0   <- fd(coef0, basisw)
lambda <- 10
WfdPar <- fdPar(Wfd0, 2, lambda)

#  register the data.  It might be a good idea to disable
#  buffered output in the Misc menu for the R Console in order
#  to track progress of this fairly slow process.

reglist <- register.fd(y0fd, yfd, WfdPar)

yregfd  <- reglist$regfd  #  registered curves
Wfd     <- reglist$Wfd    #  functions defining warping functions

#  evaluate the registered curves and warping functions

agefine <- seq(1, 18, len=101)
ymat    <- eval.fd(agefine, yfd)
y0vec   <- eval.fd(agefine, y0fd)
yregmat <- eval.fd(agefine, yregfd)
warpmat <- eval.monfd(agefine, Wfd)
warpmat <- 1 + 17*warpmat/(matrix(1,101,1)%*%warpmat[101,])

#  plot the results for each girl:
#    blue:  unregistered curve
#    red:   target curve
#    green: registered curve

par(mfrow=c(1,2),pty="s",ask=T)
for (i in 1:ncasef) {
  plot (agefine, ymat[,i], type="l", ylim=c(0,20), col=4,
        xlab="Year", ylab="Velocity", main=paste("Case",i))
  lines(agefine, y0vec, lty=2, col=2)
  lines(agefine, yregmat[,i],  col=3)
  plot (agefine, warpmat[,i], type="l",
        xlab="Clock year", ylab="Biological Year")
  abline(0,1,lty=2)
}

#  Comments:  we see that not all curves are properly registered.
#     Curves 7, 11, 13 and 25, to mention a few, are so far from
#     the target that the registration is unsuccessful.  This
#     argues for a preliminary landmark registration of the 
#     velocity curves prior to the continuous registration 
#     process.  However, we will see some improvement below.  

#  compute the new mean curve as a target

y0fd2 <- mean(yregfd)

#  plot the unregistered mean and the registered mean

par(mfrow=c(1,1),pty="s",ask=F)
plot(y0fd2, col=4, xlab="Year", ylab="Mean Velocity")
lines(y0fd, col=3)
legend(10,15, c("Registered", "Unregistered"), lty=c(1,1), col=c(4,3))

#  Comment:  The new mean has a sharper peak at the pubertal
#      growth spurt, which is what we wanted to achieve.

#  define the registered curves and the new curves to be registered

yfd2 <- yregfd

#  register the curves again, this time to a better target

reglist2 <- register.fd(y0fd2, yfd2, WfdPar)

yregfd2  <- reglist2$regfd  #  registered curves
Wfd2     <- reglist2$Wfd    #  functions defining warping functions

y0vec2   <- eval.fd(agefine, y0fd2)
yregmat2 <- eval.fd(agefine, yregfd2)
warpmat2 <- eval.monfd(agefine, Wfd2)
warpmat2 <- 1 + 17*warpmat2/(matrix(1,101,1)%*%warpmat2[101,])

#  plot the results for each girl:
#    blue:  unregistered curve
#    red:   target curve
#    green: registered curve

par(mfrow=c(1,2),pty="s",ask=T)
for (i in 1:ncasef) {
  #  plot velocity curves
  plot (agefine, ymat[,i], type="l", ylim=c(0,20), col=4,
        xlab="Year", ylab="Velocity", main=paste("Case",i))
  lines(agefine, y0vec2, lty=2, col=2)
  lines(agefine, yregmat[,i],   col=3, lty=3)
  lines(agefine, yregmat2[,i],  col=3)
  #  plot warping functions
  plot (agefine, warpmat2[,i], type="l",
        xlab="Clock year", ylab="Biological Year")
  abline(0,1,lty=2)
}

#  compute the new mean curve as a target

y0fd3 <- mean(yregfd2)

#  plot the unregistered mean and the registered mean

par(mfrow=c(1,1),pty="s",ask=F)
plot(y0fd3, col=4, xlab="Year", ylab="Mean Velocity")
lines(y0fd2, col=3)
lines(y0fd, col=3, lty=3)
legend(10,15, c("Registered twice", "Registered once", "Unregistered"), 
       lty=c(1,1,3), col=c(4,3,3))

#  Comment:  The second round of registered made hardly any
#    difference for either the individual curves or the mean curve.


#See the analyses of the growth data for examples.

##
## 1.  smooth the growth data for the Berkeley boys
##
# Specify smoothing weight
lambda.gr2.3 <- .03
# Specify what to smooth, namely the rate of change of curvature
Lfdobj.growth    <- 2
# Set up a B-spline basis for smoothing the discrete data
nage <- length(growth$age)
norder.growth <- 6
nbasis.growth <- nage + norder.growth - 2
rng.growth <- range(growth$age)
wbasis.growth <- create.bspline.basis(rangeval=rng.growth,
                                      nbasis=nbasis.growth, norder=norder.growth,
                                      breaks=growth$age)
# Smooth the data
# in afda-ch06.R, and register to individual smooths:
cvec0.growth <- matrix(0,nbasis.growth,1)
Wfd0.growth  <- fd(cvec0.growth, wbasis.growth)
growfdPar2.3 <- fdPar(Wfd0.growth, Lfdobj.growth, lambda.gr2.3)
hgtmfd.all   <- with(growth, smooth.basis(age, hgtm, growfdPar2.3)$fd)
# Register the growth velocity rather than the
# growth curves directly
smBv <- deriv.fd(hgtmfd.all, 1)

##
## 2.  Register the first 2 Berkeley boys using the default basis
##     for the warping function
##
# register.fd takes time, so we use only 2 curves as an illustration
# to minimize computing time in these examples
nBoys <- 2
#  Define the target function as the mean of the first nBoys records
smBv0 = mean.fd(smBv[1:nBoys])
#  Register these curves.  The default choice for the functional
#  parameter object WfdParObj is used.
smB.reg.0 <- register.fd(smBv0, smBv[1:nBoys])
#  plot each curve.  Click on the R Graphics window to show each plot.
#  The left panel contains:
#    -- the unregistered curve (dashed blue line)
#    -- the target function (dashed red line)
#    -- the registered curve (solid blue line)
#  The right panel contains:
#    -- the warping function h(t)
#    -- the linear function corresponding to no warping
plotreg.fd(smB.reg.0)
#  Notice that all the warping functions all have simple shapes
#  due to the use of the simplest possible basis

if (!CRAN()) {
  ##
  ## 3.  Define a more flexible basis for the warping functions
  ##
  Wnbasis   <- 4
  Wbasis    <- create.bspline.basis(rng.growth, Wnbasis)
  Wfd0      <- fd(matrix(0,Wnbasis,1),Wbasis)
  #  set up the functional parameter object using only
  #      a light amount smoothing
  WfdParobj <- fdPar(Wfd0, Lfdobj=2, lambda=0.01)
  #  register the curves
  smB.reg.1 <- register.fd(smBv0, smBv[1:nBoys], WfdParobj)
  plotreg.fd(smB.reg.1)
  #  Notice that now the warping functions can have more complex shapes
  
  ##
  ## 4.  Change the target to the mean of the registered functions ...
  ##     this should provide a better target for registration
  ##
  smBv1 <- mean.fd(smB.reg.1$regfd)
  #  plot the old and the new targets
  par(mfrow=c(1,1),ask=FALSE)
  plot(smBv1)
  lines(smBv0, lty=2)
  #  Notice how the new target (solid line) has sharper features and
  #  a stronger pubertal growth spurt relative to the old target
  #  (dashed line).  Now register to the new target
  smB.reg.2 <- register.fd(smBv1, smBv[1:nBoys], WfdParobj)
  plotreg.fd(smB.reg.2)
  #  Plot the mean of these curves as well as the first and second targets
  par(mfrow=c(1,1),ask=FALSE)
  plot(mean.fd(smB.reg.2$regfd))
  lines(smBv0, lty=2)
  lines(smBv1, lty=3)
  #  Notice that there is almost no improvement over the age of the
  #  pubertal growth spurt, but some further detail added in the
  #  pre-pubertal region.  Now register the previously registered
  #  functions to the new target.
  smB.reg.3 <- register.fd(smBv1, smB.reg.1$regfd, WfdParobj)
  plotreg.fd(smB.reg.3)
  #  Notice that the warping functions only deviate from the straight line
  #  over the pre-pubertal region, and that there are some small adjustments
  #  to the registered curves as well over the pre-pubertal region.
}

##
## 5.  register and plot the angular acceleration of the gait data
##
gaittime  <- as.matrix(0:19)+0.5
gaitrange <- c(0,20)
#  set up a fourier basis object
gaitbasis <- create.fourier.basis(gaitrange, nbasis=21)
# set up a functional parameter object penalizing harmonic acceleration
harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)
gaitfdPar    <- fdPar(gaitbasis, harmaccelLfd, 1e-2)
#  smooth the data
gaitfd <- smooth.basis(gaittime, gait, gaitfdPar)$fd
#  compute the angular acceleration functional data object
D2gaitfd    <- deriv.fd(gaitfd,2)
names(D2gaitfd$fdnames)[[3]] <- "Angular acceleration"
D2gaitfd$fdnames[[3]] <- c("Hip", "Knee")
#  compute the mean angular acceleration functional data object
D2gaitmeanfd  <- mean.fd(D2gaitfd)
names(D2gaitmeanfd$fdnames)[[3]] <- "Mean angular acceleration"
D2gaitmeanfd$fdnames[[3]] <- c("Hip", "Knee")
#  register the functions for the first 2 boys
#  argument periodic = TRUE causes register.fd to estimate a horizontal shift
#  for each curve, which is a possibility when the data are periodic
#  set up the basis for the warping functions
nwbasis   <- 4
wbasis    <- create.bspline.basis(gaitrange,nwbasis,3)
Warpfd    <- fd(matrix(0,nwbasis,nBoys),wbasis)
WarpfdPar <- fdPar(Warpfd)
#  register the functions
gaitreglist <- register.fd(D2gaitmeanfd, D2gaitfd[1:nBoys], WarpfdPar,
                           periodic=TRUE)
#  plot the results
plotreg.fd(gaitreglist)
#  display horizonal shift values
print(round(gaitreglist$shift,1))

