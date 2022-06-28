library(Surrogate)
library(MASS) ##for generate multinormal data
library(pracma) ##for integral
library(matrixcalc) ##for positive definite
library(extraDistr) ##for dvnorm

data("Schizo")
Schizo<- Schizo[-c(405,  705, 1358, 1719, 2111),]
placebo<-Schizo[Schizo$Treat == "-1",]
experimental<-Schizo[Schizo$Treat == "1",]

Tr<-Schizo$PANSS
S<-Schizo$BPRS

T0<- placebo$PANSS
T1<- experimental$PANSS
S0<- placebo$BPRS
S1<- experimental$BPRS

##means
mT0<-mean(T0)
mT1<-mean(T1)
mS0<-mean(S0)
mS1<-mean(S1)

beta<-mT1-mT0
alpha<- mS1-mS0
muu<- c(mT0, mT1, mS0, mS1)
mudelta<-c(beta,alpha)

##variances
vT0T0<-var(T0,T0)
vT1T1<-var(T1,T1)
vS1S1<-var(S1,S1)
vS0S0<-var(S0,S0)
vT1S1<-var(T1,S1)
vT0S0<-var(T0,S0)


##correlations
cT0S0<-cor(T0,S0)
cT1S1<-cor(T1,S1)
cTS<-cor(Tr,S)

ICA<-ICA.ContCont(cT0S0, cT1S1, vT0T0, vT1T1, vS0S0, vS1S1, T0T1=seq(-1, 1, by=.05),
                  T0S1=seq(-1, 1, by=.05), T1S0=seq(-1, 1, by=.05), S0S1=seq(-1, 1, by=.05))



##ICA features
rhodelta<-ICA$ICA
min(rhodelta)
Ica<-rhodelta*rhodelta
min(Ica)

##prints
print( paste("mean ICA=",mean(ICA$ICA)))
print( paste("median ICA=",median(ICA$ICA)))
print( paste("maximum ICA=",max(ICA$ICA)))
print( paste("minimum ICA=",min(ICA$ICA)))
print( paste("sd ICA=",sd(ICA$ICA)))

pdf<-cbind(ICA$Pos.Def,rhodelta,Ica)

##mutual information
-0.5*log(1-mean(ICA$ICA)^2)
1-exp(-2*1.214241)



##according to pdf matrix: sigma_delta_T and sigma_delta_S and check ICA again
sigmadelta_ss<-list()
sigmadelta_tt<-list()
truefalseresult<-list()
ICAformul1<-list()

for (i in 1:nrow(ICA$Pos.Def) )
  
{
  
  ### ICA values calculated agian for double check
  pay<-(sqrt(vT0T0*vS0S0)*cT0S0)+(sqrt(vT1T1*vS1S1)*cT1S1)-(sqrt(vT1T1*vS0S0)*ICA$Pos.Def[i,4])-(sqrt(vT0T0*vS1S1)*ICA$Pos.Def[i,3])
  payda<-sqrt((vT0T0+vT1T1-2*sqrt(vT0T0*vT1T1)*ICA$Pos.Def[i,1])*(vS0S0+vS1S1-2*sqrt(vS0S0*vS1S1)*ICA$Pos.Def[i,6]))
  ICAformul<-pay/payda
  
  ICAformul1<- append(ICAformul, ICAformul1)
  ICAformul1<-as.matrix(ICAformul1)
  ICAformul1<-as.numeric(ICAformul1)
  
  sigmadelta_s<-var(S0,S0)+var(S1,S1)-2*ICA$Pos.Def[i,6]*sqrt(var(S0,S0)*var(S1,S1))
  sigmadelta_ss<- append(sigmadelta_s,sigmadelta_ss)
  sigmadelta_t<-var(T0,T0)+var(T1,T1)-2*ICA$Pos.Def[i,1]*sqrt(var(T0,T0)*var(T1,T1))
  sigmadelta_tt<- append(sigmadelta_t,sigmadelta_tt)
  cormatrix<- matrix(c(1, ICA$Pos.Def[i,1], ICA$Pos.Def[i,2],ICA$Pos.Def[i,3],
                       ICA$Pos.Def[i,1],1,ICA$Pos.Def[i,4],ICA$Pos.Def[i,5],
                       ICA$Pos.Def[i,2],ICA$Pos.Def[i,4],1,ICA$Pos.Def[i,6],
                       ICA$Pos.Def[i,3],ICA$Pos.Def[i,5],ICA$Pos.Def[i,6], 1 ), nrow = 4)
  result<- is.positive.definite(cormatrix)
  truefalseresult<-append(result,truefalseresult)
}


###combine all results and check
ICAformul1<-matrix(c(ICAformul1), ncol = 1)
df2=ICAformul1[order(nrow(ICAformul1):1),] ##for order
pdf<- cbind(pdf,df2)
sigmadelta_tt<-as.matrix(sigmadelta_tt)
sigmadelta_ss<-as.matrix(sigmadelta_ss)

sigmadeltass<-cbind(sigmadelta_tt, sigmadelta_ss)
sigmadelta<-sigmadeltass[order(nrow(sigmadeltass):1),]

ICAS<-cbind(ICA$Pos.Def, df2)
matrixforr<- cbind(sigmadelta_tt,   sigmadelta_ss,  ICAformul1, truefalseresult)
matrixforr<- matrixforr[order(nrow(matrixforr):1),]
fullmatrix<-cbind(pdf, sigmadelta)
print(fullmatrix)



##for using above results RENYİ and KL calculate


####Renyi 1.25
Renyi125<-list()
ICA_alpha125<-list()

for (i in 1:nrow(fullmatrix)  )
  
{
  
  fx<-function(x) dnorm(x,mean = beta,sd=sqrt( as.numeric(fullmatrix[i,10] )))
  fy<-function(y) dnorm(y,mean=alpha,sd=sqrt( as.numeric(fullmatrix[i,11])))
  fxy<- function(x,y) dbvnorm(x,y, mean1 = beta, mean2 = alpha, sd1 =sqrt( as.numeric(fullmatrix[i,10])) , sd2 = sqrt(as.numeric(fullmatrix[i,11])), cor =as.numeric(fullmatrix[i,7]))
  ralpha125<-1.25
  RY125<- function(x,y) (fxy(x,y)^ralpha125) * ( (fx(x)*fy(y)) ^(1-ralpha125) )
  
  
  #try(print(paste("Renyi1.25","=",     R125<-integral2(RY125,-140, 140, -135,135, reltol = 1e-10))))
  
  R125<-integral2(RY125,-105, 105, -135,135, reltol = 1e-10)
  R125for<- (1/(ralpha125-1))* log(R125$Q)
  Renyi125<-append(R125for, Renyi125)
  ICA_alpha125for<-1-exp(-2*R125for)
  ICA_alpha125<-append(ICA_alpha125for,ICA_alpha125)
  
}

ICA_alpha125<-as.numeric(as.matrix(ICA_alpha125))
mean(ICA_alpha125)
min(ICA_alpha125)
max(ICA_alpha125)

hist(ICA_alpha125,  main = bquote('Histogram of'~ICA[1.25]), xlab = bquote(ICA[1.25]))


##Renyi 0.5
ICA_alpha05<-list()
Renyi05<-list()
for (i in 1:nrow(fullmatrix)  )
  
{
  ralpha05<-0.5
  
  fx<-function(x) dnorm(x,mean = beta,sd=sqrt( as.numeric(fullmatrix[i,10] )))
  fy<-function(y) dnorm(y,mean =alpha,sd=sqrt( as.numeric(fullmatrix[i,11])))
  fxy<- function(x,y) dbvnorm(x,y, mean1 = beta, mean2 = alpha, sd1 =sqrt( as.numeric(fullmatrix[i,10])) , sd2 = sqrt(as.numeric(fullmatrix[i,11])), cor =fullmatrix[i,7])
  
  RY05<- function(x,y) fxy(x,y)^ralpha05 * ( (fx(x)*fy(y)) ^(1-ralpha05))
  
  #try(print(paste("Renyi05","=",     R05<-integral2(RY, -135, 135, -135, 135, reltol = 1e-10))))
  R05<-integral2(RY05, -135, 135, -135, 135, reltol = 1e-10)
  R05for<- (1/ (ralpha05-1))* log(R05$Q)
  Renyi05<-append(R05for, Renyi05)
  ICA_alpha05for<-1-exp(-2*R05for)
  ICA_alpha05<-append(ICA_alpha05for,ICA_alpha05)
}


ICA_alpha05<-as.numeric(as.matrix(ICA_alpha05))
mean(ICA_alpha05)
min(ICA_alpha05)
max(ICA_alpha05)
hist(ICA_alpha05,  main = bquote('Histogram of'~ICA[0.5]), xlab = bquote(ICA[0.5]))


###KL (with try function)

KL<- list()
ICA_KL<-list()
for (i in 1:nrow(fullmatrix)  )
  
{
  
  fx<-function(x) dnorm(x,mean = beta,sd=sqrt( as.numeric(fullmatrix[i,10] )))
  fy<-function(y) dnorm(y,mean =alpha,sd=sqrt( as.numeric(fullmatrix[i,11])))
  fxy<- function(x,y) dbvnorm(x,y, mean1 = beta, mean2 = alpha, sd1 =sqrt( as.numeric(fullmatrix[i,10])) , sd2 = sqrt(as.numeric(fullmatrix[i,11])),   cor =fullmatrix[i,7])
  KLfun<- function(x,y)  fxy(x,y)* ( log(  (fxy(x,y)) / (fx(x)*fy(y)) ))
  try(print(paste(i,"KLfor","=",     KLfor<-integral2(KLfun,-105, 105, -105, 105,  reltol = 1e-10)
  )))
  
  KL<- append(KLfor$Q, KL)
  ICA_KLfor<-1-exp(-2*KLfor$Q)
  ICA_KL<- append(ICA_KLfor, ICA_KL)
  
}

##if I use all result
as.numeric(as.matrix(ICA_KL))
mean(as.numeric(as.matrix(ICA_KL)))
min(as.numeric(as.matrix(ICA_KL)))
max(as.numeric(as.matrix(ICA_KL)))

###if I delete duplicate (becasue error terms convert the previously step)
KLL<-as.matrix(ICA_KL)
KLL<-KLL[c(which(duplicated(KLL)== 'FALSE')),]
KLL<-as.numeric(KLL)

print(mean(KLL))
print(median(KLL))
print(max(KLL))
print(min(KLL))

length(KLL)


##Example: above i=2 doesnt work, but for i=2 change bounds
i<-2


fx<-function(x) dnorm(x,mean = beta,sd=sqrt( as.numeric(fullmatrix[i,10] )))
fy<-function(y) dnorm(y,mean =alpha,sd=sqrt( as.numeric(fullmatrix[i,11])))
fxy<- function(x,y) dbvnorm(x,y, mean1 = beta, mean2 = alpha, sd1 =sqrt( as.numeric(fullmatrix[i,10])) , sd2 = sqrt(as.numeric(fullmatrix[i,11])),   cor =fullmatrix[i,7])
KLfun<- function(x,y)  fxy(x,y)* ( log(  (fxy(x,y)) / (fx(x)*fy(y)) ))
##KLfor<-integral2(sKLfun,-171, 89, -151,105, reltol = 1e-10) ##for 1. metric
KLfor<-integral2(KLfun, -15, 175, -15,105, reltol = 1e-10) ##for 2. metric

1-exp(-2*KLfor$Q)
