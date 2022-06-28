dffirst<-df[1:100,]

dffirst[,1]<-seq(1:100)

dffirst


library(multimode)
dffirst
modetest(dffirst[,2])



lowscoredata<-dffirst[,2][which(dffirst[,2]<2.137284)]
bigscoredata<-dffirst[,2][which(dffirst[,2]>2.137284)]

length(bigscoredata)
length(lowscoredata)

lowscoredatamatrix<-matrix(NA,ncol=2, nrow=length(lowscoredata))
lowscoredatamatrix[,2]<-dffirst[,2][which(dffirst[,2]<2.137284)]
lowscoredatamatrix[,1]<-dffirst[,1][which(dffirst[,2]<2.137284)]

bigscoredatamatrix<-matrix(NA,ncol=2, nrow=length(bigscoredata))
bigscoredatamatrix[,2]<-dffirst[,2][which(dffirst[,2]>2.137284)]
bigscoredatamatrix[,1]<-dffirst[,1][which(dffirst[,2]>2.137284)]


lowscoredatamatrix
bigscoredatamatrix

T24traindata<-read.csv("C:/Users/cevah/Desktop/data_for_registration/1-T24.csv", header = FALSE)
T24traindata[1,1] <- 641.82
T24traindata<-as.matrix(T24traindata)
T24traindata<-as.numeric(T24traindata)
T24traindata<-matrix(T24traindata,100,362)
colnames(T24traindata) <- newheaders
T24traindata

T24initials<-T24traindata[,1]

T24initials

lowscoreinitials<-T24initials[c(lowscoredatamatrix[,1])]
highscoreinitials<-T24initials[c(bigscoredatamatrix[,1])]

lowscoreinitials
highscoreinitials

#Compute the empirical distribution function for both separately in a grid:

gridxy=seq(min(lowscoreinitials,highscoreinitials),max(lowscoreinitials,highscoreinitials),length.out=1000)
F=numeric()
G=numeric()
for (i in 1:1000){
  F[i]=mean(lowscoreinitials<=gridxy[i])
  G[i]=mean(highscoreinitials<=gridxy[i])
}

F
G



#The Youden index is the maximum difference between these two functions: 
J=max()
plot(gridxy,F,type="l",ylim=c(0,1))
lines(gridxy,G,col="red")

J=min(F-G)
which((F-G)==J)

#T=max(F-G)
#p=which((F-G)==T)


#This last sentence gives you the optimal cutoff point. It may be ties (not probable but not impossible). In such a case, we can just take any of the cases. Let's define then:
k=which((F-G)==J)
c=gridxy[k[1]]
c 

#So those curves whose initial point is below c will be classified as red (see **) and black otherwise

#testing

youdenlow <- T24initials[which(T24initials<c)]
length(youdenlow)
length(lowscoredatamatrix[,2])

youdenbig <- T24initials[which(T24initials>c)]
length(youdenbig)


initialsmatrix<-matrix(NA,ncol=2, nrow=100)
initialsmatrix[,2]<-T24initials
initialsmatrix[,1]<-seq(1:100)


initialsYoudenLow<-matrix(NA,ncol=2, nrow=length(youdenlow))
initialsYoudenLow[,2]<-initialsmatrix[,2][which(initialsmatrix[,2]<c)]
initialsYoudenLow[,1]<-initialsmatrix[,1][which(initialsmatrix[,2]<c)]


initialsYoudenBig<-matrix(NA,ncol=2, nrow=length(youdenbig))
initialsYoudenBig[,2]<-initialsmatrix[,2][which(initialsmatrix[,2]>c)]
initialsYoudenBig[,1]<-initialsmatrix[,1][which(initialsmatrix[,2]>c)]


lowscoredatamatrix
initialsYoudenLow

equal(lowscoredatamatrix,initialsYoudenLow)

all.equal(as.vector(lowscoredatamatrix[,1]),as.vector(initialsYoudenLow[,1]))


