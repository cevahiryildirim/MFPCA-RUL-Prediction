
###########################20####################
###########################20####################
###########################20####################
###########################20####################
Trainandindiv20_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_20.csv", header = FALSE,row.names = 1)
Trainandindiv20_T24
Trainandindiv20_T24[1,1] <- 641.82
Trainandindiv20_T24
Trainandindiv20_T24<-as.matrix(Trainandindiv20_T24)
Trainandindiv20_T24

T24_184 <- matrix(data = NA, nrow=101, ncol=184)
for (i in 1:101) {
  T24_184[i,]<-Trainandindiv20_T24[i,1:184]
}

ENGINES<-matrix(c(seq(1,101,by=1)), nrow=101, ncol=1)

Trainandindiv20_T24<-cbind(ENGINES,T24_184)
noNA_Trainandindiv20_T24<-na.omit(Trainandindiv20_T24)
tnoNA_Trainandindiv20_T24<-t(noNA_Trainandindiv20_T24[,2:185])
tnoNA_Trainandindiv20_T24
dim(tnoNA_Trainandindiv20_T24)


###smoothing yaparak liste yapmak.
listindiv20T24_184<-list()
for (i in 1:71)
{
  SmoothT24_184 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv20_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv20_T24[,i]))),
                              y= as.vector(na.omit(noNA_Trainandindiv20_T24[i,2:185])), 
                              fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv20_T24[,i]))),5))
  listindiv20T24_184[[i]]<-SmoothT24_184
}

plot(listindiv20T24_184[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:71)
{
  lines(listindiv20T24_184[[i]], col="black")
  lines(listindiv20T24_184[[71]], col="red")}

#################DISTANCE########################
listindiv20T24_184[[1]][["fd"]][["coefs"]]

forDistance<-matrix(NA,71,5)
for(i in 1:71) {
  forDistance[i,]<-listindiv20T24_184[[i]][["fd"]][["coefs"]]
}
forDistance
tforDistance<- t(forDistance)

a<-distance(forDistance, method = "euclidean" )
a

B<-matrix(NA,71,1)
B[,1]<-a[,71]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[4]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(4,53,55,100,34,42,44,72,1,68)) {
  lines(listeT24[[i]])
} 
lines(listindiv20T24_184[[71]], col="red")


## spline 5 ise   c(40,1,29,37,3,34,27,70,69,28)
plot(listeT24[[4]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(72,1,44,53,4)) {
  lines(listeT24[[i]])
} 
lines(listindiv20T24_184[[71]], col="red")


###########################31####################
###########################31####################
###########################31####################
###########################31####################

Trainandindiv31_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_31.csv", header = FALSE,row.names = 1)
Trainandindiv31_T24
Trainandindiv31_T24[1,1] <- 641.82
Trainandindiv31_T24
Trainandindiv31_T24<-as.matrix(Trainandindiv31_T24)
Trainandindiv31_T24

T24_31_196 <- matrix(data = NA, nrow=101, ncol=196)
for (i in 1:101) {
  T24_31_196[i,]<-Trainandindiv31_T24[i,1:196]
}

Trainandindiv31_T24<-cbind(ENGINES,T24_31_196)
noNA_Trainandindiv31_T24<-na.omit(Trainandindiv31_T24)
tnoNA_Trainandindiv31_T24<-t(noNA_Trainandindiv31_T24[,2:197])
tnoNA_Trainandindiv31_T24
dim(tnoNA_Trainandindiv31_T24)


###smoothing yaparak liste yapmak.
listindiv20T24_31_196<-list()
for (i in 1:54)
{
  SmoothT24__31_196 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv31_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv31_T24[,i]))),
                                 y= as.vector(na.omit(noNA_Trainandindiv31_T24[i,2:197])), 
                                 fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv31_T24[,i]))),5))
  listindiv20T24_31_196[[i]]<-SmoothT24__31_196
}

plot(listindiv20T24_31_196[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:54)
{
  lines(listindiv20T24_31_196[[i]], col="black")
  lines(listindiv20T24_31_196[[54]], col="red")}

#################DISTANCE########################
listindiv20T24_31_196[[1]][["fd"]][["coefs"]]

forDistance31<-matrix(NA,54,5)
for(i in 1:54) {
  forDistance31[i,]<-listindiv20T24_31_196[[i]][["fd"]][["coefs"]]
}
forDistance31
tforDistance31<- t(forDistance31)

a<-distance(forDistance31, method = "euclidean" )
a

B<-matrix(NA,54,1)
B[,1]<-a[,54]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[43]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(43,42,79,72,100,50,88,89,33,47)) {
  lines(listeT24[[i]])
} 
lines(listindiv20T24_31_196[[54]], col="red")

###########################34####################
###########################34####################
###########################34####################
###########################34####################

Trainandindiv34_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_34.csv", header = FALSE,row.names = 1)
Trainandindiv34_T24
Trainandindiv34_T24[1,1] <- 641.82
Trainandindiv34_T24
Trainandindiv34_T24<-as.matrix(Trainandindiv34_T24)
Trainandindiv34_T24

T24_34_198 <- matrix(data = NA, nrow=101, ncol=198)
for (i in 1:101) {
  T24_34_198[i,]<-Trainandindiv34_T24[i,1:198]
}

Trainandindiv34_T24<-cbind(ENGINES,T24_34_198)
noNA_Trainandindiv34_T24<-na.omit(Trainandindiv34_T24)
tnoNA_Trainandindiv34_T24<-t(noNA_Trainandindiv34_T24[,2:199])
tnoNA_Trainandindiv34_T24
dim(tnoNA_Trainandindiv34_T24)


###smoothing yaparak liste yapmak.
listindiv20T24_34_198<-list()
for (i in 1:53)
{
  SmoothT24__34_198 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv34_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv34_T24[,i]))),
                                     y= as.vector(na.omit(noNA_Trainandindiv34_T24[i,2:199])), 
                                     fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv34_T24[,i]))),5))
  listindiv20T24_34_198[[i]]<-SmoothT24__34_198
}

plot(listindiv20T24_34_198[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:53)
{
  lines(listindiv20T24_34_198[[i]], col="black")
  lines(listindiv20T24_34_198[[53]], col="red")}

#################DISTANCE########################
listindiv20T24_34_198[[1]][["fd"]][["coefs"]]

forDistance34<-matrix(NA,53,5)
for(i in 1:53) {
  forDistance34[i,]<-listindiv20T24_34_198[[i]][["fd"]][["coefs"]]
}
forDistance34
tforDistance34<- t(forDistance34)

a<-distance(forDistance34, method = "euclidean" )
a

B<-matrix(NA,53,1)
B[,1]<-a[,53]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[33]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(33,43,79,9,51)) {
  lines(listeT24[[i]])
} 
lines(listindiv20T24_31_196[[54]], col="red")

###########################35####################
###########################35####################
###########################35####################
###########################35####################

Trainandindiv35_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_35.csv", header = FALSE,row.names = 1)
Trainandindiv35_T24
Trainandindiv35_T24[1,1] <- 641.82
Trainandindiv35_T24
Trainandindiv35_T24<-as.matrix(Trainandindiv35_T24)
Trainandindiv35_T24

T24_35_199 <- matrix(data = NA, nrow=101, ncol=199)
for (i in 1:101) {
  T24_35_199[i,]<-Trainandindiv35_T24[i,1:199]
}

Trainandindiv35_T24<-cbind(ENGINES,T24_35_199)
noNA_Trainandindiv35_T24<-na.omit(Trainandindiv35_T24)
tnoNA_Trainandindiv35_T24<-t(noNA_Trainandindiv35_T24[,2:199])
tnoNA_Trainandindiv35_T24
dim(tnoNA_Trainandindiv35_T24)


###smoothing yaparak liste yapmak.
listindiv35T24_199<-list()
for (i in 1:51)
{
  SmoothT24_35_199 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv35_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv35_T24[,i]))),
                                     y= as.vector(na.omit(noNA_Trainandindiv35_T24[i,2:199])), 
                                     fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv35_T24[,i]))),5))
  listindiv35T24_199[[i]]<-SmoothT24_35_199
}

plot(listindiv35T24_199[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:51)
{
  lines(listindiv35T24_199[[i]], col="black")
  lines(listindiv35T24_199[[51]], col="red")}

#################DISTANCE########################
listindiv35T24_199[[1]][["fd"]][["coefs"]]

forDistance35<-matrix(NA,51,5)
for(i in 1:51) {
  forDistance35[i,]<-listindiv35T24_199[[i]][["fd"]][["coefs"]]
}
forDistance35
tforDistance35<- t(forDistance35)

a<-distance(forDistance35, method = "euclidean" )
a

B<-matrix(NA,51,1)
B[,1]<-a[,51]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[71]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(71,75,81,76,26)) {
  lines(listeT24[[i]])
} 
lines(listindiv35T24_199[[51]], col="red")


###########################49####################
###########################49####################
###########################49####################
###########################49####################

Trainandindiv49_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_49.csv", header = FALSE,row.names = 1)
Trainandindiv49_T24
Trainandindiv49_T24[1,1] <- 641.82
Trainandindiv49_T24
Trainandindiv49_T24<-as.matrix(Trainandindiv49_T24)
Trainandindiv49_T24

T24_49_303 <- matrix(data = NA, nrow=101, ncol=303)
for (i in 1:101) {
  T24_49_303[i,]<-Trainandindiv49_T24[i,1:303]
}

Trainandindiv49_T24<-cbind(ENGINES,T24_49_303)
noNA_Trainandindiv49_T24<-na.omit(Trainandindiv49_T24)
tnoNA_Trainandindiv49_T24<-t(noNA_Trainandindiv49_T24[,2:304])
tnoNA_Trainandindiv49_T24
dim(tnoNA_Trainandindiv49_T24)


###smoothing yaparak liste yapmak.
listindiv49T24_303<-list()
for (i in 1:5)
{
  SmoothT24_35_199 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv49_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv49_T24[,i]))),
                                    y= as.vector(na.omit(noNA_Trainandindiv49_T24[i,2:304])), 
                                    fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv49_T24[,i]))),5))
  listindiv49T24_303[[i]]<-SmoothT24_35_199
}

plot(listindiv49T24_303[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:4)
{
  lines(listindiv49T24_303[[i]], col="black")
  lines(listindiv49T24_303[[5]], col="red")}

#################DISTANCE########################
listindiv49T24_303[[1]][["fd"]][["coefs"]]

forDistance49<-matrix(NA,5,5)
for(i in 1:5) {
  forDistance49[i,]<-listindiv49T24_303[[i]][["fd"]][["coefs"]]
}
forDistance49
tforDistance49<- t(forDistance49)

a<-distance(forDistance49, method = "euclidean" )
a

B<-matrix(NA,5,1)
B[,1]<-a[,5]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[67]],xlim=c(0,362), ylim=c(641.7,644))
for (i in c(67,69,92,96)) {
  lines(listeT24[[i]])
} 
lines(listindiv49T24_303[[5]], col="red")


###########################68####################
###########################68####################
###########################68####################
###########################68####################

Trainandindiv68_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_68.csv", header = FALSE,row.names = 1)
Trainandindiv68_T24
Trainandindiv68_T24[1,1] <- 641.82
Trainandindiv68_T24
Trainandindiv68_T24<-as.matrix(Trainandindiv68_T24)
Trainandindiv68_T24

T24_68_187 <- matrix(data = NA, nrow=101, ncol=187)
for (i in 1:101) {
  T24_68_187[i,]<-Trainandindiv68_T24[i,1:187]
}

Trainandindiv68_T24<-cbind(ENGINES,T24_68_187)
noNA_Trainandindiv68_T24<-na.omit(Trainandindiv68_T24)
tnoNA_Trainandindiv68_T24<-t(noNA_Trainandindiv68_T24[,2:188])
tnoNA_Trainandindiv68_T24
dim(tnoNA_Trainandindiv68_T24)


###smoothing yaparak liste yapmak.
listindiv68T24_187<-list()
for (i in 1:68)
{
  SmoothT24_68_187 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv68_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv68_T24[,i]))),
                                    y= as.vector(na.omit(noNA_Trainandindiv68_T24[i,2:188])), 
                                    fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv68_T24[,i]))),5))
  listindiv68T24_187[[i]]<-SmoothT24_68_187
}

plot(listindiv68T24_187[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:68)
{
  lines(listindiv68T24_187[[i]], col="black")
  lines(listindiv68T24_187[[68]], col="red")}

#################DISTANCE########################
listindiv68T24_187[[1]][["fd"]][["coefs"]]

forDistance68<-matrix(NA,68,5)
for(i in 1:68) {
  forDistance68[i,]<-listindiv68T24_187[[i]][["fd"]][["coefs"]]
}
forDistance68
tforDistance68<- t(forDistance68)

a<-distance(forDistance68, method = "euclidean" )
a

B<-matrix(NA,68,1)
B[,1]<-a[,68]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[42]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(42,100,50,52,4)) {
  lines(listeT24[[i]])
} 
lines(listindiv68T24_187[[68]], col="red")


###########################76####################
###########################76####################
###########################76####################
###########################76####################

Trainandindiv76_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_76.csv", header = FALSE,row.names = 1)
Trainandindiv76_T24
Trainandindiv76_T24[1,1] <- 641.82
Trainandindiv76_T24
Trainandindiv76_T24<-as.matrix(Trainandindiv76_T24)
Trainandindiv76_T24

T24_76_205 <- matrix(data = NA, nrow=101, ncol=205)
for (i in 1:101) {
  T24_76_205[i,]<-Trainandindiv76_T24[i,1:205]
}

Trainandindiv76_T24<-cbind(ENGINES,T24_76_205)
noNA_Trainandindiv76_T24<-na.omit(Trainandindiv76_T24)
tnoNA_Trainandindiv76_T24<-t(noNA_Trainandindiv76_T24[,2:206])
tnoNA_Trainandindiv76_T24
dim(tnoNA_Trainandindiv76_T24)


###smoothing yaparak liste yapmak.
listindiv76T24_205<-list()
for (i in 1:43)
{
  SmoothT24_76_205 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv76_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv76_T24[,i]))),
                                    y= as.vector(na.omit(noNA_Trainandindiv76_T24[i,2:206])), 
                                    fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv76_T24[,i]))),5))
  listindiv76T24_205[[i]]<-SmoothT24_76_205
}

plot(listindiv76T24_205[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:43)
{
  lines(listindiv76T24_205[[i]], col="black")
  lines(listindiv76T24_205[[43]], col="red")}

#################DISTANCE########################
listindiv76T24_205[[1]][["fd"]][["coefs"]]

forDistance76<-matrix(NA,43,5)
for(i in 1:43) {
  forDistance76[i,]<-listindiv76T24_205[[i]][["fd"]][["coefs"]]
}
forDistance76
tforDistance76<- t(forDistance76)

a<-distance(forDistance76, method = "euclidean" )
a

B<-matrix(NA,43,1)
B[,1]<-a[,43]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[89]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(89,10,82,47,59)) {
  lines(listeT24[[i]])
} 
lines(listindiv76T24_205[[43]], col="red")


###########################81####################
###########################81####################
###########################81####################
###########################81####################

Trainandindiv81_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_81.csv", header = FALSE,row.names = 1)
Trainandindiv81_T24
Trainandindiv81_T24[1,1] <- 641.82
Trainandindiv81_T24
Trainandindiv81_T24<-as.matrix(Trainandindiv81_T24)
Trainandindiv81_T24

T24_81_213 <- matrix(data = NA, nrow=101, ncol=213)
for (i in 1:101) {
  T24_81_213[i,]<-Trainandindiv81_T24[i,1:213]
}

Trainandindiv81_T24<-cbind(ENGINES,T24_81_213)
noNA_Trainandindiv81_T24<-na.omit(Trainandindiv81_T24)
tnoNA_Trainandindiv81_T24<-t(noNA_Trainandindiv81_T24[,2:214])
tnoNA_Trainandindiv81_T24
dim(tnoNA_Trainandindiv81_T24)


###smoothing yaparak liste yapmak.
listindiv81T24_213<-list()
for (i in 1:38)
{
  SmoothT24_81_213 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv81_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv81_T24[,i]))),
                                    y= as.vector(na.omit(noNA_Trainandindiv81_T24[i,2:214])), 
                                    fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv81_T24[,i]))),5))
  listindiv81T24_213[[i]]<-SmoothT24_81_213
}

plot(listindiv81T24_213[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:38)
{
  lines(listindiv81T24_213[[i]], col="black")
  lines(listindiv81T24_213[[38]], col="red")}

#################DISTANCE########################
listindiv81T24_213[[1]][["fd"]][["coefs"]]

forDistance81<-matrix(NA,38,5)
for(i in 1:38) {
  forDistance81[i,]<-listindiv81T24_213[[i]][["fd"]][["coefs"]]
}
forDistance81
tforDistance81<- t(forDistance81)

a<-distance(forDistance81, method = "euclidean" )
a

B<-matrix(NA,38,1)
B[,1]<-a[,38]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[89]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(89,88,72,41,81)) {
  lines(listeT24[[i]])
} 
lines(listindiv81T24_213[[38]], col="red")


###########################82####################
###########################82####################
###########################82####################
###########################82####################

Trainandindiv82_T24<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_82.csv", header = FALSE,row.names = 1)
Trainandindiv82_T24
Trainandindiv82_T24[1,1] <- 641.82
Trainandindiv82_T24
Trainandindiv82_T24<-as.matrix(Trainandindiv82_T24)
Trainandindiv82_T24

T24_82_162 <- matrix(data = NA, nrow=101, ncol=162)
for (i in 1:101) {
  T24_82_162[i,]<-Trainandindiv82_T24[i,1:162]
}

Trainandindiv82_T24<-cbind(ENGINES,T24_82_162)
noNA_Trainandindiv82_T24<-na.omit(Trainandindiv82_T24)
tnoNA_Trainandindiv82_T24<-t(noNA_Trainandindiv82_T24[,2:163])
tnoNA_Trainandindiv82_T24
dim(tnoNA_Trainandindiv82_T24)


###smoothing yaparak liste yapmak.
listindiv82T24_162<-list()
for (i in 1:85)
{
  SmoothT24_82_162 <- smooth.basis( argvals = seq(1,length(na.omit(tnoNA_Trainandindiv82_T24[,i])), length.out= length(na.omit(tnoNA_Trainandindiv82_T24[,i]))),
                                    y= as.vector(na.omit(noNA_Trainandindiv82_T24[i,2:163])), 
                                    fdParobj = create.bspline.basis(c(1,length(na.omit(tnoNA_Trainandindiv82_T24[,i]))),5))
  listindiv82T24_162[[i]]<-SmoothT24_82_162
}

plot(listindiv82T24_162[[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 2:85)
{
  lines(listindiv82T24_162[[i]], col="black")
  lines(listindiv82T24_162[[85]], col="red")}

#################DISTANCE########################
listindiv82T24_162[[1]][["fd"]][["coefs"]]

forDistance82<-matrix(NA,85,5)
for(i in 1:85) {
  forDistance82[i,]<-listindiv82T24_162[[i]][["fd"]][["coefs"]]
}
forDistance82
tforDistance82<- t(forDistance82)

a<-distance(forDistance82, method = "euclidean" )
a

B<-matrix(NA,85,1)
B[,1]<-a[,85]
B

## spline 10 ise  c(3,37,39,70,23,27,29,49,1,46)
plot(listeT24[[3]],xlim=c(0,305), ylim=c(641.7,644))
for (i in c(3,87,100,44,53,4,74,55,72,
} 
lines(listindiv82T24_162[[85]], col="red")


