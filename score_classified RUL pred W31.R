library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(philentropy)

W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
W31_all_train_test
W31_all_train_test<-as.matrix(W31_all_train_test)


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
  for (j in 1:66) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,])]
    list_test_matrix_W31_SCORE[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE[[i]][66,]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W31_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,]))
  for (j in 1:36) {
    W31_test_matrix[j,]<-W31_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_W31_SCORE[[i]])[1,])]
    list_test_matrix_W31_SCORE[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_matrix_W31_SCORE[[i]][36,]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_W31_SCORE[[4]]
test3<-list_test_matrix_W31_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_W31_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_W31_SCORE[[i]][66,])))
  noNA_test_matrix_W31 <-na.omit(list_test_matrix_W31_SCORE[[i]])
  list_test_matrix_SCORE_W31_noNA[[i]]<-noNA_test_matrix_W31
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_W31_SCORE[[i]][36,])))
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

