library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(philentropy)

ps30_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/5-ps30train_test_all.csv", header = TRUE,row.names = 1)
ps30_all_train_test
ps30_all_train_test<-as.matrix(ps30_all_train_test)


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

