library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(philentropy)

BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
BPR_all_train_test
BPR_all_train_test<-as.matrix(BPR_all_train_test)


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
  for (j in 1:66) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,])]
    list_test_matrix_BPR_SCORE[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE[[i]][66,]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  BPR_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,]))
  for (j in 1:36) {
    BPR_test_matrix[j,]<-BPR_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_BPR_SCORE[[i]])[1,])]
    list_test_matrix_BPR_SCORE[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_matrix_BPR_SCORE[[i]][36,]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_BPR_SCORE[[4]]
test3<-list_test_matrix_BPR_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_BPR_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_BPR_SCORE[[i]][66,])))
  noNA_test_matrix_BPR <-na.omit(list_test_matrix_BPR_SCORE[[i]])
  list_test_matrix_SCORE_BPR_noNA[[i]]<-noNA_test_matrix_BPR
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_BPR_SCORE[[i]][36,])))
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

