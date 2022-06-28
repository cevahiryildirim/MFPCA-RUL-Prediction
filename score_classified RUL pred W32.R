library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
library(philentropy)

W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
W32_all_train_test
W32_all_train_test<-as.matrix(W32_all_train_test)


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
  for (j in 1:66) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainLowScores[j],1:length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,])]
    list_test_matrix_W32_SCORE[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE[[i]][66,]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in TestBigClassEng) { 
  W32_test_matrix<- matrix(data = NA, nrow=(length(TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,]))
  for (j in 1:36) {
    W32_test_matrix[j,]<-W32_all_train_test[TrainBigScores[j],1:length(as.matrix(list_test_matrix_W32_SCORE[[i]])[1,])]
    list_test_matrix_W32_SCORE[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_matrix_W32_SCORE[[i]][36,]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_W32_SCORE[[4]]
test3<-list_test_matrix_W32_SCORE[[5]]


length(TestLowClassEng)
#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_W32_noNA<-list()
for (i in TestLowClassEng)
{
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_W32_SCORE[[i]][66,])))
  noNA_test_matrix_W32 <-na.omit(list_test_matrix_W32_SCORE[[i]])
  list_test_matrix_SCORE_W32_noNA[[i]]<-noNA_test_matrix_W32
}
for (i in TestBigClassEng)
{ 
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_W32_SCORE[[i]][36,])))
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

