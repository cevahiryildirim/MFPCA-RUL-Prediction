library(funData)
library(ggplot2)
library(fda)
library(MFPCA)

T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
T24_all_train_test
T24_all_train_test<-as.matrix(T24_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_matrix<-list()
for (i in 1:100)
{ 
    T24_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(T24_all_train_test[100+i,])))
    list_test_matrix[[i]]<-T24_test_matrix_length
}



#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
T24TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
T24TESTinit[,2]<-as.vector(T24_all_train_test[101:200,2])
T24TESTinit[,1]<-seq(1:100)

T24TESTinitlow<-T24TESTinit[,1][which(T24TESTinit[,2]<642.4918)] #test datasında albanın bulduğu low scoreları ayırıdm
T24TESTinitbig<-T24TESTinit[,1][which(T24TESTinit[,2]>642.4918)] #test datasında albanın bulduğu high scoreları ayırıdm.

####train datasında low ve high scoreları ayırdım.
T24TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
T24TrainScoresmatrix<-matrix(NA,100,3)
T24TrainScoresmatrix[,1]<-seq(1:100)
T24TrainScoresmatrix[,2]<-T24TrainScores[,1]
T24TrainScoresmatrix[,3]<-T24TrainScores[,2]

T24TrainLowScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Low")])
T24TrainBigScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Big")])

##testte low ve high scorelar için liste yaptım
list_test_matrix_SCORE<-list()
for (i in T24TrainLowScores)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(T24TrainLowScores)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_SCORE[[i]]<-T24_test_matrix_length
}

for (i in T24TrainBigScores)
{ 
  T24_test_matrix_length <- matrix(data = NA, nrow=(length(T24TrainBigScores)+1), ncol=length(na.omit(T24_all_train_test[100+i,])))
  list_test_matrix_SCORE[[i]]<-T24_test_matrix_length
}

T24TrainLowScores[i]
i=4
#### fill all lists with 100 train + row101 for related  test engine
for (i in T24TrainLowScores) { 
 T24_test_matrix<- matrix(data = NA, nrow=(length(T24TrainLowScores)+1), ncol=length(as.matrix(list_test_matrix_SCORE[[i]])[1,]))
  for (j in 1:66) {
    T24_test_matrix[j,]<-T24_all_train_test[T24TrainLowScores[j],1:length(as.matrix(list_test_matrix_SCORE[[i]])[1,])]
    list_test_matrix_SCORE[[i]][j,]<-T24_test_matrix[j,]
  }
 list_test_matrix_SCORE[[i]][66,]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

for (i in T24TrainBigScores) { 
  T24_test_matrix<- matrix(data = NA, nrow=(length(T24TrainBigScores)+1), ncol=length(as.matrix(list_test_matrix_SCORE[[i]])[1,]))
  for (j in 1:36) {
    T24_test_matrix[j,]<-T24_all_train_test[T24TrainBigScores[j],1:length(as.matrix(list_test_matrix_SCORE[[i]])[1,])]
    list_test_matrix_SCORE[[i]][j,]<-T24_test_matrix[j,]
  }
  list_test_matrix_SCORE[[i]][36,]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
}

test3<-list_test_matrix_SCORE[[4]]
test3<-list_test_matrix_SCORE[[5]]
########################################################################

 for (i in 1:100) { 
   T24_test_matrix<- matrix(data = NA, nrow=101, ncol=length(as.matrix(list_test_matrix[[i]])[1,]))
   for (j in 1:101) {
     T24_test_matrix[j,]<-T24_all_train_test[j,1:length(as.matrix(list_test_matrix[[i]])[1,])]
     list_test_matrix[[i]][j,]<-T24_test_matrix[j,]
   }
   list_test_matrix[[i]][101,]<-T24_all_train_test[100+i, 1:length(as.matrix(list_test_matrix[[i]])[1,])]
 }



#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_matrix_SCORE_noNA<-list()
for (i in T24TrainLowScores)
{ 
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=66, ncol=length(na.omit(list_test_matrix_SCORE[[i]][66,])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_SCORE[[i]])
  list_test_matrix_SCORE_noNA[[i]]<-noNA_test_matrix_T24
}
for (i in T24TrainBigScores)
{ 
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=36, ncol=length(na.omit(list_test_matrix_SCORE[[i]][36,])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix_SCORE[[i]])
  list_test_matrix_SCORE_noNA[[i]]<-noNA_test_matrix_T24
}

#####list with transpose of each matrix

list_test_matrix_t_SCORE_noNA<-list()
for (i in 1:100)
{ 
  list_test_matrix_t_SCORE_noNA[[i]]<-t(list_test_matrix_SCORE_noNA[[i]])
}


######smoothing for each engine
list_test_all_smooth_T24<-list()
list_test_smoothing_T24<-list()
no_of_splines<-10
for (j in 1:100) {
  
  for (i in 1:nrow(list_test_matrix_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24[[i]]<-Smooth
  }
  list_test_all_smooth_T24[[j]]<-list_test_smoothing_T24
}
i=1
j=4
######smoothing for each engine
list_test_all_smooth_T24_scores<-list()
list_test_smoothing_T24_scores<-list()
no_of_splines<-10
for (j in 1:100) {
  
  for (i in 1:nrow(list_test_matrix_SCORE_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T24_scores[[j]]<-list_test_smoothing_T24_scores
}

T24TrainBigScores
######smoothing for each engine
list_test_all_smooth_T24_big_scores<-list()
list_test_smoothing_T24_big_scores<-list()
no_of_splines<-10
for (j in T24TrainBigScores) {
  
  for (i in 1:nrow(list_test_matrix_SCORE_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_big_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T24_big_scores[[j]]<-list_test_smoothing_T24_big_scores
}

T24TrainLowScores
######smoothing for each engine
list_test_all_smooth_T24_Low_scores<-list()
list_test_smoothing_T24_Low_scores<-list()
no_of_splines<-10
for (j in T24TrainLowScores) {
  
  for (i in 1:nrow(list_test_matrix_SCORE_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_matrix_SCORE_noNA[[j]][i,2:length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_SCORE_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_T24_Low_scores[[i]]<-Smooth
  }
  list_test_all_smooth_T24_Low_scores[[j]]<-list_test_smoothing_T24_Low_scores
}


# for (j in 2:2) {
#   
#   for (i in 1:nrow(list_test_matrix_noNA[[i]]))
#   {
#     Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_matrix_t_noNA[[i]][,1]))-1, length.out= length(na.omit(list_test_matrix_t_noNA[[2]][,1]))-1),
#                            y= as.vector(list_test_matrix_noNA[[i]][i,2:length(na.omit(list_test_matrix_t_noNA[[i]][,1]))]), 
#                            fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_matrix_t_noNA[[i]][,1]))-1),no_of_splines))
#     list_test_smoothing_T24[[i]]<-Smooth
#   }
#   list_test_all_smooth_T24[[j]]<-list_test_smoothing_T24
# }


###plot for cutted at observation
testengine=20
plot(list_test_all_smooth_T24[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T24[[testengine]][[i]], col="black")
  lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")}

###plot for (full observation trains - cutted test)
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_noNA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

###burası 66 ve 36 lık smoothlar
###plot for cutted at observation
testengine=20
plot(list_test_all_smooth_T24_scores[[testengine]][[1]],xlim=c(0,330), ylim=c(641.7,644))
for (i in 1:nrow(list_test_matrix_SCORE_noNA[[testengine]]))
{
  lines(list_test_all_smooth_T24_scores[[testengine]][[i]], col="black")
  lines(list_test_all_smooth_T24_scores[[testengine]][[nrow(list_test_matrix_SCORE_noNA[[testengine]])]], col="red")}

###plot for (full observation trains - cutted test)
plot(list_test_all_smooth_T24_scores[[testengine]][[nrow(list_test_matrix_SCORE_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in as.vector(list_test_matrix_SCORE_noNA[[testengine]][,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24_scores[[testengine]][[nrow(list_test_matrix_SCORE_noNA[[testengine]])]], col="red")


list_test_all_smooth_T24_scores

#####DISTANCE Calculation
#################DISTANCE########################
list_for_distance_T24<-list()
for (j in 1:100) {
  distancematrixT24<-matrix(NA,nrow(list_test_matrix_noNA[[j]]),no_of_splines) 
for(i in 1:nrow(list_test_matrix_noNA[[j]])) {
  distancematrixT24[i,]<-list_test_all_smooth_T24[[j]][[i]][["fd"]][["coefs"]]
}
  list_for_distance_T24[[j]]<-distancematrixT24
}

j=4
list_for_distance_T24_scores<-list()
for (j in 1:100) {
  distancematrixT24<-matrix(NA,nrow(list_test_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_matrix_noNA[[j]])) {
    distancematrixT24[i,]<-list_test_all_smooth_T24[[j]][[i]][["fd"]][["coefs"]]
  }
  list_for_distance_T24[[j]]<-distancematrixT24
}

######ALL L2 DISTANCES FOR ALL ENGINES
library(philentropy)

list_distance_T24<-list()
for (i in 1:100) {
    DISTANCE<-distance(list_for_distance_T24[[i]], method = "euclidean" )
    list_distance_T24[[i]]<-DISTANCE
}


#########################REGISTRATION OF SMOOTHED
dim(list_test_matrix)
maxT24<-max(na.omit(unlist(list_test_matrix)))

unlistedT24<-unlist(list_test_matrix)

meanT24<-mean(unlistedT24[which(unlistedT24>202)])

#####REGISTERED NEW DISTANCE Calculation
#################DISTANCE########################
list_test_all_smooth_T24_Low_scores
list_test_all_smooth_T24_big_scores
T24TrainLowScores
T24TrainBigScores
j=2
i=1
list_for_distance_T24_reg_big_scores<-list()
for (j in T24TrainBigScores) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_matrix_SCORE_noNA[[j]])) {
    distancematrixT24reg[i,]<-list_test_all_smooth_T24_big_scores[[j]][[i]][["fd"]][["coefs"]]/meanT24
  }
  list_for_distance_T24_reg_big_scores[[j]]<-distancematrixT24reg
}

list_for_distance_T24_reg_low_scores<-list()
for (j in T24TrainLowScores) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_SCORE_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_matrix_SCORE_noNA[[j]])) {
    distancematrixT24reg[i,]<-list_test_all_smooth_T24_Low_scores[[j]][[i]][["fd"]][["coefs"]]/meanT24
  }
  list_for_distance_T24_reg_low_scores[[j]]<-distancematrixT24reg
}

library(philentropy)
list_distance_T24_reg_low_scores<-list()
i=8
for (i in T24TrainLowScores) {
  DISTANCE<-distance(list_for_distance_T24_reg_low_scores[[i]], method = "euclidean" )
  list_distance_T24_reg_low_scores[[i]]<-DISTANCE
}

list_distance_T24_reg_big_scores<-list()
i=8
for (i in T24TrainBigScores) {
  DISTANCE<-distance(list_for_distance_T24_reg_big_scores[[i]], method = "euclidean" )
  list_distance_T24_reg_big_scores[[i]]<-DISTANCE
}


####SIRALAMA multivariate olarak toplandıktan sonra yapılacak aşağısı geçersiz.
############## SORTED DISTANCES FOR ALL ENGINES
list_dist_t24_all<-list()
for (i in 1:100) {
DISTANCE<-matrix(NA,nrow(list_test_matrix_noNA[[i]]),2)
DISTANCE[,1]<-list_test_matrix_noNA[[i]][,1]
DISTANCE[,2]<-list_distance_T24[[i]][,nrow(list_test_matrix_noNA[[i]])]
DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
DISTANCE
list_dist_t24_all[[i]]<-DISTANCE
}

i=5
####SIRALAMA multivariate olarak toplandıktan sonra yapılacak aşağısı geçersiz.
############## SORTED DISTANCES FOR ALL ENGINES
#############scorelu
list_dist_t24_big_scores<-list()
for (i in T24TrainBigScores) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_noNA[[i]][,1]
  DISTANCE[,2]<-list_distance_T24_reg_big_scores[[i]][,nrow(list_test_matrix_SCORE_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_t24_big_scores[[i]]<-DISTANCE
}
list_dist_t24_low_scores<-list()
for (i in T24TrainLowScores) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_noNA[[i]][,1]
  DISTANCE[,2]<-list_distance_T24_reg_low_scores[[i]][,nrow(list_test_matrix_SCORE_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_t24_low_scores[[i]]<-DISTANCE
}

############## PLOT  the most closest 5 - score classified - but one dimensional domain.
testengine=2
plot(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_low_scores[[testengine]][2:6,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

############## PLOT 10 the most closest
testengine=2
plot(list_test_all_smooth_T24_big_scores[[testengine]][[nrow(list_test_matrix_SCORE_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_big_scores[[testengine]][2:6,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")


##before
list_close5
####SIRALAMA multivariate olarak toplandıktan sonra yapılacak aşağısı geçersiz.
############## SORTED DISTANCES FOR ALL ENGINES

#test datasının initiallarını alıp matrix yaptım. İlk sütün motor no. ikinci sütun initialler
T24TESTinit<-matrix(NA, nrow = 100 , ncol = 2)
T24TESTinit[,2]<-as.vector(T24_all_train_test[101:200,2])
T24TESTinit[,1]<-seq(1:100)

T24TESTinitlow<-T24TESTinit[,1][which(T24TESTinit[,2]<642.4918)] #test datasında albanın bulduğu low scoreları ayırıdm
T24TESTinitbig<-T24TESTinit[,1][which(T24TESTinit[,2]>642.4918)] #test datasında albanın bulduğu high scoreları ayırıdm.

####train datasında low ve high scoreları ayırdım.
T24TrainScores<-read.csv("C:/Users/cevah/Desktop/data_for_registration/train mfpc scores_with low-big.csv", header = TRUE,row.names = 1)
T24TrainScoresmatrix<-matrix(NA,100,3)
T24TrainScoresmatrix[,1]<-seq(1:100)
T24TrainScoresmatrix[,2]<-T24TrainScores[,1]
T24TrainScoresmatrix[,3]<-T24TrainScores[,2]

T24TrainLowScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Low")])
T24TrainBigScores<-as.numeric(T24TrainScoresmatrix[,1][which(T24TrainScoresmatrix[,3]=="Big")])


list_test_matrix_noNA

list_test_matrix_noNA_Lowscores<-list() #tüm train ve low score testler listesi. bu listeden big score trainler çıkacak
for (i in T24TESTinitlow) {
  list_test_matrix_noNA_Lowscores[[i]]<-list_test_matrix_noNA[[i]]
}

list_test_matrix_noNA_Lowscores_bigtrainsleft<- list()
for (i in T24TESTinitlow) {
  list_test_matrix_noNA_Lowscores_bigtrainsleft[[i]]<-list_test_matrix_noNA_Lowscores[[i]][which(list_test_matrix_noNA_Lowscores[[i]][,1]== T24TrainLowScores)]
}

list_test_matrix_noNA_lowtrainsonly<-list()
for (i in 100) {
  list_test_matrix_noNA_lowtrainsonly[[8]]<-list_test_matrix_noNA[[8]]
}



test<-list_test_matrix_noNA_Lowscores[[8]]

list_test_matrix_noNA_Bigscores<-list()
for (i in T24TrainBigScores) {
  list_test_matrix_noNA_Bigscores[[i]]<-list_test_matrix_noNA[[i]][list_test_matrix_noNA[[i]][which(list_test_matrix_noNA[[i]][,1]==2,3,5)],]
}


test3<-list_test_matrix_noNA[[8]]
test3<-test3[,][which(test3[,1]!=c(1,2,3))]
c(1,2,4,5)




i=5
list_dist_t24_lowscores<-list()
list_dist_t24_bigscores<-list()
for (i in 1:T24TESTinitlow) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_noNA_Lowscores[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_noNA[[T24TESTinitlow[i]]][,1]
  DISTANCE[,2]<-list_distance_T24[[T24TESTinitlow[i]]][,nrow(list_test_matrix_noNA[[T24TESTinitlow[i]]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_t24_lowscores[[i]]<-DISTANCE
}


i=5
list_dist_t24_lowscores<-list()
list_dist_t24_bigscores<-list()
for (i in 1:T24TESTinitlow) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_noNA[[T24TESTinitlow[i]]]),2)
  DISTANCE[,1]<-list_test_matrix_noNA[[T24TESTinitlow[i]]][,1]
  DISTANCE[,2]<-list_distance_T24[[T24TESTinitlow[i]]][,nrow(list_test_matrix_noNA[[T24TESTinitlow[i]]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_t24_lowscores[[i]]<-DISTANCE
}
############## PLOT 5 the most closest
testengine=31
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_lowscores[[5]][2:6,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

list_dist_t24_all[[3]]
list_dist_t24_lowscores[[1]]

all.equal(list_dist_t24_all[[3]],list_dist_t24_lowscores[[1]])
all.equal(list_dist_t24_all[[5]],list_dist_t24_lowscores[[2]])
all.equal(list_dist_t24_all[[6]],list_dist_t24_lowscores[[3]])
all.equal(list_dist_t24_all[[8]],list_dist_t24_lowscores[[4]])
all.equal(list_dist_t24_all[[12]],list_dist_t24_lowscores[[5]])
all.equal(list_dist_t24_all[[15]],list_dist_t24_lowscores[[6]])
all.equal(list_dist_t24_all[[16]],list_dist_t24_lowscores[[7]])
all.equal(list_dist_t24_all[[17]],list_dist_t24_lowscores[[8]])
all.equal(list_dist_t24_all[[18]],list_dist_t24_lowscores[[9]])
all.equal(list_dist_t24_all[[19]],list_dist_t24_lowscores[[10]])
all.equal(list_dist_t24_all[[20]],list_dist_t24_lowscores[[11]])
all.equal(list_dist_t24_all[[21]],list_dist_t24_lowscores[[12]])



############## PLOT 5 the most closest
testengine=31
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_all[[testengine]][2:6,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

############## PLOT 10 the most closest
testengine=20
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_all[[testengine]][2:11,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

####TRUE RULimport
T24_RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
T24_RUL_test
T24_RUL_test<-as.matrix(T24_RUL_test)

T24_TRUE_RUL_DECREASING<-T24_RUL_test[order(T24_RUL_test[,3],decreasing = FALSE),]
plot(T24_TRUE_RUL_DECREASING[,3])

####RUL Prediction
T24_LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
T24_LIFE_TRAIN<-as.matrix(T24_LIFE_TRAIN)
T24_LIFE_TRAIN

close5<-list_dist_t24_all[[20]][2:6,1] #most closest 5
close5

##############FIND CLOSEST 5 and list
list_close5<-list()
for (i in 1:48) {
  close5<-list_dist_t24_all[[i]][2:6,1]
  list_close5[[i]]<-close5
}
list_close5[[49]]<-list_dist_t24_all[[49]][2:5,1]
for (i in 50:100) {
  close5<-list_dist_t24_all[[i]][2:6,1]
  list_close5[[i]]<-close5
}

#1 NO OF ENGINE
#2 OBS Test
#3 RUL TEST
#4 LIFE TEST
#5 PRED LIFE
#6 PRED RUL
#7 RUL ERROR
#8 PRED LIFE MEDIAN
#9 MEDIAN ERROR
PREDICTION<- matrix(data = NA, nrow=100, 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(PREDICTION) <- newheaderstest
PREDICTION[,1]<-T24_RUL_test[,1]
PREDICTION[,2]<-T24_RUL_test[,2]
PREDICTION[,3]<-T24_RUL_test[,3]
PREDICTION[,4]<-T24_RUL_test[,4]
  for (i in 1:100) {
    PREDICTION[i,5]<-mean(T24_LIFE_TRAIN[list_close5[[i]],2])
    PREDICTION[i,6]<-PREDICTION[i,5]-PREDICTION[i,2]
    PREDICTION[i,7]<-PREDICTION[i,6]-PREDICTION[i,3]
    PREDICTION[i,8]<-median(T24_LIFE_TRAIN[list_close5[[i]],2]) 
    PREDICTION[i,9]<-PREDICTION[i,8]-PREDICTION[i,2]
    PREDICTION[i,10]<-PREDICTION[i,9]-PREDICTION[i,3]
  }

ordervector_mean<- as.vector(T24_TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
PREDICTION_SORTED <- PREDICTION[match(ordervector_mean, PREDICTION),]##SORT increasing RUL

###PLOT for TRUE RUL and PRED RUL
plot(T24_TRUE_RUL_DECREASING[,3])
for (i in 1:100) {
  points(i,PREDICTION_SORTED[i,6], col="red")
}

###PLOT for TRUE RUL and PRED MEDIAN
ordervector_mean<- as.vector(T24_TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
PREDICTION_SORTED <- PREDICTION[match(ordervector_mean, PREDICTION),]##SORT increasing RUL

plot(T24_TRUE_RUL_DECREASING[,3])
for (i in 1:100) {
  points(i,PREDICTION_SORTED[i,9], col="red")
}

######RMSE MEAN
ERRORSQUARE <- PREDICTION_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- PREDICTION_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)
