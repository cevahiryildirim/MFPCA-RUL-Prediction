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

#### fill all lists with 100 train + row101 for related  test engine
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
list_test_matrix_noNA<-list()
for (i in 1:100)
{ 
  noNA_test_matrix_T24 <- matrix(data = NA, nrow=101, ncol=length(na.omit(list_test_matrix[[i]][101,])))
  noNA_test_matrix_T24 <-na.omit(list_test_matrix[[i]])
  list_test_matrix_noNA[[i]]<-noNA_test_matrix_T24
}

#####list with transpose of each matrix

list_test_matrix_t_noNA<-list()
for (i in 1:100)
{ 
  list_test_matrix_t_noNA[[i]]<-t(list_test_matrix_noNA[[i]])
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

######ALL L2 DISTANCES FOR ALL ENGINES
library(philentropy)

list_distance_T24<-list()
for (i in 1:100) {
    DISTANCE<-distance(list_for_distance_T24[[i]], method = "euclidean" )
    list_distance_T24[[i]]<-DISTANCE
}


#########################REGISTRATION OF SMOOTHED
maxT24<-max(na.omit(unlist(list_test_matrix)))

#####REGISTERED NEW DISTANCE Calculation
#################DISTANCE########################
list_for_distance_T24_reg<-list()
for (j in 1:100) {
  distancematrixT24reg<-matrix(NA,nrow(list_test_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_matrix_noNA[[j]])) {
    distancematrixT24reg[i,]<-list_test_all_smooth_T24[[j]][[i]][["fd"]][["coefs"]]/maxT24
  }
  list_for_distance_T24_reg[[j]]<-distancematrixT24reg
}

list_distance_T24_reg<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_T24_reg[[i]], method = "euclidean" )
  list_distance_T24_reg[[i]]<-DISTANCE
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

############## PLOT 5 the most closest
testengine=49
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
for (i in c(list_dist_t24_all[[testengine]][2:5,1])) {
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

