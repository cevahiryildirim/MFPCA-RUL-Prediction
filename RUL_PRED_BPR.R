library(funData)
library(ggplot2)
library(fda)
library(MFPCA)

BPR_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/7-BPRtrain_test_all.csv", header = TRUE,row.names = 1)
BPR_all_train_test
BPR_all_train_test<-as.matrix(BPR_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_BPR_matrix<-list()
for (i in 1:100)
{ 
  BPR_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(BPR_all_train_test[100+i,])))
  list_test_BPR_matrix[[i]]<-BPR_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in 1:100) { 
  BPR_test_matrix<- matrix(data = NA, nrow=101, ncol=length(as.matrix(list_test_BPR_matrix[[i]])[1,]))
  for (j in 1:101) {
    BPR_test_matrix[j,]<-BPR_all_train_test[j,1:length(as.matrix(list_test_BPR_matrix[[i]])[1,])]
    list_test_BPR_matrix[[i]][j,]<-BPR_test_matrix[j,]
  }
  list_test_BPR_matrix[[i]][101,]<-BPR_all_train_test[100+i, 1:length(as.matrix(list_test_BPR_matrix[[i]])[1,])]
}


#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_BPR_matrix_noNA<-list()
for (i in 1:100)
{ 
  noNA_test_matrix_BPR <- matrix(data = NA, nrow=101, ncol=length(na.omit(list_test_BPR_matrix[[i]][101,])))
  noNA_test_matrix_BPR <-na.omit(list_test_BPR_matrix[[i]])
  list_test_BPR_matrix_noNA[[i]]<-noNA_test_matrix_BPR
}

#####list with transpose of each matrix

list_test_BPR_matrix_t_noNA<-list()
for (i in 1:100)
{ 
  list_test_BPR_matrix_t_noNA[[i]]<-t(list_test_BPR_matrix_noNA[[i]])
}


######smoothing for each engine
list_test_all_smooth_BPR<-list()
list_test_smoothing_BPR<-list()
no_of_splines<-10
for (j in 1:100) {
  
  for (i in 1:nrow(list_test_BPR_matrix_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_BPR_matrix_t_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_BPR_matrix_t_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_BPR_matrix_noNA[[j]][i,2:length(na.omit(list_test_BPR_matrix_t_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_BPR_matrix_t_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_BPR[[i]]<-Smooth
  }
  list_test_all_smooth_BPR[[j]]<-list_test_smoothing_BPR
}

#for (j in 2:2) {
#  
#  for (i in 1:nrow(list_test_BPR_matrix_noNA[[i]]))
#  {
#    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_BPR_matrix_t_noNA[[i]][,1]))-1, length.out= length(na.omit(list_test_BPR_matrix_t_noNA[[2]][,1]))-1),
#                           y= as.vector(list_test_BPR_matrix_noNA[[i]][i,2:length(na.omit(list_test_BPR_matrix_t_noNA[[i]][,1]))]), 
#                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_BPR_matrix_t_noNA[[i]][,1]))-1),10))
#    list_test_smoothing_BPR[[i]]<-Smooth
#  }
#  list_test_all_smooth_BPR[[j]]<-list_test_smoothing_BPR
#}


###plot for cutted at observation
testengine=20
plot(list_test_all_smooth_BPR[[testengine]][[1]],xlim=c(0,330), ylim=c(8.37,8.55))
for (i in 1:nrow(list_test_BPR_matrix_noNA[[testengine]]))
{
  lines(list_test_all_smooth_BPR[[testengine]][[i]], col="black")
  lines(list_test_all_smooth_BPR[[testengine]][[nrow(list_test_BPR_matrix_noNA[[testengine]])]], col="red")}

###plot for (full observation trains - cutted test)
plot(list_test_all_smooth_BPR[[testengine]][[nrow(list_test_BPR_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55))
for (i in as.vector(list_test_BPR_matrix_noNA[[testengine]][,1])) {
  lines(listeBPR[[i]])
} 
lines(list_test_all_smooth_BPR[[testengine]][[nrow(list_test_BPR_matrix_noNA[[testengine]])]], col="red")



#####DISTANCE Calculation
#################DISTANCE########################
list_for_distance_BPR<-list()
for (j in 1:100) {
  distancematrixBPR<-matrix(NA,nrow(list_test_BPR_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_BPR_matrix_noNA[[j]])) {
    distancematrixBPR[i,]<-list_test_all_smooth_BPR[[j]][[i]][["fd"]][["coefs"]]
  }
  list_for_distance_BPR[[j]]<-distancematrixBPR
}

######ALL L2 DISTANCES FOR ALL ENGINES
library(philentropy)

list_distance_BPR<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_BPR[[i]], method = "euclidean" )
  list_distance_BPR[[i]]<-DISTANCE
}

#########################REGISTRATION OF SMOOTHED
maxBPR<-8.5848

#####REGISTERED NEW DISTANCE Calculation
#################DISTANCE########################
list_for_distance_BPR_reg<-list()
for (j in 1:100) {
  distancematrixBPRreg<-matrix(NA,nrow(list_test_BPR_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_BPR_matrix_noNA[[j]])) {
    distancematrixBPRreg[i,]<-list_test_all_smooth_BPR[[j]][[i]][["fd"]][["coefs"]]/maxBPR
  }
  list_for_distance_BPR_reg[[j]]<-distancematrixBPRreg
}

list_distance_BPR_reg<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_BPR_reg[[i]], method = "euclidean" )
  list_distance_BPR_reg[[i]]<-DISTANCE
}


