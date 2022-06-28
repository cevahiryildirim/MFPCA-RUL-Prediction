library(funData)
library(ggplot2)
library(fda)
library(MFPCA)
install.packages("distr6")

library(distr6)
library(extraDistr)

W31_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/8-W31train_test_all.csv", header = TRUE,row.names = 1)
W31_all_train_test
W31_all_train_test<-as.matrix(W31_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_W31_matrix<-list()
for (i in 1:100)
{ 
  W31_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(W31_all_train_test[100+i,])))
  list_test_W31_matrix[[i]]<-W31_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in 1:100) { 
  W31_test_matrix<- matrix(data = NA, nrow=101, ncol=length(as.matrix(list_test_W31_matrix[[i]])[1,]))
  for (j in 1:101) {
    W31_test_matrix[j,]<-W31_all_train_test[j,1:length(as.matrix(list_test_W31_matrix[[i]])[1,])]
    list_test_W31_matrix[[i]][j,]<-W31_test_matrix[j,]
  }
  list_test_W31_matrix[[i]][101,]<-W31_all_train_test[100+i, 1:length(as.matrix(list_test_W31_matrix[[i]])[1,])]
}


#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_W31_matrix_noNA<-list()
for (i in 1:100)
{ 
  noNA_test_matrix_W31 <- matrix(data = NA, nrow=101, ncol=length(na.omit(list_test_W31_matrix[[i]][101,])))
  noNA_test_matrix_W31 <-na.omit(list_test_W31_matrix[[i]])
  list_test_W31_matrix_noNA[[i]]<-noNA_test_matrix_W31
}

#####list with transpose of each matrix

list_test_W31_matrix_t_noNA<-list()
for (i in 1:100)
{ 
  list_test_W31_matrix_t_noNA[[i]]<-t(list_test_W31_matrix_noNA[[i]])
}


######smoothing for each engine
list_test_all_smooth_W31<-list()
list_test_smoothing_W31<-list()
no_of_splines<-10
for (j in 1:100) {
  
  for (i in 1:nrow(list_test_W31_matrix_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_W31_matrix_t_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_W31_matrix_t_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_W31_matrix_noNA[[j]][i,2:length(na.omit(list_test_W31_matrix_t_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_W31_matrix_t_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W31[[i]]<-Smooth
  }
  list_test_all_smooth_W31[[j]]<-list_test_smoothing_W31
}

#for (j in 2:2) {
#  
#  for (i in 1:nrow(list_test_W31_matrix_noNA[[i]]))
#  {
#    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_W31_matrix_t_noNA[[i]][,1]))-1, length.out= length(na.omit(list_test_W31_matrix_t_noNA[[2]][,1]))-1),
#                           y= as.vector(list_test_W31_matrix_noNA[[i]][i,2:length(na.omit(list_test_W31_matrix_t_noNA[[i]][,1]))]), 
#                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_W31_matrix_t_noNA[[i]][,1]))-1),10))
#    list_test_smoothing_W31[[i]]<-Smooth
#  }
#  list_test_all_smooth_W31[[j]]<-list_test_smoothing_W31
#}


###plot for cutted at observation
testengine=20
plot(list_test_all_smooth_W31[[testengine]][[1]],xlim=c(0,330), ylim=c(38.2,39.2))
for (i in 1:nrow(list_test_W31_matrix_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W31[[testengine]][[i]], col="black")
  lines(list_test_all_smooth_W31[[testengine]][[nrow(list_test_W31_matrix_noNA[[testengine]])]], col="red")}

###plot for (full observation trains - cutted test)
plot(list_test_all_smooth_W31[[testengine]][[nrow(list_test_W31_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2))
for (i in as.vector(list_test_W31_matrix_noNA[[testengine]][,1])) {
  lines(listeW31[[i]])
} 
lines(list_test_all_smooth_W31[[testengine]][[nrow(list_test_W31_matrix_noNA[[testengine]])]], col="red")



#####DISTANCE Calculation
#################DISTANCE########################
list_for_distance_W31<-list()
for (j in 1:100) {
  distancematrixW31<-matrix(NA,nrow(list_test_W31_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_W31_matrix_noNA[[j]])) {
    distancematrixW31[i,]<-list_test_all_smooth_W31[[j]][[i]][["fd"]][["coefs"]]
  }
  list_for_distance_W31[[j]]<-distancematrixW31
}

######ALL L2 DISTANCES FOR ALL ENGINES
library(philentropy)

list_distance_W31<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_W31[[i]], method = "euclidean" )
  list_distance_W31[[i]]<-DISTANCE
}

#########################REGISTRATION OF SMOOTHED
maxW31<-39.43

#####REGISTERED NEW DISTANCE Calculation
#################DISTANCE########################
list_for_distance_W31_reg<-list()
for (j in 1:100) {
  distancematrixW31reg<-matrix(NA,nrow(list_test_W31_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_W31_matrix_noNA[[j]])) {
    distancematrixW31reg[i,]<-list_test_all_smooth_W31[[j]][[i]][["fd"]][["coefs"]]/maxW31
  }
  list_for_distance_W31_reg[[j]]<-distancematrixW31reg
}

list_distance_W31_reg<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_W31_reg[[i]], method = "euclidean" )
  list_distance_W31_reg[[i]]<-DISTANCE
}


