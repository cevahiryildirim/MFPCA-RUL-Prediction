library(funData)
library(ggplot2)
library(fda)
library(MFPCA)

W32_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/9-W32train_test_all.csv", header = TRUE,row.names = 1)
W32_all_train_test
W32_all_train_test<-as.matrix(W32_all_train_test)

#### list for 100 test engine - all 101 x number of observation matrix
list_test_W32_matrix<-list()
for (i in 1:100)
{ 
  W32_test_matrix_length <- matrix(data = NA, nrow=101, ncol=length(na.omit(W32_all_train_test[100+i,])))
  list_test_W32_matrix[[i]]<-W32_test_matrix_length
}

#### fill all lists with 100 train + row101 for related  test engine
for (i in 1:100) { 
  W32_test_matrix<- matrix(data = NA, nrow=101, ncol=length(as.matrix(list_test_W32_matrix[[i]])[1,]))
  for (j in 1:101) {
    W32_test_matrix[j,]<-W32_all_train_test[j,1:length(as.matrix(list_test_W32_matrix[[i]])[1,])]
    list_test_W32_matrix[[i]][j,]<-W32_test_matrix[j,]
  }
  list_test_W32_matrix[[i]][101,]<-W32_all_train_test[100+i, 1:length(as.matrix(list_test_W32_matrix[[i]])[1,])]
}


#### ignore NAs for all
#####list with ignoring train data with observation less than test data
list_test_W32_matrix_noNA<-list()
for (i in 1:100)
{ 
  noNA_test_matrix_W32 <- matrix(data = NA, nrow=101, ncol=length(na.omit(list_test_W32_matrix[[i]][101,])))
  noNA_test_matrix_W32 <-na.omit(list_test_W32_matrix[[i]])
  list_test_W32_matrix_noNA[[i]]<-noNA_test_matrix_W32
}

#####list with transpose of each matrix

list_test_W32_matrix_t_noNA<-list()
for (i in 1:100)
{ 
  list_test_W32_matrix_t_noNA[[i]]<-t(list_test_W32_matrix_noNA[[i]])
}


######smoothing for each engine
list_test_all_smooth_W32<-list()
list_test_smoothing_W32<-list()
no_of_splines<-10
for (j in 1:100) {
  
  for (i in 1:nrow(list_test_W32_matrix_noNA[[j]]))
  {
    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_W32_matrix_t_noNA[[j]][,1]))-1, length.out= length(na.omit(list_test_W32_matrix_t_noNA[[j]][,1]))-1),
                           y= as.vector(list_test_W32_matrix_noNA[[j]][i,2:length(na.omit(list_test_W32_matrix_t_noNA[[j]][,1]))]), 
                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_W32_matrix_t_noNA[[j]][,1]))-1),no_of_splines))
    list_test_smoothing_W32[[i]]<-Smooth
  }
  list_test_all_smooth_W32[[j]]<-list_test_smoothing_W32
}

#for (j in 2:2) {
#  
#  for (i in 1:nrow(list_test_W32_matrix_noNA[[i]]))
#  {
#    Smooth<- smooth.basis( argvals = seq(1,length(na.omit(list_test_W32_matrix_t_noNA[[i]][,1]))-1, length.out= length(na.omit(list_test_W32_matrix_t_noNA[[2]][,1]))-1),
#                           y= as.vector(list_test_W32_matrix_noNA[[i]][i,2:length(na.omit(list_test_W32_matrix_t_noNA[[i]][,1]))]), 
#                           fdParobj = create.bspline.basis(c(1,length(na.omit(list_test_W32_matrix_t_noNA[[i]][,1]))-1),10))
#    list_test_smoothing_W32[[i]]<-Smooth
#  }
#  list_test_all_smooth_W32[[j]]<-list_test_smoothing_W32
#}


###plot for cutted at observation
testengine=20
plot(list_test_all_smooth_W32[[testengine]][[1]],xlim=c(0,330), ylim=c(22.97,23.5))
for (i in 1:nrow(list_test_W32_matrix_noNA[[testengine]]))
{
  lines(list_test_all_smooth_W32[[testengine]][[i]], col="black")
  lines(list_test_all_smooth_W32[[testengine]][[nrow(list_test_W32_matrix_noNA[[testengine]])]], col="red")}

###plot for (full observation trains - cutted test)
plot(list_test_all_smooth_W32[[testengine]][[nrow(list_test_W32_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
for (i in as.vector(list_test_W32_matrix_noNA[[testengine]][,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32[[testengine]][[nrow(list_test_W32_matrix_noNA[[testengine]])]], col="red")

#####DISTANCE Calculation
#################DISTANCE########################
list_for_distance_W32<-list()
for (j in 1:100) {
  distancematrixW32<-matrix(NA,nrow(list_test_W32_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_W32_matrix_noNA[[j]])) {
    distancematrixW32[i,]<-list_test_all_smooth_W32[[j]][[i]][["fd"]][["coefs"]]
  }
  list_for_distance_W32[[j]]<-distancematrixW32
}

######ALL L2 DISTANCES FOR ALL ENGINES
library(philentropy)

list_distance_W32<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_W32[[i]], method = "euclidean" )
  list_distance_W32[[i]]<-DISTANCE
}

#########################REGISTRATION OF SMOOTHED
maxW32<-23.6184

#####REGISTERED NEW DISTANCE Calculation
#################DISTANCE########################
list_for_distance_W32_reg<-list()
for (j in 1:100) {
  distancematrixW32reg<-matrix(NA,nrow(list_test_W32_matrix_noNA[[j]]),no_of_splines) 
  for(i in 1:nrow(list_test_W32_matrix_noNA[[j]])) {
    distancematrixW32reg[i,]<-list_test_all_smooth_W32[[j]][[i]][["fd"]][["coefs"]]/maxW32
  }
  list_for_distance_W32_reg[[j]]<-distancematrixW32reg
}

list_distance_W32_reg<-list()
for (i in 1:100) {
  DISTANCE<-distance(list_for_distance_W32_reg[[i]], method = "euclidean" )
  list_distance_W32_reg[[i]]<-DISTANCE
}
