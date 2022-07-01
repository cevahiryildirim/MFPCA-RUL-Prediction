################################
list_all_distance_merged<-list()
for (i in 1:100) {
  
  a<-list_distance_T24_reg[[i]]+
     list_distance_T30_reg[[i]]+
     list_distance_T50_reg[[i]]+
     list_distance_P30_reg[[i]]+
     list_distance_ps30_reg[[i]]+
     list_distance_phi_reg[[i]]+
     list_distance_BPR_reg[[i]]+
     list_distance_W31_reg[[i]]+
     list_distance_W32_reg[[i]]
  list_all_distance_merged[[i]]<-a
}

A<-list_distance_T24_reg[[1]]

i=49

############## SORTED DISTANCES FOR ALL ENGINES
list_dist_all_sorted<-list()
for (i in 1:100) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_noNA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged[[i]][,nrow(list_test_matrix_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted[[i]]<-DISTANCE
}

no_of_closest=8
testengine=31

par(mfrow=c(3,3))
plot(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644))
title(main="T24")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeT24[[i]])
} 
lines(list_test_all_smooth_T24[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_T30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610),)
title(main="T30")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeT30[[i]])
} 
lines(list_test_all_smooth_T30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_T50[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435))
title(main="T50")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeT50[[i]])
} 
lines(list_test_all_smooth_T50[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_P30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2))
title(main="P30")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeP30[[i]])
} 
lines(list_test_all_smooth_P30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_ps30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4))
title(main="ps30")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeps30[[i]])
} 
lines(list_test_all_smooth_ps30[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_phi[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8))
title(main="phi")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listephi[[i]])
} 
lines(list_test_all_smooth_phi[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_BPR[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55))
title(main="BPR")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeBPR[[i]])
} 
lines(list_test_all_smooth_BPR[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W31[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2))
title(main="W31")
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1])) {
  lines(listeW31[[i]])
} 
lines(list_test_all_smooth_W31[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5))
title(main="W32")
for (i in c(list_dist_all_sorted[[testengine]][2:no_of_closest+1,1])) {
  lines(listeW32[[i]])
} 
lines(list_test_all_smooth_W32[[testengine]][[nrow(list_test_matrix_noNA[[testengine]])]], col="red")




####TRUE RULimport
RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
RUL_test
RUL_test<-as.matrix(RUL_test)

TRUE_RUL_DECREASING<-RUL_test[order(RUL_test[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING[,3])

####RUL Prediction

no_of_closest_engine<-8

LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
LIFE_TRAIN<-as.matrix(LIFE_TRAIN)
LIFE_TRAIN





list_closest_X<-list()
for (i in 1:48) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}
list_closest_X[[49]]<-list_dist_all_sorted[[49]][2:5,1]
for (i in 50:100) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}

#1 NO OF ENGINE
#2 OBS Test
#3 RUL TEST
#4 LIFE TEST
#5 PRED LIFE
#6 PRED RUL
#7 MEAN RUL ERROR
#8 PRED LIFE MEDIAN
#9 PRED RUL MEDIAN
#9 MEDIAN RUL ERROR
RUL_PREDICTION<- matrix(data = NA, nrow=100, 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION) <- newheaderstest
RUL_PREDICTION[,1]<-RUL_test[,1]
RUL_PREDICTION[,2]<-RUL_test[,2]
RUL_PREDICTION[,3]<-RUL_test[,3]
RUL_PREDICTION[,4]<-RUL_test[,4]
for (i in 1:100) {
  RUL_PREDICTION[i,5]<-mean(LIFE_TRAIN[list_closest_X[[i]],2])
  RUL_PREDICTION[i,6]<-RUL_PREDICTION[i,5]-RUL_PREDICTION[i,2]
  RUL_PREDICTION[i,7]<-RUL_PREDICTION[i,6]-RUL_PREDICTION[i,3]
  RUL_PREDICTION[i,8]<-median(LIFE_TRAIN[list_closest_X[[i]],2]) 
  RUL_PREDICTION[i,9]<-RUL_PREDICTION[i,8]-RUL_PREDICTION[i,2]
  RUL_PREDICTION[i,10]<-RUL_PREDICTION[i,9]-RUL_PREDICTION[i,3]
}

ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL

###PLOT for TRUE RUL and PRED RUL
plot(TRUE_RUL_DECREASING[,3])
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,6], col="red")
}

###PLOT for TRUE RUL and PRED MEDIAN
ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL

plot(TRUE_RUL_DECREASING[,3])
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,9], col="red")
}

######RMSE MEAN

ERROR<-RUL_PREDICTION_SORTED[,7]
ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2
ERRORSQUARE <- as.numeric(ERRORSQUARE)
SUM_ERRORSQUARE <- sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)



A<-3+5
A

######RMSE MEDIAN
ERRORSQUAREMED <- sum()
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)

max(RUL_PREDICTION[,7])
min(RUL_PREDICTION[,7])
max(RUL_PREDICTION[,10])
min(RUL_PREDICTION[,10])

length(which(RUL_PREDICTION[,10]< (-0.5)) )

length(which(RUL_PREDICTION[,10]> (0.5)) )

length(which(RUL_PREDICTION[,10]==(-0.5)) )
length(which(RUL_PREDICTION[,10]==(0.5)) )
length(which(RUL_PREDICTION[,10]==(0)) )

length(which(RUL_PREDICTION[,7]< (-0.5)) )

length(which(RUL_PREDICTION[,7]> (0.5)) )

length(which(RUL_PREDICTION[,7]==(-0.5)) )
length(which(RUL_PREDICTION[,7]==(0.5)) )
length(which(RUL_PREDICTION[,7]==(0)) )




