#Multivariate Distance calculation
list_all_distance_merged_low_scores<-list()
for (i in TestLowClassEng) {
  
  a<-list_distance_T24_reg_low_scores[[i]]+
    list_distance_T30_reg_low_scores[[i]]+
    list_distance_T50_reg_low_scores[[i]]+
    list_distance_P30_reg_low_scores[[i]]+
    list_distance_ps30_reg_low_scores[[i]]+
    list_distance_phi_reg_low_scores[[i]]+
    list_distance_BPR_reg_low_scores[[i]]+
    list_distance_W31_reg_low_scores[[i]]+
    list_distance_W32_reg_low_scores[[i]]
  list_all_distance_merged_low_scores[[i]]<-a
}

list_all_distance_merged_big_scores<-list()
for (i in TestBigClassEng) {
  
  a<-list_distance_T24_reg_big_scores[[i]]+
    list_distance_T30_reg_big_scores[[i]]+
    list_distance_T50_reg_big_scores[[i]]+
    list_distance_P30_reg_big_scores[[i]]+
    list_distance_ps30_reg_big_scores[[i]]+
    list_distance_phi_reg_big_scores[[i]]+
    list_distance_BPR_reg_big_scores[[i]]+
    list_distance_W31_reg_big_scores[[i]]+
    list_distance_W32_reg_big_scores[[i]]
  list_all_distance_merged_big_scores[[i]]<-a
}

############## SORTED DISTANCES FOR ALL ENGINES
list_dist_all_sorted<-list()
for (i in TestLowClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_low_scores[[i]][,nrow(list_test_matrix_SCORE_W31_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted[[i]]<-DISTANCE
}

for (i in TestBigClassEng) {
  DISTANCE<-matrix(NA,nrow(list_test_matrix_SCORE_W31_noNA[[i]]),2)
  DISTANCE[,1]<-list_test_matrix_SCORE_W31_noNA[[i]][,1]
  DISTANCE[,2]<-list_all_distance_merged_big_scores[[i]][,nrow(list_test_matrix_SCORE_W31_noNA[[i]])]
  DISTANCE<-DISTANCE[order(DISTANCE[,2],decreasing = FALSE),]
  DISTANCE
  list_dist_all_sorted[[i]]<-DISTANCE
}



######################PLOTS

TestBigClassEng
TestLowClassEng
no_of_closest=5
testengine=93

#lows
par(mfrow=c(3,3))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]], col="red")
}
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")
text(70, 643, "Test Engine no:70", col="blue")
text(200, 642.5, "5 closest train engines (low score group)", col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


testengine=81
#Bigs
par(mfrow=c(3,3))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(641.7,644), xlab="Cycle Time")
title(main="T24", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT24[[i]])
}
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")
text(60, 642.5, "Test Engine no:81", col="blue")

text(220, 642.3, "5 closest train engines (big score group)", col="black")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1580,1610), xlab="Cycle Time")
title(main="T30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT30[[i]])
}
lines(list_test_all_smooth_T30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="blue")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(1390,1435), xlab="Cycle Time")
title(main="T50", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeT50[[i]])
}
lines(list_test_all_smooth_T50_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(550.5,555.2), xlab="Cycle Time")
title(main="P30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeP30[[i]])
}
lines(list_test_all_smooth_P30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(47.05,48.4), xlab="Cycle Time")
title(main="ps30", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeps30[[i]])
}
lines(list_test_all_smooth_ps30_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(519.1,522.8), xlab="Cycle Time")
title(main="phi", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listephi[[i]])
}
lines(list_test_all_smooth_phi_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")

plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(8.37,8.55), xlab="Cycle Time")
title(main="BPR", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeBPR[[i]])
}
lines(list_test_all_smooth_BPR_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(38.2,39.2), xlab="Cycle Time")
title(main="W31", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW31[[i]])
}
lines(list_test_all_smooth_W31_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


plot(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]],xlim=c(0,370), ylim=c(22.97,23.5), xlab="Cycle Time")
title(main="W32", cex.main=3, )
for (i in c(list_dist_all_sorted[[testengine]][2:(no_of_closest+1),1]))
{
  lines(listeW32[[i]])
}
lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")



#bigs cutted
#plot(list_test_all_smooth_W32_Big_scores[[testengine]][[1]],xlim=c(0,370), ylim=c(22.97,23.5))
#for (i in 1:nrow(list_test_matrix_SCORE_W32_noNA[[testengine]]))
#{
#  lines(list_test_all_smooth_W32_Big_scores[[testengine]][[i]], col="black")
#}
#lines(list_test_all_smooth_W32_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W32_noNA[[testengine]])]], col="red")


####TRUE RULimport
RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
RUL_test
RUL_test<-as.matrix(RUL_test)

TRUE_RUL_DECREASING<-RUL_test[order(RUL_test[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING[,3])

RUL_test_bigscore<- matrix(NA,ncol=4, nrow= length(TestBigClassEng))
for (i in 1:length(TestBigClassEng)) {
  RUL_test_bigscore[i,]<- RUL_test[TestBigClassEng[i],]
}

TRUE_RUL_DECREASING_bigscore<-RUL_test_bigscore[order(RUL_test_bigscore[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING_bigscore[,3])

RUL_test_lowscore<- matrix(NA,ncol=4, nrow= length(TestLowClassEng))
for (i in 1:length(TestLowClassEng)) {
  RUL_test_lowscore[i,]<- RUL_test[TestLowClassEng[i],]
}

TRUE_RUL_DECREASING_lowscore<-RUL_test_lowscore[order(RUL_test_lowscore[,3],decreasing = FALSE),]
plot(TRUE_RUL_DECREASING_lowscore[,3])


####RUL Prediction

par(mfrow=c(1,1))
par(cex.lab=cex, cex.axis=cex, cex.main=cex)
cex=2
no_of_closest_engine=6

LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
LIFE_TRAIN<-as.matrix(LIFE_TRAIN)
LIFE_TRAIN

list_closest_X_lowscore<-list()
for (i in TestLowClassEng ) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_lowscore[[i]]<-close
}

#27
TestBigClassEngexcept49<-TestBigClassEng
TestBigClassEngexcept49[27]<-NA
TestBigClassEngexcept49<-na.omit(TestBigClassEngexcept49)

list_closest_X_bigcore<-list()
for (i in TestBigClassEngexcept49 ) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X_bigcore[[i]]<-close
}
list_closest_X_bigcore[[49]]<-c(96,69,92,67)



# FOR ENGINE 49 - 96,69,92,67
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

RUL_PREDICTION_lowscore<- matrix(data = NA, nrow=length(TestLowClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_lowscore) <- newheaderstest
RUL_PREDICTION_lowscore[,1]<-RUL_test_lowscore[,1]
RUL_PREDICTION_lowscore[,2]<-RUL_test_lowscore[,2]
RUL_PREDICTION_lowscore[,3]<-RUL_test_lowscore[,3]
RUL_PREDICTION_lowscore[,4]<-RUL_test_lowscore[,4]
i=1
for (i in 1:length(TestLowClassEng)) {
  RUL_PREDICTION_lowscore[i,5]<-mean(LIFE_TRAIN[list_closest_X_lowscore[[TestLowClassEng[i]]],2])
  RUL_PREDICTION_lowscore[i,6]<-RUL_PREDICTION_lowscore[i,5]-RUL_PREDICTION_lowscore[i,2]
  RUL_PREDICTION_lowscore[i,7]<-RUL_PREDICTION_lowscore[i,6]-RUL_PREDICTION_lowscore[i,3]
  RUL_PREDICTION_lowscore[i,8]<-median(LIFE_TRAIN[list_closest_X_lowscore[[TestLowClassEng[i]]],2]) 
  RUL_PREDICTION_lowscore[i,9]<-RUL_PREDICTION_lowscore[i,8]-RUL_PREDICTION_lowscore[i,2]
  RUL_PREDICTION_lowscore[i,10]<-RUL_PREDICTION_lowscore[i,9]-RUL_PREDICTION_lowscore[i,3]
}

RUL_PREDICTION_bigscore<- matrix(data = NA, nrow=length(TestBigClassEng), 10)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_bigscore) <- newheaderstest
RUL_PREDICTION_bigscore[,1]<-RUL_test_bigscore[,1]
RUL_PREDICTION_bigscore[,2]<-RUL_test_bigscore[,2]
RUL_PREDICTION_bigscore[,3]<-RUL_test_bigscore[,3]
RUL_PREDICTION_bigscore[,4]<-RUL_test_bigscore[,4]

for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_bigscore[i,5]<-mean(LIFE_TRAIN[list_closest_X_bigcore[[TestBigClassEng[i]]],2])
  RUL_PREDICTION_bigscore[i,6]<-RUL_PREDICTION_bigscore[i,5]-RUL_PREDICTION_bigscore[i,2]
  RUL_PREDICTION_bigscore[i,7]<-RUL_PREDICTION_bigscore[i,6]-RUL_PREDICTION_bigscore[i,3]
  RUL_PREDICTION_bigscore[i,8]<-median(LIFE_TRAIN[list_closest_X_bigcore[[TestBigClassEng[i]]],2]) 
  RUL_PREDICTION_bigscore[i,9]<-RUL_PREDICTION_bigscore[i,8]-RUL_PREDICTION_bigscore[i,2]
  RUL_PREDICTION_bigscore[i,10]<-RUL_PREDICTION_bigscore[i,9]-RUL_PREDICTION_bigscore[i,3]
}

TRUE_RUL_DECREASING_lowscore
ordervector_mean<- as.vector(TRUE_RUL_DECREASING_lowscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_lowscore_SORTED <- RUL_PREDICTION_lowscore[match(ordervector_mean, RUL_PREDICTION_lowscore),]##SORT increasing RUL

ordervector_mean<- as.vector(TRUE_RUL_DECREASING_bigscore[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_bigscore_SORTED <- RUL_PREDICTION_bigscore[match(ordervector_mean, RUL_PREDICTION_bigscore),]##SORT increasing RUL


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_lowscore_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_lowscore_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUARE))

max(RUL_PREDICTION_lowscore[,7])
min(RUL_PREDICTION_lowscore[,7])
max(RUL_PREDICTION_lowscore[,10])
min(RUL_PREDICTION_lowscore[,10])

length(which(RUL_PREDICTION_lowscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0)) )

length(which(RUL_PREDICTION_lowscore[,7]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,7]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_lowscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_lowscore[,10]==(0)) )


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_bigscore_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
length(ERRORSQUARE)
SUM_ERRORSQUARE/length(ERRORSQUARE)
sqrt(SUM_ERRORSQUARE/length(ERRORSQUARE))


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_bigscore_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/length(ERRORSQUAREMED)
sqrt(SUM_ERRORSQUAREMED/length(ERRORSQUAREMED))

max(RUL_PREDICTION_bigscore[,7])
min(RUL_PREDICTION_bigscore[,7])
max(RUL_PREDICTION_bigscore[,10])
min(RUL_PREDICTION_bigscore[,10])

length(which(RUL_PREDICTION_bigscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0)) )

length(which(RUL_PREDICTION_bigscore[,7]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,7]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]< (-0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]> (0.5)) )

length(which(RUL_PREDICTION_bigscore[,10]==(-0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0.5)) )
length(which(RUL_PREDICTION_bigscore[,10]==(0)) )


no_of_closest_engine=5

list_closest_X<-list()
for (i in 1:48) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}
list_closest_X[[49]]<-c(96,69,92,67)
for (i in 50:100) {
  close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
  list_closest_X[[i]]<-close
}

length(TestBigClassEng)+length(TestLowClassEng)

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
plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200),ylab="RUL", xlab="Mean - 100 total Test Engines")
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,6], col="red")
}
legend(75, -50, legend=c("Predicted RUL", "True RUL"),
       col=c("red", "black"), pch=1:1, cex=0.8)

###PLOT for TRUE RUL and PRED MEDIAN
ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL

plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200), ylab="RUL", xlab="Median - 100 total Test Engines")
for (i in 1:100) {
  points(i,RUL_PREDICTION_SORTED[i,9], col="red")
}
legend(75, -50, legend=c("Predicted RUL", "True RUL"),
       col=c("red", "black"), pch=1:1, cex=0.8)
RUL_PREDICTION


#plot(RUL_PREDICTION[,2],abs(RUL_PREDICTION[,10]))



######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_SORTED[,10]^2
SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
SUM_ERRORSQUAREMED/100
sqrt(SUM_ERRORSQUAREMED/100)


######RMSE MEAN
ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2

SUM_ERRORSQUARE<-sum(ERRORSQUARE)
SUM_ERRORSQUARE/100
sqrt(SUM_ERRORSQUARE/100)


######RMSE MEDIAN
ERRORSQUAREMED <- RUL_PREDICTION_SORTED[,10]^2
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

length(which(RUL_PREDICTION[,10]< (-0.5)) )

length(which(RUL_PREDICTION[,10]> (0.5)) )

length(which(RUL_PREDICTION[,10]==(-0.5)) )
length(which(RUL_PREDICTION[,10]==(0.5)) )
length(which(RUL_PREDICTION[,10]==(0)) )



#############
##############
##################aşağısı önemsiz

# 
# ####TRUE RULimport
# RUL_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24test_RUL.csv", header = TRUE)
# RUL_test
# RUL_test<-as.matrix(RUL_test)
# 
# TRUE_RUL_DECREASING<-RUL_test[order(RUL_test[,3],decreasing = FALSE),]
# plot(TRUE_RUL_DECREASING[,3])
# 
# ####RUL Prediction
# 
# no_of_closest_engine=9
# 
# LIFE_TRAIN<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_LIFE.csv", header = TRUE)
# LIFE_TRAIN<-as.matrix(LIFE_TRAIN)
# LIFE_TRAIN
# 
# list_closest_X<-list()
# for (i in 1:48 ) {
#   close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
#   list_closest_X[[i]]<-close
# }
# list_closest_X[[49]]<-c(96,69,92,67)
# for (i in 50:100) {
#   close<-list_dist_all_sorted[[i]][2:(no_of_closest_engine+1),1]
#   list_closest_X[[i]]<-close
# }
# # FOR ENGINE 49 - 96,69,92,67
# #1 NO OF ENGINE
# #2 OBS Test
# #3 RUL TEST
# #4 LIFE TEST
# #5 PRED LIFE
# #6 PRED RUL
# #7 MEAN RUL ERROR
# #8 PRED LIFE MEDIAN
# #9 PRED RUL MEDIAN
# #9 MEDIAN RUL ERROR
# 
# 
# RUL_PREDICTION_lowscore<- matrix(data = NA, nrow=100, 10)
# newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
#                    "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
#                    "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
# RUL_PREDICTION_lowscore[,1]<-RUL_test[,1]
# RUL_PREDICTION_lowscore[,2]<-RUL_test[,2]
# RUL_PREDICTION_lowscore[,3]<-RUL_test[,3]
# RUL_PREDICTION_lowscore[,4]<-RUL_test[,4]
# 
# for (i in TestLowClassEng) {
#   RUL_PREDICTION_lowscore[i,5]<-mean(LIFE_TRAIN[list_closest_X[[i]],2])
#   RUL_PREDICTION_lowscore[i,6]<-RUL_PREDICTION_lowscore[i,5]-RUL_PREDICTION_lowscore[i,2]
#   RUL_PREDICTION_lowscore[i,7]<-RUL_PREDICTION_lowscore[i,6]-RUL_PREDICTION_lowscore[i,3]
#   RUL_PREDICTION_lowscore[i,8]<-median(LIFE_TRAIN[list_closest_X[[i]],2]) 
#   RUL_PREDICTION_lowscore[i,9]<-RUL_PREDICTION_lowscore[i,8]-RUL_PREDICTION_lowscore[i,2]
#   RUL_PREDICTION_lowscore[i,10]<-RUL_PREDICTION_lowscore[i,9]-RUL_PREDICTION_lowscore[i,3]
# }
# for (i in TestBigClassEng) {
#   RUL_PREDICTION_lowscore[i,5]<-mean(LIFE_TRAIN[list_closest_X[[i]],2])
#   RUL_PREDICTION_lowscore[i,6]<-RUL_PREDICTION_bigscore[i,5]-RUL_PREDICTION_bigscore[i,2]
#   RUL_PREDICTION_lowscore[i,7]<-RUL_PREDICTION_bigscore[i,6]-RUL_PREDICTION_bigscore[i,3]
#   RUL_PREDICTION_lowscore[i,8]<-median(LIFE_TRAIN[list_closest_X[[i]],2]) 
#   RUL_PREDICTION_lowscore[i,9]<-RUL_PREDICTION_bigscore[i,8]-RUL_PREDICTION_bigscore[i,2]
#   RUL_PREDICTION_lowscore[i,10]<-RUL_PREDICTION_bigscore[i,9]-RUL_PREDICTION_bigscore[i,3]
# }
# 
# ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
# RUL_PREDICTION_lowscore_SORTED <- RUL_PREDICTION_lowscore[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL
# 
# RUL_PREDICTION_lowscore<- matrix(data = NA, nrow=100, 10)
# newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
#                    "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
#                    "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
# colnames(RUL_PREDICTION) <- newheaderstest
# RUL_PREDICTION[,1]<-RUL_test[,1]
# RUL_PREDICTION[,2]<-RUL_test[,2]
# RUL_PREDICTION[,3]<-RUL_test[,3]
# RUL_PREDICTION[,4]<-RUL_test[,4]
# for (i in 1:100) {
#   RUL_PREDICTION[i,5]<-mean(LIFE_TRAIN[list_closest_X[[i]],2])
#   RUL_PREDICTION[i,6]<-RUL_PREDICTION[i,5]-RUL_PREDICTION[i,2]
#   RUL_PREDICTION[i,7]<-RUL_PREDICTION[i,6]-RUL_PREDICTION[i,3]
#   RUL_PREDICTION[i,8]<-median(LIFE_TRAIN[list_closest_X[[i]],2]) 
#   RUL_PREDICTION[i,9]<-RUL_PREDICTION[i,8]-RUL_PREDICTION[i,2]
#   RUL_PREDICTION[i,10]<-RUL_PREDICTION[i,9]-RUL_PREDICTION[i,3]
# }
# 
# ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
# RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL
# 
# ###PLOT for TRUE RUL and PRED RUL
# plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200),ylab="RUL", xlab="Mean - 100 total Test Engines")
# for (i in 1:100) {
#   points(i,RUL_PREDICTION_SORTED[i,6], col="red")
# }
# legend(75, -50, legend=c("Predicted RUL", "True RUL"),
#        col=c("red", "black"), pch=1:1, cex=0.8)
# 
# ###PLOT for TRUE RUL and PRED MEDIAN
# ordervector_mean<- as.vector(TRUE_RUL_DECREASING[,1]) # ORDER vector for increasing RUL
# RUL_PREDICTION_SORTED <- RUL_PREDICTION[match(ordervector_mean, RUL_PREDICTION),]##SORT increasing RUL
# 
# plot(TRUE_RUL_DECREASING[,3], ylim=c(-100,200), ylab="RUL", xlab="Median - 100 total Test Engines")
# for (i in 1:100) {
#   points(i,RUL_PREDICTION_SORTED[i,9], col="red")
# }
# legend(75, -50, legend=c("Predicted RUL", "True RUL"),
#        col=c("red", "black"), pch=1:1, cex=0.8)
# RUL_PREDICTION
# 
# ######RMSE MEAN
# ERRORSQUARE <- RUL_PREDICTION_SORTED[,7]^2
# 
# SUM_ERRORSQUARE<-sum(ERRORSQUARE)
# SUM_ERRORSQUARE/100
# sqrt(SUM_ERRORSQUARE/100)
# 
# 
# ######RMSE MEDIAN
# ERRORSQUAREMED <- RUL_PREDICTION_SORTED[,10]^2
# SUM_ERRORSQUAREMED<-sum(ERRORSQUAREMED)
# SUM_ERRORSQUAREMED/100
# sqrt(SUM_ERRORSQUAREMED/100)
# 
# 
# max(RUL_PREDICTION[,7])
# min(RUL_PREDICTION[,7])
# max(RUL_PREDICTION[,10])
# min(RUL_PREDICTION[,10])
# 
# length(which(RUL_PREDICTION[,10]< (-0.5)) )
# 
# length(which(RUL_PREDICTION[,10]> (0.5)) )
# 
# length(which(RUL_PREDICTION[,10]==(-0.5)) )
# length(which(RUL_PREDICTION[,10]==(0.5)) )
# length(which(RUL_PREDICTION[,10]==(0)) )
# 
# length(which(RUL_PREDICTION[,7]< (-0.5)) )
# 
# length(which(RUL_PREDICTION[,7]> (0.5)) )
# 
# length(which(RUL_PREDICTION[,10]< (-0.5)) )
# 
# length(which(RUL_PREDICTION[,10]> (0.5)) )
# 
# length(which(RUL_PREDICTION[,10]==(-0.5)) )
# length(which(RUL_PREDICTION[,10]==(0.5)) )
# length(which(RUL_PREDICTION[,10]==(0)) )
# 
# 
# 
