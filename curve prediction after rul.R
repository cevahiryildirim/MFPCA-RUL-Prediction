
RUL_PREDICTION_bigscore
RUL_PREDICTION_lowscore
TestBigClassEngexcept49
TestLowClassEng
TestBigClassEng
####curve prediction
class(fdsmoothallT30)

RUL_PREDICTION_low_big_merged<-matrix(NA, nrow = 100, ncol=10)
i
for (i in 1:length(TestLowClassEng)) {
RUL_PREDICTION_low_big_merged[TestLowClassEng[i],]<-RUL_PREDICTION_lowscore[i,]
}
for (i in 1:length(TestBigClassEng)) {
  RUL_PREDICTION_low_big_merged[TestBigClassEng[i],]<-RUL_PREDICTION_bigscore[i,]
}

RUL_PREDICTION_low_big_merged<-round(RUL_PREDICTION_low_big_merged,0)
newheaderstest<- c("Engine No","No of OBS", "True RUL", "TRUE Life",
                   "PRED Life(Mean)","Pred RUL(Mean)","RUL ERROR(Mean)",
                   "PRED Life(Med)","Pred RUL(Med)","RUL ERROR(Med)")
colnames(RUL_PREDICTION_low_big_merged) <- newheaderstest


funDatasmoothallT24_list<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT24,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT24_list[[i]]<-funDatasmoothallT24
}
funDatasmoothallT30_list<-list()
for (i in 1:100) {
  funDatasmoothallT24<-fd2funData(fdsmoothallT30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT30_list[[i]]<-funDatasmoothallT24
}
funDatasmoothallT50_list<-list()
for (i in 1:100) {
  funDatasmoothallT50<-fd2funData(fdsmoothallT50,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallT50_list[[i]]<-funDatasmoothallT50
}
funDatasmoothallP30_list<-list()
for (i in 1:100) {
  funDatasmoothallP30<-fd2funData(fdsmoothallP30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallP30_list[[i]]<-funDatasmoothallP30
}
funDatasmoothallps30_list<-list()
for (i in 1:100) {
  funDatasmoothallps30<-fd2funData(fdsmoothallps30,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallps30_list[[i]]<-funDatasmoothallps30
}
funDatasmoothallphi_list<-list()
for (i in 1:100) {
  funDatasmoothallphi<-fd2funData(fdsmoothallphi,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallphi_list[[i]]<-funDatasmoothallphi
}
funDatasmoothallBPR_list<-list()
for (i in 1:100) {
  funDatasmoothallBPR<-fd2funData(fdsmoothallBPR,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallBPR_list[[i]]<-funDatasmoothallBPR
}
funDatasmoothallW31_list<-list()
for (i in 1:100) {
  funDatasmoothallW31<-fd2funData(fdsmoothallW31,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallW31_list[[i]]<-funDatasmoothallW31
}
funDatasmoothallW32_list<-list()
for (i in 1:100) {
  funDatasmoothallW32<-fd2funData(fdsmoothallW32,argvals = seq(0,1, length.out=RUL_PREDICTION_low_big_merged[i,5]))
  funDatasmoothallW32_list[[i]]<-funDatasmoothallW32
}


#curve 20'ye en yakın 10 train engine 
as.vector(list_dist_all_sorted[[20]][2:6,1])

VALUEST24<-funDatasmoothallT24@X
VALUEST30<-funDatasmoothallT30@X
VALUEST50<-funDatasmoothallT50@X
VALUESP30<-funDatasmoothallP30@X
VALUESps30<-funDatasmoothallps30@X
VALUESphi<-funDatasmoothallphi@X
VALUESBPR<-funDatasmoothallBPR@X
VALUESW31<-funDatasmoothallW31@X
VALUESW32<-funDatasmoothallW32@X

predictedcurves<-list
curve_prediction_list<-list()  
for (j in 1:48) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =RUL_PREDICTION_low_big_merged[j,6])
  predictedcurve<-matrix(NA,nrow = 1, ncol =RUL_PREDICTION_low_big_merged[j,6])
   for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list[[j]]@X[list_dist_all_sorted[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged[j,2]+1):RUL_PREDICTION_low_big_merged[j,5]]
   }
curve_prediction_list[[j]]<-CURVETAHMIN
tahmin<-matrix(NA,nrow = 1, ncol =RUL_PREDICTION_low_big_merged[j,6] )  
   for (k in 1:RUL_PREDICTION_low_big_merged[j,6]) {
tahmin[,k]<-mean(curve_prediction_list[[j]][,k])
}
  predictedcurves[[j]]<- tahmin
}

k=1
j=1

for (j in 50:100) {
  CURVETAHMIN<-matrix(NA,nrow = 5, ncol =RUL_PREDICTION_low_big_merged[j,6])
  predictedcurve<-matrix(NA,nrow = 1, ncol =RUL_PREDICTION_low_big_merged[j,6])
  for (i in 1:5) {
    CURVETAHMIN[i,]<-funDatasmoothallT24_list[[j]]@X[list_dist_all_sorted[[j]][2:6,1][i],(RUL_PREDICTION_low_big_merged[j,2]+1):RUL_PREDICTION_low_big_merged[j,5]]
  }
  curve_prediction_list[[j]]<-CURVETAHMIN
  tahmin<-matrix(NA,nrow = 1, ncol =RUL_PREDICTION_low_big_merged[j,6] )  
  for (k in 1:RUL_PREDICTION_low_big_merged[j,6]) {
    tahmin[,k]<-mean(curve_prediction_list[[j]][,k])
  }
  predictedcurves[[j]]<- tahmin
}

predictedcurves## predicted curve list for each test engine

#T24_all_train_test<-read.csv("C:/Users/cevah/Desktop/data_for_registration/test/NEW/1-T24train_test_all.csv", header = TRUE,row.names = 1)
#T24_all_train_test
#T24_all_train_test<-as.matrix(T24_all_train_test)

T24_TEST_OBSERVED<-T24_all_train_test[101:200, 2:363]
dim(T24_TEST_OBSERVED)

T24_TEST_PREDICTED<-list()
for (i in 1:48) {
  T24_TEST_PREDICTED[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED[i,]))),as.matrix(predictedcurves[[i]])) 
}
for (i in 50:100) {
  T24_TEST_PREDICTED[[i]]<-cbind(t(as.matrix(na.omit(T24_TEST_OBSERVED[i,]))),as.matrix(predictedcurves[[i]])) 
}



bsplinebasis2<- create.bspline.basis(c(0,1), 8)
bsplinebasis2
plot(bsplinebasis2)
bsplinebasis
################# separately smoothing and merging.############

#A<-smooth.basis(ENGINEallArgvals,as.vector(na.omit(K[,i])),bsplinebasis2)
#ENGINEallArgvals
#K


####buradaki for ile her bir datayı ayrı ayrı bspile ile smoot yapıyoruz
smoothallT24 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(K[,i])))
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(K[,i])),bsplinebasis2)[["fd"]][["coefs"]]
}


predictedcurves_smooth_matrix<-matrix(data = NA, nrow=8, ncol=100)
for (j in 1:48) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged[j,5]),
                         y= as.vector(T24_TEST_PREDICTED[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged[j,5]),no_of_splines))
  predictedcurves_smooth_list[[j]]<-Smooth
}
for (j in 50:100) {
  Smooth<- smooth.basis( argvals = seq(1,RUL_PREDICTION_low_big_merged[j,5]),
                         y= as.vector(T24_TEST_PREDICTED[[j]]), 
                         fdParobj = create.bspline.basis(c(1,RUL_PREDICTION_low_big_merged[j,5]),no_of_splines))
  predictedcurves_smooth_list[[j]]<-Smooth
}


#10,

T24_TEST_PREDICTED[[testengine]]
plot(predictedcurves_smooth_list[[20]] , xlim=c(0,370), ylim=c(641.7,644), col="red")
for (i in TestLowClassEng) {
 lines(predictedcurves_smooth_list[[i]],col="red")
}
for (i in TestBigClassEngexcept49) {
  lines(predictedcurves_smooth_list[[i]], col="black")
}
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")



testengine=35

#bigscore individual curves
TestBigClassEng
plot(predictedcurves_smooth_list[[testengine]] , xlim=c(0,370), ylim=c(641.7,644), col="red")
lines(list_test_all_smooth_T24_Big_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

#low score individual curves
TestLowClassEng
testengine=70
testengine=80
testengine=97
testengine=98

plot(predictedcurves_smooth_list[[testengine]] , xlim=c(0,370), ylim=c(641.7,644), col="red")
lines(list_test_all_smooth_T24_Low_scores[[testengine]][[nrow(list_test_matrix_SCORE_W31_noNA[[testengine]])]], col="blue")

smoothallT24 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT24traindata[,i])))
  smoothallT24[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT24traindata[,i]))/meanT24,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallT30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT30traindata[,i])))
  smoothallT30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT30traindata[,i]))/meanT30,bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallT50 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tT50traindata[,i])))
  smoothallT50[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tT50traindata[,i]))/meanT50,bsplinebasis2)[["fd"]][["coefs"]]
}


smoothallP30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tP30traindata[,i])))
  smoothallP30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tP30traindata[,i]))/meanP30,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallps30 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tps30traindata[,i])))
  smoothallps30[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tps30traindata[,i]))/meanps30,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallphi <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tphitraindata[,i])))
  smoothallphi[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tphitraindata[,i]))/meanphi,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallBPR <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tBPRtraindata[,i])))
  smoothallBPR[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tBPRtraindata[,i]))/meanBPR,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW31 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW31traindata[,i])))
  smoothallW31[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW31traindata[,i]))/meanW31,bsplinebasis2)[["fd"]][["coefs"]]
}

smoothallW32 <- matrix(data = NA, nrow=8, ncol=100)
for (i in 1:100) {
  ENGINEallArgvals <-seq(0,1, length.out= length(na.omit(tW32traindata[,i])))
  smoothallW32[,i]=smooth.basis(ENGINEallArgvals,as.vector(na.omit(tW32traindata[,i]))/meanW32,bsplinebasis2)[["fd"]][["coefs"]]
}




T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g

##bspline compare
T24g <- autoplot(BsplineRegisteredsmoothedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g
##weighted compare
T24g <- autoplot(UnivRegSmthedWeightedMFPCA$functions[[1]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T24g


T30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[2]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T30g

T50g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[3]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
T50g

P30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[4]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
P30g

ps30g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[5]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
ps30g

phig <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[6]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
phig

BPRg <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[7]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
BPRg

W31g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[8]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
W31g

W32g <- autoplot(UnivRegisteredsmoothedMFPCA$functions[[9]]) + geom_hline(yintercept = 0, col = "grey", lwd = 1.5) + geom_line(aes(colour = obs), lwd = 1.25)+ labs(x = "Cycle Time") + scale_color_manual(values = c("#B80C0C", "#0C0CB8", "#129412"), name = "Princ. Comp.", labels= c("1st", "2nd", "3rd")) 
W32g


get_legend<-function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


gridExtra::grid.arrange(T24g + theme(legend.position = "none"), 
                        T30g + theme(legend.position = "none"), 
                        T50g + theme(legend.position = "none"), 
                        P30g + theme(legend.position = "none"), 
                        ps30g + theme(legend.position = "none"), 
                        phig + theme(legend.position = "none"),
                        BPRg + theme(legend.position = "none"), 
                        W31g + theme(legend.position = "none"), 
                        W32g + theme(legend.position = "none"),
                        nrow= 3, ncol = 3, widths = c(1, 1, 1))


