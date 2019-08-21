###########################
#Last_30_views_prediction 
#Predicting without Last_30_eng
##########################
#install.packages("randomForest")
require(randomForest)
require(dplyr)
setwd("Scoring_algo/Raw dumps for scoring training/channel_views_last_30_day")
#########################
# Preparing test data
#########################
data2<-read.table("Scoring_algo\\consolidated.csv", header=T, stringsAsFactors = F, sep = ",")
head(data2)
dataa3_3<-data2[data2$last_30_views==0,]

data3_3$last_30_eng[data3_3$last_30_eng==""]=0
data3_3$last_30_eng[data3_3$last_30_eng=="N/A"]=0
data3_3$last_30_eng[data3_3$last_30_eng=="NA"]=0

tempp1<-data3_3[data3_3$last_30_eng==0,]
if (nrow(tempp1)>0)
{
  test=NULL
  test=tempp1[,c(2:4,9)]
  
  test$view_range<-NA
  test$upload_range<-NA
  test$sub_range<-NA
  
  repeat {f<-tryCatch({test[test$subscriber_lifetime<1000 & test$subscriber_lifetime>0,]$sub_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$subscriber_lifetime>1000 & test$subscriber_lifetime<100000,]$sub_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$subscriber_lifetime>100000 ,]$sub_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  
  repeat {f<-tryCatch({test[test$views_lifetime<1000000 & test$views_lifetime>0,]$view_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$views_lifetime>1000000 & test$views_lifetime<10000000,]$view_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$views_lifetime>10000000 ,]$view_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  
  repeat {f<-tryCatch({test[test$life_uploads<1000 & test$life_uploads>0,]$upload_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$life_uploads>1000 & test$life_uploads<10000,]$upload_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  repeat {f<-tryCatch({test[test$life_uploads>10000 ,]$upload_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
  
  test$subscriber_lifetime<-as.numeric(test$subscriber_lifetime)
  test$views_lifetime<-as.numeric(test$views_lifetime)
  test$life_uploads<-as.numeric(test$life_uploads)
  test$last_30_views<-NULL
  test$catid<-NULL
  test$category<-as.factor(test$category)
  test$view_range<-as.factor(test$view_range)
  test$upload_range<-as.factor(test$upload_range)
  test$sub_range<-as.factor(test$sub_range)
  print(c("test is prepared",nrow(test) ))
  ##########################
  #importing training data
  #########################
  overall_data<-read.csv("Scoring_algo\\Raw dumps for scoring training\\channel_views_last_30_day\\New_training_data_full.csv",header = T, stringsAsFactors = F)
  head(overall_data)
  overall_data<-overall_data[overall_data$last_30_views>0,]
  overall_data1<-overall_data[c(2:10)]
  table(overall_data1$category)
  set.seed(12345)
  overall_data11<-overall_data1 %>% group_by(catid) %>% sample_n(size = 5000, replace = T)
  head(overall_data11)
  
  overall_data11$views_lifetime<-overall_data11$views_lifetime+1
  overall_data11$last_30_views<-overall_data11$last_30_views+1
  overall_data11$subscriber_lifetime<-overall_data11$subscriber_lifetime+1
  overall_data11$life_uploads<-overall_data11$life_uploads+1
  overall_data11$category<-as.factor(overall_data11$category)
  overall_data11$View_range<-as.factor(overall_data11$View_range)
  overall_data11$upload_range<-as.factor(overall_data11$upload_range)
  overall_data11$sub_range<-as.factor(overall_data11$sub_range)
  overall_data11$catid<-NULL
  overall_data11<-as.data.frame(overall_data11)
  head(overall_data11)
  
  log_fun<-function(x)
  {
    log(x)
  }
  
  data13<-data.frame(matrix(nrow=nrow(overall_data11),ncol=8))
  colnames(data13)<-colnames(overall_data11)
  
  data13[,1]<-log_fun(overall_data11[,1])
  data13[,2]<-log_fun(overall_data11[,2])
  data13[,3]<-log_fun(overall_data11[,3])
  data13[,4]<-log_fun(overall_data11[,4])
  data13[,5]<-(overall_data11[,5])
  data13$category<-as.factor(data13$category)
  data13[,6]<-overall_data11[,6]
  data13$View_range<-as.factor(data13$View_range)
  data13[,7]<-overall_data11[,7]
  data13$upload_range<-as.factor(data13$upload_range)
  data13[,8]<-overall_data11[,8]
  data13$sub_range<-as.factor(data13$sub_range)

  x <- data13$views_lifetime
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  
  y<- data13$subscriber_lifetime
  qnt <- quantile(y, probs=c(.25, .75), na.rm = T)
  caps <- quantile(y, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(y, na.rm = T)
  y[y < (qnt[1] - H)] <- caps[1]
  y[y > (qnt[2] + H)] <- caps[2]
  
  z<- data13$life_uploads
  qnt <- quantile(z, probs=c(.25, .75), na.rm = T)
  caps <- quantile(z, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(z, na.rm = T)
  z[z < (qnt[1] - H)] <- caps[1]
  z[z > (qnt[2] + H)] <- caps[2]
  
  zz<- data13$last_30_views
  qnt <- quantile(zz, probs=c(.25, .75), na.rm = T)
  caps <- quantile(zz, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(zz, na.rm = T)
  zz[zz < (qnt[1] - H)] <- caps[1]
  zz[zz > (qnt[2] + H)] <- caps[2]
  
  
  data13$subscriber_lifetime<-y
  data13$views_lifetime<-x
  data13$life_uploads<-z
  data13$last_30_views<-zz
  print(c("training data is prepared",nrow(data13)))
  
  smp_size <- floor(0.80 * nrow(data13))
  RMSE = function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data13)), size = smp_size)
  train <- data13[train_ind, ]
  cv <- data13[-train_ind, ]
  
  ##################################################
  #Random forest####
  ##################################################
  print("model is running")
  model.rf<-randomForest(last_30_views ~ ., data= train, keep.forest=TRUE, ntree=500)
  summary(model.rf)
  
  model.rf.predict<-predict(model.rf,newdata=cv,type='response', predict.all=TRUE,  
                            norm.votes=TRUE)
  
  which.min(model.rf$mse)
  
  cv$pred<-model.rf.predict$aggregate
  cv$pred_exp<-exp(model.rf.predict$aggregate)
  cv$act<-cv$last_30_views
  
  ########################
  #predicting test data
  ########################
  test$subscriber_lifetime<-log_fun(test$subscriber_lifetime)
  test$views_lifetime<-log_fun(test$views_lifetime)
  test$life_uploads<-log_fun(test$life_uploads)
  
  levels(test$category)<-levels(train$category)
  levels(test$view_range)<-levels(train$View_range)
  levels(test$upload_range)<-levels(train$upload_range)
  levels(test$sub_range)<-levels(train$sub_range)
  
  colnames(test)<-c("subscriber_lifetime","views_lifetime","life_uploads", "category","View_range","upload_range","sub_range")
  model.rf.predict1<-predict(model.rf,newdata=test,type='response', predict.all=TRUE,  
                             norm.votes=TRUE)
  
  test$pred<-model.rf.predict1$aggregate
  test$pred_exp<-exp(model.rf.predict1$aggregate)
  print(c("prediction is done",nrow(test)))
  tempp1$last_30_views<-round(test$pred_exp)
  for (i in 1:length(unique(tempp1$channelid)))
  {
  data3_3$last_30_views[data3_3$channelid %in% tempp1$channelid]<-tempp1$last_30_views[tempp1$channelid %in% data3_3$channelid]
  }
  write.csv(data3_3, "last_30_views_pred_WO_30_eng.csv")
}


###########################################
#Predicting with last_30_eng
###########################################
tempp2<-data3_3[data3_3$last_30_eng!=0,]
if (nrow(tempp2)>0)
{

test=NULL
test=tempp2[,c(2:6,9)]

test$view_range<-NA
test$upload_range<-NA
test$sub_range<-NA

repeat {f<-tryCatch({test[test$subscriber_lifetime<1000 & test$subscriber_lifetime>0,]$sub_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$subscriber_lifetime>1000 & test$subscriber_lifetime<100000,]$sub_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$subscriber_lifetime>100000 ,]$sub_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}

repeat {f<-tryCatch({test[test$views_lifetime<1000000 & test$views_lifetime>0,]$view_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$views_lifetime>1000000 & test$views_lifetime<10000000,]$view_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$views_lifetime>10000000 ,]$view_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}

repeat {f<-tryCatch({test[test$life_uploads<1000 & test$life_uploads>0,]$upload_range="low";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$life_uploads>1000 & test$life_uploads<10000,]$upload_range="medium";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}
repeat {f<-tryCatch({test[test$life_uploads>10000 ,]$upload_range="high";f<-1;},error=function(e) "0" );if(f==1) {break};print("Passing..");break}

test$subscriber_lifetime<-as.numeric(test$subscriber_lifetime)
test$views_lifetime<-as.numeric(test$views_lifetime)
test$life_uploads<-as.numeric(test$life_uploads)

test$last_30_views<-NULL
test$catid<-NULL
test$category<-as.factor(test$category)
test$view_range<-as.factor(test$view_range)
test$upload_range<-as.factor(test$upload_range)
test$sub_range<-as.factor(test$sub_range)
test$last_30_eng<-as.numeric(test$last_30_eng)
print(c("test is prepared",nrow(test) ))
##########################
#importing training data
#########################
overall_data<-read.csv("Scoring_algo\\Raw dumps for scoring training\\channel_views_last_30_day\\New_training_data_full W 30_eng.csv",header = T, stringsAsFactors = F)
head(overall_data)
overall_data<-overall_data[overall_data$last_30_views>0,]
overall_data1<-overall_data[c(2:11)]
table(overall_data1$category)
set.seed(12345)
overall_data11<-overall_data1 %>% group_by(catid) %>% sample_n(size = 5000, replace = T)
head(overall_data11)

overall_data11$views_lifetime<-overall_data11$views_lifetime+1
overall_data11$last_30_views<-overall_data11$last_30_views+1
overall_data11$subscriber_lifetime<-overall_data11$subscriber_lifetime+1
overall_data11$life_uploads<-overall_data11$life_uploads+1
overall_data11$last_30_eng<-overall_data11$last_30_eng+1
overall_data11$category<-as.factor(overall_data11$category)
overall_data11$View_range<-as.factor(overall_data11$View_range)
overall_data11$upload_range<-as.factor(overall_data11$upload_range)
overall_data11$sub_range<-as.factor(overall_data11$sub_range)
overall_data11$catid<-NULL
overall_data11<-as.data.frame(overall_data11)
head(overall_data11)

log_fun<-function(x)
{
  log(x)
}

data13<-data.frame(matrix(nrow=nrow(overall_data11),ncol=9))
colnames(data13)<-colnames(overall_data11)

data13[,1]<-log_fun(overall_data11[,1])
data13[,2]<-log_fun(overall_data11[,2])
data13[,3]<-log_fun(overall_data11[,3])
data13[,4]<-log_fun(overall_data11[,4])
data13[,5]<-(overall_data11[,5])
data13$category<-as.factor(data13$category)
data13[,6]<-overall_data11[,6]
data13$View_range<-as.factor(data13$View_range)
data13[,7]<-overall_data11[,7]
data13$upload_range<-as.factor(data13$upload_range)
data13[,8]<-overall_data11[,8]
data13$sub_range<-as.factor(data13$sub_range)
data13[,9]<-log_fun(overall_data11[,9])
print(c("training data is prepared",nrow(data13)))

x <- data13$views_lifetime
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]

y<- data13$subscriber_lifetime
qnt <- quantile(y, probs=c(.25, .75), na.rm = T)
caps <- quantile(y, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt[1] - H)] <- caps[1]
y[y > (qnt[2] + H)] <- caps[2]

z<- data13$life_uploads
qnt <- quantile(z, probs=c(.25, .75), na.rm = T)
caps <- quantile(z, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(z, na.rm = T)
z[z < (qnt[1] - H)] <- caps[1]
z[z > (qnt[2] + H)] <- caps[2]

zz<- data13$last_30_views
qnt <- quantile(zz, probs=c(.25, .75), na.rm = T)
caps <- quantile(zz, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(zz, na.rm = T)
zz[zz < (qnt[1] - H)] <- caps[1]
zz[zz > (qnt[2] + H)] <- caps[2]

yy<- data13$last_30_eng
qnt <- quantile(yy, probs=c(.25, .75), na.rm = T)
caps <- quantile(yy, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(yy, na.rm = T)
yy[yy < (qnt[1] - H)] <- caps[1]
yy[yy > (qnt[2] + H)] <- caps[2]


data13$subscriber_lifetime<-y
data13$views_lifetime<-x
data13$life_uploads<-z
data13$last_30_views<-zz
data13$last_30_eng<-yy

################################################

smp_size <- floor(0.80 * nrow(data13))
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

set.seed(123)
train_ind <- sample(seq_len(nrow(data13)), size = smp_size)
train <- data13[train_ind, ]
cv <- data13[-train_ind, ]
##################################################
#Random forest####
##################################################
print("model is running")
model.rf.WENG<-randomForest(last_30_views ~ (subscriber_lifetime)+(views_lifetime)+(life_uploads)+(category)+(View_range)+(upload_range)+(sub_range)+(last_30_eng)^2, data= train, keep.forest=TRUE, ntree=500)
summary(model.rf.WENG)

model.rf.WENG.predict<-predict(model.rf.WENG,newdata=cv,type='response', predict.all=TRUE,  
                          norm.votes=TRUE)

which.min(model.rf.WENG$mse)

cv$pred<-model.rf.WENG.predict$aggregate
cv$pred_exp<-exp(model.rf.WENG.predict$aggregate)
cv$act<-cv$last_30_views

########################
#predicting test data
########################
test$subscriber_lifetime<-log_fun(test$subscriber_lifetime)
test$views_lifetime<-log_fun(test$views_lifetime)
test$life_uploads<-log_fun(test$life_uploads)
test$last_30_eng<-log_fun(test$last_30_eng)
levels(test$category)<-levels(train$category)
levels(test$view_range)<-levels(train$View_range)
levels(test$upload_range)<-levels(train$upload_range)
levels(test$sub_range)<-levels(train$sub_range)

colnames(test)<-c("subscriber_lifetime","views_lifetime","life_uploads","last_30_eng", "category","View_range","upload_range","sub_range")
model.rf.WENG.predict1<-predict(model.rf.WENG,newdata=test,type='response', predict.all=TRUE,  
                           norm.votes=TRUE)

test$pred<-model.rf.WENG.predict1$aggregate
test$pred_exp<-exp(model.rf.WENG.predict1$aggregate)
print(c("prediction is done",nrow(test)))
tempp2$last_30_views<-round(test$pred_exp)
for (i in 1:length(unique(tempp2$channelid)))
{
  data3_3$last_30_views[data3_3$channelid %in% tempp2$channelid]<-tempp2$last_30_views[tempp2$channelid %in% data3_3$channelid]
}
write.csv(data3_3, "last_30_views_pred_W_30_eng.csv")
}
