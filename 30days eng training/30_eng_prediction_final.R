###########################
#LifetimeSubscriber prediction
###########################
#install.packages("randomForest")
require(randomForest)
require(dplyr)
setwd("E:/Laxmi_Rnd/My Laptop/Work/Scoring_algo/Raw dumps for scoring training/30days eng training")
##########################
#preparing test data
##########################

data2<-read.table("E:\\Laxmi_Rnd\\My Laptop\\Work\\Scoring_algo\\consolidated.csv", header=T, stringsAsFactors = F, sep = ",")
head(data2)
data3_3<-data2[data2$last_30_eng==0,]

tempp1<-data3_3

test=data3_3[,c(2:5,8:9)]
test$view_range<-NA
test$upload_range<-NA

test[test$views_lifetime<1000000 & test$views_lifetime>0,]$view_range="low"
test[test$views_lifetime>1000000 & test$views_lifetime<10000000,]$view_range="medium"
test[test$views_lifetime>10000000 ,]$view_range="high"

test[test$life_uploads<1000 & test$life_uploads>0,]$upload_range="low"
test[test$life_uploads>1000 & test$life_uploads<10000,]$upload_range="medium"
test[test$life_uploads>10000 ,]$upload_range="high"

test$subscriber_lifetime<-as.numeric(test$subscriber_lifetime)
test$views_lifetime<-as.numeric(test$views_lifetime)
test$life_uploads<-as.numeric(test$life_uploads)
test$last_30_views<-as.numeric(test$last_30_views)

test$category<-as.factor(test$category)
test$view_range<-as.factor(test$view_range)
test$upload_range<-as.factor(test$upload_range)
#####################################################
#preparing training data
#####################################################
overall_data<-read.csv("E:\\Laxmi_Rnd\\My Laptop\\Work\\Scoring_algo\\Raw dumps for scoring training\\30days eng training\\New_training_data.csv",header = T, stringsAsFactors = F)
overall_data1<-overall_data[c(2:10)]
overall_data1<-overall_data1[overall_data1$last_30_views>0,]

set.seed(12345)
overall_data11<-overall_data1 %>% group_by(catid) %>% sample_n(size = 500, replace = T)
head(overall_data11)

overall_data11$views_lifetime<-overall_data11$views_lifetime+1
overall_data11$last_30_eng<-overall_data11$last_30_eng+1
overall_data11$last_30_views<-overall_data11$last_30_views+1

overall_data11$subscriber_lifetime<-overall_data11$subscriber_lifetime+1
overall_data11$life_uploads<-overall_data11$life_uploads+1
overall_data11$category<-as.factor(overall_data11$category)
overall_data11$View_range<-as.factor(overall_data11$View_range)
overall_data11$upload_range<-as.factor(overall_data11$upload_range)
overall_data11<-as.data.frame(overall_data11)

log_fun<-function(x)
{
  log(x)
}

data13<-data.frame(matrix(nrow=nrow(overall_data11),ncol=9))
colnames(data13)<-colnames(overall_data11)

data13[,1]<-log_fun(overall_data11[,1])
data13[,2]<-log_fun(overall_data11[,2])
data13[,3]<-log_fun(overall_data11[,3])
data13[,4]<-(overall_data11[,4])
data13[,5]<-(overall_data11[,5])
data13$category<-as.factor(data13$category)
data13[,6]<-overall_data11[,6]
data13$View_range<-as.factor(data13$View_range)
data13[,7]<-(overall_data11[,7])
data13$upload_range<-as.factor(data13$upload_range)
data13[,8]<-log_fun(overall_data11[,8])
data13[,9]<-log_fun(overall_data11[,9])

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
data13$catid<-NULL

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
model.rf<-randomForest(last_30_eng ~ ., data= train, keep.forest=TRUE, ntree=100, mtry=100)
summary(model.rf)
cv11<-cv

model.rf.predict<-predict(model.rf,newdata=cv11,type='response', predict.all=TRUE,  
                          norm.votes=TRUE)

which.min(model.rf$mse)

cv11$pred<-model.rf.predict$aggregate
cv11$pred_exp<-exp(model.rf.predict$aggregate)
cv11$act<-cv$last_30_eng

test$catid<-NULL
test$subscriber_lifetime<-log_fun(test$subscriber_lifetime)
test$views_lifetime<-log_fun(test$views_lifetime)
test$life_uploads<-log_fun(test$life_uploads)
test$last_30_views<-log_fun(test$last_30_views)
levels(test$category)<-levels(train$category)
levels(test$view_range)<-levels(train$View_range)
levels(test$upload_range)<-levels(train$upload_range)
colnames(test)<-c("subscriber_lifetime","views_lifetime","life_uploads", "last_30_views","category","View_range","upload_range")

model.rf.predict1<-predict(model.rf,newdata=test,type='response', predict.all=TRUE,  
                           norm.votes=TRUE)

test$pred<-model.rf.predict1$aggregate
test$pred_exp<-exp(model.rf.predict1$aggregate)
tempp1$last_30_eng<-round(test$pred_exp)
for (i in 1:length(unique(data3_3$channelid)))
{
  data3_3$last_30_eng[data3_3$channelid %in% tempp1$channelid]<-tempp1$last_30_eng[tempp1$channelid %in% data3_3$channelid]
}
write.csv(data3_3, "last_30_eng_pred.csv")