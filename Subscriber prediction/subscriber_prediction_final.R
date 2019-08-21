###########################
#LifetimeSubscriber prediction
##########################
require(randomForest)
require(dplyr)

##########################
#data importing and processing
##########################
data2<-read.table("Scoring_algo\\consolidated.csv", header=T, stringsAsFactors = F, sep = ",")
head(data2)
dataa3_2<-data2[data2$subscriber_lifetime==0,]

test=dataa3_2 [,c(2:4,8:9)]
test$view_range<-NA
test$upload_range<-NA

test[test$views_lifetime<1000000 & test$views_lifetime>0,]$view_range="low"
test[test$views_lifetime>1000000 & test$views_lifetime<10000000,]$view_range="medium"
test[test$views_lifetime>10000000 ,]$view_range="high"

test[test$life_uploads<1000 & test$life_uploads>0,]$upload_range="low"
test[test$life_uploads>1000 & test$life_uploads<10000,]$upload_range="medium"
test[test$life_uploads>10000 ,]$upload_range="high"

test$views_lifetime<-as.numeric(test$views_lifetime)
test$life_uploads<-as.numeric(test$life_uploads)
test$category<-as.factor(test$category)
test$view_range<-as.factor(test$view_range)
test$upload_range<-as.factor(test$upload_range)

overall_data<-read.csv("Scoring_algo\\Raw dumps for scoring training\\Subscriber prediction\\New_training_data.csv",header = T, stringsAsFactors = F)

overall_data1<-overall_data[c(2:8)]

set.seed(12345)
overall_data11<-overall_data1 %>% group_by(catid) %>% sample_n(size = 1000, replace = T)
head(overall_data11)
overall_data11$views_lifetime<-overall_data11$views_lifetime+1
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

data13<-data.frame(matrix(nrow=nrow(overall_data11),ncol=7))
colnames(data13)<-colnames(overall_data11)

data13[,1]<-log_fun(overall_data11[,1])
data13[,2]<-log_fun(overall_data11[,2])
data13[,3]<-log_fun(overall_data11[,3])
data13[,4]<-(overall_data11[,4])
data13[,5]<-(overall_data11[,5])
data13$category<-as.factor(data13$category)
data13[,6]<-overall_data11[,6]
data13$View_range<-as.factor(data13$View_range)
data13[,7]<-overall_data11[,7]
data13$upload_range<-as.factor(data13$upload_range)
data13$catid<-NULL

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


data13$subscriber_lifetime<-y
data13$views_lifetime<-x
data13$life_uploads<-z
################################################
#Train test split##
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
model.rf<-randomForest(subscriber_lifetime ~ ., data= train, keep.forest=TRUE, ntree=500, mtry=100)
summary(model.rf)

model.rf.predict<-predict(model.rf,newdata=cv,type='response', predict.all=TRUE,  
                          norm.votes=TRUE)

which.min(model.rf$mse)

cv$pred<-model.rf.predict$aggregate
cv$pred_exp<-exp(model.rf.predict$aggregate)
cv$act_exp<-cv$subscribers_lifetime1

test$catid<-NULL
test$subscriber_lifetime<-NULL
test$views_lifetime<-log_fun(test$views_lifetime)
test$life_uploads<-log_fun(test$life_uploads)
levels(test$category)<-levels(train$category)
levels(test$view_range)<-levels(train$View_range)
levels(test$upload_range)<-levels(train$upload_range)
colnames(test)<-c("views_lifetime","life_uploads", "category","View_range","upload_range")
##########################
#Prediction on test data
##########################
model.rf.predict1<-predict(model.rf,newdata=test,type='response', predict.all=TRUE,  
                           norm.votes=TRUE)

test$pred<-model.rf.predict1$aggregate
test$pred_exp<-exp(model.rf.predict1$aggregate)

dataa3_2$subscriber_lifetime<-round(test$pred_exp)
write.csv(data3_2, "subscriber_pred.csv")
