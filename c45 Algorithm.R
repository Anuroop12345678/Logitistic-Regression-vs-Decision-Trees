library(readxl)
getwd()
data <- read_excel("C:/Users/user/Documents/Honours Modules/WST 795/Churn1.xlsx")

data[,20] <- lapply(data[,20], as.factor)

vec_dummy1 <- data$`International plan`   # Duplicate vector
vec_dummy1<- as.character(vec_dummy1)    # Convert vector to character
vec_dummy1[vec_dummy1 == "Yes"] <- 1      # Replace "Yes" by 1
vec_dummy1[vec_dummy1 == "No"] <- 0       # Replace "No" by 0
vec_dummy1 <- as.factor(vec_dummy1)      # Convert vector to numeric
data$`International plan`<-vec_dummy1

vec_dummy2 <- data$`Voice mail plan`   # Duplicate vector
vec_dummy2<- as.character(vec_dummy2)    # Convert vector to character
vec_dummy2[vec_dummy2 == "Yes"] <- 1      # Replace "Yes" by 1
vec_dummy2[vec_dummy2 == "No"] <- 0       # Replace "No" by 0
vec_dummy2 <- as.factor(vec_dummy2)      # Convert vector to numeric
data$`Voice mail plan`<-vec_dummy2

df = subset(data, select = -c(State) )
class(df$`Total day calls`)
#check class imabalance
table(df$Churn)
#check classes distribution
imbalance<-prop.table(table(df$Churn))
library(ggplot2)
ggplot(data = df, aes(x = Churn))+
  geom_bar(width = 0.5, color = "black")

sum(is.na(df$Churn))

groups<-as.factor(df$Churn)
newdata<-df[,6:15]
pairs(newdata,                     # Data frame of variables
      labels = colnames(newdata),  # Variable names
      pch = 21,                 # Pch symbol
      bg = rainbow(3)[groups],  # Background color of the symbol (pch 21 to 25)
      col = rainbow(3)[groups], # Border color of the symbol
      main = "Telecommunications dataset",    # Title of the plot
      row1attop = TRUE,         # If FALSE, changes the direction of the diagonal
      gap = 1,                  # Distance between subplots
      cex.labels = NULL,        # Size of the diagonal text
      font.labels = 1)          # Font style of the diagonal text

library(ggcorrplot)
library(dplyr)
model.matrix(~0+., data=df) %>% 
  cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

set.seed(1)

#use 70% of dataset as training set and 30% as test set
colnames(df)[c(1:19)]<-sub(" ", "_",colnames(df)[c(1:19)]) 
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

library(ROSE)
#HAVE to run this line separately otherwise give an error
data_balanced_under <- ovun.sample(Churn ~ ., data =train, method = "under", seed = 1)$data
table(data_balanced_under$Churn)


train<-ROSE(Churn ~., data = train, seed = 1)$data
table(train$Churn)



#check class imabalance
table(train$Churn)
#check classes distribution
prop.table(table(train$Churn))

#number of missing values
sum(as.numeric(is.na(train)))
sum(is.na(test$Churn))


###########################################################################################################
#install.packages("RWeka")

library(RWeka)
m1<-J48(Churn ~ ., data = train,)

predicted_m1<-as.numeric(predict(m1, test[1:(length(test)-1)], type = 'class'))

#install.packages("ROSE")
library(ROSE)
accuracy.meas(test$Churn, predicted_m1)  #Low F-score suggests weak accuracy of this model

#Performing accuracy measures and plotting ROC curve
#install.packages("caret")
library(caret)
classification_matrix<- table(as.numeric(test$Churn), predicted_m1)
classification_matrix
confusionMatrix(classification_matrix,positive="2")

#calculate AUC
library(ROCR)
probs = predict(m1,newdata=test[,-c(19)], type="class")
probs<- as.character(probs)   
probs[probs == "FALSE"] <- 0     
probs[probs== "TRUE"] <- 1 
probs<-as.numeric(probs)
pred <- prediction(probs, test$Churn)
# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
auci<-performance(pred,"auc")@y.values
auc_value<-auci[[1]]

#calculate TDL
#install.packages("lift")
library(lift)
probs<-predict(m1,test[1:(length(test)-1)],"probability")
TopDecileLift(predicted=probs,labels=as.numeric(test$Churn))

#install.packages("ROSE")

library(precrec)
testing.for.mJ48 <-as.numeric(predict(m1, newdata = test[1:(length(test)-1)],type = "class" ))
testing_mdatJ48 <- mmdata(scores = testing.for.mJ48,labels = test$Churn,
                       modnames = c("J48 Tree"))
library(ggplot2)
testing_ROC1<- autoplot(evalmod(testing_mdatJ48),curvetype = c("ROC"))+theme(legend.position = "bottom") +ggtitle("ROC Curve - Testing Data")+
  geom_abline(intercept = 0, slope = 1, size = 0.5) 
testing_PR1<- autoplot(evalmod(testing_mdatJ48),curvetype = c("PR"))+theme(legend.position = "bottom") +ggtitle("PR Curve - Testing Data")


testing_ROC1
testing_PR1

library(partykit)
if(require("partykit", quietly = TRUE)) 
plot(m1)
summary(m1)

##############################################################################################

library(LLM)
data <- read_excel("C:/Users/user/Documents/Honours Modules/WST 795/Churn1.xlsx")

data[,20] <- lapply(data[,20], as.factor)

vec_dummy1 <- data$`International plan`   # Duplicate vector
vec_dummy1<- as.character(vec_dummy1)    # Convert vector to character
vec_dummy1[vec_dummy1 == "Yes"] <- 1      # Replace "Yes" by 1
vec_dummy1[vec_dummy1 == "No"] <- 0       # Replace "No" by 0
vec_dummy1 <- as.numeric(vec_dummy1)      # Convert vector to numeric
data$`International plan`<-vec_dummy1

vec_dummy2 <- data$`Voice mail plan`   # Duplicate vector
vec_dummy2<- as.character(vec_dummy2)    # Convert vector to character
vec_dummy2[vec_dummy2 == "Yes"] <- 1      # Replace "Yes" by 1
vec_dummy2[vec_dummy2 == "No"] <- 0       # Replace "No" by 0
vec_dummy2 <- as.numeric(vec_dummy2)      # Convert vector to numeric
data$`Voice mail plan`<-vec_dummy2

df = subset(data, select = -c(State) )
library(ROSE)
colnames(df)[c(1:19)]<-sub(" ", "_",colnames(df)[c(1:19)])  #HAVE to run this line separately otherwise give an error

set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

train<-ROSE(Churn ~., data = train, seed = 1)$data
table(train$Churn)
#check class imabalance
table(train$Churn)

Pima.llm <- llm(X = train[,-c(19)],Y = train$Churn,
                threshold_pruning = 0.25,nbr_obs_leaf = 100)
summary(Pima.llm)
plot(Pima.llm[["Full decision tree for segmentation"]])

#help(predict.llm)
predicted_llm<-predict.llm(Pima.llm,test[,-c(19)])
yhat<-as.numeric((predicted_llm>0.5))

classification_matrix2<- table((test$Churn), as.logical(yhat))
classification_matrix2
library(caret)
confusionMatrix(classification_matrix2,positive="TRUE")

#calculate AUC
library(ROCR)
probs<-predicted_llm$probability
pred <- prediction(probs, test$Churn)
# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
auci<-performance(pred,"auc")@y.values
auc_value<-auci[[1]]
auc_value

probs = predict(Pima.llm,newdata=test[,-c(19)], type="response") #we cannot apply predict() function to an object of class logitleafmodel

#calculate F-score
library(ROSE)
accuracy.meas(test$Churn, predicted_m1)  #Low F-score suggests weak accuracy of this model

#calculate TDL
library(lift)
TopDecileLift(predicted=probs,labels=as.numeric(test$Churn))

#caclulate ROC
library(precrec)

testing.for.mLOGIT <-predicted_llm
testing_mdatLOGIT <- mmdata(scores = testing.for.mLOGIT,labels = test$Churn,
                       modnames = c("LLM"))

testing_ROC2 <- autoplot(evalmod(testing_mdatLOGIT),curvetype = c("ROC"))+theme(legend.position = "bottom") +ggtitle("ROC Curve - Testing Data")+
  geom_abline(intercept = 0, slope = 1, size = 0.5)
testing_PR2 <- autoplot(evalmod(testing_mdatLOGIT),curvetype = c("PR"))+theme(legend.position = "bottom") +ggtitle("PR Curve - Testing Data")

testing_ROC2
testing_PR2

###########################################################################################################

# Build the logistic regression model
log_model_multi <- glm(Churn~ .-Churn , family = "binomial", data = train)
#log_model_multi <- glm(Churn~ `Number vmail messages` + `Total day charge` +`Total day calls` , family = "binomial", data = df)
# Obtain significance levels using summary()
summary(log_model_multi)

pred<-predict(log_model_multi, newdata = test[,-c(19)],type="response")

#Confusion Matrix
glm_probs = data.frame(probs = predict(log_model_multi,newdata=test[,-c(19)], type="response"))
head(glm_probs)
contrasts(test$Churn)
glm_pred = glm_probs %>%
  mutate(pred = ifelse(probs>.5, "TRUE", "FALSE"))
glm_pred = cbind(test, glm_pred)

glm_pred %>% 
  count(pred, Churn) %>%
  spread(Churn, n, fill = 0)

glm_pred %>%
  summarize(score = mean(pred == Churn),
            recip=mean(pred!=Churn))

yhat<-as.numeric((pred>0.5))

classification_matrix3<- table(as.factor(test$Churn), as.logical(yhat))
classification_matrix3
library(caret)
confusionMatrix(classification_matrix3,positive="TRUE")


#calculate AUC
probs = predict(log_model_multi,newdata=test[,-c(19)], type="response")
library(ROCR)
pred <- prediction(probs, test$Churn)
# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
auci<-performance(pred,"auc")@y.values
auc_value<-auci[[1]]
auc_value


#calculate F-score
library(ROSE)
accuracy.meas(test$Churn, yhat)  #Low F-score suggests weak accuracy of this model

#calculate TDL
#install.packages("lift")
library(lift)
TopDecileLift(predicted=probs,labels=as.numeric(test$Churn))

#caclulate ROC
library(precrec)
testing.for.mLR <-as.numeric(predict(log_model_multi, newdata = test[,-c(19)],type = "response" ))
testing_mdatLR <- mmdata(scores = testing.for.mLR,labels = test$Churn,
                       modnames = c("Logistic Regression"))

testing_ROC3 <- autoplot(evalmod(testing_mdatLR),curvetype = c("ROC"))+theme(legend.position = "bottom") +ggtitle("ROC Curve - Testing Data")+
  geom_abline(intercept = 0, slope = 1, size = 0.5)
testing_PR3 <- autoplot(evalmod(testing_mdatLR),curvetype = c("PR"))+theme(legend.position = "bottom") +ggtitle("PR Curve - Testing Data")

testing_ROC3
testing_PR3


###########################################################################################################
library(rpart)
library(partykit)
banking.tree.3 <- rpart(data = test,formula = Churn~.-Churn, control = rpart.control(  maxdepth = 3)  )
banking.models <- list(banking.tree.3)
tree.to.feature <- function(tree.model,dt){
  require(partykit)
  tree_labels <- factor( predict(as.party(tree.model), dt ,type = "node") )
  return(tree_labels)}
#Fit the decision tree to logistic regression model
banking.train.preds <-lapply(X = banking.models,FUN = function(x){
  tree.to.feature(tree.model = x,dt = train)  }) %>% data.frame
names(banking.train.preds) <- c("four.nodes")
train<- cbind(train,banking.train.preds )
train <- train[1:(length(train)-1)]              #sometimes you do not need to run this column
train$four.nodes <- as.numeric(train$four.nodes)
df_new <- train
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train_new  <- df_new[sample, ]
test_new   <- df_new[!sample, ]
train_new<-na.omit(train_new)
test_new<-na.omit(test_new)
dim(test_new)
dim(train_new)
sum(is.na(train_new$Churn))
#train_new <- train_new[1:(length(train_new)-1)]
#test_new <- test_new[1:(length(test_new)-1)]
library(glmnet)
newX_train<- model.matrix(~.-train_new$Churn,data=train_new)
banking.mode.four.deep <- glmnet(x=newX_train[,-c(20)],y=newX_train[,20],family = "binomial")
options(na.action="na.fail")
newX_test <- model.matrix(Churn~.-test_new$Churn,data=test_new)
sum(is.na(test_new$Churn))
pred_DGLM<-predict(banking.mode.four.deep,newx=newX_test,type="response",s =0.05) 
yhat<-as.numeric((pred_DGLM>0.5))
yhat
as.numeric(test_new$Churn)-1
classification_matrix2<- table(as.numeric(test_new$Churn)-1, yhat)
classification_matrix2
library(caret)
confusionMatrix(classification_matrix2,positive="1")
#calculate AUC
library(pROC)
pred_DGLM<-predict(banking.mode.four.deep,newx=newX_test,type="class",s =0.05)
names(pred_DGLM)<-c("predicted")
auc.roc(test_new$Churn,pred_DGLM)
pred = predict(fit, newx = data, type = 'response',s ="lambda.min")
auc(label,pred)
library(lift)
TopDecileLift(predicted=yhat,labels=as.numeric(test_new$Churn))
library(precrec)
newX_test<-cbind(newX_test,test_new$Churn)
testing.for.mmdata <-predict(banking.mode.four.deep, newx=newX_test[,-c(20)],type = "response" )
testing_mdat <- mmdata(scores = testing.for.mmdata[,c(3)],labels = test_new$Churn,
                       modnames = c("Tree w/ GLM"))
testing_ROC <- autoplot(evalmod(testing_mdat),curvetype = c("ROC"))+theme(legend.position = "bottom") +ggtitle("ROC Curve - Testing Data")+
  geom_abline(intercept = 0, slope = 1, size = 0.5)
testing_PR <- autoplot(evalmod(testing_mdat),curvetype = c("PR"))+theme(legend.position = "bottom") +ggtitle("PR Curve - Testing Data")


###############################################################################################
library(rpart)
library(partykit)
banking.tree.3 <- rpart(data = test,
                        formula = Churn~.-Churn, control = rpart.control(  maxdepth = 3)  )
banking.models <- list(banking.tree.3)

tree.to.feature <- function(tree.model,dt){
  require(partykit)
  tree_labels <- factor( predict(as.party(tree.model), dt ,type = "node") )
  return(tree_labels)}

banking.train.preds <-lapply(X = banking.models,FUN = function(x){ 
  tree.to.feature(tree.model = x,dt = train)  })
banking.train.preds<-data.frame(banking.train.preds)

names(banking.train.preds) <- c("four.nodes")

train<- cbind(train,banking.train.preds )
#train <- train[1:(length(train)-1)]

train$four.nodes <- as.numeric(train$four.nodes)
df_new <- train
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train_new  <- df_new[sample, ]
test_new   <- df_new[!sample, ]
train_new<-na.omit(train_new)
test_new<-na.omit(test_new)


library(glmnet)
newX_train<- model.matrix(~.-train_new$Churn,data=train_new)
banking.mode.four.deep <- glmnet(x=newX_train[,-c(20)],y=newX_train[,20],family = "binomial")
newX_test <- model.matrix(Churn~.-test_new$Churn,data=test_new)

library(ROCR)
library(glmnet)
library(caret)

## Ridge Regression to create the Adaptive Weights Vector
x = newX_train[,-c(20)]
y = newX_train[,c(20)]
set.seed(999)
library(doParallel)
registerDoParallel(cores=4)

cv.ridge <- cv.glmnet(x, y, family='binomial', alpha=0, parallel=TRUE, standardize=TRUE)

w3 <- 1/abs(matrix(coef(cv.ridge, s=cv.ridge$lambda.min)
                   [, 1][2:(ncol(x)+1)] ))^1 ## Using gamma = 1

w3[w3[,1] == Inf] <- 999999999 ## Replacing values estimated as Infinite for 999999999

#Adaptive lasso model
lasso.model <- cv.glmnet(x = newX_train[,-c(20)], y = newX_train[,c(20)], 
                         family = 'binomial', alpha=1,standardize=TRUE,type.measure = 'auc',penalty.factor=w3)
summary(lasso.model)
# Apply model to testing dataset
prob<- predict(lasso.model,type="response", 
                           newx = newX_test[,-c(21)], s = 'lambda.min')
pred <- prediction(prob, test_new$Churn)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")                            # shows calculated AUC for model
testing_ROC6<-plot(perf,colorize=FALSE, col="black")    # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

#calculate confusion matrix
pred_DGLM<-prob
yhat<-as.numeric((pred_DGLM>0.5))
yhat
as.numeric(test_new$Churn)-1
classification_matrix2<- table(as.numeric(test_new$Churn)-1, yhat)
classification_matrix2
library(caret)
confusionMatrix(classification_matrix2,positive="1")

#calculate F-score
library(ROSE)
test_new$Churn
accuracy.meas(as.numeric(test_new$Churn)-1, pred_DGLM)  #Low F-score suggests weak accuracy of this model


library(lift)
TopDecileLift(predicted=yhat,labels=as.numeric(test_new$Churn))


#############################################################################################################


data5_new = data.frame(x=perf@x.values,y=perf@y.values)
colnames(data5_new) <- c("x", "y")
#data1_new= data.frame(x=testing_ROC[["data"]]$x,y=testing_ROC[["data"]]$y)
data2_new = data.frame(x=testing_ROC1[["data"]]$x,y=testing_ROC1[["data"]]$y)
data3_new = data.frame(x=testing_ROC2[["data"]]$x,y=testing_ROC2[["data"]]$y)
data4_new = data.frame(x=testing_ROC3[["data"]]$x,y=testing_ROC3[["data"]]$y)

#color<-c("GLMDT"="orange","J48"="red","LLM"="blue","LR"="green4")
ggplot(data5_new,aes(x,y))  +geom_line(data=data2_new,aes(color='J48'),size = 2)+geom_line(data=data3_new,aes(colour='LLM'),size = 2)+
  geom_line(data=data4_new,aes(color='LR'),size = 2)+geom_line(data=data5_new,aes(color='GLMDT2'),size = 2)+
  ggtitle("ROC Curves for each model on the testing data")+
  theme(plot.title=element_text(color="black",size=18,face="bold"),)+
  xlim(0, 1) + xlab("1-Specificity") +ylab("Sensitivity")+
  scale_color_manual(name='model',breaks=c('J48','LLM','LR','PLTR'),values=c("J48"="red","LLM"="blue","LR"="green4","PLTR"="brown"))+
  geom_abline(intercept = 0, slope = 1, size = 0.5)
