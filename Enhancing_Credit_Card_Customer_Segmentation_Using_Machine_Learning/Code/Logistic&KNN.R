                          
                          #################################################
                          #         Logistics regression model            #
                          #  To predict default credit card client        #
                          #                                               #
                          #################################################


##### Importing the Library

packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ModelMetrics")
lapply(packages, library, character.only = TRUE)


#### Reading the CSV file 

rawdata <- read.csv("C:/Users/Kamal/Documents/ML/Logistic&KNN/default_credit_card.csv",header=T,stringsAsFactors = T)

#### Data exploration 

sapply(rawdata,function(x) sum(is.na(x)))
missmap(rawdata, main = "Missing values vs observed")

M <- cor(rawdata)
corrplot(M, type="lower")

print(table(rawdata$default_payment_next_month))


### Data cleaning and transformation 

# 1, Dropping the ID column as it is unneccessary to our modelling 

rawdata <- subset(rawdata, select = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))

# 2, pay_0 column name has been changed to pay_1 for naming convenience

names(rawdata)[names(rawdata) == "PAY_0"] <- "PAY_1"

# 3, casting the categorical columns to factors 

rawdata$SEX=as.factor(rawdata$SEX)
rawdata$EDUCATION=as.factor(rawdata$EDUCATION)
rawdata$MARRIAGE=as.factor(rawdata$MARRIAGE)
rawdata$default_payment_next_month=as.factor(rawdata$default_payment_next_month)

# 4 Data set is divided to 70:30 ratio

set.seed(1400)
id  <- sample(2,nrow(rawdata),prob= c(0.70,0.30),replace=T)
logisticTrain <-df1[id==1,]
logisticTest <-df1[id==2,]

#### Data modeling using logistic regression taking all the independent variable  


logisticModel<-glm(default_payment_next_month~.,family=binomial(link="logit"), data = logisticTrain)
summary(logisticModel)

### Removing the independent variable which are insignificant in predicting the dependent variable and training the model

logisticModel1<-glm(default_payment_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
                      AGE + PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 +
                      PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4,
                    family=binomial(link="logit"),data=logisticTrain)
summary(logisticModel1)

### Predicting the Model 

predicted_values <- predict(logisticModel1, type = "response", newdata = logisticTest)
logisticTest$predicted_values <- predicted_values
logisticTest$prediction <- logisticTest$predicted_values > 0.5
logisticTest$prediction <- as.numeric(logisticTest$prediction)
logisticTest$prediction <- as.factor(logisticTest$prediction)


### Evaluating the model 

caret::confusionMatrix(logisticTest$prediction, logisticTest$default_payment_next_month)



### Improving the performance of the model using - (k cross fold validation)


ctrl <- trainControl(method = "cv", number = 20, savePredictions = TRUE)

mod_fit <- train(default_payment_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
                   AGE + PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 +
                   PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4, method="glm", data = logisticTrain,family="binomial",
                 trControl = ctrl, tuneLength = 5)

predicted_values <- predict(mod_fit, type = "raw", newdata = logisticTest)


### Evaluating the improved model performance - (k cross fold validation)

caret::confusionMatrix(predicted_values, logisticTest$default_payment_next_month)

### Improving the performance of the model - (Stepwise regression model)


train.control <- trainControl(method = "cv", number = 10)

model<-glm(default_payment_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
             AGE + PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 +
             PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4,
           family=binomial(link="logit"),data=logisticTrain)

step_model <- stepAIC(model, direction = "both", trace = FALSE,trControl = train.control) 
summary(step_model)

predicted_values <- predict(step.model,type='response',newdata = logisticTest)
logisticTest$predicted_values <- predicted_values
logisticTest$prediction <- logisticTest$predicted_values > 0.5
logisticTest$prediction <- as.numeric(logisticTest$prediction)
logisticTest$prediction <- as.factor(logisticTest$prediction)

### Evaluating the model - (Stepwise regression model)

logistic <- caret::confusionMatrix(logisticTest$prediction, logisticTest$default_payment_next_month)




 

                        #################################################
                        #         K- Nearest Neighbors model            #
                        #  To predict default credit card client        #
                        #                                               #
                        #################################################


### Data preprocessing and transformation


    # 1, Normalizing the data for better result while calculaing the Euclidean distance using KNN

df1 <- subset(rawdata, select = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))
str(df1)

df1$SEX=as.factor(df1$SEX)
df1$EDUCATION=as.factor(df1$EDUCATION)
df1$MARRIAGE=as.factor(df1$MARRIAGE)
df1$default_payment_next_month=as.factor(df1$default_payment_next_month)


str(df1)
n <-sapply(df1,function(x){is.numeric(x)})
numerics<-df1[,n]

normalize <- function(x) {return((x - min(x))/(max(x) - min(x)))}
numericNormal <- normalize(numerics)

KNNData <- df1[,!n]
normalizedDataKNN <- cbind(KNNData,numericNormal)


tkNN <- dummy.data.frame(normalizedDataKNN[,-4])


normalizedData <- cbind(tkNN,normalizedDataKNN$default_payment_next_month)

   # 2, Since the dependent variables is unbalanced oversampling is done to the dataset

prop.table(table(normalizedDataKNN$default_payment_next_month)) * 100

overknn <- ovun.sample(default_payment_next_month~.,data = normalizedDataKNN,method = "over")$data 

prop.table(table(overknn$default_payment_next_month)) * 100
   # 3, casting the categorical column to factors

overknn$SEX=as.factor(overknn$SEX)
overknn$EDUCATION=as.factor(overknn$EDUCATION)
overknn$MARRIAGE=as.factor(overknn$MARRIAGE)
overknn$default_payment_next_month=as.factor(overknn$default_payment_next_month)
   
   # 4, Splittig the data into training and testing dataset

set.seed(1234)
id  <- sample(2,nrow(overknn),prob= c(0.70,0.30),replace=T)
kNNTraining <-overknn[id==1,]
kNNTesting <-overknn[id==2,]

Train_labels <- overknn[id==1,4]
Test_labels <- overknn[id==2,4]



### Training  the model by finding the best k value using caret function

control <- caret::trainControl(method="repeatedcv", repeats=3)
metric <- "Accuracy"

knnmodel <- caret::train(default_payment_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
                    AGE + PAY_0 + PAY_2 + PAY_3 + BILL_AMT1 +
                    PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4,data=kNNTraining
                  , method="knn", metric=metric, trControl=control,preProcess = c("center","scale"),tuneLength = 10)

### Predicting the model 

knnprediction <- predict(knnmodel, kNNTesting)


### Model Evaluation
knnmodels <- caret::confusionMatrix(knnprediction, Test_labels)


#-------- AUC AND ROC CURVE for the improved model for both KNN and logistics

library(ModelMetrics)
auc(Test_labels,knnprediction)
auc(logisticTest$default_payment_next_month,logisticTest$prediction)

roccurvemodellknn <- prediction(as.numeric(Test_labels),as.numeric(knnprediction))
roccurvemodellogistic <- prediction(as.numeric(logisticTest$default_payment_next_month),as.numeric(logisticTest$prediction))

perf <- performance( roccurvemodellknn, "tpr", "fpr" )
perf2 <- performance(roccurvemodellogistic, "tpr", "fpr")
plot( perf, main="AUC ROC Curve",col ="red")
plot(perf2, add = TRUE, col ="blue")

   legend("bottomright", c( "KNN ROC curve","Logistic ROC curve"), lty=5, 
         col = c("red", "blue"), bty="n", inset=c(0,0.15))
   legend("bottomright", c( "KNN - AUC = 0.67 ","Logistic - AUC = 0.72"), lty=8, 
          col = c("red", "blue"), bty="n", inset=c(0,0.50))

