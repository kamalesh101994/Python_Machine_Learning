                 #################################################
                 #           Decision Tree model                 #
                 #     Prediction of customer who accepeted the  #
                 #           credit card offer                   # 
                 #################################################



##### ------------------Importing the Library
                 
packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ROCR","ModelMetrics")
lapply(packages, library, character.only = TRUE)
                 
                 
#####------------------ Importing the file 
                 
treeRawdata <- read.csv("C:/Users/Kamal/Documents/ML/Forest&Tree/creditcardmarketing.csv",header=T,stringsAsFactors = T)
str(treeRawdata)                 

#####------------------ Data Exploration

View(treeRawdata)
summary(treeRawdata)
str(treeRawdata)

par(mfrow=c(3,2))
hist(treeRawdata$Average.Balance)
hist(treeRawdata$Q1.Balance)
hist(treeRawdata$Q2.Balance)
hist(treeRawdata$Q3.Balance)
hist(treeRawdata$Q4.Balance)


####------------------ Data cleaning and Transformation

# 1, Dropping the ID column as it is unneccessary to our modelling 

#treeRawdata <- subset(treeRawdata, select = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))

# Feature selected column using Variable importnace plot

treeRawdata <- subset(treeRawdata, select = c(3,7,12,13,14,15,16,17))

# 2, Transforming the categorical column to numeric 


treeRawdata$Offer.Accepted = factor(treeRawdata$Offer.Accepted,levels = c('Yes', 'No'),labels = c(1, 2))

treeRawdata$Own.Your.Home = factor(treeRawdata$Own.Your.Home ,levels = c('Yes', 'No'),labels = c(1, 2))

treeRawdata$Credit.Rating = factor(treeRawdata$Credit.Rating ,levels = c('Low', 'Medium','High'),labels = c(1, 2,3))

treeRawdata$Overdraft.Protection = factor(treeRawdata$Overdraft.Protection ,levels = c('Yes', 'No'),labels = c(1, 2))

treeRawdata$Reward = factor(treeRawdata$Reward ,levels = c('Air Miles', 'Cash Back','Points'),labels = c(1, 2,3))

treeRawdata$Mailer.Type = factor(treeRawdata$Mailer.Type ,levels = c('Letter', 'Postcard'),labels = c(1, 2))

treeRawdata$Income.Level = factor(treeRawdata$Income.Level ,levels = c('Low', 'Medium','High'),labels = c(1, 2,3))

# 3, casting the categorical column from numeric to factors


treeRawdata$Credit.Cards.Held=as.factor(treeRawdata$Credit.Cards.Held)
treeRawdata$Homes.Owned=as.factor(treeRawdata$Homes.Owned)
treeRawdata$Household.Size=as.factor(treeRawdata$Household.Size)
treeRawdata$Bank.Accounts.Open=as.factor(treeRawdata$Bank.Accounts.Open)

# 4, Since the dependent variable is imbalanced oversampling is performed on the dataset

oversamplingtree <- ovun.sample(Offer.Accepted ~.,data = treeRawdata,method = "over") $data
str(oversamplingtree)

# 5, Splitting the data to training and testing dataset 

set.seed(1234)
id  <- sample(2,nrow(oversamplingtree),prob= c(0.70,0.30),replace=T)
treetrain <-oversamplingtree[id==1,]
treetest <-oversamplingtree[id==2,]

###-------------------- Traning the model using Decicion tree



tree_model <- C5.0(treetrain[-8], treetrain$Offer.Accepted)


###--------------------- Predicting the model

tree_pred <- predict(tree_model, newdata = treetest,type = "class")

###--------------------- Evalution 

caret::confusionMatrix(tree_pred, treetest$Offer.Accepted)

### ----------------- Performace tuning using K - CROSS FOLD and feature selction 

control <- trainControl(method="repeatedcv", number=10, repeats=4)

featureselection <- train(Offer.Accepted ~ Mailer.Type+Credit.Rating+Average.Balance+Q1.Balance+Q2.Balance+Q3.Balance+Q4.Balance, data=treetrain, method="C5.0")

tree_pred <- predict(featureselection, newdata = treetest,type = "class")

decision_tree <- caret::confusionMatrix(tree_pred, treetest$Offer.Accepted)


               #################################################
               #           Random forest model                 #
               #     Prediction of customer who accepeted the  #
               #           credit card offer                   # 
               #################################################

###------ Training the Model using all the independent variable

library(randomForest)
forest <- randomForest(treetrain[-8], treetrain$Offer.Accepted, importance=TRUE, ntree=1000,mtry = 6)

###------ predicting the Model

###trees above 1000 is decresing below 0.98 and 1000 trees is the saturation point

forestPrediction <- predict(forest, treetest)



###------ Evaluation Method

caret::confusionMatrix(forestPrediction, treetest$Offer.Accepted)


###------ Improving the model performance using Variable Importance plot

varImpPlot(forest)

###----- Training the model using Feature selection

library(randomForest)
forest <- randomForest(Offer.Accepted ~ Mailer.Type+Credit.Rating+Average.Balance+Q1.Balance+Q2.Balance+Q3.Balance+Q4.Balance, importance=TRUE, ntree=1000,mtry = 6)

forestPrediction <- predict(forest, treetest)

forest_model <- caret::confusionMatrix(forestPrediction, treetest$Offer.Accepted)

#-------- AUC AND ROC CURVE for the improved model for both Decision tree  and Random forest

library(ModelMetrics)
auc(tree_pred,treetest$Offer.Accepted)
auc(treetest$Offer.Accepted,forestPrediction)

roctreemodel <- prediction(as.numeric(treetest$Offer.Accepted),as.numeric(tree_pred))
rocforestmodellogistic <- prediction(as.numeric(treetest$Offer.Accepted),as.numeric(forestPrediction))

perf <- performance( roctreemodel, "tpr", "fpr" )
perf2 <- performance(rocforestmodellogistic, "tpr", "fpr")
plot( perf, main="AUC ROC Curve",col ="red")
plot(perf2, add = TRUE, col ="blue")

legend("bottomright", c( "Decision tree ROC curve","Random forest ROC curve"), lty=5, 
       col = c("red", "blue"), bty="n", inset=c(0,0.15))
legend("bottomright", c( "Decision tree - AUC = 0.93 ","Random forest - AUC = 0.98"), lty=8, 
       col = c("red", "blue"), bty="n", inset=c(0,0.50))






