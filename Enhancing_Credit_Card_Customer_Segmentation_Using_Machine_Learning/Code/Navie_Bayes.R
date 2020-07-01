            #################################################
            #         Navie Bayes model                     #
            #     Prediction of customer who is eligible    #
            #         to get the credit card                #
            #################################################

##### ------------------Importing the Library
            
packages <- c("dummies","descr","ROSE","ggplot2", "dplyr", "MASS" ,"pROC","caret","e1071","corrplot","data.table","Amelia","arm","ROCR","ModelMetrics")
lapply(packages, library, character.only = TRUE)


#####------------------ Importing the file 

naviesampledata <- read.csv("C:/Users/Kamal/Documents/ML/Navie/navie_R.csv",header=T,stringsAsFactors = T)


#####------------------ Data Exploration

View(naviesampledata)
summary(naviesampledata)
str(naviesampledata)

par(mfrow=c(3,2))
hist(naviesampledata$est_income)
hist(naviesampledata$imp_cscore)
hist(naviesampledata$RiskScore)
naviesampledata$card_offer=as.factor(naviesampledata$card_offer)

M <- cor(naviesampledata)
corrplot(M, method = "number")

plot <- table(naviesampledata$card_offer)


barplot(plot, main="Unbalanced Data",
        xlab="Dependent variable", col=c("darkblue","red"),
        legend = rownames(plot))



####------------------ Data cleaning and Transformation

# 1, Dropping the ID column as it is unneccessary to our modelling 

naviesampledata <- subset(naviesampledata, select = c(2,3,4,5,6,7,8,9,10,11,12))

# 2, Transforming the categorical column to numeric 1 and 2


naviesampledata$ad_exp = factor(naviesampledata$ad_exp,levels = c('Y', 'N'),labels = c(1, 2))

naviesampledata$country_reg = factor(naviesampledata$country_reg ,levels = c('W', 'E'),labels = c(1, 2))

naviesampledata$demographic_slice = factor(naviesampledata$demographic_slice ,levels = c('AX03efs', 'BWEsk45','CARDIF2','DERS3w5'),labels = c(1, 2,3,4))

naviesampledata$card_offer = factor(naviesampledata$card_offer ,levels = c('TRUE', 'FALSE'),labels = c(1, 2))

# 3, selecting the independent column which is significant to our model 

naviesampledata <- subset(naviesampledata, select = c(1,2,3,4,7,8,11))

# 4, converting the  numeric column to categorical using  binning

summary(naviesampledata)

naviesampledata$RiskScore <- ifelse(naviesampledata$RiskScore >= 324 & naviesampledata$RiskScore <= 669.97, 1, 
                                          ifelse(naviesampledata$RiskScore >= 670 & naviesampledata$RiskScore <= 1005 , 2 ,NA))

naviesampledata$imp_cscore <- ifelse(naviesampledata$imp_cscore >= 500 & naviesampledata$imp_cscore <= 625, 1, 
                                           ifelse(naviesampledata$imp_cscore >= 626 & naviesampledata$imp_cscore <= 699, 2, 
                                                  ifelse(naviesampledata$imp_cscore >= 700, 3, NA)))

naviesampledata$est_income <- ifelse(naviesampledata$est_income >= 2 & naviesampledata$est_income <= 39178.22, 1, 
                                     ifelse(naviesampledata$est_income >= 39179 & naviesampledata$est_income <= 65855.82, 2, 
                                            ifelse(naviesampledata$est_income >= 65855.83& naviesampledata$est_income <=91033, 3,
                                                   ifelse(naviesampledata$est_income >= 91034, 4,NA))))
                                     
str(naviesampledata)
naviesampledata$est_income=as.factor(naviesampledata$est_income)
naviesampledata$RiskScore=as.factor(naviesampledata$RiskScore)
naviesampledata$imp_cscore=as.factor(naviesampledata$imp_cscore)


# 5, Since the data is not balanced oversampling is done

oversampdata <- ovun.sample(card_offer~.,data = naviesampledata,method = "over")$data
plots <- table(oversampdata$card_offer)
barplot(plots, main="Balanced Data",
        xlab="Dependent variable", col=c("darkblue","red"),
        legend = rownames(plots))


# 6, Data set is divided into 70 to 30 ratio

set.seed(1234)
id  <- sample(2,nrow(oversampdata),prob= c(0.75,0.35),replace=T)
navietrain <-oversampdata[id==1,]
navietest <-oversampdata[id==2,]

x = navietrain[,-1:-3]
x = x[,-4]
y = navietrain$card_offer


###-------------------- Traning the model using navie bayes

model = train(x,y,'nb',trControl=trainControl(method='cv',number=10),laplace=1)

###--------------------- Predicting the model

Predict <- predict(model,newdata = navietest )

###--------------------- Evalution   ---------------------- Without Binning 

without_binning <- caret::confusionMatrix(navietest$card_offer,Predict)



####-------------------- Improving the model performance for Navie Bayes
  
  
naviesampledata1 <- read.csv("C:/Users/Kamal/Documents/ML/Navie/navie_R.csv",header=T,stringsAsFactors = T)
str(naviesampledata1)  
naviesampledata1$card_offer=as.factor(naviesampledata1$card_offer)

naviesampledata1$card_offer = factor(naviesampledata1$card_offer ,levels = c('TRUE', 'FALSE'),labels = c(1, 2))
naviesampledata1 <- subset(naviesampledata1, select = c(2,3,4,5,6,7,8,9,10,11,12))

oversampdata <- ovun.sample(card_offer~.,data = naviesampledata1,method = "over")$data


set.seed(1234)
id  <- sample(2,nrow(oversampdata),prob= c(0.70,0.30),replace=T)
navietrain <-oversampdata[id==1,]
navietest <-oversampdata[id==2,]

x = navietrain[,-11]
y = navietrain$card_offer


model = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

Predicts <- predict(model,newdata = navietest )

######---------------------------------------------------With Binning 

with_binning <- caret::confusionMatrix(navietest$card_offer,Predicts)

# ROC & AUC CURVE for the improved model


auc(navietest$card_offer,Predict)


roccurvemodel <- prediction(as.numeric(navietest$card_offer),as.numeric(Predict))
roccurveplot <- performance(roccurvemodel,measure="tpr",x.measure="fpr")
plot(roccurveplot,main="Navie bayes-ROC Curve",xaxis.col='green', xaxis.col.axis="blue", yaxis.col='green', yaxis.col.axis="blue",colorize=TRUE)

legend("bottomright", c( "AUC - 0.943"), lty=5, bty="n", inset=c(0,0.50))




