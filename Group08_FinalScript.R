
###################################################DATA TRANFORMATION###############################################
str(S1)
S1$ï..Country <- NULL
S1$Number.of.tourists.per.year <- log10(S1$Number.of.tourists.per.year)
S1$Urban.Population <- log10(S1$Urban.Population)
S1$Per.Capita.Income <- log10(S1$Per.Capita.Income)
S1$Labour.Force <- log10(S1$Labour.Force)
#################################################DATA EXPLORATION###############################################################

corrplot::corrplot(cor(S1), method = "number")
plot(S1$Co2.Emission ~ S1$Per.Capita.Income , ylab = "Carbon Emission", xlab = "Real GDP per Capita")
plot(S1$Co2.Emission ~ S1$Total.Energy.Consumption , ylab = "Carbon Emission", xlab = "Energy Consumption")
plot(S1$Co2.Emission ~ S1$Urban.Population, ylab = "Carbon Emission", xlab = "Urban Population")
plot(S1$Co2.Emission ~ S1$Coal.and.Lignite.per.person, ylab = "Carbon Emission", xlab = "Coal consumption per person in tonnes")
plot(S1$Co2.Emission ~ S1$Year, ylab = "Carbon Emission", xlab = "Year")

######################################################################NULL MODEL#######################################
set.seed(310)
train_idx <- sample(1:nrow(S1), size =0.8*nrow(S1))
train.data2 <- S1[train_idx,]
test.data2 <- S1[-train_idx,]

NULL.model <- sqrt(mean(test.data2$Co2.Emission - mean(train.data2$Co2.Emission))^2) #Null RMSE
print(NULL.model)
##########################################################################ridge and lasso##########################
library(ElemStatLearn)
library(glmnet)
library(ISLR)
library(MASS)
library(leaps)

set.seed(123)



train.X <- as.matrix(train.data2[,-2])
train.Y <- as.matrix(train.data2[,2])
test.X <- as.matrix(test.data2[,-2])
test.Y <- as.matrix(test.data2[,2])
str(train.X)

# Ridge Regression
set.seed(123)

cv <- cv.glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0)
cv$lambda.min

ridge.fit <- glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0, lambda = cv$lambda.min)
#ridge.fit <- glmnet(x=train.X, y=train.Y, family = 'gaussian', alpha = 0)
ridge.pred <- predict(ridge.fit, newx=test.X)
ridge.mse <- mean((ridge.pred - test.Y)^2)
print(ridge.mse)

# Lasso Regression

cv2 <- cv.glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 1)
cv2$lambda.min

lasso.fit <- glmnet(x=train.X, y=train.Y, family='gaussian', alpha = 1, lambda = cv2$lambda.min)
lasso.pred <- predict(lasso.fit, newx=test.X)
lasso.mse <- mean((lasso.pred - test.Y)^2)
# Best Subset

#regfit.full=regsubsets(train.data2$Co2.Emission ~., data = train.data2, method = "backward")
regfit.full=regsubsets(train.data2$Co2.Emission ~., data = train.data2, method = "backward")

plot(summary(regfit.full)$cp)

num_var <- which.min(summary(regfit.full)$cp)
feat.cp <- which(summary(regfit.full)$which[num_var,] == TRUE)
#subset.mod <- lm(train.data2$Co2.Emission ~ ., data=train.data2[,feat.cp])
subset.mod <- lm(train.data2$Co2.Emission ~ ., data=train.data2[,feat.cp])

subset.pred <- predict(subset.mod, newdata=test.data2[,feat.cp])
subset.mse <- mean((subset.pred - test.Y)^2)

ridge.mse
ridge.rmse = sqrt(ridge.mse)
print(ridge.rmse)

lasso.mse
lasso.rmse = sqrt(lasso.mse)
print(lasso.rmse)

subset.mse
subset.rmse = sqrt(subset.mse)
print(subset.rmse)
######################################################GAM################################################################
install.packages("mgcv")

library(mgcv)

set.seed(123)




S1_GAM_Smooth<-gam(Co2.Emission ~ s(Per.Capita.Income, bs="cr") + s(Year, bs="cr") 
                   + s(Labour.Force, bs="cr") + s(Literacy.Rate, bs="cr")
                   +s(Urban.Population.Percentage, bs="cr") + s(Urban.Population)
                   + s(Number.of.tourists.per.year, bs="cr") + s(Oil.Product.Per.person, bs="cr")
                   + s(Natural.Gas.per.person, bs="cr")
                   + s(Coal.and.Lignite.per.person, bs="cr") + s(Total.Energy.Consumption, bs="cr"), data=train.data2)


par(mfrow=c(2,2))

gam.check(S1_GAM_Smooth)

summary(S1_GAM_Smooth)

layout(matrix(c(1:1),1,1,byrow=TRUE))

residuals.gam <- c()

gam.predict <- predict(S1_GAM_Smooth, newdata = test.data2, type="response")

summary(gam.predict)

residuals.gam <- (test.data2$Co2.Emission-gam.predict)

plot(test.data2$Co2.Emission, gam.predict, pch="o", col='black',lty=5,  main="GAM: Actual vs. Predicted",
     xlab = "Actual Co2", ylab="Predicted Co2")

abline(0,1)

gam.insample.RMSE = sqrt(mean((test.data2$Co2.Emission-gam.predict)^2))
gam.insample.RMSE
gam.insample.MAE = mean(abs(test.data2$Co2.Emission-gam.predict))
gam.insample.MAE 


########-----Without splines

S1_GAM_Smooth2<-gam(Co2.Emission~ Per.Capita.Income + Year 
                    + Labour.Force + Literacy.Rate
                    + Urban.Population.Percentage + Urban.Population
                    + Number.of.tourists.per.year + Oil.Product.Per.person
                    + Natural.Gas.per.person
                    + Coal.and.Lignite.per.person + Total.Energy.Consumption, data=train.data2)

par(mfrow=c(2,2))

gam.check(S1_GAM_Smooth2)


summary(S1_GAM_Smooth2)

layout(matrix(c(1:1),1,1,byrow=TRUE))

residuals.gam2 <- c()

gam.predict2 <- predict(S1_GAM_Smooth2, newdata = test.data2, type="response")

summary(gam.predict2)

residuals.gam2 <- (test.data2$Co2.Emission-gam.predict2)

plot(test.data2$Co2.Emission, gam.predict2, pch="o", col='black',lty=5,  main="GAM: Actual vs. Predicted",
     xlab = "Actual Co2", ylab="Predicted Co2")

abline(0,1)

gam2.insample.RMSE = sqrt(mean((test.data2$Co2.Emission-gam.predict2)^2))
gam2.insample.RMSE
gam2.insample.MAE = mean(abs(test.data2$Co2.Emission-gam.predict2))
gam2.insample.MAE 

#######Significant values only

S1_GAM_Smooth_3<-gam(Co2.Emission ~ Per.Capita.Income + Year
                     + s(Labour.Force, bs="cr") + s(Literacy.Rate, bs="cr")
                     +s(Urban.Population.Percentage, bs="cr") + s(Urban.Population)
                     + s(Number.of.tourists.per.year, bs="cr") + s(Oil.Product.Per.person, bs="cr")
                     + s(Natural.Gas.per.person)
                     + Coal.and.Lignite.per.person + Total.Energy.Consumption , data=train.data2)

par(mfrow=c(2,2))

gam.check(S1_GAM_Smooth_3)

summary(S1_GAM_Smooth_3)

layout(matrix(c(1:1),1,1,byrow=TRUE))

residuals.gam3 <- c()

gam.predict3 <- predict(S1_GAM_Smooth_3, newdata = test.data2, type="response")

summary(gam.predict3)

residuals.gam3 <- (test.data2$Co2.Emission-gam.predict3)

plot(test.data2$Co2.Emission, gam.predict3, pch="o", col='black',lty=5,  main="GAM: Actual vs. Predicted",
     xlab = "Actual Co2", ylab="Predicted Co2")

abline(0,1)

gam3.insample.RMSE = sqrt(mean((test.data2$Co2.Emission-gam.predict3)^2))
gam3.insample.RMSE
gam3.insample.MAE = mean(abs(test.data2$Co2.Emission-gam.predict3))
gam3.insample.MAE 


###Cross Validation K-Fold###

install.packages('caret')

library('caret')
train.control.gam<-trainControl(method="cv", number = 5)

cv_gam<-train(Co2.Emission ~ ., data=train.data2, method="gam",trControl=train.control.gam)

print(cv_gam)
gam.predict4 <- predict(cv_gam, newdata = test.data2, type = "raw")
gam4.insample.RMSE = sqrt(mean((gam.predict4 - test.data2$Co2.Emission)^2))

print(gam3.insample.RMSE)

save(list = ls(all=TRUE), file = "GAM Model.RData")
##########################################################Linear Model#####################################################################

set.seed(123)


lm.fit=lm(Co2.Emission~.,data=train.data2)# Fitting the model

summary(lm.fit)

par(mfrow=c(2,2))

plot(lm.fit)

lm.pred <- predict(lm.fit, newdata = test.data2)

lm.mse <- mean((lm.pred - test.data2[,2])^2)
print(lm.mse)

lm.rmse <-sqrt(lm.mse)

print(sqrt(lm.mse))


par(mfrow=c(1,1))

plot(test.data2$Co2.Emission, lm.pred)

abline(0,1)

######Predicting based on significant predictors only
lm.fitnew=lm(Co2.Emission~ Year + Labour.Force 
             + Natural.Gas.per.person + Oil.Product.Per.person
             + Coal.and.Lignite.per.person + Number.of.tourists.per.year + Total.Energy.Consumption , data=train.data2)

summary(lm.fitnew)

par(mfrow=c(2,2))

plot(lm.fitnew)

lm.prednew <- predict(lm.fitnew, newdata=test.data2)

lm.mse_new <- mean((lm.prednew - test.data2[,2])^2)

lm.pred.rmse<-sqrt(lm.mse_new)
print(lm.pred.rmse)

par(mfrow=c(1,1))

plot(test.data2$Co2.Emission, lm.prednew)

abline(0,1)

###K-Fold Cross Validation Approach###


install.packages('caret')

library('caret')

set.seed(123)

train.control<-trainControl(method="cv", number=10)

cv_lm<-train(Co2.Emission~Year + Labour.Force 
             + Natural.Gas.per.person + Oil.Product.Per.person
             + Coal.and.Lignite.per.person + Number.of.tourists.per.year + Total.Energy.Consumption , data= train.data2, method="lm",
             trControl=train.control)

print(cv_lm)

pred.cv.lm <- predict(cv_lm, newdata = test.data2, type = "raw")

predcv.lm.RMSE = sqrt(mean((pred.cv.lm - test.data2$Co2.Emission)^2))
print(predcv.lm.RMSE)

 

save(list = ls(all=TRUE), file = "Linear Regression Model.RData")
################################################################MARS################################################
library(rsample)   # data splitting 
library(ggplot2)   # plotting
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
library(vip)       # variable importance
library(pdp)       # variable relationships

set.seed(123)

MARS.mod0 <- earth(S1$Co2.Emission~., data=S1, degree = 1, penalty=2, pmethod = 'cv', nfold = 10, ncross = 10)
summary(MARS.mod0)
MARS.mod0$rss
MARS.mod0$rsq
MARS.mod0$gcv
MARS.mod0$grsq

#Finding out the important values
mars.imp = evimp(MARS.mod0, trim = FALSE)
print(mars.imp)

#MARS.mod <- earth(S1$`Co2 Emission (Mt/year)`~ S1$`Total Energy Consumption(Mtoe)` + S1$`Coal and lignite domestic consumption(Mt)`, data=S1, pmethod="forward", degree = 1, penalty=4, nfold=10, ncross=10)
#MARS.mod <- earth(S1$`Co2 Emission (Mt/year)`~ S1$`GDP (USD)` + S1$`Urban Population` + S1$Year + S1$`Number of tourists per year`, data = S1, pmethod="forward", degree = 1, penalty=4, nfold=10, ncross=10)
MARS.mod <- earth(Co2.Emission~ Urban.Population + Urban.Population + Total.Energy.Consumption, data = train.data2, pmethod="forward", degree = 4, penalty=2, nfold=10, ncross=10)
summary(MARS.mod)
MARS.mod$rss
MARS.mod$rsq
MARS.mod$gcv
MARS.mod$grsq

MARS.predict <- predict(MARS.mod, test.data2)
residuals.mars <- (test.data2$Co2.Emission-MARS.predict)
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.mars, main = "MARS: Residual Plot") 

plot(test.data2$Co2.Emission, MARS.predict, pch="o", col='black',lty=5,  main="MARS: Actual vs. Predicted",
     xlab = "Actual Emission", ylab="Predicted Emission")
abline(0,1)
MARS.mse <- mean((test.data2$Co2.Emission- MARS.predict)^2)
print(MARS.mse)
MARS.rmse <- sqrt(MARS.mse)
print(MARS.rmse)


##############################################################CART#########################################################
library(tree)
set.seed(123)


train = sample(1:nrow(S1), nrow(S1)/2)

tree.S1=tree(S1$Co2.Emission ~., data=S1, subset=train)

summary(tree.S1) 

par(mfrow = c(1,1))
tree.S1=tree(S1$Co2.Emission ~., data=S1, subset=train)

plot(tree.S1)
text(tree.S1,pretty=0)


cv.S1=cv.tree(tree.S1)

plot(cv.S1$size,cv.S1$dev,type='b')# Best number of nodes


prune.S1=prune.tree(tree.S1,best=4.0)# Pruning the tree

plot(prune.S1)
text(prune.S1,pretty=0)


yhat1=predict(tree.S1,newdata=S1[-train,])
S1.test=S1[-train,"Co2.Emission"]
plot(yhat1,S1.test)
abline(0,1)
sqrt(mean((yhat1-S1.test)^2)) #RMSE

###################################################################################################################################
set.seed(123)


# Helper function for calculating RMSE

rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}
# Regular Regression Tree

##############################
# Bagging
##############################
library(randomForest)
set.seed(123)
# Checking how many trees to fit:

bag.Co2.Emission <- randomForest(Co2.Emission ~ ., data=train.data2, mtry = ncol(train.data2) - 1, importance = TRUE, ntree=400)
plot(bag.Co2.Emission, type='l', main='MSE by ntree for Bagging')
# Select ~200 trees
bag.Co2.Emission1 <- randomForest(Co2.Emission ~ ., data=train.data2, mtry = ncol(test.data2) - 1, importance = TRUE, ntree=200)
rmse_reg(bag.Co2.Emission1, test.data2, "Co2.Emission")#RMSE
predict1=predict(bag.Co2.Emission1,newdata = test.data2, type = "response")
plot(predict1,test.data2$Co2.Emission)
abline(1,1)

# Variable importance according to bagging
importance(bag.Co2.Emission1)
varImpPlot(bag.Co2.Emission1, main = "Bagging")
####################################################################################################################
##############################
# Random Forest
##############################

# We will try different values of m
set.seed(123)
rf.mse <- c()
for(i in 1:(ncol(train.data2)-1)){
  rf.Co2.Emission <- randomForest(Co2.Emission~., data=train.data2, mtry=4, importance=TRUE, ntree=400)
  rf.mse[i] <- rf.Co2.Emission$mse[400]
}
plot(rf.mse, main='Training Error by m', xlab='Number of Predictors', ylab='MSE')
# Select final model, 6 predictors per tree.
rf.S <- randomForest(Co2.Emission~., data=train.data2, mtry=6, importance=TRUE, ntree=400)
rmse_reg(rf.S, test.data2, "Co2.Emission")
# Variable importance according to bagging
importance(rf.S)
varImpPlot(rf.S, main = "Random Forest")

######### K FOLD Cross Validation

library('caret')


set.seed(123)

train.control<-trainControl(method="cv", number=10)

cv_rf<-train(Co2.Emission~. , data= train.data2, method="rf",
             trControl=train.control)

print(cv_rf)

pred.cv.rf <- predict(cv_rf, newdata = test.data2, type = "raw")

predcv.rf.RMSE = sqrt(mean((pred.cv.rf - test.data2$Co2.Emission)^2))
print(predcv.rf.RMSE)
##########################################################Boosting###########################################################
library(gbm)
set.seed(123)


# gbm function to fit boosting model.
boost.S <- gbm(Co2.Emission ~ ., distribution='gaussian', data=train.data2, n.trees=150, interaction.depth = 4, shrinkage = 0.1)

# Get variable importance and variable importance plot
summary(boost.S)

# Get partial dependence for very important variables with resepct to medv.
par(mfrow=c(1,4))
plot(boost.S, i='Total.Energy.Consumption', main = "Total Energy Consumption vs Carbon Emission")
plot(boost.S, i='Labour.Force', main = "Labour Force vs Carbon Emission")
plot(boost.S, i='Urban.Population', main ="Urban Population vs Carbon Emission")
plot(boost.S, i='Year')

# Prediction for boosting is a bit different. Need include n.trees
y.boost <- predict(boost.S, newdata=test.data2, n.trees=150)
sqrt(mean(y.boost - test.data2[,2])^2)
par(mfrow=c(1,1))
plot(y.boost,test.data2$Co2.Emission)

abline(1,1)



# Repeat process for different shrinkage parameters
shrinkage_parameters <- c(0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1)

boost.rmse <- c()
for(i in 1:length(shrinkage_parameters)){
  s.boost <-gbm(Co2.Emission ~ ., distribution='gaussian', data=train.data2, n.trees=150,interaction.depth = 4, shrinkage=shrinkage_parameters[i])
  s.pred <- predict(s.boost, newdata=test.data2, n.trees=20)
  boost.rmse[i] <- sqrt(mean(s.pred - test.data2[,'Co2.Emission'])^2)
}
plot(shrinkage_parameters,boost.rmse)
############################################K-Fold Cross Validation############################################
library('caret')

set.seed(123)

train.control<-trainControl(method="cv", number=10)

cv_gbm<-train(Co2.Emission~. , data= train.data2, method="gbm",
             trControl=train.control)

print(cv_gbm)

pred.cv.gbm <- predict(cv_gbm, newdata = test.data2, type = "raw")

predcv.gbm.RMSE = sqrt(mean((pred.cv.gbm - test.data2$Co2.Emission)^2))
print(predcv.gbm.RMSE)

###################################################################################################################


library('caret')

set.seed(123)

train.control<-trainControl(method="cv", number=10)

cv_gbm<-train(Co2.Emission~. , data= train.data2, method="rf",
              trControl=train.control)

print(cv_gbm)

pred.cv.gbm <- predict(cv_gbm, newdata = test.data2, type = "raw")

predcv.gbm.RMSE = sqrt(mean((pred.cv.gbm - test.data2$Co2.Emission)^2))
print(predcv.gbm.RMSE)


