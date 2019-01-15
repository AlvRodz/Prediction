


# ALVARO RODRIGUEZ

# LENDING CLUB - CREDIT DEFAUL - 2016 Q3


##############################################################
##############################################################
# PACKAGES ###################################################

library(dplyr)
library(DescTools)
library(ggplot2)
library(caTools)
library(MASS)
library(leaps)
library(gmodels)
library(verification)
library(ROCR)
library(e1071)
library(ROSE)
library(caret)
library(rsample)
library(glmnet)
library(AmesHousing)
library(stringr)

##############################################################
##############################################################
# LOAD  ######################################################


setwd("C:/Users/Alvaro_2/Desktop/DS MASTER/Prediccion/Practica 2/Practica 2")
loan.Q4<-read.csv("LoanStats_2016Q4.csv",skip=1,na.strings=c("","n/a"))

loan<-loan.Q4

##############################################################
##############################################################
# DATA CLEANING ##############################################


##############################################################
# Visualization

glimpse(loan)

##############################################################
# VARIABLES WITH MISSING VALUES
# Variables with more than 10% NA values

n.fil<-nrow(loan)

n.col<-ncol(loan)

variables.na<-data.frame("Nombre",0,0)
colnames(variables.na)<-c("Variable","Number","% NA")

for (i in (1:n.col)) { 
  Porcentaje<- round(sum(is.na(loan[,i])) / n.fil,3)
  variable.na<-data.frame(cbind(colnames(loan[i]),i,Porcentaje))
  colnames(variable.na)<-c("Variable","Number","% NA")
  if(Porcentaje>0.1){variables.na=rbind(variables.na,variable.na)}
}

variables.na[-1,]

# Eliminate the NA variables

eliminate<-as.numeric(variables.na$Number)
loan.clean<-loan[,-eliminate]
dim(loan.clean)

##############################################################
# OTHER MISSING VALUES

loan.clean<-na.omit(loan.clean)
dim(loan.clean)


##############################################################
##############################################################
# EDA  #######################################################

# Loan Status
Desc(loan.clean$loan_status, plotit = T, main="PORTFOLIO 2016 Q4 - Loan Status")

box_status <- ggplot(loan.clean, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) + labs(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount")  


# Grade
Desc(loan.clean$grade, plotit = T, main="PORTFOLIO 2016 Q4 - Grades")

# Interest Rate
ggplot(loan.clean , aes(x = grade , y = int_rate , fill = grade)) + 
  geom_boxplot(aes(fill=grade)) +  
  labs(y = 'Interest Rate' , x = 'Grade')

# Loan Purpose
ggplot(data=loan.clean, aes(x=reorder(purpose,loan_amnt),y=loan_amnt,fill=loan_status))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=30, hjust=1))


##############################################################
##############################################################
# PRE-ANALYSIS  ##############################################


# Variables selection
cols <- c("loan_status", "grade", "sub_grade", "open_acc","pub_rec", "dti", "delinq_2yrs",
          "inq_last_6mths", "emp_length", "annual_inc", "home_ownership",  "purpose", "addr_state",
          "loan_amnt","int_rate", "installment", "issue_d", "revol_bal", "revol_util")

loan.data <- subset(loan.clean, select = cols) 

dim(loan.data)

str(loan.data)
# LOAN STATUS: changing the variable into 0 or 1.
default<-c("Charged Off","Default","Late (31-120 days)") # 1 for default credits,otherwise,0.
loan.data$loan_status <- ifelse(loan.data$loan_status %in% default,1,0)

table(loan.data$loan_status)

#EMP_LENGTH standarise
loan_emp_length_clean<-sub("(\\d)[^0-9]+$", "\\1", loan.clean$emp_length)
loan_emp_length_clean<-gsub("< 1", "0", loan_emp_length_clean)
loan.clean$emp_length<-loan_emp_length_clean
table(loan.clean$emp_length)

#ANNUAL_INC standarise
qqnorm(loan.data$annual_inc, pch = 1,frame = FALSE)
qqline(loan.data$annual_inc, col = "steelblue") # heavy tailed distribution
loan.data$annual_inc<-cut(loan.data$annual_inc, breaks=quantile(loan.data$annual_inc, seq(0,1, length.out=11)), include.lowest=T, labels=F)
table(loan.data$annual_inc) # categorized in 10 groups.


# REVOL_UTIL from % to number
loan.data[,"revol_util"] <- as.numeric(sub("%", "",loan.data$"revol_util", fixed =TRUE))/100
class(loan.data$revol_util)
qqnorm(loan.data$revol_util, pch = 1,frame = FALSE)
qqline(loan.data$revol_util, col = "steelblue")


#INT_RATE from % to number
loan.data$int_rate <- as.numeric(sub("%","",loan.data$int_rate))
loan.data$int_rate <- loan.data$int_rate / 100
class(loan.data$int_rate) 
qqnorm(loan.data$int_rate, pch = 1,frame = FALSE)
qqline(loan.data$int_rate, col = "steelblue")


#LOAN_AMNT
qqnorm(loan.data$loan_amnt, pch = 1,frame = FALSE)
qqline(loan.data$loan_amnt, col = "steelblue")


# INSTALLMENT
qqnorm(loan.data$installment, pch = 1,frame = FALSE)
qqline(loan.data$installment, col = "steelblue")

# PURPOSE


loan.data<-subset(loan.data,loan.data$purpose !="wedding")

anyNA(loan.data)
dim(loan.data)


##############################################################
##############################################################
# MODEL  #####################################################

modelo<-loan.data

# TRAINING AND TEST SAMPLES
set.seed(1234)
n=nrow(modelo)
id_train <- sample(1:n , 0.7*n)
train.data = modelo[id_train,]
test.data = modelo[-id_train,]
dim(train.data)
dim(test.data)


# REGRESSION
regression <- glm(loan_status ~ ., family = "binomial", data = train.data)
summary(regression)


# STEP IDEAS
regfit.full=regsubsets(loan_status ~ .,train.data, method ="backward")
reg.summary=summary(regfit.full)
reg.summary


stepAIC(regression, direction="backward")


step <- stepAIC(regression, direction="both")
step$anova


# PROBABILITY PREDICT
hist(predict(regression,type="response"),main="PROBABILITY FORECAST FOR OBSERVATIONS")



##############################################################
##############################################################
# PREDICTION  ################################################

# CUT OFF 0.15

# TRAIN
table(predict(regression,type="response") > 0.2)

prob.glm1.insample <- predict(regression,type="response")
predicted.glm1.insample <- prob.glm1.insample > 0.2
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

table(train.data$loan_status, predicted.glm1.insample, dnn=c("Truth","Predicted"))

# OUT OF SAMPLE
prob.glm1.outsample <- predict(regression,test.data,type="response")
predicted.glm1.outsample <-  prob.glm1.outsample> 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(test.data$loan_status, predicted.glm1.outsample, dnn=c("Truth","Predicted"))
# Confusion matrix
confusionMatrix(as.factor(test.data$loan_status),as.factor(predicted.glm1.outsample))

# ERROR
mean(ifelse(test.data$loan_status != predicted.glm1.outsample, 1, 0))

# ROC CURVE

pred <- prediction(prob.glm1.outsample, test.data$loan_status)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))



##############################################################
##############################################################
# RE-MODEL WIT RIDGE, LASSO AND ELASTIC NET ##################


modelo

# TRAINING AND TEST SAMPLES
set.seed(123)
split <- initial_split(modelo, prop = .7, strata = "loan_status")
train <- training(split)
test  <- testing(split)
dim(train)
dim(test)


##############################################################
##############################################################
# RIDGE REGRESSION  ##########################################

# -1 for discard the intercept
train_x<-model.matrix(loan_status ~ .,train)[, -1]

# training dim
dim(train_x)

# applying

ridge<-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0)

plot(ridge, xvar = "lambda")

#LAMBDAS APPLIED AS PENALTY
ridge$lambda %>% head()


# Apply CV Ridge regression to loan data
ridgecv <- cv.glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0,nfolds=3)

# plot results
plot(ridgecv)

# RESULTS

min(ridgecv$cvm)       # MSE min
## [1] 0.06753315
ridgecv$lambda.min     # lambda for MSE Min
## [1] 0.0073962
ridgecv$cvm[ridgecv$lambda == ridgecv$lambda.1se]  # 1 st.error of min MSE
## [1] 0.6819968
ridgecv$lambda.1se  # lambda for this MSE
## [1] 0.1593463


plot(ridge, xvar = "lambda")
abline(v = log(ridgecv$lambda.1se), col = "red", lty = "dashed")


# COEFFICIENTS

coef(ridgecv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


##############################################################
##############################################################
# LASSO  #####################################################

lasso <- glmnet(x = train_x, y = train$loan_status, family = "binomial", alpha = 1)


lassocv <- cv.glmnet(x = train_x, y = train$loan_status, family = "binomial", alpha = 1, nfolds = 8)
plot(lassocv, xvar = "lambda")
abline(v = log(lassocv$lambda.1se), col = "red", lty = "dashed")
min(cv_lasso$cvm)


coef(lassocv, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)




##############################################################
##############################################################
# ELASTIC NET REGRESSION  ####################################



elastic1 <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0.75)
elastic2 <-glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha=0.25)


par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(lasso, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ridge, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")


# maintain the same folds across all models
fold_id <- sample(1:3, size = length(train$loan_status), replace=TRUE)

# Tibble for iterate with diferent alphas
tuning_grid <- tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA)


for(i in seq_along(tuning_grid$alpha)) {
  
  # fit CV model for each alpha value
  fit <- cv.glmnet(x=train_x, y = train$loan_status, family = "binomial", alpha = tuning_grid$alpha[i],type.measure = "deviance", paralle = TRUE,foldid = fold_id)
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

tuning_grid


tuning_grid %>%
  mutate(se = mse_1se - mse_min) %>%
  ggplot(aes(alpha, mse_min)) +
  geom_line(size = 2) +
  geom_ribbon(aes(ymax = mse_min + se, ymin = mse_min - se), alpha = 0.25) +
  ggtitle("MSE ± one standard error")




##############################################################
##############################################################
# # TESTING  #################################################


table(test$loan_status)

test_x<-model.matrix(loan_status ~ .,test)[, -1]

pred.lasso <- predict(lasso_2,type="response", test_x)
mean((test$loan_status - pred.lasso)^2)
hist(pred.lasso)

# CUTOFF

# 0.2

pred_cut_off <- ifelse(pred.lasso > 0.2, 1,0)
table(pred_cut_off)
hist(pred_cut_off)

confusionMatrix(as.factor(test$loan_status),as.factor(pred_cut_off) )
pred <- prediction(pred_cut_off,as.factor(test$loan_status))
perf <- performance(pred, "tpr", "fpr")
#Printing AUC Value
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
#Plotting the ROC-curve

plot(perf, colorize = TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))










