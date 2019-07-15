## PWC 3 ##

# LOGICTIC REGRESSION
# install.packages("caTools")
# install.packages("caret")
# library(caret)
# library(caTools)
# install.packages('e1071', dependencies=TRUE)
# install.packages("ROCR")
# library(ROCR)


# STEP 1 AND 2 : loading
social_data = read.csv("Social_Network_Ads.csv")
summary(social_data)

# STEP 3 : splitting
set.seed(123) 
split = sample.split(social_data$Purchased, SplitRatio = 0.75)
training_set = subset(social_data, split == TRUE)
test_set = subset(social_data, split == FALSE)

# STEP 4 : scaling numeric columns
columns = c("User.ID", "Age", "EstimatedSalary")
training_set[columns] = lapply(training_set[columns], function(x) c(scale(x)))
test_set[columns] = lapply(test_set[columns], function(x) c(scale(x)))
# check for 0 mean and 1 std
summary(training_set)
summary(test_set)

# STEP 5 AND 6 : modeling
purchased_model = glm(Purchased ~ Age, family = "binomial", data=training_set)
# we must pass the binomial parameter to tell R that it is the logistic regression model
summary(purchased_model)

# STEP 7 : equation
# P(X) = exp(beta0 + beta1*X) / (1 + exp(beta0 + beta1*X))

# STEP 8 : Model evaluation
# Age is different from 0, so age is affecting the probability of a purchased ad

# STEP 9 : Akaike Information Criterion (from model)
AIC = 256.11 

# STEP 10 : plot
plot(training_set$Age, training_set$Purchased, xlim=c(-2,3), xlab = "Age",
     ylab = "Purchased probability")
x = seq(-2, 3, l=100)
y = exp(-(purchased_model$coefficients[1] + purchased_model$coefficients[2] * x))
y = 1 / (1 + y)
lines(x, y, col=2, lwd=2)

# STEP 11 : extra feature
purchased_model_bis = glm(Purchased ~ Age + EstimatedSalary, family = "binomial", data=training_set)
summary(purchased_model_bis)
x = seq(-2, 3, l=100)
y = exp(-(purchased_model_bis$coefficients[1] + purchased_model_bis$coefficients[2] * x))
y = 1 / (1 + y)
lines(x, y, col='green', lwd=2)


# STEP 12 AND 13 : relvance
# the predictors are still significant (different from 0) and the AIC (205.78 is lower => better model)

# STEP 14 : prediction on test set
preds = predict(purchased_model_bis, test_set[c("Age", "EstimatedSalary")], type="response")
# STEP 15 : Transform to binary
bin_preds = ifelse(preds > 0.5, 1, 0)
bin_preds

# STEP 16 AND 17 : metrics
conf = confusionMatrix(table(bin_preds, test_set$Purchased))
conf
# 17 misclassifications

# STEP 18 : ROC and AUC
pred = prediction(bin_preds, test_set$Purchased)
auc = performance(pred, "tpr", "fpr")
auc
plot(auc, col = 'green')
abline(0,1)

# STEP 19 : Comparison
preds_one = predict(purchased_model, test_set[c("Age", "EstimatedSalary")], type="response")
bin_preds_one = ifelse(preds_one > 0.5, 1, 0)
conf_one = confusionMatrix(table(bin_preds_one, test_set$Purchased))
pred_one = prediction(bin_preds_one, test_set$Purchased)
auc_one = performance(pred_one, "tpr", "fpr")
plot(auc_one, add=TRUE, col='red')
