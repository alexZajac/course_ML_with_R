## PWC 1 ##

# R BASICS AND LINEAR REGRESSION

# setting current working directiory to source code file location

# Basics 1
x = c(1, 7, 3, 4)
y = 100:1
x[3] + y[4]
cos(x[3]) + sin(x[2])*exp(-y[2])

# Basics 2
fisher_distribution_90 = qf(p=0.9, df1 = 1, df2 = 5)
print(fisher_distribution_90)
fisher_distribution_95 = qf(p=0.95, df1 = 1, df2 = 5)
print(fisher_distribution_95)
fisher_distribution_99 = qf(p=0.99, df1 = 1, df2 = 5)
print(fisher_distribution_99)

x <- seq(-4, 4, length= 100)
poisson <- rpois(100, lambda = 5)
plot(x, poisson)

t1 <- pt(q = x, df=1)
plot(x, t1)
t5 <- pt(q = x, df=5)
t10 <- pt(q = x, df=10)
t50 <- pt(q = x, df=50)
t100 <- pt(q = x, df=100)
lines(x, t5, col='green')
lines(x, t10, col='blue')
lines(x, t50, col='red')
lines(x, t100, col='yellow')


# LINEAR REGRESSION
load("EU.RData")
summary(EU)
myModel <- lm(formula = CamCom2011 ~ Population2010, data = EU)
myModel

names(myModel)

myModel$coefficients
myModel$residuals
myModel$sigma

summaryMyModel <- summary(myModel)
summaryMyModel$sigma




# BOSTON HOUSE PREDICTION
library(MASS)

dim(Boston)
names(Boston)

# STEP 1
# Split the data by using the 400 first observations as the training and the remaining as the testing data
train = 1:400
test = -train

#we are only going to use two variables
variables = which(names(Boston) == c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

dim(training_data)

# STEP 2
# linearity check

plot(training_data$lstat, 
     training_data$medv, 
     main="Households with low economical status in function of median house value",
     xlab="Low economical status",
     ylab="Median Value", 
     pch=0,
     col='green',
     cex=1.2)

plot(log(training_data$lstat), training_data$medv)

# STEP 3
# linear model

model = lm(formula = medv ~ log(lstat), data=training_data)
model

summary(model)

confint(model, level = 0.95)

plot(log(training_data$lstat), training_data$medv, 
     xlab="Log transform of % of Household with low socioecomic income", 
     ylab="Median House Value",
     col='red', 
     pch = 20)

abline(model, col='blue', lwd = 3)

# Prediction

predict(model, data.frame(lstat = c(5)))
predict(model, data.frame(lstat = c(5, 10, 15), interval= "prediction"))

y = testing_data$medv

y_hat = predict(model, data.frame(lstat = testing_data$lstat))

error = y - y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE

## Alexandre Zajac, June 2019.