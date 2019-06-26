## PWC 2 ##

# MULTIVARIATE LINEAR REGRESSION

# STEP 1 : Load dataset
library(MASS)
library(corrplot)

dim(Boston)
names(Boston)

# STEP 2 : Split the data by using the 75% first observations as the training data and the remaining as the testing data
train = 1:(0.75*nrow(Boston))
test = -train

# STEP 3 : Linearity check between medv and age
cor(Boston$medv, Boston$age)

# slight negative correlation

# STEP 4 : Fit linear model between medv and age, with plots
variables = which(names(Boston) == c("age", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

model <- lm(formula = medv ~ age, data = training_data)

plot(training_data$age, 
     training_data$medv, 
     main="Households with age in function of median house value",
     xlab="Age",
     ylab="Median Value", 
     pch=3,
     col='blue',
     cex=1.2)
abline(model, col='red')


# STEP 5 : Fit linear model with lstat and age as predictors
varNames = c("lstat", "medv", "age")
training_data = Boston[train, varNames]
testing_data = Boston[test, varNames]

model <- lm(formula = medv ~ age + log(lstat), data = training_data)
# multivariate regression model

# STEP 6 : Summary of the model
summary(model)
anova(model)

# STEP 7 AND 8 : Interpretation
# The predictors are quite significant given their p-value (age contributiong less than log(lstat))
# F stat is not close to zerro, thus we can reject the null hypothesis 
# R²_adj is 0.63, which is okay, but the model is not ideal

# STEP 9 : Train with all variables
training_data = Boston[train, 1:length(Boston)]
testing_data = Boston[test, 1:length(Boston)]

model <- lm(formula = medv ~ ., data = training_data)
# R²_adj is 0.7197

# STEP 10 : Train with all variables (log(lstat))
training_data = Boston[train, 1:length(Boston)]
testing_data = Boston[test, 1:length(Boston)]

model <- lm(formula = medv ~ ((. - lstat) + log(lstat)), data = training_data)
summary(model)

# STEP 11 : R² comparison
# R²_adj is 0.771 => it improved

# STEP 12 : Correlation matrix
corMatrix <- round(cor(Boston), 2)

# STEP 13 : Corrplot visualization
corrplot(corMatrix, method="square")

# STEP 14 : Cor between tax and rad
corTaxRad <- corMatrix["tax", "rad"]
# 0.91

# STEP 15 : Model without tax
training_data = Boston[train, which(names(Boston) != c("tax"))]
testing_data = Boston[test, which(names(Boston) != c("tax"))]

model <- lm(formula = medv ~ ((. - lstat) + log(lstat)), data = training_data)
summary(model)
# R goes a little lower, but F-stat goes from 98.27 to 102.7 => our model is better without tax

# STEP 16 : MSE
y = testing_data$medv
y_hat = predict(model, data.frame(testing_data))

error = y - y_hat
error_squared = error^2
MSE = mean(error_squared)

# STEP 17 : Exploring charles river variables
str(Boston$chas) # vector of logical ints (0 or 1)
length(Boston$chas[Boston$chas == 1]) / length(Boston$chas)
# around 7% live bounded to the river

# STEP 18 : Boxplot of medv with chas
boxplot(Boston$medv ~ Boston$chas, 
        col=c("blue","red"),
        names=c("Not bounding", "Bounds to river"),
        ylab="Medv",
        main="Media house value with respect to chas"
)
# Medv is higher when house is bounding the river

# STEP 19 : Aggregating means
means <- aggregate(x=Boston$medv, by=list(Boston$chas), FUN=mean)

# STEP 20 : Variance explanation
fit <- aov(Boston$medv ~ Boston$chas, data=Boston)
summary(fit)
# F is not small thus we can reject the null hypothesis and there is a correlation between chas and medv

# STEP 21 and 22 : Model with qualitative predcitors
training_data = Boston[train, 1:length(Boston)]
testing_data = Boston[test, 1:length(Boston)]

model <- lm(formula = medv ~ crim + chas, data = training_data)
summary(model)
# chas adds more information for explaining the house price than crime rate 
# he's much less significant in presence of other predictors (p-value increased)

# STEP 23 : Interaction terms
model <- lm(formula = medv ~ lstat * age, data = training_data)
summary(model)
# interaction term is small but not as significant as lstat

# STEP 24 : All interaction terms model
names(Boston)
model <- lm(formula = medv ~ (.)^2, data = training_data)
summary(model)

## Alexandre Zajac, June 2019.