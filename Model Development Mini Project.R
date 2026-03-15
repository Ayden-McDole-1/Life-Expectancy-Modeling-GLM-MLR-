library(readr)
library(MASS)
library(car)
library(lmtest)
## Load in Data
data <- read.csv("C:\\Users\\abmcd\\OneDrive\\Desktop\\Career Stuff\\Mini Project\\Life Expectancy Data Clean.csv")

## Convert Status to Categorical Variable
data$Status <- factor(data$Status, levels = c("Developing", "Developed"), labels = c(0, 1))
str(data)


## Making Training Data and Test Data
set.seed(123)
training_indeces <- sample(x = seq_len(nrow(data)), size = floor(0.75 * nrow(data)))
training_data <- data[training_indeces, ]
test_data <- data[-training_indeces, ]
dim(training_data)
dim(test_data)

## Defining Regression Variables:
y <- training_data$Life_Expectancy
x1 <- training_data$Status
x2 <- training_data$Adult_Mortality
x3 <- training_data$Infant_Deaths
x4 <- training_data$Alcohol
x5 <- training_data$Percentage_Expenditure
x6 <- training_data$Hepatitis_B
x7 <- training_data$Measles
x8 <- training_data$BMI
x9 <- training_data$Under_Five_Deaths
x10 <- training_data$Polio
x11 <- training_data$Total_Expenditure
x12 <- training_data$Diphtheria
x13 <- training_data$HIV.AIDS
x14 <- training_data$GDP
x15 <- training_data$Population
x16 <- training_data$Thinness_Ages_10_to_19
x17 <- training_data$Thinness_Ages_5_to_9
x18 <- training_data$Income_Composition_of_Resources
x19 <- training_data$Schooling


## Forward Stepwise Regression
null_model <- lm(y~1, data = training_data)
full_model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19, data = training_data)

forward_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model), 
                         direction = "forward")

summary(forward_model)

best_forward_model <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x13+x17+x18+x19, data = training_data)


##Backward Stepwise Model
null_model <- lm(y~1, data = training_data)
full_model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19, data = training_data)

backward_model <- stepAIC(full_model, scope = list(lower = null_model, upper = full_model), 
                         direction = "backward")

summary(backward_model)

best_backward_model <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x13+x17+x18+x19, data=training_data)

##Compare Best Forward and Backward Models:
summary(best_forward_model)
summary(best_backward_model)

#They are the same model which is good that they agree

#I don't necessarily like this, but x1 would be rejected at alpha = 0.05 so we should take
#it out of the model. Having developed vs nondeveloped as a predictor seems important but
#it should be dropped

best_model <- lm(y~x2+x3+x4+x5+x8+x9+x10+x13+x17+x18+x19, data=training_data)
summary(best_model)

#This affects other predictors now as x4 should be dropped. I am going to leave x1 in as
#this could be a result of the training data. I also used all the data and did the same process
#and x1 was in that model. 

best_model <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x13+x17+x18+x19, data=training_data)
summary(best_model)

##Model Diagnostics

#Multicolinearity using VIF
model_vif <- vif(best_model)
print(model_vif)

#It seems like x3 and x9 have very very high VIFs (around 220)
#This makes sense because x3 is infant deaths and x9 is deaths of children under the age of 5
#To combat this we will just use x9 since it is the broader variable
vif_adjusted_model <- lm(y~x1+x2+x4+x5+x8+x9+x10+x13+x17+x18+x19, data=training_data)
new_model_vif <- vif(vif_adjusted_model)
print(new_model_vif)
#This fixed the multicolinearity problem, let's check the summary to make sure it's still a good model
summary(best_model)
summary(vif_adjusted_model)
#R^2 went down a little bit, x1 still not signifitcant and x17 is really not significant now
#Let's look at the model without x1 and x17
summary(lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data))
#This model has all signifcant predictors and R^2 is pretty much the same
#Double check it has no multicolinearity problems
print(vif(lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data)))

##All good, we will make this our new working model
best_model <- lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data)


##Heteroscedasticity
#We can use Breusch Pagan to see if heteroscedasticity is present

bptest(best_model)

#Heteroscedasticity is present, use box cox to see if transformation works
boxcox(best_model)
#We get roughly 1.3 for a value of lambda

hs_adjusted_model <- lm(y^1.3~x1+x2+x4+x5+x8+x9+x10+x12+x13+x18+x19, data = training_data)
summary(hs_adjusted_model)
bptest(hs_adjusted_model)

##This made the bp test statistic decrease slightly but heteroscedasticity is still present

#Let's look at the plot of residuals
plot(fitted(hs_adjusted_model), resid(hs_adjusted_model),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0)

#There doesn't seem to be much pattern to the residuals here, they look approximately normal
#Let's look at our original best_model we started with:
plot(fitted(best_model), resid(best_model),
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0)

#Residuals also appear normal but bp-test says they're not

#Let's look at individual variables and see if those have patterns
summary(best_model)
plot(best_model$model$x2, resid(best_model), main = "x2")
plot(best_model$model$x4, resid(best_model), main = "x4")
plot(best_model$model$x5, resid(best_model), main = "x5")
plot(best_model$model$x8, resid(best_model), main = "x8")
plot(best_model$model$x9, resid(best_model), main = "x9")
plot(best_model$model$x10, resid(best_model), main = "x10")
plot(best_model$model$x13, resid(best_model), main = "x13")
plot(best_model$model$x18, resid(best_model), main = "x18")
plot(best_model$model$x19, resid(best_model), main = "x19")

##More rigorous test to see which variables are causing the problems
res <- resid(best_model)
summary(lm(abs(res)~best_model$model$x2))
#x2 looks to be ok, the slope parameter is statistically indifferent from 0

summary(lm(abs(res)~best_model$model$x4))
#x4 looks to be ok, the slope parameter is statistically indifferent from 0

summary(lm(abs(res)~best_model$model$x5))
#x5 looks to be ok, the slope parameter is statistically indifferent from 0

summary(lm(abs(res)~best_model$model$x8))
#x8 has a nonzero negative slope parameter but it is quite low

summary(lm(abs(res)~best_model$model$x9))
#x9 looks to be ok, the slope parameter is statistically indifferent from 0

summary(lm(abs(res)~best_model$model$x10))
#x10 has a nonzero negative slope parameter but it is quite low

summary(lm(abs(res)~best_model$model$x13))
#x13 has a nonzero positive slope parameter, it is the largest so far

summary(lm(abs(res)~best_model$model$x18))
#x18 has a negative slope parameter, it is quite large but looks as if this is due to outliers

summary(lm(abs(res)~best_model$model$x19))
#x19 has a really large negative slope parameter, again looking at the plot it might be due to outliers

##Let's try using weights for x13 and seeing if that will fix our problem
wls_model <- lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data, weights = 1/best_model$model$x13)
summary(wls_model)

##This model is much worse, predictors are nonsignificant, R^2 is down
bptest(wls_model)
##Much worse heteroscedasticity here as well

##Let's base weights of fitted values instead of x13
wls_model <- lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data, weights = 1/fitted(best_model))
summary(wls_model)

bptest(wls_model)
##We now have homoscedasticity as well, so this wls model is our new working model

best_model <- lm(y~x2+x4+x5+x8+x9+x10+x13+x18+x19, data=training_data, weights = 1/fitted(best_model))


##Let's now use test data
coefs <- coef(best_model)
pred_vars <- c("Adult_Mortality", "Alcohol", "Percentage_Expenditure", "BMI", "Under_Five_Deaths", 
               "Polio", "HIV.AIDS", "Income_Composition_of_Resources", "Schooling")
X_test <- as.matrix(test_data[, pred_vars])
X_test <- cbind(Intercept = 1, X_test)

pred <- X_test %*% coefs
pred <- as.vector(pred)
length(pred) 
test_mse <- mean((test_data$Life_Expectancy - pred)^2)
print(test_mse)


results_df <- data.frame(Actual = test_data$Life_Expectancy, Predicted = pred)


##Compare Test MSE to other models

#Null Model
null_test <- lm(test_data$Life_Expectancy~1)
test_mse_null <- mean((test_data$Life_Expectancy - fitted(null_test))^2)
print(test_mse_null)



##The model I have now is great, but maybe a GLM will perform better
#Is Life_Expectancy normally distributed?
hist(data$Life_Expectancy, probability = TRUE, main = "Histogram with Normal Curve Overlay")
curve(dnorm(x, mean = mean(data$Life_Expectancy), sd = sd(data$Life_Expectancy)), add = TRUE, col = "blue", lwd = 2)
qqnorm(data$Life_Expectancy)
qqline(data$Life_Expectancy)

#Doesn't appear to be normally distributed
#Maybe a gamma distribution fits better
#MOM estimates for gamma
mu <- mean(data$Life_Expectancy)
var <- var(data$Life_Expectancy)
alpha <- mu^2/var
lambda <- mu/var

hist(data$Life_Expectancy, probability = TRUE, main = "Histogram with Gamma Curve Overlay")
curve(dgamma(x, alpha, lambda), add = TRUE, col = "blue", lwd = 2)

##Still not great but gamma will probably be the best exponential family distribution

##Forward stepwise selection for glm
#First try inverse link
null_glm <- glm(y~1, data = training_data, family = Gamma(link="inverse"))
full_glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19,
              data = training_data, family = Gamma(link = "inverse"))

forward_glm <- stepAIC(null_glm, scope = list(lower = null_glm, upper = full_glm), 
                         direction = "forward")

summary(forward_glm)

best_forward_glm <- glm(y~x2+x3+x5+x8+x9+x10+x13+x18+x19, 
                        data = training_data, family = Gamma(link = "inverse"))
#Best AIC is 6686.8

#Now let's try a log link
null_glm <- glm(y~1, data = training_data, family = Gamma(link="log"))
full_glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19,
                data = training_data, family = Gamma(link = "log"))

forward_glm <- stepAIC(null_glm, scope = list(lower = null_glm, upper = full_glm), 
                       direction = "forward")

summary(forward_glm)

#Here the best AIC is 6720.8 so we will stick with the inverse link

##Now for backward stepwise
null_glm <- glm(y~1, data = training_data, family = Gamma(link="inverse"))
full_glm <- glm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19,
                data = training_data, family = Gamma(link = "inverse"))

backward_glm <- stepAIC(full_glm, scope = list(lower = null_glm, upper = full_glm), 
                          direction = "backward")

summary(backward_glm)

#Best AIC is also 6686.8 so we pick the forward model since we get the same model

best_glm <- glm(y~x2+x3+x5+x8+x9+x10+x13+x18+x19, 
                data = training_data, family = Gamma(link = "inverse"))
summary(best_glm)

##We know multicolinearity between x3 and x9 is prominent, let's try the model without x3

new_glm <- glm(y~x2+x5+x8+x9+x10+x13+x18+x19, 
                data = training_data, family = Gamma(link = "inverse"))
summary(new_glm)

##We now have an AIC of 6788.4, might just keep with the old model with X3

##Compute Test MSE of our GLM
test_data_copy <- test_data
test_data_copy$x2 <- test_data_copy$Adult_Mortality
test_data_copy$x3 <- test_data_copy$Infant_Deaths
test_data_copy$x5 <- test_data_copy$Percentage_Expenditure
test_data_copy$x8 <- test_data_copy$BMI
test_data_copy$x9 <- test_data_copy$Under_Five_Deaths
test_data_copy$x10 <- test_data_copy$Polio
test_data_copy$x13 <- test_data_copy$HIV.AIDS
test_data_copy$x18 <- test_data_copy$Income_Composition_of_Resources
test_data_copy$x19 <- test_data_copy$Schooling

pred_glm <- predict(best_glm, newdata = test_data_copy, type = "response")
test_mse_glm <- mean((test_data_copy$Life_Expectancy - pred_glm)^2)
print(test_mse_glm)

results_glm_df <- data.frame(Actual = test_data_copy$Life_Expectancy, Predicted = pred_glm)

##Now we can compare our two models
print(test_mse)
print(test_mse_glm)

##GLM has lower test MSE so we choose that as our model

final_life_expectancy_model <- glm(y~x2+x3+x5+x8+x9+x10+x13+x18+x19, 
                                   data = training_data, family = Gamma(link = "inverse"))