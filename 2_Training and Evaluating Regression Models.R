# a residual plot plots the residuals (difference between outcomes and the prediction against the predictions)
# goal: evenly distributed plot points
# the gain curve plot is useful for sorting instances

library(ggplot2)
library(WVPlots)
library(tidyverse)  # for mpg data set
library(vtreat)

# In this exercise, you will graphically evaluate the unemployment model, that you fit to the unemployment data in the previous chapter.

# THE DATASET IS NOT LOADED HERE SO THE CODE WON'T RUN

# unemployment and unemployment_model are available
summary(unemployment)
summary(unemployment_model)

# Make predictions from the model
unemployment$predictions <- predict(unemployment_model)

# Fill in the blanks to plot predictions (on x-axis) versus the female_unemployment rates
ggplot(unemployment, aes(x = predictions, y = female_unemployment)) + 
  geom_point() + 
  geom_abline()

# Calculate residuals
unemployment$residuals <- unemployment$female_unemployment - unemployment$predictions

# Fill in the blanks to plot predictions (on x-axis) versus the residuals
ggplot(unemployment, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("residuals vs. linear model prediction")

# Plot the Gain Curve
GainCurvePlot(unemployment, "predictions", "female_unemployment", "Unemployment model")


# Root Mean Squared Error (RMSE): key metric for validating prediction performance; typical prediction error

# For convenience put the residuals in the variable res
res <- unemployment$residuals

# Calculate RMSE, assign it to the variable rmse and print it
(rmse <- sqrt(mean(res^2)))

# Calculate the standard deviation of female_unemployment and print it
(sd_unemployment <- sd(unemployment$female_unemployment))


# R-squared: another key metric for evaluating regression models
# glance() returns r-squared

# Calculate and print the mean female_unemployment: fe_mean
(fe_mean <- mean(unemployment$female_unemployment))

# Calculate and print the total sum of squares: tss
(tss <- sum( (unemployment$female_unemployment - fe_mean)^2 ))

# Calculate and print residual sum of squares: rss
(rss <- sum(unemployment$residuals^2))

# Calculate and print the R-squared: rsq
(rsq <- 1 - (rss/tss))

# Get R-squared from glance and print it
(rsq_glance <- glance(unemployment_model)$r.squared)

# Get the correlation between the prediction and true outcome: rho and print it
(rho <- cor(unemployment$predictions, unemployment$female_unemployment))

# Square rho: rho2 and print it
(rho2 <- rho ^ 2)

# Get R-squared from glance and print it
(rsq_glance <- glance(unemployment_model)$r.squared)


# when you have a lot of data, the best thing to do it split the data into two: 
# one set to train the model, one set to test the model
# when you don't have a lot of data, use cross-validation

# mpg is available with library(tidyverse)
summary(mpg)
dim(mpg)

# Use nrow to get the number of rows in mpg (N) and print it
(N <- nrow(mpg))

# Calculate how many rows 75% of N should be and print it
# Hint: use round() to get an integer
(target <- round(N * 0.75))

# Create the vector of N uniform random variables: gp
gp <- runif(N)

# Use gp to create the training set: mpg_train (75% of data) and mpg_test (25% of data)
mpg_train <- mpg[gp < 0.75, ]
mpg_test <- mpg[gp >= 0.75, ]

# Use nrow() to examine mpg_train and mpg_test
nrow(mpg_train)
nrow(mpg_test)

# Create a formula to express cty as a function of hwy: fmla and print it.
(fmla <- cty ~ hwy)

# Now use lm() to build a model mpg_model from mpg_train that predicts cty from hwy 
mpg_model <- lm(fmla, data = mpg_train)

# Use summary() to examine the model
summary(mpg_model)

# Examine the objects that have been loaded
ls.str()

# predict cty from hwy for the training set
mpg_train$pred <- predict(mpg_model)

# predict cty from hwy for the test set
mpg_test$pred <- predict(mpg_model, newdata = mpg_test)

# Evaluate the rmse on both training and test data and print them
(rmse_train <- rmse(mpg_train$pred, mpg_train$cty))
(rmse_test <- rmse(mpg_test$pred, mpg_test$cty))

# Evaluate the r-squared on both training and test data and print them
(rsq_train <- r_squared(mpg_train$pred, mpg_train$cty))
(rsq_test <- r_squared(mpg_test$pred, mpg_test$cty))

# Plot the predictions (on the x-axis) against the outcome (cty) on the test data
ggplot(mpg_test, aes(x = pred, y = cty)) + 
  geom_point() + 
  geom_abline()

# Get the number of rows in mpg
nRows <- nrow(mpg)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows, 3, NULL, NULL)

# Examine the split plan
str(splitPlan)

# Run the 3-fold cross validation plan from splitPlan
k <- 3  # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(cty ~ hwy, data = mpg[split$train, ])
  mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app, ])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))

# Get the rmse of the full model's predictions
rmse(mpg$pred, mpg$cty)

# Get the rmse of the cross-validation predictions
rmse(mpg$pred.cv, mpg$cty)