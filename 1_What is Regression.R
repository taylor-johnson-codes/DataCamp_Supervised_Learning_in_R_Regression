# Regression: Predict a numerical outcome (dependent variable) from a set of inputs (independent variables)
# variable_name <- lm(outcome ~ input/s)
# plotting: predictions on x-axis, outcomes on y axis
#   x = y line running through center of points is a "line of perfect prediction"

library(broom)
library(sigr)
library(ggplot2)

# THE DATASET IS NOT LOADED HERE SO THE CODE WON'T RUN

# unemployment is available
summary(unemployment)

# Define a formula to express female_unemployment as a function of male_unemployment
fmla <- female_unemployment ~ male_unemployment

# Print it
fmla

# Use the formula to fit a model: unemployment_model
unemployment_model <- lm(fmla, data = unemployment)

# Print unemployment_model
unemployment_model

# Call summary() on unemployment_model to get more details
summary(unemployment_model)

# Call glance() on unemployment_model to see the details in a tidier form
glance(unemployment_model)

# Call wrapFTest() on unemployment_model to see the most relevant details
wrapFTest(unemployment_model)

# newrates is available
newrates

# Predict female unemployment in the unemployment dataset
unemployment$prediction <- predict(unemployment_model)

# Make a plot to compare predictions to actual (prediction on x axis)
ggplot(unemployment, aes(x = prediction, y = female_unemployment)) + 
  geom_point() +
  geom_abline(color = "blue")

# Predict female unemployment rate when male unemployment is 5%
pred <- predict(unemployment_model, newdata = newrates)
pred


# In this exercise, you will work with the blood pressure dataset 
# (Source: https://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/frames/frame.html)

# bloodpressure is available
summary(bloodpressure)

# Create the formula and print it
fmla <- blood_pressure ~ age + weight
fmla

# Fit the model: bloodpressure_model
bloodpressure_model <- lm(fmla, data = bloodpressure)

# Print bloodpressure_model and call summary()
bloodpressure_model
summary(bloodpressure_model)

# Predict blood pressure using bloodpressure_model: prediction
bloodpressure$prediction <- predict(bloodpressure_model)

# Plot the results
ggplot(bloodpressure, aes(x = prediction, y = blood_pressure)) + 
  geom_point() +
  geom_abline(color = "blue")