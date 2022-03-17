### Predicting test scores according to sleep hours using Supervised ML (linear regression)

# Set up the work directory and read the file
setwd(choose.dir())
df = read.csv('student_scores.csv')

# Check its dimension, type of variables and number of observations
str(df)

# Check for empty rows
df[df=='NA']

# Plot the table to visualize it
library(ggplot2)
ggplot() +
  geom_point(aes(x = df$Hours, y = df$Score), colour = 'red') +
  ggtitle('X vs Y (Training set)') +
  xlab('No_SalesMan') +
  ylab('Revenue')
# At first glance, the relationship between the variables are positively correlated

# Splitting the dataset
library(caTools)
set.seed(19800)
split = sample.split(df$Score, SplitRatio = 0.7)
training_set = subset(df, split == T)
test_set = subset(df, split = F)

# Feature scaling is not necessary due to having 2 variables only.
# Fitting the linear regression to the training set
regressor = lm(formula = Scores ~ ., data = training_set)

# The intercepts are as follows
regressor

# Predicting the test set results
y_predict = predict(regressor, newdata = test_set)
summary(y_predict)

# Visualize the predicted scores vs the actual scores
# The line shows the predicted values
ggplot() +
  geom_point(aes(x = df$Hours, y = df$Score),
             colour = 'red') +
  geom_line(aes(x = test_set$Hours, y = predict(regressor, newdata = df)),
            colour = 'blue') +
  ggtitle('Sleep Hours vs Score') +
  xlab('Hours') +
  ylab('Score')

# How much is a student expected to score if they slept for 9.25 hours?
predict(regressor, data.frame(Hours = 9.25))
# A score of roughly 92 is predicted by the algorithm

# Parameters to validate the accuracy of the model and improvise.
library(MLmetrics)
MAPE(y_predict,test_set$Score)
RMSE(y_predict,test_set$Score)

# The MAPE is found to be 11.6%. For an accurate model a MAPE of 5% or less
# is necessary. On the other hand, over 25% is considered to have low accuracy.
# In this case, the prediction is accepted since feature scaling is not possible
# and any further changes would result in minuscule differences.
