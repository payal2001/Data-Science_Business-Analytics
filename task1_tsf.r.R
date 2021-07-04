# PRESENTED BY PAYAL MISHRA
# DATA SCIENCE AND BUSINESS ANALYTICS INTERN
# PREDICTION USING SUPERVISED LEARNING 
#PREDICT PERCENTAGE OF STUDENT BASED ON THE NO OF STUDY HOURS
# Then run library() function to use the library# 

library(ggplot2)

# Read the CSV file using read.csv()
url<-'https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv'
ds<-read.csv(url)
head(ds,10)

# Check for NULL values using is.null()
is.null(ds)

# Plot the variables into a scatter plot using the package ggplot2
ggplot(ds,aes(x=Hours,y=Scores))+geom_point(shape=1)

#  the above plot we can see that there is some correlatioFromn between the variables
# To confirm the Type of Correlation we add a regression line of line of best fit to our plot
ggplot(ds,aes(x=Hours,y=Scores))+geom_point(shape=1)+
  geom_smooth(formula=y~x,method='lm',colour='RED')
cor(ds$Hours,ds$Scores)
# As the correlation is 0.971 we can confirm that the variables are positively correlated

# Now we split the data into training and test sets in the ratio 80/20(%) using sample() function
# we set a seed so that we could get the same values for training and test sets
set.seed(21)

Split_data <- sample(nrow(ds),0.8*nrow(ds))
train <- ds[Split_data,]
test <- ds[-Split_data,]


# Divide the coloumns of training set as x_train(input) and y_train(output)
x_train=train$Hours 
y_train=train$Scores

# Divide the coloumns of testing set as x_test(input) and y_test(output)
x_test=test$Hours
y_test=test$Scores

# We create a linear model of training sets using 'lm()' function and fit the data in the plot
Linear_model <- lm(y_train ~ x_train)
Linear_model$fitted
plot(x_train,Linear_model$fitted)

# We create a linear model for test sets to test for the prediction
Lm=lm(y_test ~ x_test)
y_pred <- predict(Lm,data.frame(x_test))
y_pred
prediction = data.frame('Hours'= x_test, 'Predicted Marks'= y_pred)
prediction

# Compare the actual and predicted score
c_score <- data.frame('Actual Score'= y_test, 'Predicted Score'= y_pred)
c_score

# we can now obtain predicted score for input of hours
a=summary(Linear_model)
a
# From summary of linear_model we use coefficients of Estimate std. column
# so that predicted_value(y)= 2.4681+9.8627*(input_value(x)) 
hours = 9.25
own_pred = a$coefficients[1,1]+a$coefficients[2,1]*hours
paste('hours :',hours)
paste('Predicted Score :',round(own_pred, digits=2))

# Calculate the accuracy of the model
# MSE - Mean Squared Error
# RMSE - Root Mean Squared Error
Mean_squared_error= mean((y_test - y_pred)^2)   
Root_Mean_Squared_Error = sqrt(Mean_squared_error)
paste('Root Mean Squared Error :',Root_Mean_Squared_Error)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
# According to the model if a student studies 9.25 hrs/day, he/she is likely to score 93.7%


