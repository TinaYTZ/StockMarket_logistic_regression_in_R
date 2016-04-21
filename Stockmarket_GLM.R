library(ISLR)
attach(Smarket)

?Smarket

cor(Smarket[,-9])
pairs(Smarket[,-9])

# split data into two group: training data and test data

training=(Year<2005)
testing=!training

training_data=Smarket[training,]
testing_data= Smarket[testing, ]

Direction_testing=Direction[testing]

#fit a logistic regression model using training data

stock_model= glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = training_data, family = binomial)

# use fitted model to do prediction for the testing data

model_pred=predict(stock_model, testing_data, type = "response")
model_pred_direction=rep("Down", 252)
model_pred_direction[model_pred>0.5]="up"

#create confusion matrix, and compute the misclassification rate

table(model_pred_direction, Direction_testing)

mean(model_pred_direction != Direction_testing)
