# Using ISLR and Stock Market Data from 1990-2010
# Examining the direction of stocks


library(ISLR)
summary(Weekly)


# Correlation Plots

pairs(Weekly)
cor(Weekly[, -9])

#Generalized binomial regression, backwards selecting variables

attach(Weekly)
reg1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(reg1)


# Plotting the regressions

reg1_prob <- predict(reg1, type = "response")
reg1_prediction <- rep("Down", length(reg1_prob))
reg1_prediction[reg1_prob > 0.5] <- "Up"
table(reg1_prediction, Direction)

#Creates a training data set  and trains a predictive model
train <- (Year < 2009)
Weekly_09 <- Weekly[!train, ]
reg2 <- glm(Direction ~ Lag2, family = binomial, subset = train)
reg2_prob <- predict(reg2, Weekly_09, type = "response")
reg2_prediction <- rep("Down", length(reg2_prob))
reg2_prediction[reg2_prob > 0.5] <- "Up"
Direction_09 <- Direction[!train]
table(reg2_prediction, Direction_09)
mean(reg2_prediction == Direction_09)


library(MASS)
lda1 <- lda(Direction ~ Lag2, subset = train)
lda1_prediction <- predict(lda1, Weekly_09)
table(lda1_prediction$class, Direction_09)
mean(lda1_prediction$class == Direction_09)


qda1 <- qda(Direction ~ Lag2, subset = train)
qda1_class <- predict(qda1, Weekly_09)$class
table(qda1_class, Direction_09)


# K- Clustering regression

library(class)
trainonX <- as.matrix(Lag2[train])
testonX <- as.matrix(Lag2[!train])
train_Direction <- Direction[train]
set.seed(1)
knn1 <- knn(trainonX, testonX, train_Direction, k = 1)
table(knn1, Direction_09)
mean(knn1 == Direction_09)

# Binomial regression models for the stock market

glm1 <- glm(Direction ~ Lag2:Lag1, family = binomial, subset = train)
glm1_prob <- predict(glm1, Weekly_09, type = "response")
glm1_prediction <- rep("Down", length(glm1_prob))
glm1_prediction[glm1.prob > 0.5] <- "Up"
Direction_09 <- Direction[!train]
table(glm1_prediction, Direction_09)
mean(glm1_prediction == Direction_09)

#LDA analysis

lda2 <- lda(Direction ~ Lag2:Lag1, subset = train)
lda2_prediction <- predict(lda2, Weekly_09)
mean(lda2_prediction$class == Direction_09)

#QDA analysis

qda2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)), subset = train)
qda2_class <- predict(qda2, Weekly_09)$class
table(qda2_class, Direction_09)
mean(qda2_class == Direction_09)



knn2_pred <- knn(trainonX, testonX, train_Direction, k = 50)
table(knn2_pred, Direction_09)
mean(knn2_pred == Direction_09)



knn3_pred <- knn(trainonX, testonX, train_Direction, k = 100)
table(knn3_pred, Direction_09)
mean(knn3_pred == Direction_09)

