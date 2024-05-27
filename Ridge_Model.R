library(glmnet)


data = imputed_data
str(data)
summary(data)

#Train/Set 80/20
set.seed(123)

size <- floor(0.8 * nrow(data))

training_index <- sample(seq_len(nrow(data)), size = size)

train <- data[training_index, ]
xtrain <- train[, -15]
ytrain <- train[,15]

test <- data[-training_index,]
xtest <- train[, -15]
ytest <- train[,15]

#Lambda Test
lambda.array <- seq(from = 0.01, to = 100, by = 0.01)

ridgeFit <- glmnet(xtrain, ytrain, alpha = 0, lambda = lambda.array, family = "binomial")


#As lambda becomes larger, the sign of coefficients will start to decrease
plot(ridgeFit, xvar = 'lambda', label = T)


#Goodness of Fit
plot(ridgeFit, xvar = 'dev', label = T)

xtrain <- as.matrix(xtrain)
xtest <- as.matrix(xtest)
#Predicted
cv.ridge <- cv.glmnet(xtrain, ytrain, alpha = 0, family = "binomial")
best_lambda <- cv.ridge$lambda.min

y_predicted <- predict(ridgeFit, s = best_lambda, newx = xtest, type = "response")

predict(ridgeFit, s = min(lambda.array), newx = xtest, type = 'coefficients')

predicted_classes <- ifelse(y_predicted > 0.5, 1, 0)
accuracy <- mean(predicted_classes == ytest)

coef(ridgeFit, s = best_lambda)


# Assuming ytest and y_predicted are your test set response and predicted values
# Convert y_predicted to binary predictions based on the threshold 0.5
predicted_classes <- ifelse(y_predicted > 0.5, 1, 0)

# Create a confusion matrix
conf_mat <- table(Actual = ytest, Predicted = predicted_classes)

# Calculate Nagelkerke's R^2
TN <- conf_mat[1, 1]
FP <- conf_mat[1, 2]
FN <- conf_mat[2, 1]
TP <- conf_mat[2, 2]

n <- sum(conf_mat)
L_intercept <- logLik(glm(ytest ~ 1, family = "binomial"))
L_full <- logLik(glm(ytest ~ predicted_classes, family = "binomial"))

# Calculate Nagelkerke's R^2
nagelkerke_R2 <- 1 - exp((2/n) * (L_intercept - L_full) / (1 - exp(L_full / n)))

print(accuracy)
print(nagelkerke_R2)
