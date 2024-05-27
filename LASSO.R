# Load required library
library(glmnet)

# Assuming imputed_data is your dataset
data <- imputed_data

# Assuming the response variable is in the 15th column
x <- as.matrix(data[, -15])
y <- data[, 15]

# Train/Test Split (80/20)
set.seed(123)
size <- floor(0.8 * nrow(data))
training_index <- sample(seq_len(nrow(data)), size = size)

train <- data[training_index, ]
test <- data[-training_index, ]

xtrain <- as.matrix(train[, -15])
ytrain <- train[, 15]

xtest <- as.matrix(test[, -15])
ytest <- test[, 15]

# Lambda Test
lambda.array <- seq(from = 0.01, to = 100, by = 0.01)

# Fit LASSO model
lassoFit <- glmnet(xtrain, ytrain, alpha = 1, lambda = lambda.array, family = "binomial")

# Plot coefficients against lambda
plot(lassoFit, xvar = 'lambda', label = TRUE)

# Plot deviance explained
plot(lassoFit, xvar = 'dev', label = TRUE)

# Cross-validation for lambda selection
cv.lasso <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "binomial")
best_lambda <- cv.lasso$lambda.min

# Predictions
y_predicted <- predict(lassoFit, s = best_lambda, newx = xtest, type = "response")

# Evaluate accuracy
predicted_classes <- ifelse(y_predicted > 0.5, 1, 0)
accuracy <- mean(predicted_classes == ytest)

# Coefficients interpretation
coefficients <- coef(lassoFit, s = best_lambda)

# Calculate Nagelkerke's R^2
conf_mat <- table(Actual = ytest, Predicted = predicted_classes)
TN <- conf_mat[1, 1]
FP <- conf_mat[1, 2]
FN <- conf_mat[2, 1]
TP <- conf_mat[2, 2]
n <- sum(conf_mat)
L_intercept <- logLik(glm(ytest ~ 1, family = "binomial"))
L_full <- logLik(glm(ytest ~ predicted_classes, family = "binomial"))
nagelkerke_R2 <- 1 - exp((2/n) * (L_intercept - L_full) / (1 - exp(L_full / n)))

# Print accuracy, coefficients, and Nagelkerke's R^2
print(paste("Accuracy:", accuracy))
print("Coefficients:")
print(coefficients)
print(paste("Nagelkerke's R^2:", nagelkerke_R2))
