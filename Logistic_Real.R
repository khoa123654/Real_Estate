library(glmnet)

attach(imputed_data)


library(MASS)
library(dplyr)

log_model <- glm(Rent_Mortgage_Hold ~ .,, data = imputed_data) %>%
  stepAIC(trace = TRUE)

summary(log_model)

train = imputed_data[0:round(0.8*dim(imputed_data)[1]),]
test = imputed_data[(round(0.8*dim(imputed_data)[1])+1):dim(imputed_data)[1],]

log_model_final <- glm(train$Rent_Mortgage_Hold ~., data = train) %>%
  stepAIC(trace = TRUE)

y_pred <- predict(log_model_final, test)
y_pred

MSE = (sum(y_pred - test$Rent_Mortgage_Hold)^2 / length(y_pred))
summary(log_model_final)


log_model_base <- glm(Rent_Mortgage_Hold ~ .,, data = imputed_data)
y_pred_base <- predict(log_model_base, test)
MSE_base = (sum(y_pred_base - test$Rent_Mortgage_Hold)^2 / length(y_pred_base))
summary(log_model_base)


predicted_classes <- ifelse(y_pred > 0.5, 1, 0)

# Calculate accuracy
accuracy <- mean(predicted_classes == test$Rent_Mortgage_Hold)

# Print the accuracy
print(paste("Accuracy:", accuracy))


detach(imputed_data)
