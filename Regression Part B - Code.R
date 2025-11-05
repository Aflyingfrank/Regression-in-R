library(tidyverse)
library(ResourceSelection)
library(pROC)

set.seed(123)

# ============================================================
# 1. Generate synthetic transaction data

n <- 5000  # number of transactions

# Generate some transaction features
amount <- rexp(n, rate = 1/100)                   # transaction amount
time_diff <- rexp(n, rate = 1/5)                  # time since last transaction
customer_age <- rnorm(n, mean = 40, sd = 12)      # customer's age
num_prev_fraud <- rpois(n, lambda = 0.3)          # previous fraud history
is_international <- rbinom(n, 1, 0.2)             # 1 = international transaction

# True model to simulate fraud probability
# Fraudulent transactions are more likely with high amount, high frequency, prior frauds, and international
lin_pred <- -5 + 
  0.015 * amount + 
  0.1 * (1 / (time_diff + 1)) + 
  0.8 * num_prev_fraud + 
  1.5 * is_international - 
  0.02 * customer_age

prob_fraud <- 1 / (1 + exp(-lin_pred))
fraudulent <- rbinom(n, 1, prob_fraud)

# Combine into a data frame
data <- data.frame(
  fraudulent = factor(fraudulent, labels = c("No", "Yes")),
  amount,
  time_diff,
  customer_age,
  num_prev_fraud,
  is_international = factor(is_international)
)

head(data)

# ============================================================
# 2. Fit logistic regression model

model <- glm(fraudulent ~ amount + time_diff + customer_age + 
               num_prev_fraud + is_international,
             data = data,
             family = binomial(link = "logit"))

summary(model)

# Extract AIC
cat("\nModel AIC:", AIC(model), "\n")

# ============================================================
# 3. Hosmer–Lemeshow Goodness-of-Fit Test

# Hosmer-Lemeshow test (grouped into 10 bins by predicted probability)
hl_test <- hoslem.test(
  x = as.numeric(data$fraudulent) - 1,
  y = fitted(model),
  g = 10
)

cat("\nHosmer–Lemeshow Test Results:\n")
print(hl_test)

# ============================================================
# 4. Model Performance: Confusion Matrix, Accuracy, ROC Curve

# Predicted probabilities and classification
data$pred_prob <- predict(model, type = "response")
data$pred_class <- ifelse(data$pred_prob > 0.5, "Yes", "No")

# Confusion Matrix
conf_mat <- table(Predicted = data$pred_class, Actual = data$fraudulent)
cat("\nConfusion Matrix:\n")
print(conf_mat)

# Accuracy
accuracy <- mean(data$pred_class == data$fraudulent)
cat("\nAccuracy:", round(accuracy, 4), "\n")

# ROC and AUC
roc_obj <- roc(data$fraudulent, data$pred_prob)
auc_value <- auc(roc_obj)
cat("\nAUC:", round(auc_value, 4), "\n")

# Plot ROC Curve
plot(roc_obj, main = "ROC Curve for Fraud Detection", col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")

# ============================================================
# 5. Interpretation
# Higher transaction amount, frequent transactions, prior fraud history,
# and international transfers all increase the odds of fraud.
# Lower Hosmer–Lemeshow p-value → poor fit (model may need improvement)

