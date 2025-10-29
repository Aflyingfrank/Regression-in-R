library(tidyverse)
library(ggplot)

set.seed(123)

n <- 60
ad_spend <- rep(1:20, each = 3)[1:n]
sales <- 20 + 2 * ad_spend + rnorm(n, mean = 0, sd = 8)

df <- data.frame(ad_spend, sales)

ggplot(df, aes(x = ad_spend, y = sales)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linewidth = 1) +
  labs(
    title = "",
    x = "Advertising Spend (USD million)",
    y = "Sales (USD million)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

model <- lm(sales ~ ad_spend, data = df)
summary(model)

# ========================================================================================================================
n <- 90
train_hours <- rep(1:30, each = 3)[1:n]
sales <- 100 + 2 * train_hours + rnorm(n, mean = 0, sd = 35)

df <- data.frame(train_hours, sales)

ggplot(df, aes(x = train_hours, y = sales)) +
  geom_point(color = "coral", size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "",
    x = "Training Time (hundred hours)",
    y = "Sales (USD million)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

model <- lm(sales ~ train_hours, data = df)
summary(model)

