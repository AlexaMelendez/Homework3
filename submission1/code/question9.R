# OLS regression for 1991–2015
ols_1991_2015 <- lm(log_sales ~ log_cost, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
print(summary(ols_1991_2015))

# First stage: predict log_cost using log_total_tax
first_stage_1991_2015 <- lm(log_cost ~ log_total_tax, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
price_hat_1991_2015 <- predict(first_stage_1991_2015)

# Second stage: use predicted price to estimate demand
iv_model_1991_2015 <- lm(log_sales ~ price_hat_1991_2015, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
print(summary(iv_model_1991_2015))