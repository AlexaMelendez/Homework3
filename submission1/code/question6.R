library(tidyverse)

df <- readRDS("data/output/TaxBurden_Data.rds")

df <- df %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_cost = log(price_cpi),
    tax_cpi = tax_state * (230 / index),
    total_tax_cpi = tax_dollar * (230 / index),
    log_total_tax = log(total_tax_cpi)
  )

ols_model <- lm(log_sales ~ log_cost, data = df %>% dplyr::filter(Year >= 1970 & Year <= 1990))
summary(ols_model)

first_stage <- lm(log_cost ~ log_total_tax, data = df %>% dplyr::filter(Year >= 1970 & Year <= 1990))
price_hat <- predict(first_stage)

iv_model <- lm(log_sales ~ price_hat, data = df %>% dplyr::filter(Year >= 1970 & Year <= 1990))
summary(iv_model)

# Extract results
ols_summary <- tidy(ols_model) %>%
  mutate(Model = "OLS")
first_stage_summary <- tidy(first_stage) %>%
  mutate(Model = "First Stage")
iv_summary <- tidy(iv_model) %>%
  mutate(Model = "IV")

# Combine into one simple table
results_table <- bind_rows(ols_summary, first_stage_summary, iv_summary) %>%
  select(Model, term, estimate, std.error, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `t-value` = statistic,
    `p-value` = p.value
  )

# Display a table suitable for Typst PDF rendering
knitr::kable(
  results_table,
  format = "simple",
  caption = "Regression Results: OLS, First Stage, and IV Models (1970–1990)"
)


#The estimated price elasticity of demand is 0.50, meaning that a 1% increase in price is linked to a 0.5% increase in cigarette sales. This goes against what we would normally expect since higher prices should lead to lower sales. It likely means there’s something off with the model or the instrument used, and the price changes might be picking up other factors that affect cigarette sales.