
# question 1

tax_change_data <- final.data %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(state) %>%
  arrange(Year) %>%
  mutate(tax_diff = tax_state - lag(tax_state),
         tax_change = ifelse(!is.na(tax_diff) & tax_diff > 0, 1, 0)) %>%
  ungroup()

# calculate the proportion of states with a tax change per year
prop_tax <- tax_change_data %>%
  group_by(Year) %>%
  summarise(prop = mean(tax_change, na.rm = TRUE), .groups = "drop")

# make the plot
tax_change_plot <- ggplot(data = prop_tax, aes(x = factor(Year), y = prop)) +
  geom_col(fill = "#bf59ce") +
  labs(
    title = "Proportion of States with Cigarette Tax Changes (1970–1985)",
    x = "Year",
    y = "Proportion of States with Tax Change"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(tax_change_plot)

# question 2

# Adjust tax and price to 2012 dollars (CPI = 229 in 2012)
final.data <- final.data %>%
  mutate(
    tax_2012 = tax_dollar * (229 / index),
    price_2012 = cost_per_pack * (229 / index)
  )

# Calculate yearly averages
avg_tax_price <- final.data %>%
  filter(Year >= 1970 & Year <= 2018) %>%
  group_by(Year) %>%
  summarise(
    avg_tax_2012 = mean(tax_2012, na.rm = TRUE),
    avg_price_2012 = mean(price_2012, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with custom legend labels and colors
avg_tax_price_plot <- ggplot(avg_tax_price, aes(x = Year)) +
  geom_line(aes(y = avg_tax_2012, color = "Average Adjusted Tax"), size = 1.2) +
  geom_line(aes(y = avg_price_2012, color = "Average Adjusted Price"), size = 1.2) +
  labs(
    title = "Average Adjusted Tax and Price of Cigarettes (1970–2018)",
    x = "Year",
    y = "Dollars (2012)"
  ) +
  scale_color_manual(
    values = c("Average Adjusted Tax" = "pink", "Average Adjusted Price" = "purple"),
    name = "Legend"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Print the plot
print(avg_tax_price_plot)

# question 3

price_increases <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5)

top_5_states <- price_increases$state
print(top_5_states)

top_states_sales <- final.data %>%
  filter(state %in% top_5_states, Year >= 1970, Year <= 2018) %>%
  group_by(state, Year) %>%
  summarise(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

top_states_plot <- ggplot(top_states_sales, aes(x = Year, y = avg_sales, color = state)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Packs Sold per Capita (Top 5 States with Highest Price Increases)",
    x = "Year",
    y = "Packs per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(top_states_plot)





top_5_states_table <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5)

print(top_5_states_table)

#OR

knitr::kable(top_5_states_table, caption = "Top 5 States with Highest Increases in Cigarette Prices")

# question 4



lowest_5_states <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = "drop") %>%
  arrange(price_increase) %>%
  slice_head(n = 5)

print(lowest_5_states)

lowest_states_sales <- final.data %>%
  filter(state %in% lowest_5_states$state, Year >= 1970, Year <= 2018) %>%
  group_by(state, Year) %>%
  summarise(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

lowest_states_plot <- ggplot(lowest_states_sales, aes(x = Year, y = avg_sales, color = state)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Packs Sold per Capita (5 States with Lowest Price Increases)",
    x = "Year",
    y = "Packs per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(lowest_states_plot)

# question 6

df <- readRDS("data/output/TaxBurden_Data.rds")

df <- df %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_cost = log(cost_per_pack),
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

print(summary(ols_model))
print(summary(first_stage))
print(summary(iv_model))

# question 7

# Regular OLS 
ols_1970_1990 <- lm(log_sales ~ log_cost, data = (df %>% filter(Year >= 1970 & Year <= 1990)))
print(summary(ols_1970_1990))

# With Instrumental Variable
first_stage_1970_1990 <- lm(log_cost ~ log_total_tax, data = (df %>% filter(Year >= 1970& Year <= 1990)))
print(summary(first_stage_1991_2015))
price_hat_1970_1990<- predict(first_stage_1970_1990)
iv_model_1970_1990<- lm(log_sales ~ price_hat_1970_1990, data = (df %>% filter(Year >= 1970& Year <= 1990)))
print(summary(iv_model_1970_1990))

# question 8

first_stage_1970_1990 <- lm(log_cost ~ log_total_tax, data = (df %>% filter(Year >= 1970& Year <= 1990)))
print(summary(first_stage_1991_2015))
price_hat_1970_1990<- predict(first_stage_1970_1990)
iv_model_1970_1990<- lm(log_sales ~ price_hat_1970_1990, data = (df %>% filter(Year >= 1970& Year <= 1990)))
print(summary(iv_model_1970_1990))

# question 9

# OLS regression for 1991–2015
ols_1991_2015 <- lm(log_sales ~ log_cost, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
print(summary(ols_1991_2015))

# First stage: predict log_cost using log_total_tax
first_stage_1991_2015 <- lm(log_cost ~ log_total_tax, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
price_hat_1991_2015 <- predict(first_stage_1991_2015)

# Second stage: use predicted price to estimate demand
iv_model_1991_2015 <- lm(log_sales ~ price_hat_1991_2015, data = (df %>% filter(Year >= 1991 & Year <= 2015)))
print(summary(iv_model_1991_2015))


save.image("C:/Users/melen/OneDrive/Documents/Econ_470/Homework3/submission1/results/hw_workspace.Rdata")

