# Load required packages
library(tidyverse)
library(ggplot2)

# Define output directory
output_dir <- "C:/Users/melen/OneDrive/Documents/Econ_470/Homework3/submission1/"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

# Load data
final.data <- read_tsv("data/output/TaxBurden_Data.txt", show_col_types = FALSE)

# 1. Bar Graph: Proportion of states with cigarette tax changes (1970-1985)
tax_change <- final.data %>%
  filter(Year >= 1970 & Year <= 1985) %>%
  group_by(state) %>%
  arrange(Year) %>%
  mutate(tax_change = tax_dollar - lag(tax_dollar)) %>%
  mutate(tax_changed = ifelse(!is.na(tax_change) & tax_change != 0, 1, 0)) %>%
  group_by(Year) %>%
  summarize(proportion = mean(tax_changed, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(tax_change, aes(x = Year, y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Proportion of States with Cigarette Tax Changes (1970-1985)",
       x = "Year", y = "Proportion of States") +
  theme_minimal()

ggsave(filename = paste0(output_dir, "tax_change_proportion.png"), plot = p1, width = 8, height = 5)

# 2. Average tax (2012 dollars) vs. Average price (1970-2018)
final.data <- final.data %>%
  mutate(tax_2012 = tax_dollar * (229 / index),  
         price_2012 = cost_per_pack * (229 / index))

avg_data <- final.data %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_2012, na.rm = TRUE),
            avg_price = mean(price_2012, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(avg_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax (2012 $)"), linewidth = 1) +
  geom_line(aes(y = avg_price, color = "Average Price (2012 $)"), linewidth = 1) +
  labs(title = "Average Tax vs. Average Price of Cigarettes (1970-2018)",
       x = "Year", y = "Dollars (2012)") +
  scale_color_manual(values = c("Average Tax (2012 $)" = "red", "Average Price (2012 $)" = "blue")) +
  theme_minimal()

ggsave(filename = paste0(output_dir, "avg_tax_vs_price.png"), plot = p2, width = 8, height = 5)

# 3. Five states with the highest increase in cigarette prices
price_diff <- final.data %>%
  group_by(state) %>%
  summarize(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5) 

top_states <- price_diff$state

sales_top <- final.data %>%
  filter(state %in% top_states) %>%
  group_by(state, Year) %>%
  summarize(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

p3 <- ggplot(sales_top, aes(x = Year, y = avg_sales, color = state)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Packs Sold Per Capita (Top 5 States with Highest Price Increases)",
       x = "Year", y = "Packs per Capita") +
  theme_minimal()

ggsave(filename = paste0(output_dir, "sales_highest_price_increase.png"), plot = p3, width = 8, height = 5)

# 4. Five states with the lowest increase in cigarette prices
price_diff_low <- final.data %>%
  group_by(state) %>%
  summarize(price_increase = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE), .groups = "drop") %>%
  arrange(price_increase) %>%
  slice_head(n = 5)

low_states <- price_diff_low$state

sales_low <- final.data %>%
  filter(state %in% low_states) %>%
  group_by(state, Year) %>%
  summarize(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(sales_low, aes(x = Year, y = avg_sales, color = state)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Packs Sold Per Capita (Top 5 States with Lowest Price Increases)",
       x = "Year", y = "Packs per Capita") +
  theme_minimal()

ggsave(filename = paste0(output_dir, "sales_lowest_price_increase.png"), plot = p4, width = 8, height = 5)

# 5. Compare trends: Highest vs. Lowest price increase states
sales_compare <- final.data %>%
  filter(state %in% c(top_states, low_states)) %>%
  group_by(state, Year) %>%
  summarize(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = ifelse(state %in% top_states, "High Price Increase", "Low Price Increase"))

p5 <- ggplot(sales_compare, aes(x = Year, y = avg_sales, color = group)) +
  geom_line(linewidth = 1) +
  labs(title = "Sales Trends: High vs. Low Price Increase States (1970-2018)",
       x = "Year", y = "Packs per Capita") +
  scale_color_manual(values = c("High Price Increase" = "red", "Low Price Increase" = "blue")) +
  theme_minimal()

ggsave(filename = paste0(output_dir, "sales_comparison.png"), plot = p5, width = 8, height = 5)

