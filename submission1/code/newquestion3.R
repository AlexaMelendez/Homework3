library(tidyverse)
library(knitr)

# Load your data
final.data <- readRDS("C:/Users/melen/OneDrive/Documents/Econ_470/Homework3/data/output/TaxBurden_Data.rds")

# Create top 5 states table based on price increase using price_cpi
top_5_states_table <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(price_cpi, na.rm = TRUE) - min(price_cpi, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5) %>%
  rename(
    State = state,
    `Price Increase` = price_increase
  )

# Print the top 5 states table nicely
knitr::kable(top_5_states_table, caption = "Top 5 States with Highest Increases in Cigarette Prices")

# Extract the top 5 state names for plotting
top_5_states <- top_5_states_table$State

# Filter data for those top 5 states and calculate average sales per year
top_states_sales <- final.data %>%
  filter(state %in% top_5_states, Year >= 1970, Year <= 2018) %>%
  group_by(state, Year) %>%
  summarise(avg_sales = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

# Create the plot
top_states_plot <- ggplot(top_states_sales, aes(x = Year, y = avg_sales, color = state)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Packs Sold per Capita (Top 5 States with Highest Price Increases)",
    x = "Year",
    y = "Packs per Capita",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10),
    legend.position = "right"
  )

# Print the plot
print(top_states_plot)
