library(tidyverse)

final.data <- readRDS("data/output/TaxBurden_Data.rds")

top_5_states_table <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(price_cpi, na.rm = TRUE) - min(price_cpi, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price_increase)) %>%
  slice_head(n = 5)

print(top_5_states_table)

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







#OR

knitr::kable(top_5_states_table, caption = "Top 5 States with Highest Increases in Cigarette Prices")

