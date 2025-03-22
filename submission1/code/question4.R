library(tidyverse)

final.data <- readRDS("data/output/TaxBurden_Data.rds")

lowest_5_states <- final.data %>%
  group_by(state) %>%
  summarise(price_increase = max(price_cpi, na.rm = TRUE) - min(price_cpi, na.rm = TRUE), .groups = "drop") %>%
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

library(kableExtra)

lowest_5_states %>%
  mutate(price_increase = round(price_increase, 2)) %>%
  kable(
    caption = "Five States with the Lowest Increase in CPI-Adjusted Cigarette Prices (1970–2018)",
    digits = 2,
    col.names = c("State", "Price Increase (CPI Adjusted)")
  ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
