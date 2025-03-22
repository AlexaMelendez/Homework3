library(tidyverse)

# Load the data
final.data <- readRDS("data/output/TaxBurden_Data.rds")

final.data <- final.data %>%
  mutate(
    tax_2012 = tax_dollar * (230 / index),
    price_2012 = cost_per_pack * (230 / index)
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

# Plot with one solid line and one dotted line
avg_tax_price_plot <- ggplot(avg_tax_price, aes(x = Year)) +
  geom_line(aes(y = avg_tax_2012, linetype = "Average Adjusted Tax"), size = 1.2, color = "black") +
  geom_line(aes(y = avg_price_2012, linetype = "Average Adjusted Price"), size = 1.2, color = "black") +
  labs(
    title = "Average Adjusted Tax and Price of Cigarettes (1970–2018)",
    x = "Year",
    y = "Dollars (2012)"
  ) +
  scale_linetype_manual(
    values = c("Average Adjusted Tax" = "solid", "Average Adjusted Price" = "dotted"),
    name = "Legend"
  ) +
  coord_cartesian(xlim = c(1970, 2019), ylim = c(0, 8)) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Print the plot
print(avg_tax_price_plot)
