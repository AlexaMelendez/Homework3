
library(tidyverse)
final.data <- readRDS("data/output/TaxBurden_Data.rds")

# Identify tax changes for each state/year
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

tax_change_plot <- ggplot(data = prop_tax, aes(x = factor(Year), y = prop)) +
  geom_col(fill = "#3f3e3e") +
  labs(
    title = "Proportion of States with Cigarette Tax Changes (1970–1985)",
    x = "Year",
    y = "Proportion of States with Tax Change"
  ) +
  ylim(0, 0.4) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(tax_change_plot)