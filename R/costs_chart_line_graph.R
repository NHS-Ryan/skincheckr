library(tidyverse)
library(scales)

# 1. Data -----------------------------------------------------------------

df <- tibble(
  year = c("Year 1", "Year 2", "Year 3", "Year 4"),
  costs = c(712000, 592600, 592600, 592600),
  revenue = c(0, 25000, 250 * 5000, 2500000)
)

plot_df <- df |>
  mutate(
    year_number = row_number(),
    cumulative_costs = cumsum(costs),
    cumulative_revenue = cumsum(revenue),
    balance = cumulative_revenue - cumulative_costs
  )

# 2. Calculate the break-even point ----------------------------------------

crossing_row <- which(diff(sign(plot_df$balance)) != 0)[1]

x1 <- plot_df$year_number[crossing_row]
x2 <- plot_df$year_number[crossing_row + 1]
difference_1 <- plot_df$balance[crossing_row]
difference_2 <- plot_df$balance[crossing_row + 1]

break_even_x <- x1 - difference_1 * (x2 - x1) / (difference_2 - difference_1)

break_even_value <- approx(
  x = plot_df$year_number,
  y = plot_df$cumulative_costs,
  xout = break_even_x
)$y

ribbon_df <- bind_rows(
  plot_df,
  tibble(
    year = "Break-even",
    costs = NA_real_,
    revenue = NA_real_,
    year_number = break_even_x,
    cumulative_costs = break_even_value,
    cumulative_revenue = break_even_value,
    balance = 0
  )
) |>
  arrange(year_number)

negative_area <- ribbon_df |>
  filter(year_number <= break_even_x)

positive_area <- ribbon_df |>
  filter(year_number >= break_even_x)

final_values <- plot_df |>
  slice_tail(n = 1)

final_surplus <- final_values$cumulative_revenue - final_values$cumulative_costs

money_millions <- label_currency(
  prefix = "£",
  scale = 1e-6,
  suffix = "m",
  accuracy = 0.01
)

# 3. Colour palette --------------------------------------------------------

cost_colour <- "#5C352B"
revenue_colour <- "#34788A"

negative_fill <- "#F0C8B3"
positive_fill <- "#BFDCC8"
revenue_point_fill <- "#DCEFF4"
profit_colour <- "#39734D"
neutral_colour <- "#5C6266"

# 4. Chart ----------------------------------------------------------------

cumulative_chart <- ggplot(plot_df, aes(x = year_number)) +
  geom_line(
    aes(y = cumulative_costs),
    colour = "#5C352B",
    linewidth = 1.7,
    lineend = "round"
  ) +
  geom_line(
    aes(y = cumulative_revenue),
    colour = "#C9795B",
    linewidth = 1.7,
    lineend = "round"
  ) +
  geom_point(
    aes(y = cumulative_costs),
    shape = 21,
    size = 5.5,
    stroke = 1.4,
    colour = "white",
    fill = "#A87567"
  ) +
  geom_point(
    aes(y = cumulative_revenue),
    shape = 21,
    size = 5.5,
    stroke = 1.4,
    colour = "white",
    fill = "#F0C8B3"
  ) +
  annotate(
    "text",
    x = 4.08,
    y = final_values$cumulative_revenue + 105000,
    label = money_millions(final_values$cumulative_revenue),
    hjust = 0,
    colour = "#A8563E",
    fontface = "bold",
    size = 7
  ) +
  annotate(
    "text",
    x = 4.08,
    y = final_values$cumulative_revenue - 105000,
    label = "Cumulative revenue",
    hjust = 0,
    colour = "#A8563E",
    fontface = "bold",
    size = 3.6
  ) +
  annotate(
    "text",
    x = 4.08,
    y = final_values$cumulative_costs + 105000,
    label = money_millions(final_values$cumulative_costs),
    hjust = 0,
    colour = "#5C352B",
    fontface = "bold",
    size = 7
  ) +
  annotate(
    "text",
    x = 4.08,
    y = final_values$cumulative_costs - 105000,
    label = "Cumulative costs",
    hjust = 0,
    colour = "#5C352B",
    fontface = "bold",
    size = 3.6
  ) +
  scale_x_continuous(
    breaks = plot_df$year_number,
    labels = plot_df$year,
    limits = c(0.85, 4.85),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 4000000, 1000000),
    labels = label_currency(
      prefix = "£",
      scale = 1e-6,
      suffix = "m",
      accuracy = 1
    ),
    limits = c(-150000, 4200000),
    expand = c(0, 0)
  ) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_text(
      colour = "#5C6266",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      colour = "#5C6266",
      margin = margin(r = 8)
    ),
    axis.ticks = element_blank(),
    plot.margin = margin(20, 150, 20, 20)
  )

cumulative_chart

# 5. Save as SVG -----------------------------------------------------------

ggsave(
  filename = "cumulative_costs_revenue.svg",
  plot = cumulative_chart,
  width = 11,
  height = 6,
  units = "in",
  bg = "white"
)
