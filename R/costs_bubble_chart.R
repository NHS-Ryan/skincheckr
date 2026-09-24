library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(igraph)
library(ggraph)

costs_df <- read_csv("data/costs.csv")

# Prepare unique node names in case the same sub-category
# occurs underneath more than one category
costs_plot_df <- costs_df |>
  mutate(
    estimated_cost = as.numeric(estimated_cost),
    category_id = paste0("category_", category),
    sub_category_id = paste0(
      "sub_",
      row_number(),
      "_",
      sub_category
    ),
    cost_label = paste0(
      "£",
      round(estimated_cost / 1000),
      "k"
    ),
    bubble_label = paste0(
      str_wrap(sub_category, width = 18),
      "\n",
      cost_label
    )
  )



# Root to category relationships
category_edges <- costs_plot_df |>
  distinct(category_id) |>
  transmute(
    from = "All costs",
    to = category_id
  )

# Category to sub-category relationships
sub_category_edges <- costs_plot_df |>
  transmute(
    from = category_id,
    to = sub_category_id
  )

cost_edges <- bind_rows(
  category_edges,
  sub_category_edges
)



root_node <- tibble(
  name = "All costs",
  display_name = "All costs",
  category_group = "Root",
  estimated_cost = 0
)

category_nodes <- costs_plot_df |>
  distinct(category_id, category) |>
  transmute(
    name = category_id,
    display_name = category,
    category_group = category,
    estimated_cost = 0
  )

sub_category_nodes <- costs_plot_df |>
  transmute(
    name = sub_category_id,
    display_name = bubble_label,
    category_group = category,
    estimated_cost = estimated_cost
  )

cost_nodes <- bind_rows(
  root_node,
  category_nodes,
  sub_category_nodes
)

sub_category_nodes <- costs_plot_df |>
  transmute(
    name = sub_category_id,
    display_name = bubble_label,
    category_group = category,
    estimated_cost = estimated_cost
  )

cost_nodes <- bind_rows(
  root_node,
  category_nodes,
  sub_category_nodes
)



cost_graph <- graph_from_data_frame(
  d = cost_edges,
  vertices = cost_nodes,
  directed = TRUE
)


costs_bubble_chart <- ggraph(
  cost_graph,
  layout = "circlepack",
  weight = estimated_cost
) +
  geom_node_circle(
    aes(
      fill = category_group,
      filter = depth > 0,
      alpha = factor(depth)
    ),
    colour = "white",
    linewidth = 1.2
  ) +
  geom_node_text(
    aes(
      label = display_name,
      filter = leaf & estimated_cost > 10000
    ),
    colour = "white",
    fontface = "bold",
    size = 6,
    lineheight = 0.9
  ) +
  scale_alpha_manual(
    values = c(
      "1" = 0.30,
      "2" = 0.90
    ),
    guide = "none"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    guide = "none"
  ) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  "costs_bubble_chart.svg",
  costs_bubble_chart,
  width = 16,
  height = 16,
  dpi = 600,
  bg = "white"
)


costs_bubble_chart



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

