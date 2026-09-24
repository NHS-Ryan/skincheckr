# 0. Introduction --------------------------------------------------------------

# This script outputs a line graph of number of urgent skin cancer referrals
# per year including a regression line from PHE Fingertips indicator 91351.

library(tidyverse)
library(scales)

# 1. Prepare England data ------------------------------------------------------

df_91351 <- read_csv("91351.csv")

england_df <- df_91351 |>
  filter(AreaType == "England") |>
  mutate(
    year_start = as.integer(str_extract(Timeperiod, "^\\d{4}")),
    Count = as.numeric(Count)
  ) |>
  arrange(year_start)


# 2. Fit linear regression excluding 2020/21 -----------------------------------

model_df <- england_df |>
  filter(Timeperiod != "2020/21")

referral_model <- lm(Count ~ year_start, data = model_df)

model_summary <- summary(referral_model)

model_statistics <- tibble(
  r_squared = model_summary$r.squared,
  adjusted_r_squared = model_summary$adj.r.squared,
  slope = unname(coef(referral_model)[["year_start"]]),
  p_value = model_summary$coefficients["year_start", "Pr(>|t|)"]
)

statistics_label <- paste0(
  "Linear trend excluding 2020/21\n",
  "R² = ", number(model_statistics$r_squared, accuracy = 0.001), "\n",
  "Adjusted R² = ",
  number(model_statistics$adjusted_r_squared, accuracy = 0.001), "\n",
  "Annual change = ",
  comma(model_statistics$slope, accuracy = 1), " referrals\n",
  "p ",
  if_else(
    model_statistics$p_value < 0.001,
    "< 0.001",
    paste0("= ", number(model_statistics$p_value, accuracy = 0.001))
  )
)

print(model_statistics)


# 3. Plot the observed values and fitted trend ---------------------------------

# Financial years to display on the x-axis
x_label_rows <- seq(1, nrow(england_df), by = 3)

england_referrals_chart <- ggplot(
  england_df,
  aes(x = year_start, y = Count)
) +
  geom_line(
    colour = "#C9795B",
    linewidth = 1.7,
    lineend = "round"
  ) +
  geom_point(
    shape = 21,
    size = 5,
    stroke = 1.3,
    colour = "white",
    fill = "#F0C8B3"
  ) +
  geom_smooth(
    data = model_df,
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "#5C352B",
    fill = "#DCEFF4",
    alpha = 0.35,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = england_df$year_start[x_label_rows],
    labels = england_df$Timeperiod[x_label_rows]
  ) +
  scale_y_continuous(
    labels = label_comma(),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.ticks = element_blank()
  )

england_referrals_chart


ggsave(
  filename = "england_skin_cancer_referrals.svg",
  plot = england_referrals_chart,
  width = 10,
  height = 6,
  units = "in",
  bg = "white"
)
