library(MASS)
library(broom)
library(tidyverse)
library(ggrepel)
library(scales)
library(plotly)

referral_model_simple <- MASS::glm.nb(
  referral_count ~ white_10pct + aged_65_plus_10pct +
    offset(log(referral_denominator)),
  data = model_complete_df
)

simple_model_results <- broom::tidy(
  referral_model_simple,
  exponentiate = TRUE,
  conf.int = TRUE
) |>
  mutate(percentage_change = 100 * (estimate - 1))

simple_model_results


## Model evaluation

pcn_predictions <- model_complete_df |>
  mutate(
    predicted_count = predict(referral_model_simple, type = "response"),
    observed_rate = 100000 * referral_count / referral_denominator,
    predicted_rate = 100000 * predicted_count / referral_denominator
  )

model_metrics <- pcn_predictions |>
  summarise(
    n = n(),
    MAE = mean(abs(observed_rate - predicted_rate)),
    RMSE = sqrt(mean((observed_rate - predicted_rate)^2)),
    correlation = cor(observed_rate, predicted_rate),
    R_squared = correlation^2,
    mean_observed = mean(observed_rate),
    mean_predicted = mean(predicted_rate)
  )

pseudo_R_squared <- 1 - referral_model_simple$deviance /
  referral_model_simple$null.deviance

calibration_model <- lm(observed_rate ~ predicted_rate, data = pcn_predictions)

tibble(
  Measure = c(
    "PCNs included",
    "Mean absolute error",
    "Root mean squared error",
    "Observed-predicted correlation",
    "Squared correlation",
    "Deviance-based pseudo-R²",
    "Calibration intercept",
    "Calibration slope"
  ),
  Value = c(
    model_metrics$n,
    model_metrics$MAE,
    model_metrics$RMSE,
    model_metrics$correlation,
    model_metrics$R_squared,
    pseudo_R_squared,
    coef(calibration_model)[1],
    coef(calibration_model)[2]
  )
) |>
  mutate(Value = round(Value, 3)) |>
  print(n = Inf)


## Model visualisation

pcn_predictions <- model_complete_df |>
  mutate(
    predicted_count = predict(referral_model_simple, type = "response"),
    observed_rate = 100000 * referral_count / referral_denominator,
    predicted_rate = 100000 * predicted_count / referral_denominator,
    residual_rate = observed_rate - predicted_rate,
    hover_text = paste0(
      "<b>", PCNName, "</b>",
      "<br>PCN code: ", PCNCode,
      "<br><br>Observed rate: ", comma(observed_rate, accuracy = 1),
      " per 100,000",
      "<br>Predicted rate: ", comma(predicted_rate, accuracy = 1),
      " per 100,000",
      "<br>Observed minus predicted: ",
      comma(residual_rate, accuracy = 1),
      " per 100,000",
      "<br><br>Referral count: ", comma(referral_count),
      "<br>Population denominator: ", comma(referral_denominator),
      "<br>White population: ",
      number(percentage_white, accuracy = 0.1), "%",
      "<br>Population aged 65+: ",
      number(percentage_aged_65_plus, accuracy = 0.1), "%"
    )
  )

x_max <- ceiling(max(pcn_predictions$predicted_rate, na.rm = TRUE) / 250) * 250
y_max <- ceiling(max(pcn_predictions$observed_rate, na.rm = TRUE) / 250) * 250
reference_max <- min(x_max, y_max)

pcn_model_plotly <- plotly::plot_ly(
  data = pcn_predictions,
  x = ~predicted_rate,
  y = ~observed_rate,
  type = "scatter",
  mode = "markers",
  text = ~hover_text,
  hoverinfo = "text",
  marker = list(
    size = 9,
    opacity = 0.85,
    color = ~residual_rate,
    colorscale = list(
      c(0, "#DCEFF4"),
      c(0.5, "#F0C8B3"),
      c(1, "#C45A42")
    ),
    cmid = 0,
    line = list(
      color = "white",
      width = 1
    ),
    colorbar = list(
      title = list(text = "Observed minus<br>predicted"),
      tickformat = ",.0f"
    )
  ),
  showlegend = FALSE
) |>
  plotly::add_lines(
    x = c(0, reference_max),
    y = c(0, reference_max),
    inherit = FALSE,
    line = list(
      color = "#5C352B",
      width = 2,
      dash = "dash"
    ),
    hoverinfo = "skip",
    showlegend = FALSE
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Model-predicted referrals per 100,000",
      range = c(0, x_max),
      tickformat = ",.0f",
      dtick = 500,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Observed referrals per 100,000",
      range = c(0, y_max),
      tickformat = ",.0f",
      dtick = 500,
      zeroline = FALSE
    ),
    hoverlabel = list(align = "left"),
    margin = list(l = 90, r = 130, b = 80, t = 20)
  ) |>
  plotly::config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d")
  )

pcn_model_plotly


## Missing Referrals

pilot_sites <- model_complete_df |>
  mutate(
    predicted_referrals = predict(referral_model_simple, type = "response"),
    referral_gap = predicted_referrals - referral_count,
    observed_rate_per_100000 = 100000 * referral_count / referral_denominator,
    predicted_rate_per_100000 = 100000 * predicted_referrals / referral_denominator,
    referral_rate_gap = predicted_rate_per_100000 - observed_rate_per_100000
  ) |>
  filter(referral_gap > 0) |>
  arrange(desc(referral_gap)) |>
  transmute(
    PCNCode,
    PCNName,
    observed_referrals = referral_count,
    predicted_referrals = round(predicted_referrals),
    estimated_referral_gap = round(referral_gap),
    observed_rate_per_100000 = round(observed_rate_per_100000),
    predicted_rate_per_100000 = round(predicted_rate_per_100000),
    referral_rate_gap = round(referral_rate_gap),
    percentage_white = round(percentage_white, 1),
    percentage_aged_65_plus = round(percentage_aged_65_plus, 1),
    population = referral_denominator
  )

print(pilot_sites, n = 20)

