# 0. Purpose ---------------------------------------------------------------

# Prepare a PCN-level dataset for modelling urgent suspected skin cancer
# referral rates using:
#
# - Indicator 1679: ethnicity estimates for GP populations
# - Indicator 93468: percentage of the population aged 65 years or over
# - Indicator 91351: urgent suspected skin cancer referral count and denominator
#
# Each indicator is restricted independently to its latest available period.
# The periods may therefore differ between indicators.
#
# Indicator 1679 contains GP-level percentages without counts or denominators.
# PCN ethnicity estimates are therefore calculated as the unweighted mean of
# the percentages for the GP practices belonging to each PCN.


# 1. Load packages ---------------------------------------------------------

library(tidyverse)


# 2. Import and standardise datasets ---------------------------------------

ethnicity_df <- read_csv("1679.csv", show_col_types = FALSE) |>
  mutate(
    Timeperiod = as.character(Timeperiod),
    TimeperiodSortable = as.numeric(TimeperiodSortable),
    Value = as.numeric(Value)
  )

age_df <- read_csv("93468.csv", show_col_types = FALSE) |>
  mutate(
    Timeperiod = as.character(Timeperiod),
    TimeperiodSortable = as.numeric(TimeperiodSortable),
    Value = as.numeric(Value)
  )

referrals_df <- read_csv("91351.csv", show_col_types = FALSE) |>
  mutate(
    Timeperiod = as.character(Timeperiod),
    TimeperiodSortable = as.numeric(TimeperiodSortable),
    Count = as.numeric(Count),
    Denominator = as.numeric(Denominator)
  )

# 4. Identify each dataset's latest available period -----------------------

latest_ethnicity_period <- max(
  ethnicity_df$TimeperiodSortable,
  na.rm = TRUE
)

latest_age_period <- max(
  age_df$TimeperiodSortable,
  na.rm = TRUE
)

latest_referral_period <- max(
  referrals_df$TimeperiodSortable,
  na.rm = TRUE
)

selected_periods <- bind_rows(
  ethnicity_df |>
    filter(TimeperiodSortable == latest_ethnicity_period) |>
    distinct(TimeperiodSortable, Timeperiod) |>
    mutate(indicator = "1679: Ethnicity estimates"),
  age_df |>
    filter(TimeperiodSortable == latest_age_period) |>
    distinct(TimeperiodSortable, Timeperiod) |>
    mutate(indicator = "93468: Population by age"),
  referrals_df |>
    filter(TimeperiodSortable == latest_referral_period) |>
    distinct(TimeperiodSortable, Timeperiod) |>
    mutate(indicator = "91351: Suspected skin cancer referrals")
) |>
  select(indicator, TimeperiodSortable, Timeperiod)

print(selected_periods, n = Inf)


# 5. Prepare ethnic composition by PCN -------------------------------------

# Indicator 1679 contains GP-level percentage estimates for five broad ethnic
# groups. Since counts and denominators are unavailable, PCN estimates are
# calculated as unweighted means across member GP practices.
#
# This gives each GP practice equal weight, regardless of registered
# population size, and should be reported as a limitation.

ethnicity_categories <- c(
  "White",
  "Mixed / Multiple ethnic groups",
  "Asian / Asian British inc Chinese",
  "Black / African / Caribbean / Black British",
  "Other ethnic group"
)

ethnicity_gp_latest <- ethnicity_df |>
  filter(
    TimeperiodSortable == latest_ethnicity_period,
    Category %in% ethnicity_categories,
    !is.na(ParentCode),
    !is.na(Value)
  ) |>
  distinct(
    AreaCode,
    ParentCode,
    ParentName,
    Category,
    .keep_all = TRUE
  )

# Calculate the mean percentage for each ethnic group within each PCN
ethnicity_pcn <- ethnicity_gp_latest |>
  group_by(
    PCNCode = ParentCode,
    PCNName = ParentName,
    Category
  ) |>
  summarise(
    percentage = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Category = recode(
      Category,
      "White" = "percentage_white",
      "Mixed / Multiple ethnic groups" = "percentage_mixed",
      "Asian / Asian British inc Chinese" = "percentage_asian",
      "Black / African / Caribbean / Black British" = "percentage_black",
      "Other ethnic group" = "percentage_other_ethnicity"
    )
  ) |>
  pivot_wider(
    names_from = Category,
    values_from = percentage
  )

# Count the number of GP practices contributing data for each PCN
ethnicity_coverage <- ethnicity_gp_latest |>
  group_by(
    PCNCode = ParentCode,
    PCNName = ParentName
  ) |>
  summarise(
    ethnicity_practices = n_distinct(AreaCode),
    .groups = "drop"
  )

ethnicity_pcn <- ethnicity_pcn |>
  left_join(
    ethnicity_coverage,
    by = c("PCNCode", "PCNName")
  ) |>
  mutate(
    ethnicity_percentage_total =
      percentage_white +
      percentage_mixed +
      percentage_asian +
      percentage_black +
      percentage_other_ethnicity
  )


# 6. Prepare percentage aged 65+ by PCN ------------------------------------

# Indicator 93468 already contains PCN-level observations. The 65+ category
# is selected because the available older-age categories overlap.

age_pcn <- age_df |>
  filter(
    TimeperiodSortable == latest_age_period,
    AreaType == "PCNs (v. 26/06/25)",
    Age == "65+ yrs"
  )

# Apply the Persons filter only if a Sex column exists
if ("Sex" %in% names(age_pcn)) {
  age_pcn <- age_pcn |>
    filter(Sex == "Persons")
}

# Check whether filtering leaves more than one record per PCN
age_duplicates <- age_pcn |>
  count(AreaCode, AreaName) |>
  filter(n > 1)

if (nrow(age_duplicates) > 0) {
  warning(
    nrow(age_duplicates),
    " PCNs have multiple age records after filtering. ",
    "Review age_duplicates before modelling."
  )
}

age_pcn <- age_pcn |>
  distinct(AreaCode, .keep_all = TRUE) |>
  transmute(
    PCNCode = AreaCode,
    PCNName = AreaName,
    percentage_aged_65_plus = Value
  )


# 7. Prepare urgent suspected skin cancer referral data --------------------

referrals_pcn <- referrals_df |>
  filter(
    TimeperiodSortable == latest_referral_period,
    Sex == "Persons",
    Age == "All ages"
  )

# Restrict to PCNs if other area types are present
if (n_distinct(referrals_pcn$AreaType) > 1) {
  referrals_pcn <- referrals_pcn |>
    filter(str_detect(
      AreaType,
      regex("^PCNs|Primary Care Network", ignore_case = TRUE)
    ))
}

referral_duplicates <- referrals_pcn |>
  count(AreaCode, AreaName) |>
  filter(n > 1)

if (nrow(referral_duplicates) > 0) {
  warning(
    nrow(referral_duplicates),
    " PCNs have multiple referral records after filtering. ",
    "Review referral_duplicates before modelling."
  )
}

referrals_pcn <- referrals_pcn |>
  distinct(AreaCode, .keep_all = TRUE) |>
  transmute(
    PCNCode = AreaCode,
    PCNName = AreaName,
    referral_count = Count,
    referral_denominator = Denominator,
    referral_rate = referral_count / referral_denominator,
    referral_rate_per_100000 =
      100000 * referral_count / referral_denominator
  )


# 8. Check PCN-code overlap before joining ---------------------------------

join_diagnostics <- tibble(
  comparison = c(
    "Referrals matched to ethnicity",
    "Referrals matched to age"
  ),
  matching_pcn_codes = c(
    length(intersect(
      referrals_pcn$PCNCode,
      ethnicity_pcn$PCNCode
    )),
    length(intersect(
      referrals_pcn$PCNCode,
      age_pcn$PCNCode
    ))
  ),
  referral_pcn_codes = n_distinct(referrals_pcn$PCNCode)
)

print(join_diagnostics)


# 9. Join the PCN-level datasets -------------------------------------------

model_df <- referrals_pcn |>
  left_join(
    ethnicity_pcn |>
      select(
        PCNCode,
        starts_with("percentage_"),
        ethnicity_practices
      ),
    by = "PCNCode"
  ) |>
  left_join(
    age_pcn |>
      select(
        PCNCode,
        percentage_aged_65_plus
      ),
    by = "PCNCode"
  ) |>
  mutate(
    white_10pct = percentage_white / 10,
    mixed_10pct = percentage_mixed / 10,
    asian_10pct = percentage_asian / 10,
    black_10pct = percentage_black / 10,
    other_ethnicity_10pct = percentage_other_ethnicity / 10,
    aged_65_plus_10pct = percentage_aged_65_plus / 10
  )


# 10. Summarise model data availability ------------------------------------

model_data_summary <- model_df |>
  summarise(
    total_pcns = n(),
    pcns_with_referrals = sum(
      !is.na(referral_count) &
        !is.na(referral_denominator)
    ),
    pcns_with_ethnicity = sum(
      !is.na(percentage_white)
    ),
    pcns_with_age = sum(
      !is.na(percentage_aged_65_plus)
    ),
    complete_pcns = sum(
      complete.cases(
        referral_count,
        referral_denominator,
        percentage_white,
        percentage_mixed,
        percentage_asian,
        percentage_black,
        percentage_other_ethnicity,
        percentage_aged_65_plus
      )
    )
  )

print(model_data_summary)


# 11. Create the complete-case modelling dataset ---------------------------

model_complete_df <- model_df |>
  filter(
    !is.na(referral_count),
    !is.na(referral_denominator),
    referral_denominator > 0,
    !is.na(percentage_white),
    !is.na(percentage_mixed),
    !is.na(percentage_asian),
    !is.na(percentage_black),
    !is.na(percentage_other_ethnicity),
    !is.na(percentage_aged_65_plus)
  )


# 12. Examine predictor correlation ----------------------------------------

predictor_correlation <- model_complete_df |>
  summarise(
    correlation = cor(
      percentage_white,
      percentage_aged_65_plus,
      use = "complete.obs"
    )
  )

print(predictor_correlation)
