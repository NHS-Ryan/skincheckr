# 0. Introduction --------------------------------------------------------------

# This script is a step by step process to explore the PHE Fingertips indicator
# catalogue and download a particular indicator from that catalogue. with the
# option to filter to particular area types.

# Download the catalogue in section 2, search it using text inputs in section 3.
# You can also see the types of areas (ICBs, PCNs etc) that are available in
# section 4. Section 5 allows you to set your parameters (indicator_id &
# areatype_id) then download this to a local dataframe in section 6 and save to
# your current working directory in section 7.


# 1. Required Libraries --------------------------------------------------------
library(fingertipsR)
library(tidyverse)


# 2. Download the indicator catalogue ------------------------------------------
all_indicators <- indicators(proxy_settings = "none")


# 3. Search indicator names ----------------------------------------------------

# Change this text to search for another subject, note you can search multiple
# criteria with "text1|text2"
search_text <- " age "

indicator_search_results <- all_indicators |>
  filter(str_detect(
    IndicatorName,
    regex(search_text, ignore_case = TRUE)
  )) |>
  distinct(
    IndicatorID,
    IndicatorName,
    ProfileID,
    ProfileName,
    DomainID,
    DomainName
  ) |>
  arrange(IndicatorName) |>
  print(n = Inf)

# 4. Search Area Types ---------------------------------------------------------

area_types_df <- area_types() |>
  select(AreaTypeID,AreaTypeName) |>
  distinct() |>
  arrange(AreaTypeName) |>
  print()


# 5. Set parameters for download -----------------------------------------------

# Example combination:
# indicator_id <- 91351 (Urgent suspected cancer referrals for suspected skin cancer)
# areatype_id <- 221 (ICBs)

indicator_id <- 337

# Define a single area type here, or if you want to just download everything go
# to section 6b:
areatype_id <- 221


# 6. Download indicator data ---------------------------------------------------

# a. Download a specific AreaTypeID ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
indicator_df <- fingertips_data(
  IndicatorID = indicator_id,
  AreaTypeID = areatype_id
)

# b. Download all AreaTypeIDs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
areatype_ids <- indicator_areatypes(
  IndicatorID = indicator_id,
  proxy_settings = "none"
) |>
  distinct(AreaTypeID) |>
  pull(AreaTypeID)

indicator_df <- map_dfr(
  areatype_ids,
  \(areatype_id) {
    message("Downloading AreaTypeID: ", areatype_id)

    fingertips_data(
      IndicatorID = indicator_id,
      AreaTypeID = areatype_id,
      categorytype = TRUE,
      proxy_settings = "none"
    )
  }
)


# 7. Save the downloaded data --------------------------------------------------
# Automatically writes to your current working directory (you can view your
# working directory using getwd(), or change it using setwd()

write_csv(indicator_df, paste0(indicator_id,".csv"))
