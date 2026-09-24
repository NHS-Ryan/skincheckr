# skincheckr

This repository is for a university course and outputs a model assessing the impact of fairness of skin and age on number of referrals for skin cancer screening by PCNs. Based on this model output a .svg is created that highlights the top 10 under referring PCNs for suspected skin cancer. 

## Orchestration

1. Run fingertips_indicator_exploration_tools.R - You need indicators 1679,91351 & 93468 which are taken from the PHE Fingertips database via their provided API. These will be saved in your working directory as .csvs. 
2. Run model_data_prep.R - this takes the saved .csvs and does the ETL required to run the model
3. Run referral_prediction_model.R - this creaes the model.


## Visualisations
1. under_referrer_map.R - final output of top 10 under referring PCNs
2. costs_bubble_chart.R - gives a bubble chart of costs as defined by costs.csv in this repo
3. costs_chart_line_graph.R - gives a line graph of costs over time including revenue and costs. Note that costs are hard coded and not derived from costs.csv. 
