# ****************************************
# Missingness Handling Simulation Study
#
# Process tables
# Generates more readable summaries of data tables from the simulation
# Make the final CSV's ready for conversion to .tex by tably
#
# Emma Tarmey
#
# Started:          15/12/2025
# Most Recent Edit: 18/12/2025
# ****************************************



# ----- Setup -----

# clear R memory
rm(list=ls())

# fix wd issue
# forces wd to be the location of this file
if (Sys.getenv("RSTUDIO") == "1") {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}

library(tidyverse)

#turn off scientific notation for all variables
options(scipen=999) 

missingness_mechanisms <- c("FULL", "MNAR", "MCAR")
true_causal_effect     <- 0.50
total_confounders      <- 32
open_backdoor_paths    <- rep(total_confounders, length.out = length(missingness_mechanisms))
  
scenario_1_FULL_data        <- read.csv("data/missingness_sim_scenario_1_missingness_method_CCA_FULL_results.csv", row.names = 1)
scenario_1_MNAR_data        <- read.csv("data/missingness_sim_scenario_1_missingness_method_CCA_MNAR_results.csv", row.names = 1)
scenario_1_MCAR_data        <- read.csv("data/missingness_sim_scenario_1_missingness_method_CCA_MCAR_results.csv", row.names = 1)
scenario_1_sample_size_data <- read.csv("data/missingness_sim_scenario_1_missingness_method_CCA_sample_size_table.csv",  row.names = 1)

scenario_2_FULL_data        <- read.csv("data/missingness_sim_scenario_2_missingness_method_naive_MI_FULL_results.csv", row.names = 1)
scenario_2_MNAR_data        <- read.csv("data/missingness_sim_scenario_2_missingness_method_naive_MI_MNAR_results.csv", row.names = 1)
scenario_2_MCAR_data        <- read.csv("data/missingness_sim_scenario_2_missingness_method_naive_MI_MCAR_results.csv", row.names = 1)
scenario_2_sample_size_data <- read.csv("data/missingness_sim_scenario_2_missingness_method_naive_MI_sample_size_table.csv",  row.names = 1)

scenario_3_FULL_data        <- read.csv("data/missingness_sim_scenario_3_missingness_method_stacked_MI_FULL_results.csv", row.names = 1)
scenario_3_MNAR_data        <- read.csv("data/missingness_sim_scenario_3_missingness_method_stacked_MI_MNAR_results.csv", row.names = 1)
scenario_3_MCAR_data        <- read.csv("data/missingness_sim_scenario_3_missingness_method_stacked_MI_MCAR_results.csv", row.names = 1)
scenario_3_sample_size_data <- read.csv("data/missingness_sim_scenario_3_missingness_method_stacked_MI_sample_size_table.csv",  row.names = 1)

scenario_4_FULL_data        <- read.csv("data/missingness_sim_scenario_4_missingness_method_indicator_encoding_FULL_results.csv", row.names = 1)
scenario_4_MNAR_data        <- read.csv("data/missingness_sim_scenario_4_missingness_method_indicator_encoding_MNAR_results.csv", row.names = 1)
scenario_4_MCAR_data        <- read.csv("data/missingness_sim_scenario_4_missingness_method_indicator_encoding_MCAR_results.csv", row.names = 1)
scenario_4_sample_size_data <- read.csv("data/missingness_sim_scenario_4_missingness_method_indicator_encoding_sample_size_table.csv",  row.names = 1)

scenario_5_FULL_data        <- read.csv("data/missingness_sim_scenario_5_missingness_method_dichotomize_FULL_results.csv", row.names = 1)
scenario_5_MNAR_data        <- read.csv("data/missingness_sim_scenario_5_missingness_method_dichotomize_MNAR_results.csv", row.names = 1)
scenario_5_MCAR_data        <- read.csv("data/missingness_sim_scenario_5_missingness_method_dichotomize_MCAR_results.csv", row.names = 1)
scenario_5_sample_size_data <- read.csv("data/missingness_sim_scenario_5_missingness_method_dichotomize_sample_size_table.csv",  row.names = 1)



# ----- Generate scenario_1 Tables -----

sample_size_before <- get(  "scenario_1_sample_size_data")[, "complete_cases"] %>% as.integer()
sample_size_after  <- get(  "scenario_1_sample_size_data")[, "sample_size_after_handling"] %>% as.integer()


# causal effect estimation table

fully_adjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"]
}

fully_adjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"] / true_causal_effect
}

fully_adjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_coverage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_coverage"]
}

unadjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"]
}

unadjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"] / true_causal_effect
}

unadjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_coverage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_coverage"]
}

two_step_lasso_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"]
}

two_step_lasso_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"] / true_causal_effect
}

two_step_lasso_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_coverage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_coverage"]
}

two_step_lasso_X_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"]
}

two_step_lasso_X_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"] / true_causal_effect
}

two_step_lasso_X_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_coverage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_coverage"]
}

two_step_lasso_union_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"]
}

two_step_lasso_union_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"] / true_causal_effect
}

two_step_lasso_union_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_coverage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_coverage"]
}

scenario_1_causal_estimation_table_first_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  fully_adjusted_bias,
  fully_adjusted_bias_percentage,
  fully_adjusted_coverage,
  unadjusted_bias,
  unadjusted_bias_percentage,
  unadjusted_coverage,
  two_step_lasso_bias,
  two_step_lasso_bias_percentage,
  two_step_lasso_coverage
)

scenario_1_causal_estimation_table_second_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  two_step_lasso_X_bias,
  two_step_lasso_X_bias_percentage,
  two_step_lasso_X_coverage,
  two_step_lasso_union_bias,
  two_step_lasso_union_bias_percentage,
  two_step_lasso_union_coverage
)

print(scenario_1_causal_estimation_table_first_half)
print(scenario_1_causal_estimation_table_second_half)



# open backdoor pathways table

fully_adjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"]
}

fully_adjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"] / total_confounders
}

unadjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"]
}

unadjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"] / total_confounders
}

two_step_lasso_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"]
}

two_step_lasso_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"] / total_confounders
}

two_step_lasso_X_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"]
}

two_step_lasso_X_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"] / total_confounders
}

two_step_lasso_union_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"]
}

two_step_lasso_union_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked_percentage[i] <- get( paste0( "scenario_1_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"] / total_confounders
}

scenario_1_backdoor_pathways <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  open_backdoor_paths,
  fully_adjusted_blocked,
  fully_adjusted_blocked_percentage,
  unadjusted_blocked,
  unadjusted_blocked_percentage,
  two_step_lasso_blocked,
  two_step_lasso_blocked_percentage,
  two_step_lasso_X_blocked,
  two_step_lasso_X_blocked_percentage,
  two_step_lasso_union_blocked,
  two_step_lasso_union_blocked_percentage
)

print(scenario_1_backdoor_pathways)



# ----- Generate scenario_2 Tables -----

sample_size_before <- get(  "scenario_2_sample_size_data")[, "complete_cases"] %>% as.integer()
sample_size_after  <- get(  "scenario_2_sample_size_data")[, "sample_size_after_handling"] %>% as.integer()


# causal effect estimation table

fully_adjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"]
}

fully_adjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"] / true_causal_effect
}

fully_adjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_coverage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_coverage"]
}

unadjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"]
}

unadjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"] / true_causal_effect
}

unadjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_coverage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_coverage"]
}

two_step_lasso_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"]
}

two_step_lasso_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"] / true_causal_effect
}

two_step_lasso_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_coverage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_coverage"]
}

two_step_lasso_X_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"]
}

two_step_lasso_X_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"] / true_causal_effect
}

two_step_lasso_X_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_coverage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_coverage"]
}

two_step_lasso_union_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"]
}

two_step_lasso_union_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"] / true_causal_effect
}

two_step_lasso_union_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_coverage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_coverage"]
}

scenario_2_causal_estimation_table_first_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  fully_adjusted_bias,
  fully_adjusted_bias_percentage,
  fully_adjusted_coverage,
  unadjusted_bias,
  unadjusted_bias_percentage,
  unadjusted_coverage,
  two_step_lasso_bias,
  two_step_lasso_bias_percentage,
  two_step_lasso_coverage
)

scenario_2_causal_estimation_table_second_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  two_step_lasso_X_bias,
  two_step_lasso_X_bias_percentage,
  two_step_lasso_X_coverage,
  two_step_lasso_union_bias,
  two_step_lasso_union_bias_percentage,
  two_step_lasso_union_coverage
)

print(scenario_2_causal_estimation_table_first_half)
print(scenario_2_causal_estimation_table_second_half)



# open backdoor pathways table

fully_adjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"]
}

fully_adjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"] / total_confounders
}

unadjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"]
}

unadjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"] / total_confounders
}

two_step_lasso_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"]
}

two_step_lasso_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"] / total_confounders
}

two_step_lasso_X_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"]
}

two_step_lasso_X_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"] / total_confounders
}

two_step_lasso_union_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"]
}

two_step_lasso_union_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked_percentage[i] <- get( paste0( "scenario_2_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"] / total_confounders
}

scenario_2_backdoor_pathways <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  open_backdoor_paths,
  fully_adjusted_blocked,
  fully_adjusted_blocked_percentage,
  unadjusted_blocked,
  unadjusted_blocked_percentage,
  two_step_lasso_blocked,
  two_step_lasso_blocked_percentage,
  two_step_lasso_X_blocked,
  two_step_lasso_X_blocked_percentage,
  two_step_lasso_union_blocked,
  two_step_lasso_union_blocked_percentage
)

print(scenario_2_backdoor_pathways)



# ----- Generate scenario_3 Tables -----

sample_size_before <- get(  "scenario_3_sample_size_data")[, "complete_cases"] %>% as.integer()
sample_size_after  <- get(  "scenario_3_sample_size_data")[, "sample_size_after_handling"] %>% as.integer()


# causal effect estimation table

fully_adjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"]
}

fully_adjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"] / true_causal_effect
}

fully_adjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_coverage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_coverage"]
}

unadjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"]
}

unadjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"] / true_causal_effect
}

unadjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_coverage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_coverage"]
}

two_step_lasso_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"]
}

two_step_lasso_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"] / true_causal_effect
}

two_step_lasso_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_coverage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_coverage"]
}

two_step_lasso_X_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"]
}

two_step_lasso_X_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"] / true_causal_effect
}

two_step_lasso_X_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_coverage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_coverage"]
}

two_step_lasso_union_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"]
}

two_step_lasso_union_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"] / true_causal_effect
}

two_step_lasso_union_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_coverage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_coverage"]
}

scenario_3_causal_estimation_table_first_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  fully_adjusted_bias,
  fully_adjusted_bias_percentage,
  fully_adjusted_coverage,
  unadjusted_bias,
  unadjusted_bias_percentage,
  unadjusted_coverage,
  two_step_lasso_bias,
  two_step_lasso_bias_percentage,
  two_step_lasso_coverage
)

scenario_3_causal_estimation_table_second_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  two_step_lasso_X_bias,
  two_step_lasso_X_bias_percentage,
  two_step_lasso_X_coverage,
  two_step_lasso_union_bias,
  two_step_lasso_union_bias_percentage,
  two_step_lasso_union_coverage
)

print(scenario_3_causal_estimation_table_first_half)
print(scenario_3_causal_estimation_table_second_half)



# open backdoor pathways table

fully_adjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"]
}

fully_adjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"] / total_confounders
}

unadjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"]
}

unadjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"] / total_confounders
}

two_step_lasso_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"]
}

two_step_lasso_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"] / total_confounders
}

two_step_lasso_X_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"]
}

two_step_lasso_X_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"] / total_confounders
}

two_step_lasso_union_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"]
}

two_step_lasso_union_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked_percentage[i] <- get( paste0( "scenario_3_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"] / total_confounders
}

scenario_3_backdoor_pathways <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  open_backdoor_paths,
  fully_adjusted_blocked,
  fully_adjusted_blocked_percentage,
  unadjusted_blocked,
  unadjusted_blocked_percentage,
  two_step_lasso_blocked,
  two_step_lasso_blocked_percentage,
  two_step_lasso_X_blocked,
  two_step_lasso_X_blocked_percentage,
  two_step_lasso_union_blocked,
  two_step_lasso_union_blocked_percentage
)

print(scenario_3_backdoor_pathways)



# ----- Generate scenario_4 Tables -----

sample_size_before <- get(  "scenario_4_sample_size_data")[, "complete_cases"] %>% as.integer()
sample_size_after  <- get(  "scenario_4_sample_size_data")[, "sample_size_after_handling"] %>% as.integer()


# causal effect estimation table

fully_adjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"]
}

fully_adjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"] / true_causal_effect
}

fully_adjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_coverage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_coverage"]
}

unadjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"]
}

unadjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"] / true_causal_effect
}

unadjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_coverage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_coverage"]
}

two_step_lasso_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"]
}

two_step_lasso_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"] / true_causal_effect
}

two_step_lasso_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_coverage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_coverage"]
}

two_step_lasso_X_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"]
}

two_step_lasso_X_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"] / true_causal_effect
}

two_step_lasso_X_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_coverage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_coverage"]
}

two_step_lasso_union_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"]
}

two_step_lasso_union_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"] / true_causal_effect
}

two_step_lasso_union_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_coverage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_coverage"]
}

scenario_4_causal_estimation_table_first_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  fully_adjusted_bias,
  fully_adjusted_bias_percentage,
  fully_adjusted_coverage,
  unadjusted_bias,
  unadjusted_bias_percentage,
  unadjusted_coverage,
  two_step_lasso_bias,
  two_step_lasso_bias_percentage,
  two_step_lasso_coverage
)

scenario_4_causal_estimation_table_second_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  two_step_lasso_X_bias,
  two_step_lasso_X_bias_percentage,
  two_step_lasso_X_coverage,
  two_step_lasso_union_bias,
  two_step_lasso_union_bias_percentage,
  two_step_lasso_union_coverage
)

print(scenario_4_causal_estimation_table_first_half)
print(scenario_4_causal_estimation_table_second_half)



# open backdoor pathways table

fully_adjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"]
}

fully_adjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"] / total_confounders
}

unadjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"]
}

unadjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"] / total_confounders
}

two_step_lasso_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"]
}

two_step_lasso_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"] / total_confounders
}

two_step_lasso_X_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"]
}

two_step_lasso_X_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"] / total_confounders
}

two_step_lasso_union_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"]
}

two_step_lasso_union_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked_percentage[i] <- get( paste0( "scenario_4_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"] / total_confounders
}

scenario_4_backdoor_pathways <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  open_backdoor_paths,
  fully_adjusted_blocked,
  fully_adjusted_blocked_percentage,
  unadjusted_blocked,
  unadjusted_blocked_percentage,
  two_step_lasso_blocked,
  two_step_lasso_blocked_percentage,
  two_step_lasso_X_blocked,
  two_step_lasso_X_blocked_percentage,
  two_step_lasso_union_blocked,
  two_step_lasso_union_blocked_percentage
)

print(scenario_4_backdoor_pathways)


# ----- Generate scenario_5 Tables -----

sample_size_before <- get(  "scenario_5_sample_size_data")[, "complete_cases"] %>% as.integer()
sample_size_after  <- get(  "scenario_5_sample_size_data")[, "sample_size_after_handling"] %>% as.integer()


# causal effect estimation table

fully_adjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"]
}

fully_adjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_bias_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_bias"] / true_causal_effect
}

fully_adjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_coverage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["fully_adjusted", "causal_coverage"]
}

unadjusted_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"]
}

unadjusted_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_bias_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_bias"] / true_causal_effect
}

unadjusted_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_coverage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["unadjusted", "causal_coverage"]
}

two_step_lasso_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"]
}

two_step_lasso_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_bias_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_bias"] / true_causal_effect
}

two_step_lasso_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_coverage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso", "causal_coverage"]
}

two_step_lasso_X_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"]
}

two_step_lasso_X_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_bias_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_bias"] / true_causal_effect
}

two_step_lasso_X_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_coverage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "causal_coverage"]
}

two_step_lasso_union_bias  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"]
}

two_step_lasso_union_bias_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_bias_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_bias"] / true_causal_effect
}

two_step_lasso_union_coverage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_coverage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "causal_coverage"]
}

scenario_5_causal_estimation_table_first_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  fully_adjusted_bias,
  fully_adjusted_bias_percentage,
  fully_adjusted_coverage,
  unadjusted_bias,
  unadjusted_bias_percentage,
  unadjusted_coverage,
  two_step_lasso_bias,
  two_step_lasso_bias_percentage,
  two_step_lasso_coverage
)

scenario_5_causal_estimation_table_second_half <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  two_step_lasso_X_bias,
  two_step_lasso_X_bias_percentage,
  two_step_lasso_X_coverage,
  two_step_lasso_union_bias,
  two_step_lasso_union_bias_percentage,
  two_step_lasso_union_coverage
)

print(scenario_5_causal_estimation_table_first_half)
print(scenario_5_causal_estimation_table_second_half)



# open backdoor pathways table

fully_adjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"]
}

fully_adjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  fully_adjusted_blocked_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["fully_adjusted", "blocked_paths"] / total_confounders
}

unadjusted_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"]
}

unadjusted_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  unadjusted_blocked_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["unadjusted", "blocked_paths"] / total_confounders
}

two_step_lasso_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"]
}

two_step_lasso_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_blocked_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso", "blocked_paths"] / total_confounders
}

two_step_lasso_X_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"]
}

two_step_lasso_X_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_X_blocked_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_X", "blocked_paths"] / total_confounders
}

two_step_lasso_union_blocked  <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"]
}

two_step_lasso_union_blocked_percentage <- c()
for (i in 1:length(missingness_mechanisms)) {
  two_step_lasso_union_blocked_percentage[i] <- get( paste0( "scenario_5_", missingness_mechanisms[i], "_data"))["two_step_lasso_union", "blocked_paths"] / total_confounders
}

scenario_5_backdoor_pathways <- data.frame(
  missingness_mechanisms,
  sample_size_before,
  sample_size_after,
  open_backdoor_paths,
  fully_adjusted_blocked,
  fully_adjusted_blocked_percentage,
  unadjusted_blocked,
  unadjusted_blocked_percentage,
  two_step_lasso_blocked,
  two_step_lasso_blocked_percentage,
  two_step_lasso_X_blocked,
  two_step_lasso_X_blocked_percentage,
  two_step_lasso_union_blocked,
  two_step_lasso_union_blocked_percentage
)

print(scenario_5_backdoor_pathways)



# ----- Rounding to 6 s.f. -----

scenario_1_causal_estimation_table_first_half[, -c(1)]  <- signif(scenario_1_causal_estimation_table_first_half[, -c(1)],  6)
scenario_1_causal_estimation_table_second_half[, -c(1)] <- signif(scenario_1_causal_estimation_table_second_half[, -c(1)], 6)
scenario_1_backdoor_pathways[, -c(1)]                   <- signif(scenario_1_backdoor_pathways[, -c(1)],                   6)

scenario_2_causal_estimation_table_first_half[, -c(1)]  <- signif(scenario_2_causal_estimation_table_first_half[, -c(1)],  6)
scenario_2_causal_estimation_table_second_half[, -c(1)] <- signif(scenario_2_causal_estimation_table_second_half[, -c(1)], 6)
scenario_2_backdoor_pathways[, -c(1)]                   <- signif(scenario_2_backdoor_pathways[, -c(1)],                   6)

scenario_3_causal_estimation_table_first_half[, -c(1)]  <- signif(scenario_3_causal_estimation_table_first_half[, -c(1)],  6)
scenario_3_causal_estimation_table_second_half[, -c(1)] <- signif(scenario_3_causal_estimation_table_second_half[, -c(1)], 6)
scenario_3_backdoor_pathways[, -c(1)]                   <- signif(scenario_3_backdoor_pathways[, -c(1)],                   6)

scenario_4_causal_estimation_table_first_half[, -c(1)]  <- signif(scenario_4_causal_estimation_table_first_half[, -c(1)],  6)
scenario_4_causal_estimation_table_second_half[, -c(1)] <- signif(scenario_4_causal_estimation_table_second_half[, -c(1)], 6)
scenario_4_backdoor_pathways[, -c(1)]                   <- signif(scenario_4_backdoor_pathways[, -c(1)],                   6)

scenario_5_causal_estimation_table_first_half[, -c(1)]  <- signif(scenario_5_causal_estimation_table_first_half[, -c(1)],  6)
scenario_5_causal_estimation_table_second_half[, -c(1)] <- signif(scenario_5_causal_estimation_table_second_half[, -c(1)], 6)
scenario_5_backdoor_pathways[, -c(1)]                   <- signif(scenario_5_backdoor_pathways[, -c(1)],                   6)



# # ----- Convert to character to preserve format -----
# 
# scenario_1_causal_estimation_table_first_half  <- (scenario_1_causal_estimation_table_first_half  %>% dplyr::mutate_all(as.character))
# scenario_1_causal_estimation_table_second_half <- (scenario_1_causal_estimation_table_second_half %>% dplyr::mutate_all(as.character))
# scenario_1_backdoor_pathways                   <- (scenario_1_backdoor_pathways                   %>% dplyr::mutate_all(as.character))
# 
# scenario_2_causal_estimation_table_first_half  <- (scenario_2_causal_estimation_table_first_half  %>% dplyr::mutate_all(as.character))
# scenario_2_causal_estimation_table_second_half <- (scenario_2_causal_estimation_table_second_half %>% dplyr::mutate_all(as.character))
# scenario_2_backdoor_pathways                   <- (scenario_2_backdoor_pathways                   %>% dplyr::mutate_all(as.character))
# 
# scenario_3_causal_estimation_table_first_half  <- (scenario_3_causal_estimation_table_first_half  %>% dplyr::mutate_all(as.character))
# scenario_3_causal_estimation_table_second_half <- (scenario_3_causal_estimation_table_second_half %>% dplyr::mutate_all(as.character))
# scenario_3_backdoor_pathways                   <- (scenario_3_backdoor_pathways                   %>% dplyr::mutate_all(as.character))
# 
# scenario_4_causal_estimation_table_first_half  <- (scenario_4_causal_estimation_table_first_half  %>% dplyr::mutate_all(as.character))
# scenario_4_causal_estimation_table_second_half <- (scenario_4_causal_estimation_table_second_half %>% dplyr::mutate_all(as.character))
# scenario_4_backdoor_pathways                   <- (scenario_4_backdoor_pathways                   %>% dplyr::mutate_all(as.character))
# 
# scenario_5_causal_estimation_table_first_half  <- (scenario_5_causal_estimation_table_first_half  %>% dplyr::mutate_all(as.character))
# scenario_5_causal_estimation_table_second_half <- (scenario_5_causal_estimation_table_second_half %>% dplyr::mutate_all(as.character))
# scenario_5_backdoor_pathways                   <- (scenario_5_backdoor_pathways                   %>% dplyr::mutate_all(as.character))



# ----- Save results -----

write.csv(scenario_1_causal_estimation_table_first_half,  "temp/scenario_1_causal_estimation_first_half.csv",  row.names = FALSE)
write.csv(scenario_1_causal_estimation_table_second_half, "temp/scenario_1_causal_estimation_second_half.csv", row.names = FALSE)
write.csv(scenario_1_backdoor_pathways,                   "temp/scenario_1_backdoor_pathways.csv",             row.names = FALSE)

write.csv(scenario_2_causal_estimation_table_first_half,  "temp/scenario_2_causal_estimation_first_half.csv",  row.names = FALSE)
write.csv(scenario_2_causal_estimation_table_second_half, "temp/scenario_2_causal_estimation_second_half.csv", row.names = FALSE)
write.csv(scenario_2_backdoor_pathways,                   "temp/scenario_2_backdoor_pathways.csv",             row.names = FALSE)

write.csv(scenario_3_causal_estimation_table_first_half,  "temp/scenario_3_causal_estimation_first_half.csv",  row.names = FALSE)
write.csv(scenario_3_causal_estimation_table_second_half, "temp/scenario_3_causal_estimation_second_half.csv", row.names = FALSE)
write.csv(scenario_3_backdoor_pathways,                   "temp/scenario_3_backdoor_pathways.csv",             row.names = FALSE)

write.csv(scenario_4_causal_estimation_table_first_half,  "temp/scenario_4_causal_estimation_first_half.csv",  row.names = FALSE)
write.csv(scenario_4_causal_estimation_table_second_half, "temp/scenario_4_causal_estimation_second_half.csv", row.names = FALSE)
write.csv(scenario_4_backdoor_pathways,                   "temp/scenario_4_backdoor_pathways.csv",             row.names = FALSE)

write.csv(scenario_5_causal_estimation_table_first_half,  "temp/scenario_5_causal_estimation_first_half.csv",  row.names = FALSE)
write.csv(scenario_5_causal_estimation_table_second_half, "temp/scenario_5_causal_estimation_second_half.csv", row.names = FALSE)
write.csv(scenario_5_backdoor_pathways,                   "temp/scenario_5_backdoor_pathways.csv",             row.names = FALSE)


# reset scientific notation
options(scipen=0)

