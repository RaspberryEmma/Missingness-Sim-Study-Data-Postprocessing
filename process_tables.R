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
# Most Recent Edit: 23/03/2026
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

scenario_1_MAR_data   <- read.csv("data/scenario_1/method_CCA_MAR_results.csv", row.names = 1)
scenario_1_MCAR_data  <- read.csv("data/scenario_1/method_CCA_MCAR_results.csv", row.names = 1)
scenario_1_MNAR_data  <- read.csv("data/scenario_1/method_CCA_MNAR_results.csv", row.names = 1)

scenario_2_MAR_data   <- read.csv("data/scenario_2/method_within_MI_MAR_results.csv", row.names = 1)
scenario_2_MCAR_data  <- read.csv("data/scenario_2/method_within_MI_MCAR_results.csv", row.names = 1)
scenario_2_MNAR_data  <- read.csv("data/scenario_2/method_within_MI_MNAR_results.csv", row.names = 1)

scenario_3_MAR_data   <- read.csv("data/scenario_3/method_across_MI_MAR_results.csv", row.names = 1)
scenario_3_MCAR_data  <- read.csv("data/scenario_3/method_across_MI_MCAR_results.csv", row.names = 1)
scenario_3_MNAR_data  <- read.csv("data/scenario_3/method_across_MI_MNAR_results.csv", row.names = 1)

scenario_4_MAR_data   <- read.csv("data/scenario_4/method_indicator_encoding_MAR_results.csv", row.names = 1)
scenario_4_MCAR_data  <- read.csv("data/scenario_4/method_indicator_encoding_MCAR_results.csv", row.names = 1)
scenario_4_MNAR_data  <- read.csv("data/scenario_4/method_indicator_encoding_MNAR_results.csv", row.names = 1)

scenario_5_MAR_data   <- read.csv("data/scenario_5/method_dichotomize_MAR_results.csv", row.names = 1)
scenario_5_MCAR_data  <- read.csv("data/scenario_5/method_dichotomize_MCAR_results.csv", row.names = 1)
scenario_5_MNAR_data  <- read.csv("data/scenario_5/method_dichotomize_MNAR_results.csv", row.names = 1)


# ----- Form MAR Table -----

MAR_variable_selection_methods   <- c(rep("Fully-adjusted", times = 5),
                                      rep("Unadjusted", times = 5),
                                      rep("Lasso", times = 5),
                                      rep("Exposure-Lasso", times = 5),
                                      rep("Union-Lasso", times = 5))
MAR_missingness_handling_methods <- rep(c("CCA", "Within-MI", "Across-MI", "Indicator-encoding", "Dichotomizing"), times = 5)

MAR_causal_estimate <- c(scenario_1_MAR_data["fully_adjusted", "causal_estimate"],
                         scenario_2_MAR_data["fully_adjusted", "causal_estimate"],
                         scenario_3_MAR_data["fully_adjusted", "causal_estimate"],
                         scenario_4_MAR_data["fully_adjusted", "causal_estimate"],
                         scenario_5_MAR_data["fully_adjusted", "causal_estimate"],
                         
                         scenario_1_MAR_data["unadjusted", "causal_estimate"],
                         scenario_2_MAR_data["unadjusted", "causal_estimate"],
                         scenario_3_MAR_data["unadjusted", "causal_estimate"],
                         scenario_4_MAR_data["unadjusted", "causal_estimate"],
                         scenario_5_MAR_data["unadjusted", "causal_estimate"],
                         
                         scenario_1_MAR_data["two_step_lasso", "causal_estimate"],
                         scenario_2_MAR_data["two_step_lasso", "causal_estimate"],
                         scenario_3_MAR_data["two_step_lasso", "causal_estimate"],
                         scenario_4_MAR_data["two_step_lasso", "causal_estimate"],
                         scenario_5_MAR_data["two_step_lasso", "causal_estimate"],
                         
                         scenario_1_MAR_data["two_step_lasso_X", "causal_estimate"],
                         scenario_2_MAR_data["two_step_lasso_X", "causal_estimate"],
                         scenario_3_MAR_data["two_step_lasso_X", "causal_estimate"],
                         scenario_4_MAR_data["two_step_lasso_X", "causal_estimate"],
                         scenario_5_MAR_data["two_step_lasso_X", "causal_estimate"],
                         
                         scenario_1_MAR_data["two_step_lasso_union", "causal_estimate"],
                         scenario_2_MAR_data["two_step_lasso_union", "causal_estimate"],
                         scenario_3_MAR_data["two_step_lasso_union", "causal_estimate"],
                         scenario_4_MAR_data["two_step_lasso_union", "causal_estimate"],
                         scenario_5_MAR_data["two_step_lasso_union", "causal_estimate"]
                        ) %>% as.numeric()

MAR_bias               <- c(scenario_1_MAR_data["fully_adjusted", "causal_bias"],
                            scenario_2_MAR_data["fully_adjusted", "causal_bias"],
                            scenario_3_MAR_data["fully_adjusted", "causal_bias"],
                            scenario_4_MAR_data["fully_adjusted", "causal_bias"],
                            scenario_5_MAR_data["fully_adjusted", "causal_bias"],
                            
                            scenario_1_MAR_data["unadjusted", "causal_bias"],
                            scenario_2_MAR_data["unadjusted", "causal_bias"],
                            scenario_3_MAR_data["unadjusted", "causal_bias"],
                            scenario_4_MAR_data["unadjusted", "causal_bias"],
                            scenario_5_MAR_data["unadjusted", "causal_bias"],
                            
                            scenario_1_MAR_data["two_step_lasso", "causal_bias"],
                            scenario_2_MAR_data["two_step_lasso", "causal_bias"],
                            scenario_3_MAR_data["two_step_lasso", "causal_bias"],
                            scenario_4_MAR_data["two_step_lasso", "causal_bias"],
                            scenario_5_MAR_data["two_step_lasso", "causal_bias"],
                            
                            scenario_1_MAR_data["two_step_lasso_X", "causal_bias"],
                            scenario_2_MAR_data["two_step_lasso_X", "causal_bias"],
                            scenario_3_MAR_data["two_step_lasso_X", "causal_bias"],
                            scenario_4_MAR_data["two_step_lasso_X", "causal_bias"],
                            scenario_5_MAR_data["two_step_lasso_X", "causal_bias"],
                            
                            scenario_1_MAR_data["two_step_lasso_union", "causal_bias"],
                            scenario_2_MAR_data["two_step_lasso_union", "causal_bias"],
                            scenario_3_MAR_data["two_step_lasso_union", "causal_bias"],
                            scenario_4_MAR_data["two_step_lasso_union", "causal_bias"],
                            scenario_5_MAR_data["two_step_lasso_union", "causal_bias"]
                            ) %>% as.numeric()

MAR_bias_as_proportion <- c(scenario_1_MAR_data["fully_adjusted", "causal_bias_proportion"],
                            scenario_2_MAR_data["fully_adjusted", "causal_bias_proportion"],
                            scenario_3_MAR_data["fully_adjusted", "causal_bias_proportion"],
                            scenario_4_MAR_data["fully_adjusted", "causal_bias_proportion"],
                            scenario_5_MAR_data["fully_adjusted", "causal_bias_proportion"],
                            
                            scenario_1_MAR_data["unadjusted", "causal_bias_proportion"],
                            scenario_2_MAR_data["unadjusted", "causal_bias_proportion"],
                            scenario_3_MAR_data["unadjusted", "causal_bias_proportion"],
                            scenario_4_MAR_data["unadjusted", "causal_bias_proportion"],
                            scenario_5_MAR_data["unadjusted", "causal_bias_proportion"],
                            
                            scenario_1_MAR_data["two_step_lasso", "causal_bias_proportion"],
                            scenario_2_MAR_data["two_step_lasso", "causal_bias_proportion"],
                            scenario_3_MAR_data["two_step_lasso", "causal_bias_proportion"],
                            scenario_4_MAR_data["two_step_lasso", "causal_bias_proportion"],
                            scenario_5_MAR_data["two_step_lasso", "causal_bias_proportion"],
                            
                            scenario_1_MAR_data["two_step_lasso_X", "causal_bias_proportion"],
                            scenario_2_MAR_data["two_step_lasso_X", "causal_bias_proportion"],
                            scenario_3_MAR_data["two_step_lasso_X", "causal_bias_proportion"],
                            scenario_4_MAR_data["two_step_lasso_X", "causal_bias_proportion"],
                            scenario_5_MAR_data["two_step_lasso_X", "causal_bias_proportion"],
                            
                            scenario_1_MAR_data["two_step_lasso_union", "causal_bias_proportion"],
                            scenario_2_MAR_data["two_step_lasso_union", "causal_bias_proportion"],
                            scenario_3_MAR_data["two_step_lasso_union", "causal_bias_proportion"],
                            scenario_4_MAR_data["two_step_lasso_union", "causal_bias_proportion"],
                            scenario_5_MAR_data["two_step_lasso_union", "causal_bias_proportion"]
                            ) %>% as.numeric()

MAR_coverage           <- c(scenario_1_MAR_data["fully_adjusted", "causal_coverage"],
                            scenario_2_MAR_data["fully_adjusted", "causal_coverage"],
                            scenario_3_MAR_data["fully_adjusted", "causal_coverage"],
                            scenario_4_MAR_data["fully_adjusted", "causal_coverage"],
                            scenario_5_MAR_data["fully_adjusted", "causal_coverage"],
                            
                            scenario_1_MAR_data["unadjusted", "causal_coverage"],
                            scenario_2_MAR_data["unadjusted", "causal_coverage"],
                            scenario_3_MAR_data["unadjusted", "causal_coverage"],
                            scenario_4_MAR_data["unadjusted", "causal_coverage"],
                            scenario_5_MAR_data["unadjusted", "causal_coverage"],
                            
                            scenario_1_MAR_data["two_step_lasso", "causal_coverage"],
                            scenario_2_MAR_data["two_step_lasso", "causal_coverage"],
                            scenario_3_MAR_data["two_step_lasso", "causal_coverage"],
                            scenario_4_MAR_data["two_step_lasso", "causal_coverage"],
                            scenario_5_MAR_data["two_step_lasso", "causal_coverage"],
                            
                            scenario_1_MAR_data["two_step_lasso_X", "causal_coverage"],
                            scenario_2_MAR_data["two_step_lasso_X", "causal_coverage"],
                            scenario_3_MAR_data["two_step_lasso_X", "causal_coverage"],
                            scenario_4_MAR_data["two_step_lasso_X", "causal_coverage"],
                            scenario_5_MAR_data["two_step_lasso_X", "causal_coverage"],
                            
                            scenario_1_MAR_data["two_step_lasso_union", "causal_coverage"],
                            scenario_2_MAR_data["two_step_lasso_union", "causal_coverage"],
                            scenario_3_MAR_data["two_step_lasso_union", "causal_coverage"],
                            scenario_4_MAR_data["two_step_lasso_union", "causal_coverage"],
                            scenario_5_MAR_data["two_step_lasso_union", "causal_coverage"]
                            ) %>% as.numeric()

MAR_blocked_paths <- c(scenario_1_MAR_data["fully_adjusted", "blocked_paths"],
                       scenario_2_MAR_data["fully_adjusted", "blocked_paths"],
                       scenario_3_MAR_data["fully_adjusted", "blocked_paths"],
                       scenario_4_MAR_data["fully_adjusted", "blocked_paths"],
                       scenario_5_MAR_data["fully_adjusted", "blocked_paths"],
                       
                       scenario_1_MAR_data["unadjusted", "blocked_paths"],
                       scenario_2_MAR_data["unadjusted", "blocked_paths"],
                       scenario_3_MAR_data["unadjusted", "blocked_paths"],
                       scenario_4_MAR_data["unadjusted", "blocked_paths"],
                       scenario_5_MAR_data["unadjusted", "blocked_paths"],
                       
                       scenario_1_MAR_data["two_step_lasso", "blocked_paths"],
                       scenario_2_MAR_data["two_step_lasso", "blocked_paths"],
                       scenario_3_MAR_data["two_step_lasso", "blocked_paths"],
                       scenario_4_MAR_data["two_step_lasso", "blocked_paths"],
                       scenario_5_MAR_data["two_step_lasso", "blocked_paths"],
                       
                       scenario_1_MAR_data["two_step_lasso_X", "blocked_paths"],
                       scenario_2_MAR_data["two_step_lasso_X", "blocked_paths"],
                       scenario_3_MAR_data["two_step_lasso_X", "blocked_paths"],
                       scenario_4_MAR_data["two_step_lasso_X", "blocked_paths"],
                       scenario_5_MAR_data["two_step_lasso_X", "blocked_paths"],
                       
                       scenario_1_MAR_data["two_step_lasso_union", "blocked_paths"],
                       scenario_2_MAR_data["two_step_lasso_union", "blocked_paths"],
                       scenario_3_MAR_data["two_step_lasso_union", "blocked_paths"],
                       scenario_4_MAR_data["two_step_lasso_union", "blocked_paths"],
                       scenario_5_MAR_data["two_step_lasso_union", "blocked_paths"]
                      ) %>% as.numeric()

MAR_proportion_paths <- c(scenario_1_MAR_data["fully_adjusted", "proportion_paths"],
                          scenario_2_MAR_data["fully_adjusted", "proportion_paths"],
                          scenario_3_MAR_data["fully_adjusted", "proportion_paths"],
                          scenario_4_MAR_data["fully_adjusted", "proportion_paths"],
                          scenario_5_MAR_data["fully_adjusted", "proportion_paths"],
                          
                          scenario_1_MAR_data["unadjusted", "proportion_paths"],
                          scenario_2_MAR_data["unadjusted", "proportion_paths"],
                          scenario_3_MAR_data["unadjusted", "proportion_paths"],
                          scenario_4_MAR_data["unadjusted", "proportion_paths"],
                          scenario_5_MAR_data["unadjusted", "proportion_paths"],
                          
                          scenario_1_MAR_data["two_step_lasso", "proportion_paths"],
                          scenario_2_MAR_data["two_step_lasso", "proportion_paths"],
                          scenario_3_MAR_data["two_step_lasso", "proportion_paths"],
                          scenario_4_MAR_data["two_step_lasso", "proportion_paths"],
                          scenario_5_MAR_data["two_step_lasso", "proportion_paths"],
                          
                          scenario_1_MAR_data["two_step_lasso_X", "proportion_paths"],
                          scenario_2_MAR_data["two_step_lasso_X", "proportion_paths"],
                          scenario_3_MAR_data["two_step_lasso_X", "proportion_paths"],
                          scenario_4_MAR_data["two_step_lasso_X", "proportion_paths"],
                          scenario_5_MAR_data["two_step_lasso_X", "proportion_paths"],
                          
                          scenario_1_MAR_data["two_step_lasso_union", "proportion_paths"],
                          scenario_2_MAR_data["two_step_lasso_union", "proportion_paths"],
                          scenario_3_MAR_data["two_step_lasso_union", "proportion_paths"],
                          scenario_4_MAR_data["two_step_lasso_union", "proportion_paths"],
                          scenario_5_MAR_data["two_step_lasso_union", "proportion_paths"]
                          ) %>% as.numeric()

MAR_empirical_SE <- c(scenario_1_MAR_data["fully_adjusted", "empirical_SE"],
                      scenario_2_MAR_data["fully_adjusted", "empirical_SE"],
                      scenario_3_MAR_data["fully_adjusted", "empirical_SE"],
                      scenario_4_MAR_data["fully_adjusted", "empirical_SE"],
                      scenario_5_MAR_data["fully_adjusted", "empirical_SE"],
                      
                      scenario_1_MAR_data["unadjusted", "empirical_SE"],
                      scenario_2_MAR_data["unadjusted", "empirical_SE"],
                      scenario_3_MAR_data["unadjusted", "empirical_SE"],
                      scenario_4_MAR_data["unadjusted", "empirical_SE"],
                      scenario_5_MAR_data["unadjusted", "empirical_SE"],
                      
                      scenario_1_MAR_data["two_step_lasso", "empirical_SE"],
                      scenario_2_MAR_data["two_step_lasso", "empirical_SE"],
                      scenario_3_MAR_data["two_step_lasso", "empirical_SE"],
                      scenario_4_MAR_data["two_step_lasso", "empirical_SE"],
                      scenario_5_MAR_data["two_step_lasso", "empirical_SE"],
                      
                      scenario_1_MAR_data["two_step_lasso_X", "empirical_SE"],
                      scenario_2_MAR_data["two_step_lasso_X", "empirical_SE"],
                      scenario_3_MAR_data["two_step_lasso_X", "empirical_SE"],
                      scenario_4_MAR_data["two_step_lasso_X", "empirical_SE"],
                      scenario_5_MAR_data["two_step_lasso_X", "empirical_SE"],
                      
                      scenario_1_MAR_data["two_step_lasso_union", "empirical_SE"],
                      scenario_2_MAR_data["two_step_lasso_union", "empirical_SE"],
                      scenario_3_MAR_data["two_step_lasso_union", "empirical_SE"],
                      scenario_4_MAR_data["two_step_lasso_union", "empirical_SE"],
                      scenario_5_MAR_data["two_step_lasso_union", "empirical_SE"]
                      ) %>% as.numeric()

MAR_table <- cbind(MAR_variable_selection_methods,
                   MAR_missingness_handling_methods,
                   MAR_causal_estimate,
                   MAR_bias,
                   MAR_bias_as_proportion,
                   MAR_coverage,
                   MAR_blocked_paths,
                   MAR_proportion_paths,
                   MAR_empirical_SE) %>% as.data.frame()

colnames(MAR_table) <- c("Variable-Selection-Method",
                         "Missingness-Handling-Method",
                         "Causal-Effect-Estimate",
                         "Bias",
                         "Proportional-Bias",
                         "Coverage",
                         "Blocked-Paths",
                         "Proportion-Paths",
                         "Empirical-Standard-Error")


# ----- Form MCAR Table -----

MCAR_variable_selection_methods   <- c(rep("Fully-adjusted", times = 5),
                                       rep("Unadjusted", times = 5),
                                       rep("Lasso", times = 5),
                                       rep("Exposure-Lasso", times = 5),
                                       rep("Union-Lasso", times = 5))
MCAR_missingness_handling_methods <- rep(c("CCA", "Within-MI", "Across-MI", "Indicator-encoding", "Dichotomizing"), times = 5)

MCAR_causal_estimate <- c(scenario_1_MCAR_data["fully_adjusted", "causal_estimate"],
                          scenario_2_MCAR_data["fully_adjusted", "causal_estimate"],
                          scenario_3_MCAR_data["fully_adjusted", "causal_estimate"],
                          scenario_4_MCAR_data["fully_adjusted", "causal_estimate"],
                          scenario_5_MCAR_data["fully_adjusted", "causal_estimate"],
                          
                          scenario_1_MCAR_data["unadjusted", "causal_estimate"],
                          scenario_2_MCAR_data["unadjusted", "causal_estimate"],
                          scenario_3_MCAR_data["unadjusted", "causal_estimate"],
                          scenario_4_MCAR_data["unadjusted", "causal_estimate"],
                          scenario_5_MCAR_data["unadjusted", "causal_estimate"],
                          
                          scenario_1_MCAR_data["two_step_lasso", "causal_estimate"],
                          scenario_2_MCAR_data["two_step_lasso", "causal_estimate"],
                          scenario_3_MCAR_data["two_step_lasso", "causal_estimate"],
                          scenario_4_MCAR_data["two_step_lasso", "causal_estimate"],
                          scenario_5_MCAR_data["two_step_lasso", "causal_estimate"],
                          
                          scenario_1_MCAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_2_MCAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_3_MCAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_4_MCAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_5_MCAR_data["two_step_lasso_X", "causal_estimate"],
                          
                          scenario_1_MCAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_2_MCAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_3_MCAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_4_MCAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_5_MCAR_data["two_step_lasso_union", "causal_estimate"]
) %>% as.numeric()

MCAR_bias               <- c(scenario_1_MCAR_data["fully_adjusted", "causal_bias"],
                             scenario_2_MCAR_data["fully_adjusted", "causal_bias"],
                             scenario_3_MCAR_data["fully_adjusted", "causal_bias"],
                             scenario_4_MCAR_data["fully_adjusted", "causal_bias"],
                             scenario_5_MCAR_data["fully_adjusted", "causal_bias"],
                             
                             scenario_1_MCAR_data["unadjusted", "causal_bias"],
                             scenario_2_MCAR_data["unadjusted", "causal_bias"],
                             scenario_3_MCAR_data["unadjusted", "causal_bias"],
                             scenario_4_MCAR_data["unadjusted", "causal_bias"],
                             scenario_5_MCAR_data["unadjusted", "causal_bias"],
                             
                             scenario_1_MCAR_data["two_step_lasso", "causal_bias"],
                             scenario_2_MCAR_data["two_step_lasso", "causal_bias"],
                             scenario_3_MCAR_data["two_step_lasso", "causal_bias"],
                             scenario_4_MCAR_data["two_step_lasso", "causal_bias"],
                             scenario_5_MCAR_data["two_step_lasso", "causal_bias"],
                             
                             scenario_1_MCAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_2_MCAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_3_MCAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_4_MCAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_5_MCAR_data["two_step_lasso_X", "causal_bias"],
                             
                             scenario_1_MCAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_2_MCAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_3_MCAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_4_MCAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_5_MCAR_data["two_step_lasso_union", "causal_bias"]
) %>% as.numeric()

MCAR_bias_as_proportion <- c(scenario_1_MCAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_2_MCAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_3_MCAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_4_MCAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_5_MCAR_data["fully_adjusted", "causal_bias_proportion"],
                             
                             scenario_1_MCAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_2_MCAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_3_MCAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_4_MCAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_5_MCAR_data["unadjusted", "causal_bias_proportion"],
                             
                             scenario_1_MCAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_2_MCAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_3_MCAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_4_MCAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_5_MCAR_data["two_step_lasso", "causal_bias_proportion"],
                             
                             scenario_1_MCAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_2_MCAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_3_MCAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_4_MCAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_5_MCAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             
                             scenario_1_MCAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_2_MCAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_3_MCAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_4_MCAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_5_MCAR_data["two_step_lasso_union", "causal_bias_proportion"]
) %>% as.numeric()

MCAR_coverage           <- c(scenario_1_MCAR_data["fully_adjusted", "causal_coverage"],
                             scenario_2_MCAR_data["fully_adjusted", "causal_coverage"],
                             scenario_3_MCAR_data["fully_adjusted", "causal_coverage"],
                             scenario_4_MCAR_data["fully_adjusted", "causal_coverage"],
                             scenario_5_MCAR_data["fully_adjusted", "causal_coverage"],
                             
                             scenario_1_MCAR_data["unadjusted", "causal_coverage"],
                             scenario_2_MCAR_data["unadjusted", "causal_coverage"],
                             scenario_3_MCAR_data["unadjusted", "causal_coverage"],
                             scenario_4_MCAR_data["unadjusted", "causal_coverage"],
                             scenario_5_MCAR_data["unadjusted", "causal_coverage"],
                             
                             scenario_1_MCAR_data["two_step_lasso", "causal_coverage"],
                             scenario_2_MCAR_data["two_step_lasso", "causal_coverage"],
                             scenario_3_MCAR_data["two_step_lasso", "causal_coverage"],
                             scenario_4_MCAR_data["two_step_lasso", "causal_coverage"],
                             scenario_5_MCAR_data["two_step_lasso", "causal_coverage"],
                             
                             scenario_1_MCAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_2_MCAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_3_MCAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_4_MCAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_5_MCAR_data["two_step_lasso_X", "causal_coverage"],
                             
                             scenario_1_MCAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_2_MCAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_3_MCAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_4_MCAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_5_MCAR_data["two_step_lasso_union", "causal_coverage"]
) %>% as.numeric()

MCAR_blocked_paths <- c(scenario_1_MCAR_data["fully_adjusted", "blocked_paths"],
                        scenario_2_MCAR_data["fully_adjusted", "blocked_paths"],
                        scenario_3_MCAR_data["fully_adjusted", "blocked_paths"],
                        scenario_4_MCAR_data["fully_adjusted", "blocked_paths"],
                        scenario_5_MCAR_data["fully_adjusted", "blocked_paths"],
                        
                        scenario_1_MCAR_data["unadjusted", "blocked_paths"],
                        scenario_2_MCAR_data["unadjusted", "blocked_paths"],
                        scenario_3_MCAR_data["unadjusted", "blocked_paths"],
                        scenario_4_MCAR_data["unadjusted", "blocked_paths"],
                        scenario_5_MCAR_data["unadjusted", "blocked_paths"],
                        
                        scenario_1_MCAR_data["two_step_lasso", "blocked_paths"],
                        scenario_2_MCAR_data["two_step_lasso", "blocked_paths"],
                        scenario_3_MCAR_data["two_step_lasso", "blocked_paths"],
                        scenario_4_MCAR_data["two_step_lasso", "blocked_paths"],
                        scenario_5_MCAR_data["two_step_lasso", "blocked_paths"],
                        
                        scenario_1_MCAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_2_MCAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_3_MCAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_4_MCAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_5_MCAR_data["two_step_lasso_X", "blocked_paths"],
                        
                        scenario_1_MCAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_2_MCAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_3_MCAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_4_MCAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_5_MCAR_data["two_step_lasso_union", "blocked_paths"]
) %>% as.numeric()

MCAR_proportion_paths <- c(scenario_1_MCAR_data["fully_adjusted", "proportion_paths"],
                           scenario_2_MCAR_data["fully_adjusted", "proportion_paths"],
                           scenario_3_MCAR_data["fully_adjusted", "proportion_paths"],
                           scenario_4_MCAR_data["fully_adjusted", "proportion_paths"],
                           scenario_5_MCAR_data["fully_adjusted", "proportion_paths"],
                           
                           scenario_1_MCAR_data["unadjusted", "proportion_paths"],
                           scenario_2_MCAR_data["unadjusted", "proportion_paths"],
                           scenario_3_MCAR_data["unadjusted", "proportion_paths"],
                           scenario_4_MCAR_data["unadjusted", "proportion_paths"],
                           scenario_5_MCAR_data["unadjusted", "proportion_paths"],
                           
                           scenario_1_MCAR_data["two_step_lasso", "proportion_paths"],
                           scenario_2_MCAR_data["two_step_lasso", "proportion_paths"],
                           scenario_3_MCAR_data["two_step_lasso", "proportion_paths"],
                           scenario_4_MCAR_data["two_step_lasso", "proportion_paths"],
                           scenario_5_MCAR_data["two_step_lasso", "proportion_paths"],
                           
                           scenario_1_MCAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_2_MCAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_3_MCAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_4_MCAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_5_MCAR_data["two_step_lasso_X", "proportion_paths"],
                           
                           scenario_1_MCAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_2_MCAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_3_MCAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_4_MCAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_5_MCAR_data["two_step_lasso_union", "proportion_paths"]
) %>% as.numeric()

MCAR_empirical_SE <- c(scenario_1_MCAR_data["fully_adjusted", "empirical_SE"],
                       scenario_2_MCAR_data["fully_adjusted", "empirical_SE"],
                       scenario_3_MCAR_data["fully_adjusted", "empirical_SE"],
                       scenario_4_MCAR_data["fully_adjusted", "empirical_SE"],
                       scenario_5_MCAR_data["fully_adjusted", "empirical_SE"],
                       
                       scenario_1_MCAR_data["unadjusted", "empirical_SE"],
                       scenario_2_MCAR_data["unadjusted", "empirical_SE"],
                       scenario_3_MCAR_data["unadjusted", "empirical_SE"],
                       scenario_4_MCAR_data["unadjusted", "empirical_SE"],
                       scenario_5_MCAR_data["unadjusted", "empirical_SE"],
                       
                       scenario_1_MCAR_data["two_step_lasso", "empirical_SE"],
                       scenario_2_MCAR_data["two_step_lasso", "empirical_SE"],
                       scenario_3_MCAR_data["two_step_lasso", "empirical_SE"],
                       scenario_4_MCAR_data["two_step_lasso", "empirical_SE"],
                       scenario_5_MCAR_data["two_step_lasso", "empirical_SE"],
                       
                       scenario_1_MCAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_2_MCAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_3_MCAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_4_MCAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_5_MCAR_data["two_step_lasso_X", "empirical_SE"],
                       
                       scenario_1_MCAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_2_MCAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_3_MCAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_4_MCAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_5_MCAR_data["two_step_lasso_union", "empirical_SE"]
) %>% as.numeric()

MCAR_table <- cbind(MCAR_variable_selection_methods,
                    MCAR_missingness_handling_methods,
                    MCAR_causal_estimate,
                    MCAR_bias,
                    MCAR_bias_as_proportion,
                    MCAR_coverage,
                    MCAR_blocked_paths,
                    MCAR_proportion_paths,
                    MCAR_empirical_SE) %>% as.data.frame()

colnames(MCAR_table) <- c("Variable-Selection-Method",
                          "Missingness-Handling-Method",
                          "Causal-Effect-Estimate",
                          "Bias",
                          "Proportional-Bias",
                          "Coverage",
                          "Blocked-Paths",
                          "Proportion-Paths",
                          "Empirical-Standard-Error")


# ----- Form MNAR Table -----

MNAR_variable_selection_methods   <- c(rep("Fully-adjusted", times = 5),
                                       rep("Unadjusted", times = 5),
                                       rep("Lasso", times = 5),
                                       rep("Exposure-Lasso", times = 5),
                                       rep("Union-Lasso", times = 5))
MNAR_missingness_handling_methods <- rep(c("CCA", "Within-MI", "Across-MI", "Indicator-encoding", "Dichotomizing"), times = 5)

MNAR_causal_estimate <- c(scenario_1_MNAR_data["fully_adjusted", "causal_estimate"],
                          scenario_2_MNAR_data["fully_adjusted", "causal_estimate"],
                          scenario_3_MNAR_data["fully_adjusted", "causal_estimate"],
                          scenario_4_MNAR_data["fully_adjusted", "causal_estimate"],
                          scenario_5_MNAR_data["fully_adjusted", "causal_estimate"],
                          
                          scenario_1_MNAR_data["unadjusted", "causal_estimate"],
                          scenario_2_MNAR_data["unadjusted", "causal_estimate"],
                          scenario_3_MNAR_data["unadjusted", "causal_estimate"],
                          scenario_4_MNAR_data["unadjusted", "causal_estimate"],
                          scenario_5_MNAR_data["unadjusted", "causal_estimate"],
                          
                          scenario_1_MNAR_data["two_step_lasso", "causal_estimate"],
                          scenario_2_MNAR_data["two_step_lasso", "causal_estimate"],
                          scenario_3_MNAR_data["two_step_lasso", "causal_estimate"],
                          scenario_4_MNAR_data["two_step_lasso", "causal_estimate"],
                          scenario_5_MNAR_data["two_step_lasso", "causal_estimate"],
                          
                          scenario_1_MNAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_2_MNAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_3_MNAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_4_MNAR_data["two_step_lasso_X", "causal_estimate"],
                          scenario_5_MNAR_data["two_step_lasso_X", "causal_estimate"],
                          
                          scenario_1_MNAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_2_MNAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_3_MNAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_4_MNAR_data["two_step_lasso_union", "causal_estimate"],
                          scenario_5_MNAR_data["two_step_lasso_union", "causal_estimate"]
) %>% as.numeric()

MNAR_bias               <- c(scenario_1_MNAR_data["fully_adjusted", "causal_bias"],
                             scenario_2_MNAR_data["fully_adjusted", "causal_bias"],
                             scenario_3_MNAR_data["fully_adjusted", "causal_bias"],
                             scenario_4_MNAR_data["fully_adjusted", "causal_bias"],
                             scenario_5_MNAR_data["fully_adjusted", "causal_bias"],
                             
                             scenario_1_MNAR_data["unadjusted", "causal_bias"],
                             scenario_2_MNAR_data["unadjusted", "causal_bias"],
                             scenario_3_MNAR_data["unadjusted", "causal_bias"],
                             scenario_4_MNAR_data["unadjusted", "causal_bias"],
                             scenario_5_MNAR_data["unadjusted", "causal_bias"],
                             
                             scenario_1_MNAR_data["two_step_lasso", "causal_bias"],
                             scenario_2_MNAR_data["two_step_lasso", "causal_bias"],
                             scenario_3_MNAR_data["two_step_lasso", "causal_bias"],
                             scenario_4_MNAR_data["two_step_lasso", "causal_bias"],
                             scenario_5_MNAR_data["two_step_lasso", "causal_bias"],
                             
                             scenario_1_MNAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_2_MNAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_3_MNAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_4_MNAR_data["two_step_lasso_X", "causal_bias"],
                             scenario_5_MNAR_data["two_step_lasso_X", "causal_bias"],
                             
                             scenario_1_MNAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_2_MNAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_3_MNAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_4_MNAR_data["two_step_lasso_union", "causal_bias"],
                             scenario_5_MNAR_data["two_step_lasso_union", "causal_bias"]
) %>% as.numeric()

MNAR_bias_as_proportion <- c(scenario_1_MNAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_2_MNAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_3_MNAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_4_MNAR_data["fully_adjusted", "causal_bias_proportion"],
                             scenario_5_MNAR_data["fully_adjusted", "causal_bias_proportion"],
                             
                             scenario_1_MNAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_2_MNAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_3_MNAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_4_MNAR_data["unadjusted", "causal_bias_proportion"],
                             scenario_5_MNAR_data["unadjusted", "causal_bias_proportion"],
                             
                             scenario_1_MNAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_2_MNAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_3_MNAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_4_MNAR_data["two_step_lasso", "causal_bias_proportion"],
                             scenario_5_MNAR_data["two_step_lasso", "causal_bias_proportion"],
                             
                             scenario_1_MNAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_2_MNAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_3_MNAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_4_MNAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             scenario_5_MNAR_data["two_step_lasso_X", "causal_bias_proportion"],
                             
                             scenario_1_MNAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_2_MNAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_3_MNAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_4_MNAR_data["two_step_lasso_union", "causal_bias_proportion"],
                             scenario_5_MNAR_data["two_step_lasso_union", "causal_bias_proportion"]
) %>% as.numeric()

MNAR_coverage           <- c(scenario_1_MNAR_data["fully_adjusted", "causal_coverage"],
                             scenario_2_MNAR_data["fully_adjusted", "causal_coverage"],
                             scenario_3_MNAR_data["fully_adjusted", "causal_coverage"],
                             scenario_4_MNAR_data["fully_adjusted", "causal_coverage"],
                             scenario_5_MNAR_data["fully_adjusted", "causal_coverage"],
                             
                             scenario_1_MNAR_data["unadjusted", "causal_coverage"],
                             scenario_2_MNAR_data["unadjusted", "causal_coverage"],
                             scenario_3_MNAR_data["unadjusted", "causal_coverage"],
                             scenario_4_MNAR_data["unadjusted", "causal_coverage"],
                             scenario_5_MNAR_data["unadjusted", "causal_coverage"],
                             
                             scenario_1_MNAR_data["two_step_lasso", "causal_coverage"],
                             scenario_2_MNAR_data["two_step_lasso", "causal_coverage"],
                             scenario_3_MNAR_data["two_step_lasso", "causal_coverage"],
                             scenario_4_MNAR_data["two_step_lasso", "causal_coverage"],
                             scenario_5_MNAR_data["two_step_lasso", "causal_coverage"],
                             
                             scenario_1_MNAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_2_MNAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_3_MNAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_4_MNAR_data["two_step_lasso_X", "causal_coverage"],
                             scenario_5_MNAR_data["two_step_lasso_X", "causal_coverage"],
                             
                             scenario_1_MNAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_2_MNAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_3_MNAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_4_MNAR_data["two_step_lasso_union", "causal_coverage"],
                             scenario_5_MNAR_data["two_step_lasso_union", "causal_coverage"]
) %>% as.numeric()

MNAR_blocked_paths <- c(scenario_1_MNAR_data["fully_adjusted", "blocked_paths"],
                        scenario_2_MNAR_data["fully_adjusted", "blocked_paths"],
                        scenario_3_MNAR_data["fully_adjusted", "blocked_paths"],
                        scenario_4_MNAR_data["fully_adjusted", "blocked_paths"],
                        scenario_5_MNAR_data["fully_adjusted", "blocked_paths"],
                        
                        scenario_1_MNAR_data["unadjusted", "blocked_paths"],
                        scenario_2_MNAR_data["unadjusted", "blocked_paths"],
                        scenario_3_MNAR_data["unadjusted", "blocked_paths"],
                        scenario_4_MNAR_data["unadjusted", "blocked_paths"],
                        scenario_5_MNAR_data["unadjusted", "blocked_paths"],
                        
                        scenario_1_MNAR_data["two_step_lasso", "blocked_paths"],
                        scenario_2_MNAR_data["two_step_lasso", "blocked_paths"],
                        scenario_3_MNAR_data["two_step_lasso", "blocked_paths"],
                        scenario_4_MNAR_data["two_step_lasso", "blocked_paths"],
                        scenario_5_MNAR_data["two_step_lasso", "blocked_paths"],
                        
                        scenario_1_MNAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_2_MNAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_3_MNAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_4_MNAR_data["two_step_lasso_X", "blocked_paths"],
                        scenario_5_MNAR_data["two_step_lasso_X", "blocked_paths"],
                        
                        scenario_1_MNAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_2_MNAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_3_MNAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_4_MNAR_data["two_step_lasso_union", "blocked_paths"],
                        scenario_5_MNAR_data["two_step_lasso_union", "blocked_paths"]
) %>% as.numeric()

MNAR_proportion_paths <- c(scenario_1_MNAR_data["fully_adjusted", "proportion_paths"],
                           scenario_2_MNAR_data["fully_adjusted", "proportion_paths"],
                           scenario_3_MNAR_data["fully_adjusted", "proportion_paths"],
                           scenario_4_MNAR_data["fully_adjusted", "proportion_paths"],
                           scenario_5_MNAR_data["fully_adjusted", "proportion_paths"],
                           
                           scenario_1_MNAR_data["unadjusted", "proportion_paths"],
                           scenario_2_MNAR_data["unadjusted", "proportion_paths"],
                           scenario_3_MNAR_data["unadjusted", "proportion_paths"],
                           scenario_4_MNAR_data["unadjusted", "proportion_paths"],
                           scenario_5_MNAR_data["unadjusted", "proportion_paths"],
                           
                           scenario_1_MNAR_data["two_step_lasso", "proportion_paths"],
                           scenario_2_MNAR_data["two_step_lasso", "proportion_paths"],
                           scenario_3_MNAR_data["two_step_lasso", "proportion_paths"],
                           scenario_4_MNAR_data["two_step_lasso", "proportion_paths"],
                           scenario_5_MNAR_data["two_step_lasso", "proportion_paths"],
                           
                           scenario_1_MNAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_2_MNAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_3_MNAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_4_MNAR_data["two_step_lasso_X", "proportion_paths"],
                           scenario_5_MNAR_data["two_step_lasso_X", "proportion_paths"],
                           
                           scenario_1_MNAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_2_MNAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_3_MNAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_4_MNAR_data["two_step_lasso_union", "proportion_paths"],
                           scenario_5_MNAR_data["two_step_lasso_union", "proportion_paths"]
) %>% as.numeric()

MNAR_empirical_SE <- c(scenario_1_MNAR_data["fully_adjusted", "empirical_SE"],
                       scenario_2_MNAR_data["fully_adjusted", "empirical_SE"],
                       scenario_3_MNAR_data["fully_adjusted", "empirical_SE"],
                       scenario_4_MNAR_data["fully_adjusted", "empirical_SE"],
                       scenario_5_MNAR_data["fully_adjusted", "empirical_SE"],
                       
                       scenario_1_MNAR_data["unadjusted", "empirical_SE"],
                       scenario_2_MNAR_data["unadjusted", "empirical_SE"],
                       scenario_3_MNAR_data["unadjusted", "empirical_SE"],
                       scenario_4_MNAR_data["unadjusted", "empirical_SE"],
                       scenario_5_MNAR_data["unadjusted", "empirical_SE"],
                       
                       scenario_1_MNAR_data["two_step_lasso", "empirical_SE"],
                       scenario_2_MNAR_data["two_step_lasso", "empirical_SE"],
                       scenario_3_MNAR_data["two_step_lasso", "empirical_SE"],
                       scenario_4_MNAR_data["two_step_lasso", "empirical_SE"],
                       scenario_5_MNAR_data["two_step_lasso", "empirical_SE"],
                       
                       scenario_1_MNAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_2_MNAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_3_MNAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_4_MNAR_data["two_step_lasso_X", "empirical_SE"],
                       scenario_5_MNAR_data["two_step_lasso_X", "empirical_SE"],
                       
                       scenario_1_MNAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_2_MNAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_3_MNAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_4_MNAR_data["two_step_lasso_union", "empirical_SE"],
                       scenario_5_MNAR_data["two_step_lasso_union", "empirical_SE"]
) %>% as.numeric()

MNAR_table <- cbind(MNAR_variable_selection_methods,
                    MNAR_missingness_handling_methods,
                    MNAR_causal_estimate,
                    MNAR_bias,
                    MNAR_bias_as_proportion,
                    MNAR_coverage,
                    MNAR_blocked_paths,
                    MNAR_proportion_paths,
                    MNAR_empirical_SE) %>% as.data.frame()

colnames(MNAR_table) <- c("Variable-Selection-Method",
                          "Missingness-Handling-Method",
                          "Causal-Effect-Estimate",
                          "Bias",
                          "Proportional-Bias",
                          "Coverage",
                          "Blocked-Paths",
                          "Proportion-Paths",
                          "Empirical-Standard-Error")


# ----- Round results to 6 d.p. -----

MAR_table$`Causal-Effect-Estimate`   <- as.numeric(MAR_table$`Causal-Effect-Estimate`)
MAR_table$`Bias`                     <- as.numeric(MAR_table$`Bias`)
MAR_table$`Proportional-Bias`        <- as.numeric(MAR_table$`Proportional-Bias`)
MAR_table$`Coverage`                 <- as.numeric(MAR_table$`Coverage`)
MAR_table$`Blocked-Paths`            <- as.numeric(MAR_table$`Blocked-Paths`)
MAR_table$`Proportion-Paths`         <- as.numeric(MAR_table$`Proportion-Paths`)
MAR_table$`Empirical-Standard-Error` <- as.numeric(MAR_table$`Empirical-Standard-Error`)

MCAR_table$`Causal-Effect-Estimate`   <- as.numeric(MCAR_table$`Causal-Effect-Estimate`)
MCAR_table$`Bias`                     <- as.numeric(MCAR_table$`Bias`)
MCAR_table$`Proportional-Bias`        <- as.numeric(MCAR_table$`Proportional-Bias`)
MCAR_table$`Coverage`                 <- as.numeric(MCAR_table$`Coverage`)
MCAR_table$`Blocked-Paths`            <- as.numeric(MCAR_table$`Blocked-Paths`)
MCAR_table$`Proportion-Paths`         <- as.numeric(MCAR_table$`Proportion-Paths`)
MCAR_table$`Empirical-Standard-Error` <- as.numeric(MCAR_table$`Empirical-Standard-Error`)

MNAR_table$`Causal-Effect-Estimate`   <- as.numeric(MNAR_table$`Causal-Effect-Estimate`)
MNAR_table$`Bias`                     <- as.numeric(MNAR_table$`Bias`)
MNAR_table$`Proportional-Bias`        <- as.numeric(MNAR_table$`Proportional-Bias`)
MNAR_table$`Coverage`                 <- as.numeric(MNAR_table$`Coverage`)
MNAR_table$`Blocked-Paths`            <- as.numeric(MNAR_table$`Blocked-Paths`)
MNAR_table$`Proportion-Paths`         <- as.numeric(MNAR_table$`Proportion-Paths`)
MNAR_table$`Empirical-Standard-Error` <- as.numeric(MNAR_table$`Empirical-Standard-Error`)


MAR_table  <- (MAR_table  %>% mutate(across(where(is.numeric), round, 6)))
MCAR_table <- (MCAR_table %>% mutate(across(where(is.numeric), round, 6)))
MNAR_table <- (MNAR_table %>% mutate(across(where(is.numeric), round, 6)))


# ----- Save results -----

write.csv(MAR_table,  "temp/MAR_results_table.csv", row.names = FALSE)
write.csv(MCAR_table, "temp/MCAR_results_table.csv", row.names = FALSE)
write.csv(MNAR_table, "temp/MNAR_results_table.csv", row.names = FALSE)


# reset scientific notation
options(scipen=0)

