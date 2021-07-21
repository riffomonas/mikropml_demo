#!/usr/bin/env Rscript

source("code/genus_process.R")
library(mikropml)
library(tidyverse)

output_file <- commandArgs(trailingOnly = TRUE)
seed <- as.numeric(str_replace(output_file, ".*_(\\d*)\\.Rds", "\\1"))

srn_genus_fit_data <- composite %>%
  select(group, taxonomy, rel_abund, fit_result, srn) %>%
  pivot_wider(names_from=taxonomy, values_from = rel_abund) %>%
  select(-group) %>%
  mutate(srn = if_else(srn, "srn", "healthy")) %>%
  select(srn, everything())

srn_genus_fit_preprocess <- preprocess_data(srn_genus_fit_data,
                                        outcome_colname = "srn")$dat_transformed

test_hp <- list(alpha = 0,
                lambda = c(0.1, 1, 2, 3, 4, 5))

model <- run_ml(srn_genus_fit_preprocess,
       method="glmnet",
       outcome_colname = "srn",
       kfold = 5,
       cv_times = 100,
       training_frac = 0.8,
       hyperparameters = test_hp,
       seed = seed)

saveRDS(model, file=output_file)
