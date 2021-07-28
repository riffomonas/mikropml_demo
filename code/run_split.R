#!/usr/bin/env Rscript

source("code/genus_process.R")
library(mikropml)
library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]
seed <- as.numeric(str_replace(output_file, ".*_(\\d*)\\.Rds", "\\1"))
feature_script <- args[2]

source(feature_script)

srn_data <- composite %>%
  feature_select() %>%
  pivot_wider(names_from=taxonomy, values_from = rel_abund) %>%
  select(-group) %>%
  mutate(srn = if_else(srn, "srn", "healthy")) %>%
  select(srn, everything())

srn_preprocess <- preprocess_data(srn_data,
                                  outcome_colname = "srn")$dat_transformed


model <- run_ml(srn_preprocess,
       method=approach,
       outcome_colname = "srn",
       kfold = 5,
       cv_times = 100,
       training_frac = 0.8,
			 find_feature_importance = TRUE,
       hyperparameters = hyperparameter,
       seed = seed)

saveRDS(model, file=output_file)
