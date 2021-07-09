#!/usr/bin/env Rscript

library(mikropml)
library(tidyverse)

rds_files <- commandArgs(trailingOnly = TRUE)

iterative_run_ml_results <- map(rds_files, readRDS)

iterative_run_ml_results %>%
  map(pluck, "trained_model") %>%
  combine_hp_performance() %>%
  pluck("dat") %>%
  write_tsv("processed_data/l2_genus_pooled_hp.tsv")

iterative_run_ml_results %>%
  map_dfr(pluck, "performance") %>%
  write_tsv("processed_data/l2_genus_pooled_performance.tsv")
  