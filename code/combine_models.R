#!/usr/bin/env Rscript

library(mikropml)
library(tidyverse)
library(glue)

rds_files <- commandArgs(trailingOnly = TRUE)
root <- str_replace(rds_files, "_\\d*\\.Rds", "") %>% unique

iterative_run_ml_results <- map(rds_files, readRDS)

iterative_run_ml_results %>%
  map(pluck, "trained_model") %>%
  combine_hp_performance() %>%
  pluck("dat") %>%
  write_tsv(glue("{root}_hp.tsv"))

iterative_run_ml_results %>%
  map_dfr(pluck, "performance") %>%
  write_tsv(glue("{root}_performance.tsv"))
