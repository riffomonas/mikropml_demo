library(furrr)
library(mikropml)
library(tidyverse)

plan("multisession")

run_ml_seed <- function(seed) {
  run_ml(otu_mini_bin, 
         'glmnet', 
         find_feature_importance = TRUE,
         seed = seed)
}
iterative_run_ml_results <- future_map(1:3, run_ml_seed,
                                       .options = furrr_options(seed=TRUE))

feature_importance %>% 
  ggplot(aes(perf_metric_diff, names)) +
  geom_boxplot() +
  labs(x = 'Difference in AUROC', y = '') +
  theme_bw()
