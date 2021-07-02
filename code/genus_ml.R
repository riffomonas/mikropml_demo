source("code/genus_process.R")
library(mikropml)

srn_genus_data <- composite %>% 
  select(group, taxonomy, rel_abund, srn) %>%
  pivot_wider(names_from=taxonomy, values_from = rel_abund) %>%
  select(-group) %>%
  mutate(srn = if_else(srn, "srn", "healthy")) %>%
  select(srn, everything())

srn_genus_results <- run_ml(srn_genus_data,
       method="glmnet",
       outcome_colname = "srn",
       kfold = 5,
       cv_times = 100,
       training_frac = 0.2,
       seed = 19760620)
