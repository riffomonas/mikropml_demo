feature_select <- function(x){
  
  x %>%
    select(group, taxonomy, rel_abund, srn)
  
}

approach <- "glmnet"

hyperparameter <- list(alpha = 0,
                       lambda = c(0.1, 1, 2, 3, 4, 5))
