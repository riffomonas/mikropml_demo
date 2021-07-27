feature_select <- function(x){
  
  x %>%
    select(group, taxonomy, rel_abund, srn)
  
}

approach <- "rf"

hyperparameter <- list(mtry= c(2, 4, 8, 17, 34))
