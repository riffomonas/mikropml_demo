library(tidyverse)

get_mean_diff <- function(x){
  
  x %>%
    group_by(condition) %>%
    summarize(mean = mean(AUC)) %>%
    summarize(diff = abs(diff(mean))) %>%
    pull(diff)
  
}

get_rand_mean_diff <- function(x){
  
  x %>%
    mutate(condition = sample(condition)) %>%
    summarize(diff = get_mean_diff(.)) %>%
    pull(diff)
  
}

# function to calculate pvalues by random permutation
perm_p_value_cond <- function(data, cond_name_1, cond_name_2){

  # subset results to select AUC and condition columns and filter to only the 
  # two conditions of interest
  test_subsample <- data %>%
    select(AUC,condition) %>%
    filter((condition == cond_name_1 | condition == cond_name_2))
  
  # quantify the absolute value of the difference in mean AUC between the
  # two conditions (observed difference)
  #obs <- abs(diff(mosaic::mean(AUC ~ condition, data = test_subsample)))
  
  obs <- get_mean_diff(test_subsample)
  
  
  # shuffle condition labels and calculate difference in mean
  # AUC - repeat 10,000 times
  # auc.null <- mosaic::do(10000) * diff(mosaic::mean(AUC ~ shuffle(condition),
  #                                           data = test_subsample))
  auc.null <- map_dbl(1:10000, ~get_rand_mean_diff(test_subsample))
  
  
  n <- length(auc.null) #number of shuffled calculations
  
  # r = #replications at least as extreme as observed effect
  r <- sum(auc.null >= obs)# count number of shuffled differences as
                          # big or bigger than observed difference
  
  # compute Monte Carlo p-value with correction (Davison & Hinkley, 1997)
  p.value <- (r+1)/(n+1)
  
  return(p.value)
}