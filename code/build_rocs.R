library(tidyverse)

get_sens_spec_lookup <- function(data){
  
  total <- data %>%
    count(observed) %>%
    pivot_wider(names_from=observed, values_from=n)
  
  data %>%
    arrange(desc(srn)) %>%
    mutate(is_srn = (observed == "srn"),
           tp = cumsum(is_srn),
           fp = cumsum(!is_srn),
           sensitivity = tp / total$srn,
           fpr = fp / total$healthy,
           specificity = 1-fpr) %>%
    select(sensitivity, specificity, fpr)

}

get_sensitivity <- function(x, data){
  
  data %>%
    filter(specificity - x >= 0) %>%
    top_n(sensitivity, n=1) %>%
    mutate(specificity = x, fpr = 1-x) %>%
    distinct()
  
}

get_pooled_sens_spec <- function(file_name, specificity){

  model <- readRDS(file_name)
  
  prob <- predict(model$trained_model, model$test_data, type="prob")
  observed <- model$test_data$srn
  
  prob_obs <- bind_cols(prob, observed = observed) %>%
    select(srn, observed)
  
  sens_spec_lookup <- get_sens_spec_lookup(prob_obs)

  map_dfr(specificity, get_sensitivity, sens_spec_lookup) %>%
    mutate(model = str_replace(file_name,
                               "processed_data/(.*)_\\d*.Rds",
                               "\\1"))

}


specificity <- seq(0, 1, 0.01)

get_pooled_sens_spec("processed_data/l2_genus_1.Rds", specificity)

pooled_sens_spec <- list.files(path="processed_data",
           pattern=".*\\d*.Rds",
           full.names=TRUE) %>%
  map_dfr(get_pooled_sens_spec, specificity)

pooled_sens_spec %>%
  group_by(model, specificity) %>%
  summarize(lquartile = quantile(sensitivity, prob=0.25),
            uquartile = quantile(sensitivity, prob=0.75),
            sensitivity = median(sensitivity),
            .groups="drop") %>%
  ggplot(aes(x=1-specificity, y=sensitivity,
             ymin=lquartile, ymax=uquartile))+
  geom_ribbon(alpha=0.25, aes(fill=model)) +
  geom_step(aes(color=model)) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.2))

ggsave("figures/roc_curve.tiff", width=5, height=5)
