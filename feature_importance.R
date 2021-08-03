library(tidyverse)
library(ggtext)

l2_files <- list.files(path="processed_data",
                       pattern="l2_genus_\\d*.Rds",
                       full.names=TRUE)

get_weights <- function(file_name){
  
  model <- readRDS(file_name) %>%
    pluck("trained_model")
  
  coef(model$finalModel, model$bestTune$lambda) %>%
    as.matrix %>%
    as_tibble(rownames="feature") %>%
    rename(weight = `1`) %>%
    mutate(seed = str_replace(file_name,
                              "processed_data/l2_genus_(\\d*).Rds",
                              "\\1"))

}

l2_weights <- map_dfr(l2_files, get_weights)

l2_weights %>%
  filter(feature != "(Intercept)") %>%
  group_by(feature) %>%
  summarize(median = median(weight),
            l_quartile = quantile(weight, prob=0.25),
            u_quartile = quantile(weight, prob=0.75)) %>%
  mutate(feature = str_replace(feature, "(.*)", "*\\1*"),
         feature = str_replace(feature, "(.*)_unclassified\\*", "Unclassified \\1*"),
         feature = str_replace(feature, "_(.*)\\*", "* \\1"),
         feature = fct_reorder(feature, median)) %>%
  filter(abs(median) > 0.01) %>%
  ggplot(aes(x=median, y=feature, xmin=l_quartile, xmax=u_quartile)) +
  geom_vline(xintercept=0, color="gray") +
  geom_point() +
  geom_linerange() +
  labs(x="Weights", y=NULL) +
  theme_classic() +
  theme(axis.text.y = element_markdown())
  
ggsave("figures/l2_weights.tiff", width=5, height=5)



get_feature_importance <- function(file_name){
  
  feature_importance <- readRDS(file_name) %>%
    pluck("feature_importance") %>%
    as_tibble() %>%
    select(names, perf_metric, perf_metric_diff)
  
}

l2_feature_importance <- map_dfr(l2_files, get_feature_importance)


l2_feature_importance %>%
  rename(feature = names) %>%
  group_by(feature) %>%
  summarize(median = median(perf_metric_diff),
            l_quartile = quantile(perf_metric_diff, prob=0.25),
            u_quartile = quantile(perf_metric_diff, prob=0.75)) %>%
  mutate(feature = str_replace(feature, "(.*)", "*\\1*"),
         feature = str_replace(feature, "(.*)_unclassified\\*", "Unclassified \\1*"),
         feature = str_replace(feature, "_(.*)\\*", "* \\1"),
         feature = fct_reorder(feature, median)) %>%
  filter(median > 0.0025) %>%
  ggplot(aes(x=median, y=feature, xmin=l_quartile, xmax=u_quartile)) +
  geom_point() +
  geom_linerange() +
  labs(x="Change in AUC when removed", y=NULL) +
  theme_classic() +
  theme(axis.text.y = element_markdown())

ggsave("figures/l2_feature_importance.tiff", width=5, height=5)
