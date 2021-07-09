library(tidyverse)

read_tsv("processed_data/l2_genus_pooled_hp.tsv") %>%
  plot_hp_performance(lambda, AUC)

read_tsv("processed_data/l2_genus_pooled_hp.tsv") %>%
  group_by(alpha, lambda) %>%
  summarize(mean_AUC = mean(AUC), 
            lquartile = quantile(AUC, prob=0.25),
            uquartile = quantile(AUC, prob=0.75),
            .groups="drop") %>%
  top_n(n=3, mean_AUC)

read_tsv("processed_data/l2_genus_pooled_performance.tsv") %>%
  select(seed, cv_metric_AUC, AUC) %>%
  pivot_longer(cols=-seed, names_to="metric", values_to="AUC") %>%
  ggplot(aes(x=metric, y=AUC)) +
  geom_boxplot()
