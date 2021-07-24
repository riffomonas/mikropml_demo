library(tidyverse)
source('code/get_pvalues.R')


genus <- read_tsv("processed_data/l2_genus_performance.tsv") %>%
  mutate(condition = "genus")

genus_fit <- read_tsv("processed_data/l2_genus_fit_performance.tsv") %>%
  mutate(condition = "genus_fit")

auc_comparison <- bind_rows(genus, genus_fit) %>%
  select(condition, AUC)

auc_comparison %>%
  ggplot(aes(x=AUC, fill=condition)) +
  geom_density(alpha=0.5) +
  stat_summary(aes(x=0.8, y=AUC, xintercept = stat(y)),
               fun=mean, geom="vline") +
  labs(y="Density")

ggsave("figures/genus_fit_auc_comparison.tiff", width=5, height=4)

auc_comparison %>%
  group_by(condition) %>%
  summarize(mean_auc = mean(AUC)) %>%
  pivot_wider(names_from = condition, values_from=mean_auc) %>%
  mutate(diff = genus_fit - genus)


p.value <- perm_p_value_cond(auc_comparison, "genus", "genus_fit")
