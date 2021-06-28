library(tidyverse)
library(broom)

shared <- read_tsv("raw_data/baxter.subsample.shared",
         col_types = cols(Group = col_character(),
                          .default = col_double())) %>%
  rename_all(tolower) %>%
  select(group, starts_with('otu')) %>%
  pivot_longer(-group, names_to = "otu", values_to = "count")

taxonomy <- read_tsv("raw_data/baxter.cons.taxonomy") %>%
  rename_all(tolower) %>%
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";$", "")) %>%
  separate(taxonomy,
           into=c("kingdom", "phylum", "class", "order", "family", "genus"),
           sep=";") %>%
  select(-size)

metadata <- read_tsv("raw_data/baxter.metadata.tsv",
         col_types=cols(sample = col_character(),
                        Hx_Prev = col_logical(),
                        Hx_of_Polyps = col_logical(),
                        Smoke = col_logical(),
                        Diabetic = col_logical(),
                        Hx_Fam_CRC = col_logical(),
                        NSAID = col_logical(),
                        Diabetes_Med = col_logical())) %>%
  rename_all(tolower) %>%
  rename(group = sample) %>%
  mutate(srn = dx_bin == "Cancer" | dx_bin == "Adv Adenoma",
         lesion = dx_bin == "Cancer" | dx_bin == "Adv Adenoma" | dx_bin == "Adenoma")


combined <- inner_join(shared, taxonomy, by="otu") %>%
  group_by(group, genus) %>%
  summarize(count = sum(count), .groups="drop") %>%
  group_by(group) %>%
  mutate(rel_abund = count / sum(count)) %>%
  ungroup() %>%
  select(-count) %>%
  inner_join(., metadata, by="group")


sig_genera <- combined %>%
  nest(data = -genus) %>%
  mutate(test = map(.x=data, ~wilcox.test(rel_abund ~ srn, data =.x) %>% tidy)) %>%
  unnest(test) %>%
  mutate(p.adjust = p.adjust(p.value, method ="BH")) %>%
  filter(p.adjust < 0.05) %>%
  select(genus, p.adjust)

combined %>%
  inner_join(., sig_genera, by="genus") %>%
  ggplot(aes(y=genus, x=100 * (rel_abund + 1/20000), color=srn)) +
  # stat_summary(fun.data=median_hilow, fun.args=list(conf.int=0.5),
  #              geom="pointrange", position = position_dodge(width=0.5)) +
  geom_jitter(position = position_jitterdodge(dodge.width = 0.8,
                                              jitter.width = 0.3)) +
  scale_x_log10() +
  geom_vline(xintercept = 100 * 1/10000)


ggsave("figures/significant_genera.tiff", width=6, height=4)
