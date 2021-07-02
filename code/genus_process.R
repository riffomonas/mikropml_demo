library(tidyverse)
library(broom)
library(ggtext)

set.seed(19760620)

shared <- read_tsv("raw_data/baxter.subsample.shared",
                   col_types = cols(Group = col_character(),
                                    .default = col_double())) %>%
  rename_all(tolower) %>%
  select(group, starts_with("otu")) %>%
  pivot_longer(-group, names_to="otu", values_to="count")

taxonomy <- read_tsv("raw_data/baxter.cons.taxonomy") %>%
  rename_all(tolower) %>%
  select(otu, taxonomy) %>%
  mutate(otu = tolower(otu),
         taxonomy = str_replace_all(taxonomy, "\\(\\d+\\)", ""),
         taxonomy = str_replace(taxonomy, ";unclassified", "_unclassified"),
         taxonomy = str_replace_all(taxonomy, ";unclassified", ""),
         taxonomy = str_replace_all(taxonomy, ";$", ""),
         taxonomy = str_replace_all(taxonomy, ".*;", "")
  ) 


metadata <- read_tsv("raw_data/baxter.metadata.tsv", 
                     col_types=cols(sample = col_character())) %>%
  rename_all(tolower) %>% 
  rename(group = sample) %>%
  mutate(srn = dx_bin == "Adv Adenoma" | dx_bin == "Cancer",
         lesion = dx_bin == "Adv Adenoma" | dx_bin == "Cancer" | dx_bin == "Adenoma")

composite <- inner_join(shared, taxonomy, by="otu") %>%
  group_by(group, taxonomy) %>%
  summarize(count = sum(count), .groups="drop") %>%
  group_by(group) %>%
  mutate(rel_abund = count / sum(count)) %>% 
  ungroup() %>%
  select(-count) %>%
  inner_join(., metadata, by="group")
