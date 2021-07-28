library(tidyverse)

read_performance <- function(file_name) {
  
  read_tsv(file_name) %>%
    mutate(method = str_replace(file_name, ".*/(.*)_performance.tsv", "\\1"))
  
}

list.files(path="processed_data",
           pattern="performance.tsv",
           full.names = TRUE) %>%
  map_dfr(., read_performance) %>%
  select(method, cv_metric_AUC, AUC) %>%
  rename(training = cv_metric_AUC,
         testing = AUC) %>%
  pivot_longer(cols=c(training, testing), 
               names_to="training_testing",
               values_to="AUC") %>%
  mutate(training_testing = factor(training_testing,
                                   levels=c('training', "testing")),
         model = str_replace(method, "_genus.*", ""),
         model = if_else(model == "l2", "L2 Regularized\nLogistic Regression",
                         "Random Forest"),
         method = if_else(str_detect(method, "fit"), "genus+fit","genus")) %>%
  ggplot(aes(x=method, y=AUC, color=training_testing)) +
  geom_hline(yintercept=0.65, color="gray", linetype="dashed") +
  #geom_boxplot()
  facet_wrap(~model, nrow=1, scales="free_x", strip.position = "bottom") +
  stat_summary(fun.data = median_hilow,
               fun.args = list(conf.int=0.5),
               geom="pointrange",
               position = position_dodge(width=0.5)) + 
  lims(y=c(0.5, 1)) +
  labs(x=NULL,
       y="Area under the receiver\noperator characteristic curve") + 
  scale_color_manual(name = NULL,
                     breaks=c("training", "testing"),
                     labels=c("Training", "Testing"),
                     values=c("gray", "dodgerblue")) +
  theme_classic() +
  theme(strip.placement = "outside",
        strip.background = element_blank())

ggsave("figures/model_compare.png", width=5, height=4)
