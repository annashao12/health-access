library(tidyverse)

load(file = "data/DS0001/38759-0001-Data.rda")
a <- da38759.0001

extract_text_after_parens <- function(x) {
  str_trim(str_replace(x, "^\\(\\d+\\)\\s*", ""))
}

self_health <- a %>%
  select('Q27'|'Q29') %>%
  rename(self_assess = Q27,
         health_condition = Q29) %>%
  mutate(health_condition = 
           recode(health_condition, 
                  "(1) Yes, one condition" = "Yes",
                  "(2) Yes, more than one condition" = "Yes",
                  "(3) No" = "No"),
         across(self_assess,
                extract_text_after_parens),
         self_assess = factor(self_assess,
                              levels = c("Poor",
                                         "Fair",
                                         "Good",
                                         "Very good",
                                         "Excellent"),
                              ordered = TRUE),
         health_condition = factor(health_condition,
                                   levels = c("No","Yes"),
                                   ordered = TRUE)) %>%
  drop_na()

health_summary <- self_health %>%
  group_by(self_assess, health_condition) %>%
  summarise(count = n()) %>%
  mutate(total = sum(count), frequency = (count / total) * 100) %>%
  ungroup()

ggplot(health_summary, aes(x = self_assess, y = frequency, fill = health_condition)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", frequency)), 
            position = position_stack(vjust = 0.5), 
            size = 3.5, color = "black") +
  labs(title = "Self-Estimate and Health Condition", 
       subtitle = "There is a clear trend that people tend to assess their health as better\nwhen they do not have a health condition",
       x = "Self-Assessed Health", 
       y = "Percentage (%)",
       fill = "Have Health Condition?") +
  theme_bw() +
  scale_fill_manual(values = c("No" = "#CEE6F2", "Yes" = "#E3867D")) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )
