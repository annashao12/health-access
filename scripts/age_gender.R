library(tidyverse)

load(file = "data/DS0001/38759-0001-Data.rda")
a <- da38759.0001

extract_text_after_parens <- function(x) {
  str_trim(str_replace(x, "^\\(\\d+\\)\\s*", ""))
}

age_gender <- a %>%
  select('PPAGE' | 'PPGENDER') %>%
  rename(age = PPAGE,
         gender = PPGENDER) %>%
  mutate(gender = as.factor(gender),
         across(gender, 
                extract_text_after_parens)) %>%
  drop_na()

ggplot(age_gender, aes(x = age, fill = gender)) +
  geom_histogram(binwidth = 5, color = "black", position = "stack") +
  labs(title = "Age Distribution by Sex",
       subtitle = "Older adults were over-represented",
       x = "Age", 
       y = "Count",
       fill = "Sex") +
  theme_bw() +
  scale_fill_manual(values = c("Male" = "#E7E8D1", "Female" = "#A7BEAE")) +
  scale_x_continuous(breaks = c(seq(from = 15, to = 70, by = 5))) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

                     