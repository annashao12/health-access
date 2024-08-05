library(tidyverse)
library(tidybayes)
library(brms)

load(file = "data/DS0001/38759-0001-Data.rda")
a <- da38759.0001
rm(da38759.0001)

extract_text_after_parens <- function(x) {
  str_trim(str_replace(x, "^\\(\\d+\\)\\s*", ""))
}

dt <- a %>%
  select('Q29' | 'PPAGE' | 'PPGENDER' | 'Q1' | 'Q34' | 'Q3') %>%
  rename(health_condition = Q29,
         age = PPAGE,
         gender = PPGENDER,
         marital_status = Q1,
         limited_access = Q34,
         num_of_children = Q3) %>%
  mutate(age = scale(age),
         health_condition = 
           recode(health_condition, 
                  "(1) Yes, one condition" = "Yes",
                  "(2) Yes, more than one condition" = "Yes",
                  "(3) No" = "No"),
         marital_status = str_replace(marital_status, fixed("(5) Never married"), "(5) NeverMarried"),
         across(c(marital_status, gender, limited_access), 
                extract_text_after_parens),
         marital_status = factor(marital_status,
                                 levels = c("NeverMarried","Married","Separated","Divorced","Widowed")),
         health_condition = factor(health_condition,
                                   levels = c("No","Yes"),
                                   ordered = TRUE),
         gender = as.factor(gender),
         limited_access = as.factor(limited_access),
         num_of_children = as.factor(num_of_children)) %>%
  drop_na()

fit_condition <- brm(
  formula = health_condition ~ age + gender + marital_status + limited_access + num_of_children,
  data = dt, 
  family = bernoulli(), # two outcomes: have health condition(s) lasting 1+ years, don't have health condition
  silent = 2,
  refresh = 0,
  seed = 12)

age <- 0
marital_status <- c("NeverMarried","Married","Separated","Divorced","Widowed")
gender <- c("Female","Male")
limited_access <- c("No","Yes")
num_of_children <- c(0:6)

newobs <- expand_grid(age, marital_status, gender, limited_access, num_of_children)

predicted <- fit_condition |> 
  add_epred_draws(newdata = newobs)

ggplot(predicted, aes(x = .epred, fill = limited_access)) +
  geom_density() + 
  scale_fill_manual(values = c("No" = "#A1BE95", "Yes" = "#F98866")) +
  labs(title = "Posterior Distribution of Health Condition",
       subtitle = "Distribution is approximately divided based on access to healthcare",
       x = "Probability of having health condition(s)",
       y = NULL,
       fill = "Difficulty\nin Access?") +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),       # Remove y-axis title
    axis.text.y = element_blank(),        # Remove y-axis text
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )
