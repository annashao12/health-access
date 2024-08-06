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

# ~~~~~~~
age <- 0
marital_status <- c("NeverMarried","Married","Separated")
gender <- c("Female","Male")
limited_access <- c("No")
num_of_children <- c(0:6)

newobs <- expand_grid(age, marital_status, gender, limited_access, num_of_children)
predicted <- fit_condition |> 
  add_epred_draws(newdata = newobs) |> 
  mutate(num_of_children = ifelse(num_of_children > 0, "Yes", "No"),
         gender_children = paste0(gender, " | ", num_of_children),
         marital_status = factor(marital_status,
                                 levels = c("NeverMarried","Married","Separated"),
                                 ordered = TRUE),
         marital_status = recode(marital_status, "NeverMarried" = "Never Married"))

ggplot(predicted, aes(x = .epred, y = fct_rev(marital_status))) +
  stat_slab(aes(fill = as.factor(gender_children)), 
            position = 'identity',
            alpha = 0.8,
            scale = 0.95) +
  scale_fill_manual(values = c("Female | No" = "lightblue4", 
                               "Female | Yes" = "lightblue1",
                               "Male | No" = "lightsalmon4",
                               "Male | Yes" = "lightsalmon1")) +
  labs(title = "Association Between Health Outcomes and Children", 
       subtitle = "Individuals with one or more children tend to be healthier, especially for males",
       x = "Probability of having health condition(s)", 
       y = "Marital Status",
       caption = "People with limited access to healthcare or are\ndivorced or widowed were not displayed",
       fill = "Sex | Have Child(ren)?") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right"
  )

#whether employed or not?
