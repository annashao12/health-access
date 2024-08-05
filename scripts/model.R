library(tidyverse)
library(brms)
library(gtsummary)

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
  family = bernoulli(),
  silent = 2,
  refresh = 0,
  seed = 12)

tbl_regression(fit_condition, intercept = TRUE) |> 
  as_gt() |>  # convert to gt table
  gt::gtsave( # save table as image
    filename = "graphs/regtable.png"
  )
