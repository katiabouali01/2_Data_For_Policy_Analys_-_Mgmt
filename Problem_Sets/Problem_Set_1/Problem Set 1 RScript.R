##
## Problem Set 1 
## Data Prep & Descriptives: NSECE 2019 Workforce Questionaire
## Katia Bouali
## R Version 4.5.2
##

## Set up 
library(dplyr)
library(tidyr)
library(tidyverse)
library(forcats)
library(ggplot2)

describe_num <- function(x) {
  x <- x[!is.na(x)]
  data.frame(
    n = length(x), mean = mean(x), sd = sd(x), median = median(x),
    min = min(x), max = max(x), range = max(x) - min(x),
    skew = mean((x - mean(x))^3) / sd(x)^3
  )
}

freq_table <- function(x) {
  x <- x[!is.na(x)]
  tab <- table(x)
  data.frame(category = names(tab), n = as.integer(tab),
             pct = round(100 * as.integer(tab) / length(x), 1))
}


## Load data

nsece_raw <- da37941.0005 

## Subset to variables
nsece <- nsece_raw %>%
  select(
    WF9_WORK_WAGE,
    WF9_WORK_ROLE,
    WF9_WORK_YRS,
    WF9_CHAR_HISP,
    WF9_CHAR_RACE,
    WF9_CHAR_EDUC,
    WF9_CHAR_GENDER
  )

## Rename variables
nsece <- nsece %>%
  rename(
    wage       = WF9_WORK_WAGE,
    role       = WF9_WORK_ROLE,
    yrs_worked = WF9_WORK_YRS,
    hispanic   = WF9_CHAR_HISP,
    race       = WF9_CHAR_RACE,
    educ       = WF9_CHAR_EDUC,
    gender     = WF9_CHAR_GENDER
  )

## Missing data

nsece %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(nsece), 1))


## Recode
# role, hispanic, race,gender: nominal -> leace as unordered factors (already are)
# edu, yrs_worked: ordinal -> set explixit order so later tests treat them correctly


nsece <- nsece %>%
  mutate(
    educ = factor(educ, levels = c(
      "(1) Less than High School",
      "(3) GED or high school equivalency",
      "(4) High school graduate",
      "(5) Some college credit but no degree",
      "(6) Associate degree (AA, AS)",
      "(7) Bachelor's degree (BA, BS, AB)",
      "(8) Graduate or professional degree"
    ), ordered = TRUE),
    
    yrs_worked = factor(yrs_worked, levels = c(
      "(01) Less than half a year (0-6 months)", "(02) 7-12 months",
      "(03) 1 year", "(04) 2 years", "(05) 3 years", "(06) 4 years",
      "(07) 5 years", "(08) 6 years", "(09) 7 years", "(10) 8 years",
      "(11) 9 years", "(12) 10 years", "(13) 11 to 15 years",
      "(14) 16 to 20 years", "(15) 21 to 25 years", "(16) 26 or more years"
    ), ordered = TRUE)
  )

## Level of measurment/variable type

# wage        -> interval/ratio, continuous   (hourly dollar amount; true zero)
# role        -> nominal, discrete            (3 categories: Aide/asst, Teacher/lead, Other)
# yrs_worked  -> ORDINAL, discrete            (16 bins of unequal width, e.g. "11 to 15 years" -
#                                               NOT a raw count, so treat as ordinal, not ratio)
# hispanic    -> nominal, discrete            (binary)
# race        -> nominal, discrete            (4 categories: White, Black, Asian, Other)
# educ        -> ORDINAL, discrete            (7 ranked categories, unequal spacing)
# gender      -> nominal, discrete            (binary)

## Descriptive Statistics


# -- Wage (continuous) --
describe_num(nsece$wage)


ggplot(nsece, aes(x = wage)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", na.rm = TRUE) +
  labs(title = "Distribution of Hourly Wage", x = "Hourly Wage ($)", y = "Count") +
  theme_minimal()

# -- Role (nominal) --
freq_table(nsece$role)

ggplot(nsece %>% filter(!is.na(role)), aes(x = fct_infreq(role))) +
  geom_bar(fill = "coral") +
  labs(title = "Staff Role", x = NULL, y = "Count") +
  coord_flip() +
  theme_minimal()


# -- Years worked (ordinal) --
freq_table(nsece$yrs_worked)

ggplot(nsece %>% filter(!is.na(yrs_worked)), aes(x = yrs_worked)) +
  geom_bar(fill = "darkseagreen") +
  labs(title = "Years Worked in Program", x = NULL, y = "Count") +
  coord_flip() +
  theme_minimal()

# -- Hispanic/Latino (nominal) --
freq_table(nsece$hispanic)

# -- Race (nominal) --
freq_table(nsece$race)

ggplot(nsece %>% filter(!is.na(race)), aes(x = fct_infreq(race))) +
  geom_bar(fill = "goldenrod") +
  labs(title = "Race", x = NULL, y = "Count") +
  coord_flip() +
  theme_minimal()

# -- Education (ordinal) --
freq_table(nsece$educ)

ggplot(nsece %>% filter(!is.na(educ)), aes(x = educ)) +
  geom_bar(fill = "plum") +
  labs(title = "Educational Attainment", x = NULL, y = "Count") +
  coord_flip() +
  theme_minimal()

# -- Gender (nominal) --
freq_table(nsece$gender)

## Question 2
##Significance Tests: Hourly Wage and Workforce Characteristics

## Set up
#'nsece' already exists from q1


## Why nonparametric tests?

##Wage is strongly right skewed -> (skewness ~5, mean $15.33 vs median $12.83 from q1)
##this means it violates the normality assumption behind t-tests/ANOVA/Pearson correlation
##Nonparametric alternatives, which tests differences in rank/distribution rather than means 
##are more appropriate and are used consistently below. 

## Wage and staff role (nominal, 3 groups)

## Test: Kruskal-Wallis (nonparametric alternative to one-way ANOVA)

kruskal.test(wage ~ role, data = nsece)
  

# Group medians for interpretation
nsece %>%
  filter(!is.na(role), !is.na(wage)) %>%
  group_by(role) %>%
  summarise(median_wage = median(wage), n = n()) 

## Wage and years worked (Ordinal)
## Test: Spearman rank correlation (handles ordinal predictor + skewed wage)
## yrs_worked is a factor -> convert to its integer rank (1-16) for the test;
## this respects the ORDER of categories without assuming equal spacing.

nsece_b <- nsece %>% filter(!is.na(wage), !is.na(yrs_worked))

cor.test(nsece_b$wage, as.integer(nsece_b$yrs_worked), method = "spearman")
cor.test(nsece_b$wage, as.integer(nsece_b$yrs_worked), method = "spearman", exact = FALSE)

## Wage and race (nominal, 4 groups)

## Test: Kruskal-Wallis

kruskal.test(wage ~ race, data = nsece)

nsece %>%
  filter(!is.na(race), !is.na(wage)) %>%
  group_by(race) %>%
  summarise(median_wage = median(wage), n = n())

## Wage and educational attainment (ordinal, 7 groups)

## Test: Spearman rank correlation (same logic as years worked)

nsece_d <- nsece %>% filter(!is.na(wage), !is.na(educ))
cor.test(nsece_d$wage, as.integer(nsece_d$educ), method = "spearman")
cor.test(nsece_d$wage, as.integer(nsece_d$educ), method = "spearman", exact = FALSE)


## Wage and gender (nominal, binary, unequal group sizes)

## Test: Mann-Whitney U / Wilcoxon rank-sum test
## (Note from Q1: sample is 97.6% female / 2.4% male, n=112 for males --
## keep this imbalance in mind when interpreting significance/effect size)

wilcox.test(wage ~ gender, data = nsece)

nsece %>%
  filter(!is.na(gender), !is.na(wage)) %>%
  group_by(gender) %>%
  summarise(median_wage = median(wage), n = n())

## Some interpretations/notes
## For each test, report: test statistic, df (where applicable), p-value,
## and a plain-language interpretation of whether the association/difference
## is statistically significant (conventionally p < .05), plus the direction
## of the effect using the group medians / correlation sign above.
