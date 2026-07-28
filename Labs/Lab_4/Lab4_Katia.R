#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 4 of the Data for Policy Analysis 
# class.
# PROJECT NAME : Data for Policy Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : Aida Pacheco-Applegate
# DATE CREATED : 07-7-2026
# NOTES : 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 1. Set up working space and import packages
#------------------------------------------------------------------------------#

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages

package.list <- c("tidyverse", "ggplot2")

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}

# Set up working directory 

## check your directory
getwd()

## set up working directory 

my_directory   <- "/Users/aidapacheco-applegate/Desktop/PhD/Summer 2026/Data for Policy Analysis/"
my_data        <- paste0(my_directory, "Data/")
output_dir     <- paste0(my_directory, "Classes/Class 4/")

#------------------------------------------------------------------------------#
# 2. Load dataset
#------------------------------------------------------------------------------#

# Import NSECE 2019 workforce questionnnaire

load(paste0(my_data, "37941-0005-Data.rda"))

## To import .CSV files use this code
## data <- read.csv("[full directory here]")

#------------------------------------------------------------------------------#
# 3. Basic exploratory data inspection
#------------------------------------------------------------------------------#

# Understand variable class WF9_WORK_FT, WF9_CHAR_GENDER, WF9_WORK_MONTHS, 
# WF9_WORK_RESPECT, WF9_WORK_WAGE 

class(da37941.0005$WF9_WORK_FT) #factor
class(da37941.0005$WF9_CHAR_GENDER) #factor
class(da37941.0005$WF9_WORK_MONTHS) #numeric
class(da37941.0005$WF9_WORK_RESPECT) #factor
class(da37941.0005$WF9_WORK_WAGE) #numeric

#------------------------------------------------------------------------------#
# 4. Basic cleaning and manipulation
#------------------------------------------------------------------------------#

# Rename dataset

nsece_2019_wf <- da37941.0005

# Subset dataset and rename variables

nsece_2019_wf_subset <- 
  nsece_2019_wf %>%
  select(WF9_WORK_FT, WF9_CHAR_GENDER, WF9_WORK_MONTHS, WF9_WORK_RESPECT,
         WF9_WORK_WAGE) %>%
  rename(fullpart_time = WF9_WORK_FT, #nominal + discrete
         gender = WF9_CHAR_GENDER, #nominal + discrete
         months_work = WF9_WORK_MONTHS, #interval + continuous
         respect = WF9_WORK_RESPECT, #ordinal + discrete
         hr_wage = WF9_WORK_WAGE) #ratio + continuous 

# I suggest keeping the factor variables for ordinal variables but work with 
# numeric / character values for all other types of variables. 

# Recode full/part-time and gender

nsece_2019_wf_recode <- 
  nsece_2019_wf_subset %>%
  mutate(fullpart_time_char = as.character(fullpart_time),
         fullpart_time_char = case_when(
           fullpart_time == "(1) Full-time worker" ~ "Full-time",
           fullpart_time == "(2) Part-time worker" ~ "Part-time"),
           
         gender_char = as.character(gender),
         gender_char = case_when(
           gender == "(2) Female" ~ "Female",
           gender == "(1) Male" ~ "Male"))

#------------------------------------------------------------------------------#
# 5. Bivariate Relationships
#------------------------------------------------------------------------------#

# Employment Status vs. Gender (discrete vs. discrete)
# a. Create contingency table (full_time status by gender / row %)

cont_table_fulltime_gender <-
  nsece_2019_wf_recode %>% 
  select(fullpart_time_char, gender_char) %>% # keep only the two variables of interest
  drop_na(fullpart_time_char, gender_char) %>% # remove observations with missing values in either variable
  count(gender_char, fullpart_time_char) %>% # counts the number of observations for each combination of gender and employment status
  group_by(gender_char) %>% # this tells R to perform subsequent calculations separately for each gender category
  mutate(proportion = (n/sum(n))*100) # sum(n) calculates the total number of observations within each gender n/sum(n) computes the proportion within each gender

# b. Interpret relationship

# The distribution of full-time and part-time employment is similar for men and women. 
# More than three-quarters of both groups work full-time, while fewer than one-quarter 
# work part-time. Specifically, 76% of women work full-time compared with 78% of men, 
# suggesting only a small difference in employment status by gender.

# Hourly wage vs. gender  (continuous vs. discrete)
# a. Create contingency table (hourly wage by gender / conditional means)

cont_table_wage_gender <-
  nsece_2019_wf_recode %>% 
  select(hr_wage, gender_char) %>% # keep only the variables of interest
  drop_na(hr_wage, gender_char) %>% # remove observations with missing values in either variable
  group_by(gender_char) %>% # group the data by gender so that summary statistics are calculated separately for each group
  summarize(n = n(), # creates a summary table containing n: the number of observations in each gender group
            mean_wage = mean(hr_wage, na.rm = TRUE)) # and mean_wage: the average hourly wage within each gender group

# b. Interpret relationship

# The distribution of average hourly wages differs slightly between men and women. 
# Although men represent a much smaller share of the workforce than women, their 
# average hourly wage is higher: men earn an average of $18.80 per hour compared 
# with $15.20 per hour among women.

# Number of months worked vs. hourly wage (continuous - continuous)
# a. Create scatterplot 

scatter_wage_months <- 
  ggplot(nsece_2019_wf_recode, aes(x = months_work, y = hr_wage)) + # create a scatterplot using the dataset
  geom_point() + # add one point for each observation
  geom_smooth(method = "lm") # tells ggplot to fit a linear model

scatter_wage_months

# b. Calculate correlation

cor_wage_months <- 
  nsece_2019_wf_recode %>%
  select(months_work, hr_wage) %>%  # keep only the two variables of interest
  drop_na(months_work, hr_wage) %>% # remove observations with missing values in either variable
  summarise(correlation = cor(months_work, hr_wage)) # create a summary table with the Pearson correlation coefficient

# b. Interpret relationship

# After examining the graph and the calculated correlation coefficient of 0.036, we can 
# conclude that there is a very weak positive association between the number of months 
# worked during the last 12 months and hourly wage. However, the correlation is very 
# close to zero, indicating that there is little linear relationship between 
# these two variables. 

#------------------------------------------------------------------------------#
# 5. Statistical tests of association 
#------------------------------------------------------------------------------#

# Employment Status vs. Gender (discrete vs. discrete) - chi-square test

chi_test_fulltime_gender <- 
  chisq.test(xtabs(n ~ gender_char + fullpart_time_char, 
                   data = cont_table_fulltime_gender))

# Create a contingency table using the count variable (n)
# the rows represent gender categories
# the columns represent employment status categories
chi_test_fulltime_gender

# Interpretation: The results indicate no statistically significant association 
# between gender and full-time/part-time status. Although 
# men and women had slightly different full-time employment rates (78% vs. 76%), 
# the difference is not statistically significant.

# Hourly wage vs. gender (continuous vs. discrete) - two sample t-test

t_test_wage_gender <- 
  t.test(hr_wage ~ gender_char, data = nsece_2019_wf_recode, na.action = na.omit)

# compare the mean of hourly wage (hr_wage) across categories of gender (gender_char)
# remove observations with missing values before running the test
t_test_wage_gender

# Interpretation: The difference was not statistically significant at the 5% 
# significance level. However, because the p-value 
# is just above the 0.05 threshold, the result is close to statistical significance 
# and would be considered statistically significant at the 10% significance level. 
# This suggests some evidence of a difference in average hourly wages by gender, 
# although the evidence is not strong enough to conclude a statistically significant 
# difference using the conventional 5% criterion.

# Number of months worked vs. hourly wage - linear regression 

lm_wage_months <- lm(hr_wage ~ months_work,
                     data = nsece_2019_wf_recode)

summary(lm_wage_months)

# Interpretation: The results indicate a statistically significant positive 
# association between months work and hourly wage. We do have enough evidence 
# to reject the null hypothesis of no relationship. The coefficient of 0.13 indicates 
# that each additional month worked is associated with an average increase 
# of approximately $0.13 in hourly wage. However, the relationship is very 
# weak, as the model explains only about 0.1% of the variation in hourly wages. 
