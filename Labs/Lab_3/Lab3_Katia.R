#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 3 of the Data for Policy Analysis 
# class.
# PROJECT NAME : Data for Policy Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : Aida Pacheco-Applegate
# DATE CREATED : 06-23-2026
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
output_dir     <- paste0(my_directory, "Classes/Class 3/")

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
  mutate(fullpart_time_char = as.character(fullpart_time), #mutate is used here to create new variables. I am creating a character variable and then recoding the values.
         fullpart_time_char = case_when(
           fullpart_time == "(1) Full-time worker" ~ "Full-time",
           fullpart_time == "(2) Part-time worker" ~ "Part-time"),
           
         gender_char = as.character(gender),
         gender_char = case_when(
           gender == "(2) Female" ~ "Female",
           gender == "(1) Male" ~ "Male"))

#------------------------------------------------------------------------------#
# 5. Variable summaries
#------------------------------------------------------------------------------#

# Full-time (create frequency table with no NAs count) and calculate 
# mode (nominal + discrete)

nsece_2019_fulltime_fqtable <- 
  nsece_2019_wf_recode %>%
  select(fullpart_time_char) %>% # I am only keeping the variable of interest
  count(fullpart_time_char) %>% # this create the table of counts 
  filter(!is.na(fullpart_time_char)) %>% # I am deleting the NA values
  mutate(prop = n/sum(n)) # this creates the % 

mode_fulltime <- 
  names(which.max(table(nsece_2019_wf_recode$fullpart_time_char, 
                                       useNA = "no"))) 
mode_fulltime

# Table in this code creates a frequency table of the variable of interest. useNA = "no" excludes 
# missing values from the frequency counts. which.max() identifies the category 
# with the highest frequency, and names() retrieves the corresponding category 
# label. This is necessary because the variable is categorical (character), so the 
# result returned by which.max() is the position of the maximum count rather than 
# the category name itself.

# Gender (nominal + discrete) + mode [SAME CODE AS FULL-TIME VARIABLE]

nsece_2019_gender_fqtable <- 
  nsece_2019_wf_recode %>%
  select(gender_char) %>%
  count(gender_char) %>%
  filter(!is.na(gender_char)) %>%
  mutate(prop = n/sum(n))

mode_gender <- 
  names(which.max(table(nsece_2019_wf_recode$gender_char, 
                        useNA = "no")))
mode_gender

# Months work (interval + continuous) + histogram + mode, median mean + range,
# SD, IQR

hist_months <- # this code creates a histogram of the variable months worked
  ggplot(nsece_2019_wf_recode, aes(x = months_work)) + 
  geom_histogram() 

hist_months

months_summary_table <- 
  nsece_2019_wf_recode %>%
  select(months_work) %>% # I am only keeping the variable of interest
  summarise( # summarise() calculates one summary value (or statistic) for the entire variable and stores them in the dataset
    n = sum(!is.na(months_work)),
    Mean = mean(months_work, na.rm = TRUE),
    Median = median(months_work, na.rm = TRUE),
    Min    = min(months_work, na.rm = TRUE),
    Max    = max(months_work, na.rm = TRUE),
    Range  = Max - Min,
    SD     = sd(months_work, na.rm = TRUE),
    IQR    = IQR(months_work, na.rm = TRUE))

months_mode <- 
  nsece_2019_wf_recode %>%
  filter(!is.na(months_work)) %>% # remove missing values
  count(months_work, sort = TRUE) %>% # count the frequency of each value
  slice(1) %>% # keep the first row (the most frequent value)
  pull(months_work) # extract the value as a vector

months_mode

# respect (ordinal + discrete) + freq table + mode, median, IQR 

nsece_2019_respect_fqtable <- # same code as FULL-TIME VARIABLE
  nsece_2019_wf_recode %>%
  select(respect) %>%
  count(respect) %>%
  filter(!is.na(respect)) %>%
  mutate(prop = n/sum(n))

nsece_2019_respect_summary <- 
  nsece_2019_wf_recode %>%
  select(respect) %>%
  summarise(
    n = sum(!is.na(respect)), # count the number of non-missing observations
    Mode = names(sort(table(respect), decreasing = TRUE))[1], 
    # table() counts the frequency of each category
    # sort(..., decreasing = TRUE) orders the counts from largest to smallest
    # names() retrieves the category labels
    # [1] selects the most frequent category
    Median = levels(respect)[round(median(as.numeric(respect), na.rm = TRUE))],
    # as.numeric() converts the ordered factor into its underlying numeric codes
    # median() computes the median of those codes
    # round() ensures the result is a whole-number category
    # levels() converts the numeric code back into the original category label
    Q1 = levels(respect)[quantile(as.numeric(respect), 0.25, na.rm = TRUE, type = 2)],
    # quantile() returns the numeric code corresponding to the 25th percentile
    # levels() converts that numeric code back to the category label
    Q3 = levels(respect)[quantile(as.numeric(respect), 0.75, na.rm = TRUE, type = 2)],
    # again, convert the numeric result back to the original category label
    IQR = IQR(as.numeric(respect), na.rm = TRUE))
    # since IQR requires numeric values, we use the numeric factor codes

# The variable respect is an ordered factor (ordinal variable). Because functions 
# such as median(), quantile(), and IQR() only work with numeric values, we 
# temporarily convert the factor to its underlying numeric codes using as.numeric(). 
# We then use levels() to translate those numeric codes back into the original 
# response categories (e.g., Agree, Strongly Agree) so the output remains interpretable.

# hr_wage (ratio + continuous) + hist + mode, median mean + range,
# SD, IQR [SAME AS MONTHS WORKED]

hist_wage <- 
  ggplot(nsece_2019_wf_recode, aes(x = hr_wage)) + 
  geom_histogram() 

hist_wage

wage_summary_table <- 
  nsece_2019_wf_recode %>%
  select(hr_wage) %>%
  summarise(
    n = sum(!is.na(hr_wage)),
    Mean = mean(hr_wage, na.rm = TRUE),
    Median = median(hr_wage, na.rm = TRUE),
    Min    = min(hr_wage, na.rm = TRUE),
    Max    = max(hr_wage, na.rm = TRUE),
    Range  = Max - Min,
    SD     = sd(hr_wage, na.rm = TRUE),
    IQR    = IQR(hr_wage, na.rm = TRUE))

wage_mode <- 
  nsece_2019_wf_recode %>%
  filter(!is.na(hr_wage)) %>%
  count(hr_wage, sort = TRUE) %>%
  slice(1) %>%
  pull(hr_wage)

wage_mode

## add-on --> what if we delete outliers? 
