#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 2 of the Data for Policy Analysis 
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

package.list <- c("tidyverse", "ggplot2") # addition a new package for this class

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
output_dir     <- paste0(my_directory, "Classes/Class 2/")

#------------------------------------------------------------------------------#
# 2. Load dataset
#------------------------------------------------------------------------------#

# Import NSECE 2019 workforce questionnnaire

load(paste0(my_data, "37941-0005-Data.rda"))

## To import .CSV files use this code
## data <- read.csv("[full directory here]")

#[ALL CODE FROM LINE 1 TO 50 IS A COPY OF THE CODE WE HAVE IN LAB 1. NO CHANGES SHOULD BE MADE UNLESS YOU NEED TO CHANGE YOUR DIRECTORY]

#------------------------------------------------------------------------------#
# 3. Basic exploratory data inspection
#------------------------------------------------------------------------------#

# Understand variable class CB9_SERVE_0TO5YRS, WF9_CHAR_GENDER, WF9_WORK_MONTHS, 
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
         respect = WF9_WORK_RESPECT, #orginal + discrete
         hr_wage = WF9_WORK_WAGE) #ratio + continuous 

# I suggest keeping the factor variables for ordinal variables but work with 
# numeric / character values for all other types of variables. 

# Recode full/part-time and gender

nsece_2019_wf_recode <- 
  nsece_2019_wf_subset %>%
  mutate(fullpart_time_char = as.character(fullpart_time),  #mutate is used here to create new variables 
         fullpart_time_char = case_when(
           fullpart_time == "(1) Full-time worker" ~ "Full-time",
           fullpart_time == "(2) Part-time worker" ~ "Part-time"),
           
         gender_char = as.character(gender),
         gender_char = case_when(
           gender == "(2) Female" ~ "Female",
           gender == "(1) Male" ~ "Male"))

#------------------------------------------------------------------------------#
# 5. Variable display
#------------------------------------------------------------------------------#

#Full-time variable 

nsece_2019_fulltime_fqtable <- 
  nsece_2019_wf_recode %>%
  select(fullpart_time_char) %>% #instead of working with the complete data I just decided to work with the variable of interest to avoid confusion
  count(fullpart_time_char) %>% # this line is create a table of counts for all the values of the variable but it is including NAs 
  mutate(total_sample = sum(n[!is.na(fullpart_time_char)]), # this line counts the total number of observations in my data but without including NAs. See !is.na (the "!" sign indicates R to DO NOT INCLUDE)
         prop = ifelse(!is.na(fullpart_time_char), # the following lines are creating the proportion of my frequency table only when the value of the variable is NOT NA
                       n / total_sample, # the "ifelse" command indicates R to do something if a condition is met, in this case: if the value of the variable fullpart_time_char is NOT NA, then generate a proportion, if the value is NA then keep NA
                       NA))
  
#Months work 

hist_months <- 
  ggplot(nsece_2019_wf_recode, aes(x = months_work)) + 
  geom_histogram() 

count_nas_month <- sum(is.na(nsece_2019_wf_recode$months_work))

hist_months

#------------------------------------------------------------------------------#
# 6. Save tables and graphs
#------------------------------------------------------------------------------#

write.csv(nsece_2019_fulltime_fqtable, 
          paste0(output_dir, "Lab/NSECE2019_fulltime_table.csv"), 
          row.names = FALSE)

ggsave(paste0(output_dir, "Lab/histogram_months.png"),
       plot = hist_months)
