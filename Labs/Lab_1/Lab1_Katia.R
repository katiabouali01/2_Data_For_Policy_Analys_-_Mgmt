#------------------------------------------------------------------------------#
# This file prepares NSECE 2019 data for Lab 1 of the Data for Policy Analysis 
# class.
# PROJECT NAME : Data for Policy Analysis
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : Aida Pacheco-Applegate
# DATE CREATED : 06-19-2026
# NOTES : 
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 1. Set up working space and import packages
#------------------------------------------------------------------------------#

# The next three lines of code (lines 17–19) are used to clean the working
# environment. Essentially, they ensure that no other objects or datasets are
# loaded into memory that could interfere with or create issues when running
# the code.

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages 

package.list <- c("tidyverse") #list all the packages you are going to use here

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}

# Set up working directory 

## check your directory 
getwd() 

## set up working directories

my_directory     <- "/Users/aidapacheco-applegate/Desktop/PhD/Summer 2026/Data for Policy Analysis/"
# my_data        <- paste0(my_directory, "Data/")
# output_dir     <- paste0(my_directory, "Classes/Class 1/")

#------------------------------------------------------------------------------#
# 2. Load dataset
#------------------------------------------------------------------------------#

# Import NSECE 2019 workforce questionnnaire (since the data is already in R
# format we just need to use the line 52 of this code to load it into our work 
# environment)

load("/Users/aidapacheco-applegate/Desktop/PhD/Summer 2026/Data for Policy Analysis/Data/37941-0005-Data.rda")

## To import .CSV files use this code
## data <- read.csv("[full directory here]")

#------------------------------------------------------------------------------#
# 3. Basic exploratory data inspection
#------------------------------------------------------------------------------#

# Look variable names 
colnames(da37941.0005) 

# Look for type and structure of WF9_CHAR_EDUC, WF9_CHAR_HISP, WF9_CHAR_GENDER, 
# WF9_WORK_WAGE

class(da37941.0005$WF9_CHAR_EDUC) # this indicates the variable class (factor / numeric / character)
str(da37941.0005$WF9_CHAR_EDUC) # this indicates the variable class and their values
tabulate(da37941.0005$WF9_CHAR_EDUC) # this creates a simple tabulation of the variable (counts)

class(da37941.0005$WF9_CHAR_HISP)
str(da37941.0005$WF9_CHAR_HISP)
tabulate(da37941.0005$WF9_CHAR_HISP)

class(da37941.0005$WF9_CHAR_GENDER)
str(da37941.0005$WF9_CHAR_GENDER)
tabulate(da37941.0005$WF9_CHAR_GENDER)

class(da37941.0005$WF9_WORK_WAGE)
str(da37941.0005$WF9_WORK_WAGE)
tabulate(da37941.0005$WF9_WORK_WAGE)

# Summarize variables
summary(da37941.0005$WF9_CHAR_EDUC) # this create a table of counts for each category of the variable (if variable is categorical) OR a table with summary statistics (if variable is numeric). 

#------------------------------------------------------------------------------#
# 4. Basic cleaning and manipulation
#------------------------------------------------------------------------------#

# Rename dataset

nsece_2019_wf <- da37941.0005

# Subset dataset 
# (this way of writing code is typical of the library dplyr. We always start with an arrow "<-" and then follow the rest of the code with "%>%")

nsece_2019_wf_subset <- # this line is creating a new dataset that contain all the manipulation performed in the following lines
  nsece_2019_wf %>% # this is the data source of the new data that I will create (the name of the "old" data)
  select(WF9_CHAR_EDUC, WF9_CHAR_HISP, WF9_CHAR_GENDER, WF9_WORK_WAGE) # the command "select" indicates that I will only work for the variables listed here. No other variables will be included in the new dataset

# Rename variables

nsece_2019_wf_rename <- # this line is creating a new dataset 
  nsece_2019_wf_subset %>% # this is the "old data"
  rename(educ_level = WF9_CHAR_EDUC, #the following lines rename variables 
         hispanic = WF9_CHAR_HISP,
         gender = WF9_CHAR_GENDER,
         hr_wage = WF9_WORK_WAGE)

# Create new variables and recode variables

nsece_2019_wf_recode <- # this line is creating a new dataset
  nsece_2019_wf_rename %>% # this is the "old data"
  mutate(educ_level_recode = case_when( # the command "mutate" is used to create a new variable and "case_when" changes values of a variable. In this case I am creating a new variable "educ_level_recode" with new value names.
    educ_level == "(1) Less than High School" ~ "Less than HS",
    educ_level == "(3) GED or high school equivalency" ~ "GED",
    educ_level == "(4) High school graduate" ~ "HS",
    educ_level == "(5) Some college credit but no degree" ~ "Some college",
    educ_level == "(6) Associate degree (AA, AS)" ~ "AA",
    educ_level == "(7) Bachelor's degree (BA, BS, AB)" ~ "BA",
    educ_level == "(8) Graduate or professional degree" ~ "Graduate"
  ))

class(nsece_2019_wf_recode$educ_level_recode)
str(nsece_2019_wf_recode$educ_level_recode)

# Work with factor variables (a factor is a type of variable that stores and manages categorical data with fixed values)

nsece_2019_wf_factor1 <- # this is creating a new dataset
  nsece_2019_wf_recode %>% # this is the "old" dataset
  mutate(educ_level_recode = as.factor(educ_level_recode)) # mutate is creating a new variable "educ_level_recode" that is a factor.

class(nsece_2019_wf_factor1$educ_level_recode)
str(nsece_2019_wf_factor1$educ_level_recode)
summary(nsece_2019_wf_factor1$educ_level_recode) # After this inspection I see that the values of the categorical variable are ordered in alphabetical order. However, educational attainment should have fixed ordered values so the next code will fix this.

nsece_2019_wf_factor2 <- # this is creating a new dataset
  nsece_2019_wf_recode %>% # this is the "old" dataset
  mutate(educ_level_recode = factor(educ_level_recode, # mutate is overwriting the variable educ_level_recode with the ordered values I want
                                    levels = c("Less than HS", 
                                               "GED",
                                               "HS",
                                               "Some college",
                                               "AA",
                                               "BA",
                                               "Graduate"),
                                    labels = c("Less than HS", 
                                               "GED",
                                               "HS",
                                               "Some college",
                                               "AA",
                                               "BA",
                                               "Graduate")))

class(nsece_2019_wf_factor2$educ_level_recode)
str(nsece_2019_wf_factor2$educ_level_recode)
summary(nsece_2019_wf_factor2$educ_level_recode) # now I see that the new factor variable is ordered the way I want

# Convert between character and numeric variables

nsece_2019_wf_cha <-  # this is creating a new dataset
  nsece_2019_wf_factor2 %>% # this is the "old" dataset
  mutate(gender_char = as.character(gender)) # mutate is creating a new variable gender_char that creates a character variable based on the factor variable "gender"

class(nsece_2019_wf_cha$gender_char)
str(nsece_2019_wf_cha$gender_char)
table(nsece_2019_wf_cha$gender_char) # with table I create a table of counts for the values of the variable gender_char

nsece_2019_wf_cha_num <- # this is creating a new dataset
  nsece_2019_wf_cha %>% # this is the "old" dataset
  mutate(female = case_when( # with mutate I am creating a numeric variable for female, where 1 represent female and 0 represents male
    gender_char == "(1) Male" ~ 0,
    gender_char == "(2) Female" ~ 1))

class(nsece_2019_wf_cha_num$female)
str(nsece_2019_wf_cha_num$female)
table(nsece_2019_wf_cha_num$female) # I can see that I created the variable successfully (compared to table from gender_char)

#------------------------------------------------------------------------------#
# 5. Save new dataset
#------------------------------------------------------------------------------#

# use this code if you want to save the new dataset as a CSV file
write.csv(nsece_2019_wf_cha_num,  
          paste0(output_dir, "Lab/NSECE2019_subset.csv"), row.names = FALSE)

# use this code if you want to save the new dataset as a R file
save(nsece_2019_wf_cha_num, file =  
     paste0(output_dir, "Lab/NSECE2019_subset.rda"))
