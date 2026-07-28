#------------------------------------------------------------------------------#
# This file xxxxx
# PROJECT NAME : xxxxx
# DATA SETS USED BY THIS CODE : 37941-0005-Data.rda
# R VERSION : 4.5.3
# AUTHOR : xxxx
# DATE CREATED : xxxxx
# NOTES : xxxx
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Set up working space and import packages
#------------------------------------------------------------------------------#

rm(list=ls())
options("error") 
options(lifecycle_disable_verbose_retirement = TRUE)

# Import packages

package.list <- c("tidyverse")

for (p in package.list){
  if (!p %in% installed.packages()[, "Package"]) install.packages(p)
  library(p, character.only=TRUE)
}