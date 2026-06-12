#----------------- Install, load libraries and datasets -------------------
# Clear environment
rm(list=ls())


# Step-1: Install and load library
Packages <- c("gtsummary","foreign","survey",'panelr',"readxl", "tidyverse", "haven", "forcats","cyphr", "getPass" )


new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = T)


# load libraries
lapply(Packages, require, character.only=T)


# remove unnecessary objects from environment
rm(list=c("new_packages", "Packages"))






