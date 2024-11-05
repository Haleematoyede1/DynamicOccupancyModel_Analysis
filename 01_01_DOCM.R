# Title:    Dynamic Occupancy Analysis (2007 - 2008)
# File:     01_01_DOCM.R
# Project:  Range Expansion Of Hadedas Ibis In South Africa 2007 - 2022

# Install required Packages
install.packages("remotes")
remotes::install_github("AfricaBirdData/ABAP")

# Load required libraries
library(ABAP)
library(dplyr)

# LOAD AND PREPARE BRID DATA ####################################
dc_01 <- getAbapData(.spp = 84, 
                     .region_type = "country", 
                     .region = "SouthAfrica",
                     .years = 2007:2008)

print(dc_01, width = Inf) #to view all columns

## Data Cleaning & Manipulation Process ================================
library(dplyr)
dc_01 <- select(dc_01, -c(StartTime,Hour1,Hour2,Hour3,Hour4,Hour5,Hour6,Hour7,Hour8,Hour9,Hour10,
                          TotalSpp,InclNight,AllHabitats,Common_name,Taxonomic_name))#excluding columns not needed
dc_01 <- dc_01 %>%
  mutate(Spp = ifelse(Spp == '84', 1, 
                      ifelse(Spp == '-', 0, NA_integer_)))
sum(dc_01$TotalHours<2, na.rm = TRUE)#checking to see where the survey which wasn't up to 2 hours
dc_01 <- dc_01[!(dc_01$TotalHours < 2 | is.na(dc_01$TotalHours)), ]#removing the submitted cards where survey wasn't up to 2 hours
sum(is.na(dc_01))#checking for any missing values 
