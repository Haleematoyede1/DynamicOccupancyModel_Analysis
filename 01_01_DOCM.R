# Title:    Dynamic Occupancy Analysis (2007 - 2008)
# File:     01_01_DOCM.R
# Project:  Range Expansion Of Hadedas Ibis In South Africa 2007 - 2022

# Install required Packages
#install.packages("remotes")
#remotes::install_github("African/ABAP")

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

sum(is.na(dc_01))#checking for any missing values
dc_01 <- dc_01 %>%
  as.data.frame() %>%
  mutate(Sequence = ifelse(Sequence == '-', 0, as.numeric(Sequence)))%>%#converting the sequence column to numeric
  rename(TotalSpecies = Sequence)
#summary(dc_01)#summary of the data set

### Investigating protocols that do not meet the  requirement for standard protocol-------------------------
library(dplyr)
library(lubridate)

sum(dc_01$TotalHours<2, na.rm = TRUE)#Investigating protocols which wasn't up to 2 hours
dc_01 <- dc_01[!(dc_01$TotalHours < 2 | is.na(dc_01$TotalHours)), ]#removing protocols < 2 hours

dc_01$StartDate <- as.Date(dc_01$StartDate, formart = '%Y-%m-%d')#Formatting the start date column
dc_01$EndDate <- as.Date(dc_01$EndDate, formart = '%Y-%m-%d')#formatting the end date column

dc_01 <- dc_01 %>%
  mutate(IntervalDays = as.numeric(difftime(EndDate, StartDate, units = "days")))#Investigating the interval days
dc_01 <- dc_01[dc_01$IntervalDays >= 0 & dc_01$IntervalDays <= 5, ]# Remove protocols that don't fall within the 0-5 days interval
#summary(dc_01)

### Grouping protocols into  different weather seasons--------------------------------------------
get_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    return("Summer")
  } else if (month %in% c(6, 7, 8)) {
    return("Winter")
  } else if (month %in% c(3, 4, 5)) {
    return("Autumn")
  } else if (month %in% c(9, 10, 11)) {
    return("Spring")
  }
}# Function to group protocols into seasons

dc_01$Season <- sapply(dc_01$StartDate, get_season)#new column for the season
#summary(dc_01)#summary of the data set

### Create latitude and longitude columns--------------------------------------------
dc_01$Latitude <- -(as.numeric(substr(dc_01$Pentad, 1, 2)) + (as.numeric(substr(dc_01$Pentad, 3, 4)) + 2.5) / 60)
dc_01$Longitude <- (as.numeric(substr(dc_01$Pentad, 6, 7)) + (as.numeric(substr(dc_01$Pentad, 8, 9)) + 2.5) / 60)
#summary(dc_01)#summary of the data set






#########################################################################
library(dplyr)
library(lubridate)

test <- dc_01 %>%
  mutate(Season = case_when(
    year(StartDate) == 2007 ~ "Season 2007",
    year(StartDate) == 2008 ~ "Season 2008",
    TRUE ~ "Other"
  ))

# View the first few rows to check the new column
head(test)
(Surv_hist <- table(test$Pentad,test$Season))  ##to see how many visits in each pentad in the different season
(Max_surv <- apply(Surv_hist,2,max))                      ###The maximum number of visits in each season for individual pentads
sum(Max_surv)
summary(Max_surv)
