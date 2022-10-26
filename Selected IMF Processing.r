#******************************************************************************#
#* Pulling from IMF API *******************************************************#
#******************************************************************************#

library(jsonlite)
library(tidyverse)
library(janitor)
library(dplyr)
library(readxl)
library(data.table)
library(stringr)


#******************************************************************************#
# Import Database/Indicators List ----------------------------------------------
##+ group indicators by DB
##+ create list of indicators
#******************************************************************************#

imf_Indicators_Full <- read.csv("Final IMF Indicators.csv")  %>%   
  clean_names(case = c("snake"))  %>%
  rename(database_id = i_database)

imf_Indicators_Compress <- imf_Indicators_Full %>%
  group_by(database_id) %>%
  summarise(all_indicators = list(indicator_code))
  
#******************************************************************************#
# Create list of countries (iso2codes) relevant to pull ------------------------
##+ Import Country List
##+ Eliminate irrelevant countries (if necessary)
##+ Create "country split" list for import
#******************************************************************************#

afr_countryList_imp <- read_excel("Country List.xlsx")
country_list <- c(unlist(list(afr_countryList_imp$iso2code), recursive = TRUE, use.names = TRUE))
country_split <- split(country_list, ceiling(seq_along(country_list) / 10))


#******************************************************************************#
# Run Functions ----------------------------------------------------------------
##+ Call the following functions in order
#******************************************************************************#

imf_DB_NamesID   <- getIMF_DbInfo(imf_Indicators_Compress)

imf_metadata <- getIMF_MetaData(imf_DB_NamesID)

date_start <- '2012'
date_end <- '2022'

imf_data <- getIMF_Data(imf_Indicators_Full)
Footer
© 2022 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
