#title: "Bill tracker cleaning"
#author: "Laurel"
#date: "2024-10-23"
#output:
#  html_document: default
#  pdf_document: default

  
library(readr)
library(tidyverse)
library(dplyr)
library(sf)
library(knitr)
library(DT)
library(lubridate)
library(stringr)

#set working directory - make sure to change to your own wd 
setwd("/Users/laureldenyer/Documents/GitHub/thesis/ca-alternative-cesci")

bills <- read.csv("data/raw/CES use tracker - Bills_leg.csv")

#getting df ready for R
colnames(bills) = bills[3, ] # the third row will be the headers for variables
bills = bills[-c(1:3), ]          # removing the first 3 rows.
rename_list <- list() #empty list

for (i in 1:8) { #renaming columns
  rename_list[[paste0("I", i, "_code")]] <- paste0("I", i, "_Code change, if any")
  rename_list[[paste0("I", i, "_text")]] <- paste0("I", i, "_CalEnviroScreen/HSC Text")
  rename_list[[paste0("I", i, "_employ")]] <- paste0("I", i, "_CES or HSC?")
  rename_list[[paste0("I", i, "_use")]] <- paste0("I", i, "_CalEnviroScreen/HSC Use Category")
}
bills <- bills %>% 
  rename(!!!rename_list)

#date standardization: applying legislative sessions categories to each bill
bills <- bills %>% 
  mutate(`Introduction date`=str_trim(`Introduction date`),
         `Introduction date`=mdy(`Introduction date`),
         leg_session=case_when(
           `Introduction date` >= as.Date("2010-10-01") & `Introduction date` <= as.Date("2012-09-30") ~ "2011-2012",
           `Introduction date` >= as.Date("2012-10-01") & `Introduction date` <= as.Date("2014-09-30") ~ "2013-2014",
           `Introduction date` >= as.Date("2014-10-01") & `Introduction date` <= as.Date("2016-09-30") ~ "2015-2016",
           `Introduction date` >= as.Date("2016-10-01") & `Introduction date` <= as.Date("2018-09-30") ~ "2017-2018",
           `Introduction date` >= as.Date("2018-10-01") & `Introduction date` <= as.Date("2020-09-30") ~ "2019-2020",
           `Introduction date` >= as.Date("2020-10-01") & `Introduction date` <= as.Date("2022-09-30") ~ "2021-2022",
           `Introduction date` >= as.Date("2022-10-01") & `Introduction date` <= as.Date("2024-09-30") ~ "2023-2024",
           TRUE ~ NA_character_))

#add unique ID for aggregating later
bills_unique <- bills %>%
  distinct(`Bill/legislative name`,`Introduction date`, .keep_all = TRUE) %>%  
  arrange(`Bill/legislative name`, leg_session) %>%  
  mutate(bill_id = row_number()) %>%                      
  relocate(bill_id, .before = 1)   


# Separate multiple categories in the Status and Topic Category columns
bills_separate <- bills_unique %>%
  separate_rows(Status, sep = ", ") %>%
  separate_rows(`Topic category`, sep = ", ")
bills_separate <- bills_separate %>%
  separate_rows(I1_use, sep = ", ") %>%
  separate_rows(I2_use, sep = ", ") %>%
  separate_rows(I3_use, sep = ", ") %>%
  separate_rows(I4_use, sep = ", ") %>%
  separate_rows(I5_use, sep = ", ") %>%
  separate_rows(I6_use, sep = ", ") %>%
  separate_rows(I7_use, sep = ", ") %>%
  separate_rows(I8_use, sep = ", ")


#Code cleaning
code_cols <- c("I1_code","I2_code","I3_code","I4_code",
               "I5_code","I6_code","I7_code","I8_code")

#Breaking up the I#_code variable for analysis
code_categories <- bills_separate %>% 
  mutate(
    across(all_of(code_cols),~case_when(
      str_detect(., "(?i)\\badded\\b")~"added",
      str_detect(.,"(?i)\\bamended\\b")~"amended",
      TRUE~NA_character_
    ), .names="action_{col}")
  ) %>% 
  mutate(
    across(all_of(code_cols), ~ case_when(
      str_detect(., "(?i)\\bpublic utilities code\\b") ~ "Public Utilities Code",
      str_detect(., "(?i)\\bhealth and safety code|hsc\\b") ~ "Health and Safety Code",
      str_detect(., "(?i)\\bstreets and highways code\\b") ~ "Streets and Highways Code",
      str_detect(., "(?i)\\bpublic resources code\\b") ~ "Public Resources Code",
      str_detect(., "(?i)\\bgovernment code\\b") ~ "Government Code",
      str_detect(., "(?i)\\bfood and agricultural code\\b") ~ "Food and Agricultural Code",
      str_detect(., "(?i)\\beducation code\\b") ~ "Education Code",
      str_detect(., "(?i)\\bcode of civil procedure\\b") ~ "Code of Civil Procedure",
      str_detect(., "(?i)\\bwater code\\b") ~ "Water Code",
      str_detect(., "(?i)\\bfish and game code\\b") ~ "Fish and Game Code",
      str_detect(., "(?i)\\bstatutes\\b") ~ "Statutes",
      str_detect(., "(?i)\\bfindings\\b") ~ "Findings",
      str_detect(., "(?i)\\bbudget\\b") ~ "Budget",
      TRUE ~ NA_character_
    ), .names = "code_type_{col}")
  )
  
code_categories %>% select(starts_with("action"), starts_with("code_type"))

#Create Dummy Variabless

# Create dummy variables for 'added', 'amend', and code types
code_categories <- bills_separate %>%
  mutate(
    # Dummy variables for 'added'
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\badded\\b"), 1, 0), .names = "added_{col}"),
    
    # Dummy variables for 'amend'
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bamend\\b|\\bamended\\b|\\bamending\\b"), 1, 0), .names = "amend_{col}"),
    
    # Dummy variables for different types of legal codes
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bpublic utilities code\\b"), 1, 0), .names = "public_utilities_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bhealth and safety code|hsc\\b"), 1, 0), .names = "health_safety_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bstreets and highways code\\b"), 1, 0), .names = "streets_highways_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bpublic resources code\\b"), 1, 0), .names = "public_resources_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bgovernment code\\b"), 1, 0), .names = "government_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bfood and agricultural code\\b"), 1, 0), .names = "food_agricultural_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\beducation code\\b"), 1, 0), .names = "education_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bcode of civil procedure\\b"), 1, 0), .names = "civil_procedure_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bwater code\\b"), 1, 0), .names = "water_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bfish and game code\\b"), 1, 0), .names = "fish_game_code_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bstatutes\\b"), 1, 0), .names = "statutes_{col}"),
    across(all_of(code_cols), ~ if_else(str_detect(., "(?i)\\bfindings\\b"), 1, 0), .names = "findings_{col}")
  )

# Check the new dummy variables
code_categories %>% select(starts_with("added"), starts_with("amend"), starts_with("public_utilities_code"), starts_with("health_safety_code"))


# Make more dummy variables
cols_to_dummy <- c('Status', 'Topic category', 'I1_employ', 'I1_use', 
                   'I2_employ', 'I2_use', 'I3_employ', 'I3_use', 
                   'I4_employ', 'I4_use', 'I5_employ', 'I5_use', 
                   'I6_employ', 'I6_use', 'I7_employ', 'I7_use', 
                   'I8_employ', 'I8_use')


dummy_df <- fastDummies::dummy_cols(bills_separate, 
                                    select_columns = cols_to_dummy, 
                                    remove_first_dummy = FALSE)

colnames(dummy_df)
# Create dummy variables for cols_to_dummy columns
#all_dv <- as.data.frame(model.matrix(~ Status + `Topic category` + I1_employ + I1_use + I2_employ + I2_use + 
#                                       I3_employ + I3_use + I4_employ + I4_use + 
#                                       I5_employ + I5_use + I6_employ + I6_use + 
#                                       I7_employ + I7_use + I8_employ + I8_use - 1, data = bills_separate))
#colnames(dummy_df) <- gsub("^'|'$", "", colnames(dummy_df))
#colnames(dummy_df) <- gsub("\\.\\.\\.[0-9]+$", "", colnames(dummy_df))

dummy_df_clean <- dummy_df %>%
  select(-ends_with("_"))
colnames(dummy_df_clean)

#remove columns that appear in both dfs
code_categories <- code_categories[-c(1:45)]

#combined_df <- bind_cols(code_categories, all_dv)
combined_df <- bind_cols(dummy_df, code_categories)
head(combined_df)
colnames(combined_df)
combined_df <- combined_df %>%
  select(-ends_with("_"))
colnames(combined_df)

#clean
#combined_df <- combined_df %>%
#  rename(StatusNew = `Status...158`, Status = `Status...5`)

 # ^ THIS IS THE MASTER COMBINED DV DF - WOOHOO

#save it for analysis work
write.csv(combined_df,"data/interim/policy/combined_dv.csv") 


