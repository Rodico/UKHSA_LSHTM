## Clear workspace
rm(list=ls())

## Load packages
library(dplyr)
library(readr)

## Read in data (April 2015 - December 2023)
# Define folder path
folder_path <- "/Users/jennypage/Desktop/Data Challenge/ICB_Data"

# List all CSV files in the folder
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Initialise an empty list to store data
data_list <- list()

# Loop through each file, read it, and store it in the list
for (file in file_list) {
  df <- read_csv(file)  # Read CSV file
  
  df <- df %>%
    mutate(FILE_NAME = basename(file))
  
  data_list[[file]] <- df  # Store in list
}

# Combine all data into one dataframe
icb_data_all <- bind_rows(data_list) %>% select(-FILE_NAME, -BNF_CHEMICAL_SUBSTANCE_CODE)
rm(data_list, df)

## Data cleaning

# Define the correct order of age bands
age_levels <- c("0-1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "36-40", 
                "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-80", 
                "81-85", "86-90", "91-95", "96-100", "101-105", "105+", "Unknown")  

# Convert AGE_BAND into an ordered factor
icb_data_all <- icb_data_all %>%
  mutate(AGE_BAND = factor(AGE_BAND, levels = age_levels, ordered = TRUE))

# Handle small number suppression - replacing '*' with 3 for UNIQUE_PATIENT_COUNT and ITEMS before summarisation
icb_data_all <- icb_data_all %>%
  mutate(
    UNIQUE_PATIENT_COUNT = ifelse(UNIQUE_PATIENT_COUNT == "*", 3, UNIQUE_PATIENT_COUNT),
    ITEMS = ifelse(ITEMS == "*", 3, ITEMS))

##  Summarise counts by YEAR_MONTH, ICB_CODE, GENDER, and AGE_BAND
prescriptions_stratified <- icb_data_all %>%
  group_by(YEAR_MONTH, ICB_CODE, GENDER, AGE_BAND) %>%
  summarise(UNIQUE_PATIENTS = sum(as.numeric(UNIQUE_PATIENT_COUNT), na.rm = TRUE),  # Sum unique patients
            TOTAL_ITEMS = sum(as.numeric(ITEMS), na.rm = TRUE),  # Sum prescription items
    .groups = "drop") %>%
  arrange(YEAR_MONTH, ICB_CODE, GENDER, AGE_BAND)

##  Summarise counts by YEAR_MONTH, and ICB_CODE
prescriptions_summary <- icb_data_all %>%
  group_by(YEAR_MONTH, ICB_CODE) %>%
  summarise(
    TOTAL_PATIENTS = sum(as.numeric(UNIQUE_PATIENT_COUNT), na.rm = TRUE),  # Total unique patients
    TOTAL_ITEMS = sum(as.numeric(ITEMS), na.rm = TRUE),  # Total prescription items
    .groups = "drop"  # Remove grouping
  )

rm(icb_data_all)
