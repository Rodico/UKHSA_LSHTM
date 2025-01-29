library(readr)
library(tidyverse)
library(readODS)
library(readxl)
Deprivation_data <- read_csv("C:/Users/dhyay/OneDrive/Desktop/Data Challange/Data_Challange/UKHSA_lSHTM/Data/Deprivation_Data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
View(Deprivation_data)

# Select all the different ranks avalable
IMD_Deprivation_V1 <- Deprivation_data %>%
  select(
    LSOA_code = `LSOA code (2011)`,
    IMD_score_byLSOA = 'Index of Multiple Deprivation (IMD) Score',
    IMD_Rank_byLSOA = 'Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)',
    IMD_decile = 'Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)'
  )
View(IMD_Deprivation_V1)

#Read LSOA and ICB
LSOA_and_ICB <- read_csv("C:/Users/dhyay/OneDrive/Desktop/Data Challange/Data_Challange/UKHSA_lSHTM/Data/Deprivation_Data/LSOA_to_ICB.csv")
View(LSOA_and_ICB)
# Select all the different ranks avalable
LSOA_and_ICB_V1 <- LSOA_and_ICB %>%
  select(
    LSOA_code = `LSOA21CD`,
    ICB_code = 'ICB23CD',
    ICB_name = 'ICB23NM'
  )
LSOA_and_ICB_V1$ICB_name <- toupper(LSOA_and_ICB_V1$ICB_name)
view(LSOA_and_ICB_V1)
#Combining the original table with ICB codes 
LSOA_to_ICB_IMD <- left_join(IMD_Deprivation_V1, LSOA_and_ICB_V1, by = "LSOA_code")
View(LSOA_to_ICB_IMD)

# Remove rows with missing values in the 'ICB_code' column
LSOA_to_ICB_IMD <- LSOA_to_ICB_IMD %>%
  filter(!is.na(ICB_code))


#Murge

LSOA_ICB_IMD_mean <- LSOA_to_ICB_IMD %>%
  group_by(ICB_code, ICB_name) %>%
  summarize(
    IMD_score_mean = mean(IMD_score_byLSOA),
    IMD_Rank_mean = mean(IMD_Rank_byLSOA),
    IMD_decile_mean = mean(IMD_decile),
    .groups = 'drop'  # This drops the grouping after summarize
  )

# View the results
View(LSOA_ICB_IMD_mean)

#Save as R Data File
save(LSOA_ICB_IMD_mean, file = "C:\\Users\\dhyay\\OneDrive\\Desktop\\Data Challange\\Data_Challange\\UKHSA_lSHTM\\RCode\\R_Data\\LSOA_ICB_IMD_mean.RData")


# Load the data from the specified file path, sheet 10, skipping the first 2 rows
Num_Cases_data <- read_ods("C:\\Users\\dhyay\\OneDrive\\Desktop\\Data Challange\\Data_Challange\\UKHSA_lSHTM\\Data\\Main_Database\\Tempotal_Trends.ods", sheet = 9, skip = 2)

# Rename the column
Num_Cases_data <- Num_Cases_data %>%
  rename(ICB_name = `Integrated care board (ICB)`)
#set to uppercase
Num_Cases_data$ICB_name <- toupper(Num_Cases_data$ICB_name)
# Join with LSOA_ICB_IMD_mean
Num_Cases_IMD <- Num_Cases_data %>%
  left_join(LSOA_ICB_IMD_mean, by = "ICB_name")

# View the result
View(Num_Cases_IMD)
save(Num_Cases_IMD, file = "C:\\Users\\dhyay\\OneDrive\\Desktop\\Data Challange\\Data_Challange\\UKHSA_lSHTM\\RCode\\R_Data\\Num_Cases_IMD.RData")

