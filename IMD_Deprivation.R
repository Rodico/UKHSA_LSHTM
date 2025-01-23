library(readr)
library(tidyverse)
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
view(IMD_Deprivation_V1)

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

# Group by 'ICB_code' and calculate the mean of 'IMD_score_byLSOA' for each group
LSOA_to_ICB_IMD_mean <- LSOA_to_ICB_IMD %>%
  group_by(ICB_code) %>%
  summarize(mean_IMD_score = mean(IMD_decile))

# Print the results
View(LSOA_to_ICB_IMD_mean)

