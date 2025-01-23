library(readr)
library(tidyverse)
Deprivation_data <- read_csv("C:/Users/dhyay/OneDrive/Desktop/Data Challange/Data_Challange/UKHSA_lSHTM/Data/Deprivation_Data/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
View(Deprivation_data)

# Select all the different ranks avalable
AGE_Deprivation_V1 <- Deprivation_data %>%
  select(
    LSOA_code = `LSOA code (2011)`,
    Total_Population = 'Total population: mid 2015 (excluding prisoners)',
    Aged_0_15 = 'Dependent Children aged 0-15: mid 2015 (excluding prisoners)',
    Aged_16_59 = 'Population aged 16-59: mid 2015 (excluding prisoners)',
    Aged_over_60 = 'Older population aged 60 and over: mid 2015 (excluding prisoners)'
  )
view(AGE_Deprivation_V1)

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
LSOA_to_ICB_AGE <- left_join(AGE_Deprivation_V1, LSOA_and_ICB_V1, by = "LSOA_code")
View(LSOA_to_ICB_AGE)

# Remove rows with missing values in the 'ICB_code' column
LSOA_to_ICB_AGE <- LSOA_to_ICB_AGE %>%
  filter(!is.na(ICB_code))

# Group by 'ICB_code' and calculate the mean of 'IMD_score_byLSOA' for each group
LSOA_to_ICB_AGE_mean <- LSOA_to_ICB_AGE %>%
  group_by(ICB_code) %>%
  summarize(mean_AGE_TP_NUM = mean(Total_Population),
            mean_AGE_0_15_NUM = mean(Aged_0_15),
            mean_AGE_16_59_NUM = mean(Aged_16_59),
            mean_AGE_over_60_NUM = mean(Aged_over_60))

# Print the results
View(LSOA_to_ICB_AGE_mean)
