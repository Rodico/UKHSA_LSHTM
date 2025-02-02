#Script for the final GP apointments dataset: 

library(tidyverse)

#setwd(/Users/ishamathur/Desktop/gp appointments/Combined_cleaned_data)


#Add a year column to each dataset
combined_data_2018 <- combined_data_2018 %>% mutate(Year=2018)
combined_data_2019 <- combined_data_2019 %>% mutate(Year=2019)
combined_data_2020 <- combined_data_2020 %>% mutate(Year=2020)
combined_data_2021 <- combined_data_2021 %>% mutate(Year=2021)
combined_data_2022 <- combined_data_2022 %>% mutate(Year=2022)
combined_data_2023 <- combined_data_2023 %>% mutate(Year=2023)
combined_data_2024 <- combined_data_2024 %>% mutate(Year=2024)


