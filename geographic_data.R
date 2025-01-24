install.packages('readxl')
library(readxl)
library(dplyr)

#loading data Attribution from A&E Providers to ICBs
ae_icb<- read_excel("~/Desktop/Trust-ICB-Attribution-File.xls")
View(ae_icb)

#loading care board data
care_boards<- read.csv("~/Desktop/Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv")
View(care_boards)

#
lsoa<- read.csv("~/Desktop/LSOA_(2021)_to_SICBL_to_ICB_to_LAD_(April_2023)_Lookup_in_EN.csv")
View(lsoa)

# clenaing ae_icb table 
# Remove the first 5 rows
ae_icb <- ae_icb[-(1:5), ]
# Make the 1st row the column names
colnames(ae_icb) <- as.character(ae_icb[1, ])
# Remove the 1st row from the dataset since it is now the header
ae_icb <- ae_icb[-1, ]

# View the updated data frame
head(ae_icb)

# Joining lsoa and care_boards 
care_lsoa <- care_boards %>%
  left_join(lsoa, by = c("ICB24CD" = "ICB23CD"))

# View the result
View(care_lsoa)

#Joining care_lsoa and ae_icb 
icb <- care_lsoa %>%
  left_join(ae_icb, by = c("ICB24CDH" = "ICB Code"))
View(icb)
