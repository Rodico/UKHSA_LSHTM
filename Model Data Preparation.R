# Clear workspace
rm(list=ls())

# Load packages
library(readxl)
library(tidyverse)

## Read in data
# Infection data
infections_summary <- read_excel("/Users/jennypage/Desktop/Data Challenge/Model Data/Infections_Summary.xlsx")
infections_strat <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/Infections_Stratified.csv")

# Prescription data
prescriptions_summary <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/Prescriptions_Summary.csv")
prescriptions_strat <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/Prescriptions_Stratified.csv")

# IMD Rank
imd_summary <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/IMD.csv")

# GP Appointments
gp_summary <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/GP Appointments.csv")

## Set up model data
summary_data <- infections_summary %>%
  pivot_longer(cols = starts_with("20"), names_to = "YEAR", values_to = "ECOLI_CASES") %>%
  arrange(YEAR, ICB_CODE) %>%
  select(YEAR, ICB_CODE, ICB_NAME, ECOLI_CASES) %>%
  mutate(YEAR = as.numeric(YEAR))

## Add prescription counts
prescriptions_summary <- prescriptions_summary %>%
  select(YEAR_MONTH, ICB_CODE, TOTAL_PRESCRIPTIONS) %>%
  mutate(YEAR = as.numeric(substr(YEAR_MONTH, 1, 4))) %>%
  select(-YEAR_MONTH) %>%
  group_by(YEAR, ICB_CODE) %>%
  summarise(TOTAL_PRESCRIPTIONS = sum(TOTAL_PRESCRIPTIONS, na.rm = TRUE), .groups = "drop")

summary_data <- summary_data %>%
  left_join(prescriptions_summary, by=c("YEAR", "ICB_CODE"))

## Add GP counts
gp_summary <- gp_summary %>%
  select(Year, ICB_Code, ICB_Name, Count_gp_appointments) %>%
  mutate(ICB_Name = case_when(
    ICB_Name == "NHS Hampshire and the Isle of Wight Integrated Care Board" ~ "NHS Hampshire and Isle of Wight Integrated Care Board",
    TRUE ~ ICB_Name)) %>%
  rename(YEAR = Year,
         ICB_CODE = ICB_Code,
         ICB_NAME = ICB_Name,
         GP_APPOINTMENTS = Count_gp_appointments) %>%
  group_by(YEAR, ICB_NAME) %>%
  summarise(GP_APPOINTMENTS = sum(GP_APPOINTMENTS, na.rm = TRUE), .groups = "drop")

summary_data <- summary_data %>%
  left_join(gp_summary, by=c("YEAR", "ICB_NAME"))

## Add IMD
imd_summary <- imd_summary %>%
  select(`Sustainability transformation partnerships code (NHS)`, ICB_name, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`) %>%
  rename(ICB_CODE = `Sustainability transformation partnerships code (NHS)`,
         ICB_NAME = ICB_name) %>%
  pivot_longer(cols = starts_with("20"), names_to = "YEAR", values_to = "IMD_SCORE") %>%
  select(YEAR, ICB_CODE, ICB_NAME, IMD_SCORE) %>%
  mutate(YEAR = as.numeric(YEAR))
  
summary_data <- summary_data %>%
  mutate(ICB_NAME = toupper(ICB_NAME)) %>%
  left_join(imd_summary, by=c("YEAR", "ICB_CODE", "ICB_NAME")) %>%
  arrange(YEAR, ICB_NAME)

# Create region mapping
region_mapping <- list(
  "North East and Yorkshire" = c("NHS North East and North Cumbria Integrated Care Board", 
                                 "NHS Humber and North Yorkshire Integrated Care Board",
                                 "NHS West Yorkshire Integrated Care Board", 
                                 "NHS South Yorkshire Integrated Care Board"),
  
  "North West" = c("NHS Cheshire and Merseyside Integrated Care Board", 
                   "NHS Lancashire and South Cumbria Integrated Care Board",
                   "NHS Greater Manchester Integrated Care Board"),
  
  "Midlands" = c("NHS Staffordshire and Stoke-on-Trent Integrated Care Board", 
                 "NHS Shropshire, Telford and Wrekin Integrated Care Board",
                 "NHS Birmingham and Solihull Integrated Care Board", 
                 "NHS Black Country Integrated Care Board", 
                 "NHS Coventry and Warwickshire Integrated Care Board",
                 "NHS Herefordshire and Worcestershire Integrated Care Board", 
                 "NHS Leicester, Leicestershire and Rutland Integrated Care Board",
                 "NHS Derby and Derbyshire Integrated Care Board", 
                 "NHS Northamptonshire Integrated Care Board",
                 "NHS Nottingham and Nottinghamshire Integrated Care Board", 
                 "NHS Lincolnshire Integrated Care Board"),
  
  "East of England" = c("NHS Norfolk and Waveney Integrated Care Board", 
                        "NHS Suffolk and North East Essex Integrated Care Board",
                        "NHS Hertfordshire and West Essex Integrated Care Board", 
                        "NHS Mid and South Essex Integrated Care Board",
                        "NHS Bedfordshire, Luton and Milton Keynes Integrated Care Board",
                        "NHS Cambridgeshire and Peterborough Integrated Care Board"),
  
  "London" = c("NHS North West London Integrated Care Board", 
               "NHS North Central London Integrated Care Board", 
               "NHS North East London Integrated Care Board",
               "NHS South East London Integrated Care Board", 
               "NHS South West London Integrated Care Board"),
  
  "South East" = c("NHS Kent and Medway Integrated Care Board", 
                   "NHS Surrey Heartlands Integrated Care Board", 
                   "NHS Sussex Integrated Care Board",
                   "NHS Frimley Integrated Care Board", 
                   "NHS Hampshire and Isle of Wight Integrated Care Board",
                   "NHS Buckinghamshire, Oxfordshire and Berkshire West Integrated Care Board"),
  
  "South West" = c("NHS Cornwall and the Isles of Scilly Integrated Care Board", 
                   "NHS Devon Integrated Care Board", 
                   "NHS Somerset Integrated Care Board",
                   "NHS Bristol, North Somerset and South Gloucestershire Integrated Care Board",
                   "NHS Bath and North East Somerset, Swindon and Wiltshire Integrated Care Board",
                   "NHS Dorset Integrated Care Board", 
                   "NHS Gloucestershire Integrated Care Board"))

summary_data <- summary_data %>%
  mutate(REGION = case_when(
    ICB_NAME %in% toupper(region_mapping[["North East and Yorkshire"]]) ~ "North East and Yorkshire",
    ICB_NAME %in% toupper(region_mapping[["North West"]]) ~ "North West",
    ICB_NAME %in% toupper(region_mapping[["Midlands"]]) ~ "Midlands",
    ICB_NAME %in% toupper(region_mapping[["East of England"]]) ~ "East of England",
    ICB_NAME %in% toupper(region_mapping[["London"]]) ~ "London",
    ICB_NAME %in% toupper(region_mapping[["South East"]]) ~ "South East",
    ICB_NAME %in% toupper(region_mapping[["South West"]]) ~ "South West",
    TRUE ~ "Unknown")) %>%
  mutate(REGION = toupper(REGION)) %>%
  select(YEAR, REGION, ICB_NAME, ICB_CODE, ECOLI_CASES, TOTAL_PRESCRIPTIONS, GP_APPOINTMENTS, IMD_SCORE) %>%
  mutate(ECOLI_CASES = round(ECOLI_CASES),
         IMD_SCORE = round(IMD_SCORE, 1))

# Add population counts
file_path <- "/Users/jennypage/Desktop/Data Challenge/Model Data/Population Data.xlsx"

# Get sheet names (years)
sheet_names <- excel_sheets(file_path)

# Read all sheets into a list
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(file_path, sheet = sheet) %>%
    mutate(YEAR = sheet)  # Add year column
})

# Combine all sheets into one dataframe
combined_population_data <- bind_rows(data_list)

combined_population_data <- combined_population_data %>%
  select(YEAR, 'ICB 2024 Name', Total) %>%
  rename(ICB_NAME = 'ICB 2024 Name',
         POPULATION_COUNT = Total) %>%
  mutate(ICB_NAME = toupper(ICB_NAME),
         YEAR = as.numeric(YEAR)) %>%
  group_by(YEAR, ICB_NAME) %>%
  summarise(POPULATION_COUNT = sum(POPULATION_COUNT, na.rm = TRUE), .groups = "drop") %>%
  mutate(ICB_NAME = gsub("ICB", "INTEGRATED CARE BOARD", ICB_NAME))

summary_data <- summary_data %>%
  left_join(combined_population_data, by=c("YEAR", "ICB_NAME"))

# Replace any missing counts with previous year
summary_data <- summary_data %>%
  group_by(ICB_NAME) %>%
  mutate(POPULATION_COUNT = ifelse(YEAR == 2024 & is.na(POPULATION_COUNT), 
                                   POPULATION_COUNT[YEAR == 2023], 
                                   POPULATION_COUNT)) %>%
  ungroup()

write.csv(summary_data, "/Users/jennypage/Desktop/Data Challenge/Model Data/modeldata_summary.csv", row.names = FALSE)
