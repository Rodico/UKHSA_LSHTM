rm(list=ls())

library(readr)
library(ggplot2)
library(dplyr)
library(scales)

population_summary <- read_csv("/Users/jennypage/Desktop/Data Challenge/Model Data/modeldata_summary.csv")

population_summary <- population_summary %>%
  select(YEAR, ICB_NAME, POPULATION_COUNT)

prescriptions_summary <- read_csv("/Users/jennypage/Desktop/Data Challenge/ICB_Data/prescriptions_summary.csv")

prescriptions_summary <- prescriptions_summary %>%
  mutate(YEAR = as.numeric(substr(YEAR_MONTH, 1, 4)),
         ICB_NAME = toupper(ICB_NAME)) %>%
  left_join(population_summary, by=c("YEAR", "ICB_NAME"))

## National-Level Prescription Trends By Month
icb_summary_national <- prescriptions_summary %>%
  group_by(YEAR_MONTH) %>%
  summarise(
    TOTAL_PRESCRIPTIONS = sum(TOTAL_PRESCRIPTIONS, na.rm = TRUE),
    TOTAL_POPULATION = sum(POPULATION_COUNT, na.rm = TRUE),  # Sum the population to match aggregation
    .groups = "drop"
  ) %>%
  mutate(
    PRESCRIPTION_RATE = (TOTAL_PRESCRIPTIONS / TOTAL_POPULATION) * 1000,
    DATE = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d")
  )

# Update ggplot to use PRESCRIPTION_RATE instead of TOTAL_PRESCRIPTIONS
national <- ggplot(icb_summary_national, aes(x = DATE, y = PRESCRIPTION_RATE)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::comma_format(suffix = " per 1,000")) + 
  labs(title = "Antibiotic Prescription Rate Over Time (National Level)",
       x = "Month",
       y = "Antibiotic Prescriptions per 1,000 People") +
  theme_minimal() +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf,
           fill = "lightgray", alpha = 0.2) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white")  # Set the background to white
  )

print(national)

# Summarise the data by YEAR_MONTH and SEX
prescriptions_stratified <- read_csv("/Users/jennypage/Desktop/Data Challenge/ICB_Data/prescriptions_stratified.csv")

icb_strat_sex <- prescriptions_stratified %>%
  group_by(YEAR_MONTH, GENDER) %>%
  summarise(TOTAL_PRESCRIPTIONS = sum(TOTAL_PRESCRIPTIONS, na.rm = TRUE), .groups = "drop") %>%
  mutate(DATE = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d")) %>%
  filter(GENDER == "Female" | GENDER == "Male")

national_sex <- ggplot(icb_strat_sex, aes(x = DATE, y = TOTAL_PRESCRIPTIONS, color = GENDER)) +
  geom_line(size = 1) +
  geom_point(size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Show month & year every 3 months
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +  # Format y-axis as millions
  labs(title = "Total Antibiotic Prescriptions Over Time by Sex",
       x = "Month",
       y = "Total Antibiotic Prescriptions (in millions)",
       color = "") +  # Legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf,
           fill = "lightgray", alpha = 0.2)  # COVID shading

print(national_sex)

## National-Level Prescription Trends By Month + Age Band
# Define AGE_GROUP categories
prescriptions_stratified <- prescriptions_stratified %>%
  mutate(AGE_GROUP = case_when(
    AGE_BAND == "0-1" ~ "<1 years",
    AGE_BAND %in% c("2-5", "6-10", "11-15") ~ "1-14 years",
    AGE_BAND %in% c("16-20", "21-25", "26-30", "31-35", "36-40", "41-45") ~ "15-44 years",
    AGE_BAND %in% c("46-50", "51-55", "56-60", "61-65") ~ "45-64 years",
    AGE_BAND %in% c("66-70", "71-75") ~ "65-74 years",
    AGE_BAND %in% c("76-80", "81-85") ~ "75-84 years",
    AGE_BAND %in% c("86-90", "91-95", "96-100", "101-105", "105+") ~ "85+ years",
    TRUE ~ "Unknown"  # Handles NA and unexpected values
  ))

# Summarise data by YEAR_MONTH and AGE_GROUP
icb_strat_age <- prescriptions_stratified %>%
  group_by(YEAR_MONTH, AGE_GROUP) %>%
  summarise(TOTAL_PRESCRIPTIONS = sum(TOTAL_PRESCRIPTIONS, na.rm = TRUE), .groups = "drop") %>%
  mutate(DATE = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d")) %>%
  filter(AGE_GROUP != "Unknown")  # Remove unknown age groups

# Plot with lines split by AGE_GROUP
national_age <- ggplot(icb_strat_age, aes(x = DATE, y = TOTAL_PRESCRIPTIONS, color = AGE_GROUP)) +
  geom_line(size = 1) +
  geom_point(size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Show month & year every 3 months
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, suffix = "M")) +  # Format y-axis as millions
  labs(title = "Total Antibiotic Prescriptions Over Time by Age Group",
       x = "Month",
       y = "Total Antibiotic Prescriptions (in millions)",
       color = "") +  # Match legend title with sex plot
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf,
           fill = "lightgray", alpha = 0.2)  # COVID shading

print(national_age)

## Regional-Level Prescription Trends By Month
# Define the updated region_mapping as a named list
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
                   "NHS Gloucestershire Integrated Care Board")
)

# Convert the region_mapping list into a data frame for easier merging
region_df <- data.frame(
  REGION = rep(names(region_mapping), lengths(region_mapping)),
  ICB_NAME = toupper(unlist(region_mapping, use.names = FALSE))
)

# Merge the region mapping with your prescriptions_summary dataset
prescriptions_summary <- prescriptions_summary %>%
  left_join(region_df, by = "ICB_NAME")

# Summarise the data by YEAR_MONTH and REGION
icb_summary_national_region <- prescriptions_summary %>%
  group_by(YEAR_MONTH, REGION) %>%
  summarise(
    TOTAL_PRESCRIPTIONS = sum(TOTAL_PRESCRIPTIONS, na.rm = TRUE),
    TOTAL_POPULATION = sum(POPULATION_COUNT, na.rm = TRUE),  # Sum population count by REGION
    .groups = "drop"
  ) %>%
  mutate(
    PRESCRIPTION_RATE = (TOTAL_PRESCRIPTIONS / TOTAL_POPULATION) * 1000,  # Prescriptions per 1,000 people
    DATE = as.Date(paste0(YEAR_MONTH, "01"), format = "%Y%m%d")
  )

# Plot with lines by REGION, now using PRESCRIPTION_RATE
regional_all <- ggplot(icb_summary_national_region, aes(x = DATE, y = PRESCRIPTION_RATE, color = REGION)) +
  geom_line(size = 0.5) +
  geom_point() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +  # Show month & year every 3 months
  scale_y_continuous(labels = scales::comma_format()) +  # Remove "per 1,000" and just format the raw value
  labs(title = "Antibiotic Prescription Rate Over Time by Region",
       x = "Month",
       y = "Antibiotic Prescription Rate (per 1,000 People)",
       color = "Region") +  # Add legend title for color
  theme_minimal() +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"), ymin = -Inf, ymax = Inf,
           fill = "lightgray", alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

print(regional_all)

# Export the plots
ggsave("/Users/jennypage/Desktop/Data Challenge/Visualisations/national_plot.png", national, width = 10, height = 6, dpi = 300)
ggsave("/Users/jennypage/Desktop/Data Challenge/Visualisations/national_sex_plot.png", national_sex, width = 10, height = 6, dpi = 300)
ggsave("/Users/jennypage/Desktop/Data Challenge/Visualisations/national_age_plot.png", national_age, width = 10, height = 6, dpi = 300)
ggsave("/Users/jennypage/Desktop/Data Challenge/Visualisations/regional_all_plot.png", regional_all, width = 10, height = 6, dpi = 300)
