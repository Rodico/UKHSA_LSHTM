#loading packages
library(readxl)
library(ggplot2)

#loading data
bed_occ<- read_xlsx('~/Desktop/Beds-Timeseries cleaned .xlsx')
View(bed_occ)

#Removing data from 2010 to 2013 
#Removing the first 16 rows from the dataset
bed_occ <- bed_occ[-(1:16), ]

#Convert values in "% Occupied Total" to percentage
bed_occ$`% Occupied Total` <- bed_occ$`% Occupied Total` * 100

#changing the years 
bed_occ$Year <- sub("/.*", "", bed_occ$Year)

# Verify the changes
print(unique(bed_occ$Year))

#Viewing the modified dataset
View(bed_occ)

# Load necessary package
library(dplyr)

# Convert Year column to numeric (if necessary)
bed_occ$Year <- as.numeric(bed_occ$Year)

# Group by Year and calculate mean percentage of bed occupancy
bed_occ_summary <- bed_occ %>%
  group_by(Year) %>%
  summarise(Mean_Bed_Occupied = mean(`% Occupied Total`, na.rm = TRUE))

# Print the summarized table
View(bed_occ_summary)

#summary data plot 
ggplot(data = bed_occ_summary, aes(x = Year, y = Mean_Bed_Occupied)) +
  geom_point(size = 3, color = "blue") +  # Scatter plot points with blue color
  geom_line(color = "red", linetype = "dashed") + 
  ylim(0,100)+
  labs(title = "Mean Percentage of Bed Occupancy by Year",
       x = "Year",
       y = "Mean Percentage of Bed Occupied") +
  theme_minimal(base_size = 14) +  # Apply minimal theme for clean appearance
  theme(panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "lightgray", size = 0.2),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # Center the plot title

#plotting all data 
#Scatter plot 
bed_occ_scatter <- ggplot(data = bed_occ, aes(x = Year, y = `% Occupied Total`, color = Period)) +
  geom_point(size = 2.5) + 
  ylim(0,100)+
  geom_line(data = subset(bed_occ, Period == "Q1"), aes(group = Period), size = 1, color = "pink") +
  geom_line(data = subset(bed_occ, Period == "Q2"), aes(group = Period), size = 1, color = "lightblue") +
  geom_line(data = subset(bed_occ, Period == "Q3"), aes(group = Period), size = 1, color = "orange") +
  geom_line(data = subset(bed_occ, Period == "Q4"), aes(group = Period), size = 1, color = "purple") +
  scale_color_manual(values = c("Q1" = "pink", "Q2" = "lightblue", "Q3" = "orange", "Q4" = "purple"),
                     labels = c("Q1 (Jan - Mar)", "Q2 (Apr - Jun)", "Q3 (Jul - Sep)", "Q4 (Oct - Dec)")
  ) +  # Assign custom colors to each quarter
  annotate("rect", xmin = "2020", xmax = "2022", ymin = -Inf, ymax = Inf,
           fill = "lightgray", alpha = 0.2) +  # Shaded area for covid
  labs(y = "Percentage of Bed Occupied (%)", x = "Year", color = "Quarter",
       title = "Yearly Percentage of Bed Occupancy") +
  theme_light(base_size = 14) +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.5),# grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.2),# grid lines
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # Center the plot title

bed_occ_scatter

ggsave("bed_occ_scatter.jpeg", plot = bed_occ_scatter, width = 10, height = 6, dpi = 300)

write.csv(bed_occ_summary, "bed_occ_summary.csv", row.names = FALSE)
write.csv(bed_occ, "bed_occ.csv", row.names = FALSE)


