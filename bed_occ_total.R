library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
# Load the dataset
bed_occ_total <- read_xlsx("~/Desktop/Final dataset.xlsx")

bed_occ_total <- bed_occ_total %>%
  mutate(year = as.numeric(str_extract(year, "^[0-9]{4}")))


# Rename columns to make them more uniform
names(bed_occ_total) <- c("year", "period", "available_day", "available_day_night", "occupied_day_night",
                          "occupied_day", "total_available", "total_occupied", "total_occupied_percent")

# Verify new column names
print(names(bed_occ_total))

#Removing data from 2010 to 2013 
#Removing the first 16 rows from the dataset
bed_occ_total <- bed_occ_total[-(1:16), ]


# Updated plotting code using the new column names
bed_occ_scatter <- ggplot(data = bed_occ_total, aes(x = year, y = total_occupied_percent, color = period)) +
  geom_point(size = 2.5) +
  ylim(0, 100) +
  geom_line(data = filter(bed_occ_total, period == "Q1"), aes(group = period), size = 1, color = "pink") +
  geom_line(data = filter(bed_occ_total, period == "Q2"), aes(group = period), size = 1, color = "lightblue") +
  geom_line(data = filter(bed_occ_total, period == "Q3"), aes(group = period), size = 1, color = "orange") +
  geom_line(data = filter(bed_occ_total, period == "Q4"), aes(group = period), size = 1, color = "purple") +
  scale_color_manual(values = c("Q1" = "pink", "Q2" = "lightblue", "Q3" = "orange", "Q4" = "purple"),
                     labels = c("Q1 (Jan - Mar)", "Q2 (Apr - Jun)", "Q3 (Jul - Sep)", "Q4 (Oct - Dec)")) +
  annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf, fill = "lightgray", alpha = 0.2) +  # Shaded area for COVID-19
  labs(y = "Percentage of Bed Occupied (%)", x = "Year", color = "Quarter",
       title = "Yearly Percentage of Bed Occupancy") +
  theme_light(base_size = 14) +
  theme(panel.grid.major = element_line(color = "lightgray", size = 0.5),# grid lines
        panel.grid.minor = element_line(color = "lightgray", size = 0.2),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(min(bed_occ_total$year), max(bed_occ_total$year), by = 1))  # Set breaks to every year

# Display the plot
print(bed_occ_scatter)

ggsave("bed_occ_scatter_total.jpeg", plot = bed_occ_scatter, width = 10, height = 6, dpi = 300)
write.csv(bed_occ, "bed_occ_total.csv", row.names = FALSE)

