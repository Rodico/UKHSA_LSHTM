
# ---- Load Required Libraries ----
library(tidyverse)    # For data manipulation and visualization
library(readxl)       # For reading Excel files
library(readODS)      # For reading ODS files
library(ggplot2)      # For creating plots
library(gridExtra)    # For arranging multiple plots
library(dplyr)        # For data manipulation
library(grid)         # For grid graphics and text handling

# ---- Data Import and Preparation ----
# Read the temporal trends data from ODS file
Age_Cases <- read_ods("C:\\Users\\dhyay\\OneDrive\\Desktop\\Data Challange\\Data_Challange\\UKHSA_lSHTM\\Age_Data\\Ages_Gender\\Tempotal_Trends.ods", 
                      sheet = 5, 
                      skip = 2)

# Filter data for E. coli and MSSA
Ecoli <- Age_Cases %>% 
  filter(Species == "Escherichia coli")

# Remove non-breaking space in MSSA species name and filter
MSSA <- Age_Cases %>% 
  filter(gsub("\u00a0", " ", Species) == "Methicillin-sensitive Staphylococcus aureus (MSSA)")

# ---- Create Basic Time Series Plots ----
# Function to create standardized time series plot
create_time_series_plot <- function(data, gender, species) {
  rate_col <- paste0("All reported case rate (per 100,000 population) (", gender, ")")
  
  ggplot(data, 
         aes(x = factor(`Financial year`, levels = unique(`Financial year`)), 
             y = .data[[rate_col]],
             color = `Age group (years)`,
             group = `Age group (years)`)) +
    # Add COVID period shading
    geom_rect(data = data.frame(
      xmin = which(unique(data$`Financial year`) == "2019 to 2020"),
      xmax = which(unique(data$`Financial year`) == "2021 to 2022"),
      ymin = -Inf, ymax = Inf),
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = 'grey90', alpha = 0.5, inherit.aes = FALSE) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(title = paste("Temporal Trends of", species, "Rates in", tools::toTitleCase(gender), "by Age Group"),
         x = "Financial Year",
         y = "Rate per 100,000 population",
         color = "Age Group",
         caption = "Note: Shaded region represents COVID-19 period (2020-2022)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create individual plots
plots <- list(
  ecoli_men = create_time_series_plot(Ecoli, "men", "E. coli"),
  ecoli_women = create_time_series_plot(Ecoli, "women", "E. coli"),
  mssa_men = create_time_series_plot(MSSA, "men", "MSSA"),
  mssa_women = create_time_series_plot(MSSA, "women", "MSSA")
)

# ---- COVID Period Analysis ----
# Function to classify time periods
create_time_period <- function(year) {
  if (year <= "2019 to 2020") return("Pre-COVID")
  if (year >= "2022 to 2023") return("Post-COVID")
  return("During-COVID")
}

# Add time period classification to datasets
Ecoli$period <- sapply(Ecoli$`Financial year`, create_time_period)
MSSA$period <- sapply(MSSA$`Financial year`, create_time_period)

# Define standard orders for factors
age_order <- c("Fewer than 1", "1 to 14", "15 to 44", "45 to 64", 
               "65 to 74", "75 to 84", "85 and over")
period_order <- c("Pre-COVID", "During-COVID", "Post-COVID")

# Convert to factors with correct ordering
Ecoli$`Age group (years)` <- factor(Ecoli$`Age group (years)`, levels = age_order)
MSSA$`Age group (years)` <- factor(MSSA$`Age group (years)`, levels = age_order)
Ecoli$period <- factor(Ecoli$period, levels = period_order)
MSSA$period <- factor(MSSA$period, levels = period_order)

# ---- Statistical Analysis ----
# Function to run statistical tests for each age group
run_statistical_tests <- function(data, rate_column) {
  age_groups <- unique(data$`Age group (years)`)
  results <- list()
  
  for(age in age_groups) {
    subset_data <- data[data$`Age group (years)` == age, ]
    
    # Run ANOVA and Kruskal-Wallis tests
    anova_result <- aov(get(rate_column) ~ period, data = subset_data)
    kw_result <- kruskal.test(get(rate_column) ~ period, data = subset_data)
    
    # Run pairwise t-tests
    pairwise_result <- pairwise.t.test(
      subset_data[[rate_column]], 
      subset_data$period, 
      p.adjust.method = "bonferroni"
    )
    
    results[[age]] <- list(
      age_group = age,
      anova_p = summary(anova_result)[[1]]["Pr(>F)"][[1]],
      kw_p = kw_result$p.value,
      pairwise_tests = pairwise_result$p.value
    )
  }
  return(results)
}

# Run statistical tests for each dataset
statistical_tests <- list(
  ecoli_men = run_statistical_tests(Ecoli, "All reported case rate (per 100,000 population) (men)"),
  ecoli_women = run_statistical_tests(Ecoli, "All reported case rate (per 100,000 population) (women)"),
  mssa_men = run_statistical_tests(MSSA, "All reported case rate (per 100,000 population) (men)"),
  mssa_women = run_statistical_tests(MSSA, "All reported case rate (per 100,000 population) (women)")
)

# ---- Time Series Analysis ----
# Function to calculate year-over-year changes
calculate_yoy_changes <- function(data, rate_column) {
  data %>%
    arrange(`Financial year`) %>%
    group_by(`Age group (years)`) %>%
    mutate(
      yoy_change = (get(rate_column) - lag(get(rate_column))) / lag(get(rate_column)) * 100,
      yoy_abs_change = get(rate_column) - lag(get(rate_column))
    ) %>%
    ungroup()
}

# Calculate COVID impact summary
summarize_covid_impact <- function(data) {
  data %>%
    filter(!is.na(yoy_change)) %>%
    group_by(`Age group (years)`) %>%
    summarise(
      max_drop = min(yoy_change[period == "During-COVID"], na.rm = TRUE),
      max_recovery = max(yoy_change[period == "Post-COVID"], na.rm = TRUE),
      time_to_recovery = sum(yoy_change < 0, na.rm = TRUE)
    )
}

# Generate summaries and plots as needed
# ---- Generate and Display Visualizations ----

# Create individual time series plots
plots <- list(
  ecoli_men = create_time_series_plot(Ecoli, "men", "E. coli"),
  ecoli_women = create_time_series_plot(Ecoli, "women", "E. coli"),
  mssa_men = create_time_series_plot(MSSA, "men", "MSSA"),
  mssa_women = create_time_series_plot(MSSA, "women", "MSSA")
)

# Create box plots function
create_box_plot <- function(data, gender, species) {
  rate_col <- paste0("All reported case rate (per 100,000 population) (", gender, ")")
  
  ggplot(data, 
         aes(x = `Age group (years)`, 
             y = .data[[rate_col]],
             fill = period)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = paste(species, "Rates in", tools::toTitleCase(gender), "by Age Group and COVID Period"),
         x = "Age Group",
         y = "Rate per 100,000 population",
         fill = "Period") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
}

# Generate box plots
box_plots <- list(
  ecoli_men = create_box_plot(Ecoli, "men", "E. coli"),
  ecoli_women = create_box_plot(Ecoli, "women", "E. coli"),
  mssa_men = create_box_plot(MSSA, "men", "MSSA"),
  mssa_women = create_box_plot(MSSA, "women", "MSSA")
)

# Display combined box plots for E. coli
grid.arrange(
  box_plots$ecoli_men + theme(legend.position = "bottom"),
  box_plots$ecoli_women + theme(legend.position = "bottom"),
  ncol = 2,
  top = textGrob("E. coli Rates by Gender, Age Group and COVID Period",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Display combined box plots for MSSA
grid.arrange(
  box_plots$mssa_men + theme(legend.position = "bottom"),
  box_plots$mssa_women + theme(legend.position = "bottom"),
  ncol = 2,
  top = textGrob("MSSA Rates by Gender, Age Group and COVID Period",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Standardize theme elements for time series plots
plots <- lapply(plots, function(p) {
  p + theme(
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )
})

# Display combined plot
grid.arrange(
  grobs = plots,
  ncol = 2,
  nrow = 2,
  top = "Temporal Trends of Infection Rates by Gender and Age Group"
)

# Calculate year-over-year changes for each dataset
yoy_changes <- list(
  ecoli_men = calculate_yoy_changes(Ecoli, "All reported case rate (per 100,000 population) (men)"),
  ecoli_women = calculate_yoy_changes(Ecoli, "All reported case rate (per 100,000 population) (women)"),
  mssa_men = calculate_yoy_changes(MSSA, "All reported case rate (per 100,000 population) (men)"),
  mssa_women = calculate_yoy_changes(MSSA, "All reported case rate (per 100,000 population) (women)")
)

# Generate COVID impact summaries
impact_summaries <- list(
  ecoli_men = summarize_covid_impact(yoy_changes$ecoli_men),
  ecoli_women = summarize_covid_impact(yoy_changes$ecoli_women),
  mssa_men = summarize_covid_impact(yoy_changes$mssa_men),
  mssa_women = summarize_covid_impact(yoy_changes$mssa_women)
)

# Print statistical test results
print_test_results <- function(test_results, title) {
  cat("\n", title, "\n", sep="")
  cat(paste(rep("-", nchar(title)), collapse=""), "\n")
  
  for(age in names(test_results)) {
    cat("\nAge Group:", age, "\n")
    cat("ANOVA p-value:", format.pval(test_results[[age]]$anova_p, digits = 3), "\n")
    cat("Kruskal-Wallis p-value:", format.pval(test_results[[age]]$kw_p, digits = 3), "\n")
    cat("Pairwise t-tests (Bonferroni-corrected):\n")
    print(test_results[[age]]$pairwise_tests)
  }
}

# Print all results
cat("\nCOVID Impact Summaries\n")
cat("=====================\n")
print("E. coli - Men:")
print(impact_summaries$ecoli_men)
print("\nE. coli - Women:")
print(impact_summaries$ecoli_women)
print("\nMSSA - Men:")
print(impact_summaries$mssa_men)
print("\nMSSA - Women:")
print(impact_summaries$mssa_women)

# Print statistical test results
print_test_results(statistical_tests$ecoli_men, "E. coli Statistical Tests - Men")
print_test_results(statistical_tests$ecoli_women, "E. coli Statistical Tests - Women")
print_test_results(statistical_tests$mssa_men, "MSSA Statistical Tests - Men")
print_test_results(statistical_tests$mssa_women, "MSSA Statistical Tests - Women")

# ---- Save Results ----
# Add code here to save your results, plots, and statistical analyses
# Example:
# ggsave("combined_plots.pdf", combined_plot, width = 12, height = 8)
# write.csv(summary_statistics, "analysis_results.csv")
Ecoli <- Age_Cases %>% 
  filter(Species == "Escherichia coli")
# Remove non-breaking space (\u00a0) in the MSSA species name
MSSA <- Age_Cases %>% 
  filter(gsub("\u00a0", " ", Species) == "Methicillin-sensitive Staphylococcus aureus (MSSA)")
colnames(Ecoli)
##########################################################################################################################################################

library(ggplot2)
library(dplyr)

# Create plots for E. coli - Men
ecoli_men_plot <- ggplot(Ecoli, 
                         aes(x = factor(`Financial year`, 
                                        levels = unique(`Financial year`)), 
                             y = `All reported case rate (per 100,000 population) (men)`,
                             color = `Age group (years)`,
                             group = `Age group (years)`)) +
  geom_rect(data = data.frame(xmin = which(unique(Ecoli$`Financial year`) == "2019 to 2020"),
                              xmax = which(unique(Ecoli$`Financial year`) == "2021 to 2022"),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'grey90', alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Temporal Trends of E. coli Rates in Men by Age Group",
       x = "Financial Year",
       y = "Rate per 100,000 population",
       color = "Age Group",
       caption = "Note: Shaded region represents COVID-19 period (2020-2022)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Create plots for E. coli - Women
ecoli_women_plot <- ggplot(Ecoli, 
                           aes(x = factor(`Financial year`, 
                                          levels = unique(`Financial year`)), 
                               y = `All reported case rate (per 100,000 population) (women)`,
                               color = `Age group (years)`,
                               group = `Age group (years)`)) +
  geom_rect(data = data.frame(xmin = which(unique(Ecoli$`Financial year`) == "2019 to 2020"),
                              xmax = which(unique(Ecoli$`Financial year`) == "2021 to 2022"),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'grey90', alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Temporal Trends of E. coli Rates in Women by Age Group",
       x = "Financial Year",
       y = "Rate per 100,000 population",
       color = "Age Group",
       caption = "Note: Shaded region represents COVID-19 period (2020-2022)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Create plots for MSSA - Men
mssa_men_plot <- ggplot(MSSA, 
                        aes(x = factor(`Financial year`, 
                                       levels = unique(`Financial year`)), 
                            y = `All reported case rate (per 100,000 population) (men)`,
                            color = `Age group (years)`,
                            group = `Age group (years)`)) +
  geom_rect(data = data.frame(xmin = which(unique(MSSA$`Financial year`) == "2019 to 2020"),
                              xmax = which(unique(MSSA$`Financial year`) == "2021 to 2022"),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'grey90', alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Temporal Trends of MSSA Rates in Men by Age Group",
       x = "Financial Year",
       y = "Rate per 100,000 population",
       color = "Age Group",
       caption = "Note: Shaded region represents COVID-19 period (2020-2022)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Create plots for MSSA - Women
mssa_women_plot <- ggplot(MSSA, 
                          aes(x = factor(`Financial year`, 
                                         levels = unique(`Financial year`)), 
                              y = `All reported case rate (per 100,000 population) (women)`,
                              color = `Age group (years)`,
                              group = `Age group (years)`)) +
  geom_rect(data = data.frame(xmin = which(unique(MSSA$`Financial year`) == "2019 to 2020"),
                              xmax = which(unique(MSSA$`Financial year`) == "2021 to 2022"),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'grey90', alpha = 0.5, inherit.aes = FALSE) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Temporal Trends of MSSA Rates in Women by Age Group",
       x = "Financial Year",
       y = "Rate per 100,000 population",
       color = "Age Group",
       caption = "Note: Shaded region represents COVID-19 period (2020-2022)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

ecoli_men_plot
ecoli_women_plot
mssa_men_plot
mssa_women_plot

##Put it in a grid of 4
library(gridExtra)

# Adjust plot sizes and layouts
plots <- list(ecoli_men_plot, ecoli_women_plot, mssa_men_plot, mssa_women_plot)

# Standardize theme elements
plots <- lapply(plots, function(p) {
  p + theme(
    plot.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )
})

# Arrange plots in 2x2 grid
grid.arrange(
  grobs = plots,
  ncol = 2,
  nrow = 2,
  top = "Temporal Trends of Infection Rates by Gender and Age Group"
)

####################################################################################################
#Relationship between covid and Ages
# Create time period classification function
create_time_period <- function(year) {
  year <- as.character(year)
  if (year <= "2019 to 2020") {
    return("Pre-COVID")
  } else if (year >= "2022 to 2023") {
    return("Post-COVID")
  } else {
    return("During-COVID")
  }
}

# Add time periods to dataframes
Ecoli$period <- sapply(Ecoli$`Financial year`, create_time_period)
MSSA$period <- sapply(MSSA$`Financial year`, create_time_period)

# Define correct age group order
age_order <- c("Fewer than 1", "1 to 14", "15 to 44", "45 to 64", "65 to 74", "75 to 84", "85 and over")

# Convert age groups to factor with correct order
Ecoli$`Age group (years)` <- factor(Ecoli$`Age group (years)`, levels = age_order)
MSSA$`Age group (years)` <- factor(MSSA$`Age group (years)`, levels = age_order)

# Set period order
period_order <- c("Pre-COVID", "During-COVID", "Post-COVID")
Ecoli$period <- factor(Ecoli$period, levels = period_order)
MSSA$period <- factor(MSSA$period, levels = period_order)

# Function to run statistical tests for each age group
run_statistical_tests <- function(data, rate_column) {
  # Split data by age group
  age_groups <- unique(data$`Age group (years)`)
  
  results <- list()
  
  for(age in age_groups) {
    # Subset data for current age group
    subset_data <- data[data$`Age group (years)` == age, ]
    
    # Run ANOVA
    anova_result <- aov(get(rate_column) ~ period, data = subset_data)
    anova_pval <- summary(anova_result)[[1]]["Pr(>F)"][[1]]
    
    # Run Kruskal-Wallis test
    kw_result <- kruskal.test(get(rate_column) ~ period, data = subset_data)
    
    # Run pairwise t-tests with Bonferroni correction
    pairwise_result <- pairwise.t.test(subset_data[[rate_column]], 
                                       subset_data$period, 
                                       p.adjust.method = "bonferroni")
    
    results[[age]] <- list(
      age_group = age,
      anova_p = anova_pval,
      kw_p = kw_result$p.value,
      pairwise_tests = pairwise_result$p.value
    )
  }
  return(results)
}

# Calculate detailed summary statistics for E. coli
ecoli_summary <- Ecoli %>%
  group_by(period, `Age group (years)`) %>%
  summarise(
    mean_rate_men = mean(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    median_rate_men = median(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    sd_rate_men = sd(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    mean_rate_women = mean(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    median_rate_women = median(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    sd_rate_women = sd(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se_men = sd_rate_men / sqrt(n),
    se_women = sd_rate_women / sqrt(n)
  )

# Calculate detailed summary statistics for MSSA
mssa_summary <- MSSA %>%
  group_by(period, `Age group (years)`) %>%
  summarise(
    mean_rate_men = mean(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    median_rate_men = median(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    sd_rate_men = sd(`All reported case rate (per 100,000 population) (men)`, na.rm = TRUE),
    mean_rate_women = mean(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    median_rate_women = median(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    sd_rate_women = sd(`All reported case rate (per 100,000 population) (women)`, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    se_men = sd_rate_men / sqrt(n),
    se_women = sd_rate_women / sqrt(n)
  )

# Run tests for each dataset and gender
ecoli_men_tests <- run_statistical_tests(Ecoli, "All reported case rate (per 100,000 population) (men)")
ecoli_women_tests <- run_statistical_tests(Ecoli, "All reported case rate (per 100,000 population) (women)")
mssa_men_tests <- run_statistical_tests(MSSA, "All reported case rate (per 100,000 population) (men)")
mssa_women_tests <- run_statistical_tests(MSSA, "All reported case rate (per 100,000 population) (women)")

# Create box plots for E. coli - Men
ecoli_men_boxplot <- ggplot(Ecoli, 
                            aes(x = `Age group (years)`, 
                                y = `All reported case rate (per 100,000 population) (men)`,
                                fill = period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "E. coli Rates in Men by Age Group and COVID Period",
       x = "Age Group",
       y = "Rate per 100,000 population",
       fill = "Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Create box plots for E. coli - Women
ecoli_women_boxplot <- ggplot(Ecoli, 
                              aes(x = `Age group (years)`, 
                                  y = `All reported case rate (per 100,000 population) (women)`,
                                  fill = period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "E. coli Rates in Women by Age Group and COVID Period",
       x = "Age Group",
       y = "Rate per 100,000 population",
       fill = "Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Create box plots for MSSA - Men
mssa_men_boxplot <- ggplot(MSSA, 
                           aes(x = `Age group (years)`, 
                               y = `All reported case rate (per 100,000 population) (men)`,
                               fill = period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "MSSA Rates in Men by Age Group and COVID Period",
       x = "Age Group",
       y = "Rate per 100,000 population",
       fill = "Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Create box plots for MSSA - Women
mssa_women_boxplot <- ggplot(MSSA, 
                             aes(x = `Age group (years)`, 
                                 y = `All reported case rate (per 100,000 population) (women)`,
                                 fill = period)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "MSSA Rates in Women by Age Group and COVID Period",
       x = "Age Group",
       y = "Rate per 100,000 population",
       fill = "Period") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Print results
cat("\nE. coli Summary Statistics:\n")
View(ecoli_summary)

cat("\nE. coli Statistical Tests - Men:\n")
for(age in names(ecoli_men_tests)) {
  cat("\nAge Group:", age, "\n")
  cat("ANOVA p-value:", format.pval(ecoli_men_tests[[age]]$anova_p, digits = 3), "\n")
  cat("Kruskal-Wallis p-value:", format.pval(ecoli_men_tests[[age]]$kw_p, digits = 3), "\n")
  cat("Pairwise t-tests (Bonferroni-corrected):\n")
  print(ecoli_men_tests[[age]]$pairwise_tests)
}

cat("\nE. coli Statistical Tests - Women:\n")
for(age in names(ecoli_women_tests)) {
  cat("\nAge Group:", age, "\n")
  cat("ANOVA p-value:", format.pval(ecoli_women_tests[[age]]$anova_p, digits = 3), "\n")
  cat("Kruskal-Wallis p-value:", format.pval(ecoli_women_tests[[age]]$kw_p, digits = 3), "\n")
  cat("Pairwise t-tests (Bonferroni-corrected):\n")
  print(ecoli_women_tests[[age]]$pairwise_tests)
}

cat("\nMSSA Summary Statistics:\n")
print(mssa_summary)

cat("\nMSSA Statistical Tests - Men:\n")
for(age in names(mssa_men_tests)) {
  cat("\nAge Group:", age, "\n")
  cat("ANOVA p-value:", format.pval(mssa_men_tests[[age]]$anova_p, digits = 3), "\n")
  cat("Kruskal-Wallis p-value:", format.pval(mssa_men_tests[[age]]$kw_p, digits = 3), "\n")
  cat("Pairwise t-tests (Bonferroni-corrected):\n")
  print(mssa_men_tests[[age]]$pairwise_tests)
}

cat("\nMSSA Statistical Tests - Women:\n")
for(age in names(mssa_women_tests)) {
  cat("\nAge Group:", age, "\n")
  cat("ANOVA p-value:", format.pval(mssa_women_tests[[age]]$anova_p, digits = 3), "\n")
  cat("Kruskal-Wallis p-value:", format.pval(mssa_women_tests[[age]]$kw_p, digits = 3), "\n")
  cat("Pairwise t-tests (Bonferroni-corrected):\n")
  print(mssa_women_tests[[age]]$pairwise_tests)
}

# Display individual plots
print(ecoli_men_boxplot)
print(ecoli_women_boxplot)
print(mssa_men_boxplot)
print(mssa_women_boxplot)

# Create combined plots for E. coli (men and women side by side)
ecoli_combined <- grid.arrange(
  ecoli_men_boxplot + theme(legend.position = "bottom"),
  ecoli_women_boxplot + theme(legend.position = "bottom"),
  ncol = 2,
  top = textGrob("E. coli Rates by Gender, Age Group and COVID Period",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Create combined plots for MSSA (men and women side by side)
mssa_combined <- grid.arrange(
  mssa_men_boxplot + theme(legend.position = "bottom"),
  mssa_women_boxplot + theme(legend.position = "bottom"),
  ncol = 2,
  top = textGrob("MSSA Rates by Gender, Age Group and COVID Period",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

# Display plots
print(ecoli_combined)
print(mssa_combined)
############################################################################################################

############################################################################################################
# Time Series Analysis Component

# Calculate year-over-year changes
calculate_yoy_changes <- function(data, rate_column) {
  data %>%
    arrange(`Financial year`) %>%
    group_by(`Age group (years)`) %>%
    mutate(
      yoy_change = (get(rate_column) - lag(get(rate_column))) / lag(get(rate_column)) * 100,
      yoy_abs_change = get(rate_column) - lag(get(rate_column))
    ) %>%
    ungroup()
}

# Calculate changes for each dataset
ecoli_men_ts <- calculate_yoy_changes(Ecoli, "All reported case rate (per 100,000 population) (men)")
ecoli_women_ts <- calculate_yoy_changes(Ecoli, "All reported case rate (per 100,000 population) (women)")
mssa_men_ts <- calculate_yoy_changes(MSSA, "All reported case rate (per 100,000 population) (men)")
mssa_women_ts <- calculate_yoy_changes(MSSA, "All reported case rate (per 100,000 population) (women)")

# Updated plotting function with trend lines
plot_yoy_changes <- function(data, title) {
  # Convert Financial year to numeric for trend line calculation
  data$year_num <- as.numeric(factor(data$`Financial year`, levels = unique(data$`Financial year`)))
  
  ggplot(data, aes(x = year_num, y = yoy_change, color = `Age group (years)`, group = `Age group (years)`)) +
    # Add trend line for each age group
    geom_smooth(method = "loess", span = 0.75, se = FALSE, linewidth = 1.2) +
    # Add original points and lines
    geom_line(linewidth = 0.5, alpha = 0.6) +
    geom_point(size = 2.5) +
    # Add zero reference line
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    # Add COVID period shading
    annotate("rect", 
             xmin = which(unique(data$`Financial year`) == "2019 to 2020"),
             xmax = which(unique(data$`Financial year`) == "2021 to 2022"),
             ymin = -Inf, ymax = Inf,
             fill = 'grey90', alpha = 0.3) +
    theme_minimal() +
    labs(title = title,
         x = "Financial Year",
         y = "Year-over-Year Change (%)",
         color = "Age Group") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right") +
    scale_color_brewer(palette = "Set2") +
    scale_x_continuous(breaks = 1:length(unique(data$`Financial year`)),
                       labels = unique(data$`Financial year`))
}

# Create year-over-year change plots
ecoli_men_yoy_plot <- plot_yoy_changes(ecoli_men_ts, "E. coli Year-over-Year Changes - Men")
ecoli_women_yoy_plot <- plot_yoy_changes(ecoli_women_ts, "E. coli Year-over-Year Changes - Women")
mssa_men_yoy_plot <- plot_yoy_changes(mssa_men_ts, "MSSA Year-over-Year Changes - Men")
mssa_women_yoy_plot <- plot_yoy_changes(mssa_women_ts, "MSSA Year-over-Year Changes - Women")

# Calculate maximum drops and recoveries during COVID period
summarize_covid_impact <- function(data) {
  data %>%
    filter(!is.na(yoy_change)) %>%
    group_by(`Age group (years)`) %>%
    summarise(
      max_drop = min(yoy_change[period == "During-COVID"], na.rm = TRUE),
      max_recovery = max(yoy_change[period == "Post-COVID"], na.rm = TRUE),
      time_to_recovery = sum(yoy_change < 0, na.rm = TRUE)
    )
}

# Generate impact summaries
ecoli_men_impact <- summarize_covid_impact(ecoli_men_ts)
ecoli_women_impact <- summarize_covid_impact(ecoli_women_ts)
mssa_men_impact <- summarize_covid_impact(mssa_men_ts)
mssa_women_impact <- summarize_covid_impact(mssa_women_ts)

# Print impact summaries
cat("\nE. coli COVID Impact Summary - Men:\n")
print(ecoli_men_impact)
cat("\nE. coli COVID Impact Summary - Women:\n")
print(ecoli_women_impact)
cat("\nMSSA COVID Impact Summary - Men:\n")
print(mssa_men_impact)
cat("\nMSSA COVID Impact Summary - Women:\n")
print(mssa_women_impact)

##################################################################################################
###################################################################################################
