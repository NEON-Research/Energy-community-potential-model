# Install packages if you haven't already
install.packages(c("readxl", "dplyr", "ggplot2"))
install.packages("scales")


# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Load the Excel file from the specified folder
setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\Energy community potential model")
file_path <- "results_monte_carlo_v2.xlsx"

##----ENERGY COMMUNITIES----

# Read the Excel file (reading the first sheet by default)
data <- read_excel(file_path)

#----INSTALLED CAPACITY-----

data_scenario1 <- read_excel(file_path, sheet = "baseCase_projects")
data_scenario2 <- read_excel(file_path, sheet = "highContagion_projects")
data_scenario3 <- read_excel(file_path, sheet = "highProf_projects")
data_scenario4 <- read_excel(file_path, sheet = "combined_projects")

# Set historic data frame
file_path_historic <- "_EC_summary.xlsx"
historical_data  <- read_excel(file_path_historic, sheet = "calibration_statistics")

# Extract the years column
years <- data_scenario1[[1]]

# Exclude the first column (years) from the data for calculations
data_without_years_scen1 <- data_scenario1[,-1]
data_without_years_scen2 <- data_scenario2[,-1]
data_without_years_scen3 <- data_scenario3[,-1]
data_without_years_scen4 <- data_scenario4[,-1]

# Multiply to get percentage of households
data_without_years_scen1 <- data_without_years_scen1*518 / 1000
data_without_years_scen2 <- data_without_years_scen2*518 / 1000
data_without_years_scen3 <- data_without_years_scen3*518 / 1000
data_without_years_scen4 <- data_without_years_scen4*518 / 1000

# Filter scenario data to start from 2023
data_scenario1_filtered <- subset(data_scenario1, year >= 2023)
data_scenario2_filtered <- subset(data_scenario2, year >= 2023)
data_scenario3_filtered <- subset(data_scenario3, year >= 2023)
data_scenario4_filtered <- subset(data_scenario4, year >= 2023)

# # Function to compute mean and confidence intervals (80%)
# get_mean_and_ci <- function(df) {
#   mean_vals <- rowMeans(df)  # Calculate mean for each time point
#   lower_ci <- apply(df, 1, function(x) quantile(x, probs = 0.10))  # 10th percentile
#   upper_ci <- apply(df, 1, function(x) quantile(x, probs = 0.90))  # 90th percentile
#   
#   return(data.frame(mean = mean_vals, lower = lower_ci, upper = upper_ci))
# }

# GET INTERQUARTILE RANGE (IQR) AND MEDIAN
# Function to compute median and interquartile range (IQR)
get_median_and_iqr <- function(df) {
  median_vals <- apply(df, 1, median)  # Calculate median for each time point
  lower_iqr <- apply(df, 1, function(x) quantile(x, probs = 0.25))  # 25th percentile
  upper_iqr <- apply(df, 1, function(x) quantile(x, probs = 0.75))  # 75th percentile
  
  return(data.frame(median = median_vals, lower = lower_iqr, upper = upper_iqr))
}

# Calculate median and IQR for each scenario
scenario1_stats <- get_median_and_iqr(data_without_years_scen1)
scenario2_stats <- get_median_and_iqr(data_without_years_scen2)
scenario3_stats <- get_median_and_iqr(data_without_years_scen3)
scenario4_stats <- get_median_and_iqr(data_without_years_scen4)

# Calculate mean and confidence intervals for each scenario
scenario1_stats <- get_mean_and_ci(data_without_years_scen1)
scenario2_stats <- get_mean_and_ci(data_without_years_scen2)
scenario3_stats <- get_mean_and_ci(data_without_years_scen3)
scenario4_stats <- get_mean_and_ci(data_without_years_scen4)

# Add the years column to each scenario data frame
scenario1_stats$Year <- years
scenario2_stats$Year <- years
scenario3_stats$Year <- years
scenario4_stats$Year <- years

# Label the scenarios
scenario1_stats$scenario <- "Base line"
scenario2_stats$scenario <- "High contagion"
scenario3_stats$scenario <- "High professionalization"
scenario4_stats$scenario <- "Combined policies"

scenario1_data_filtered <-subset(scenario1_stats, year >=2023)
