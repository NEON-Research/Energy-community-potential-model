# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# Load the Excel files
setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\Energy community potential model")
file_path <- "results_monte_carlo_v2.xlsx"
file_path_historic <- "_EC_summary.xlsx"

# graph_title = "Energy communities"
graph_title = "Projects"
# # graph_title = "Members"
# # graph_title = "Installed capacity"
y_axis = "#"
if( graph_title == "Energy communities"){
  # Read the Excel files
  data_scenario1 <- read_excel(file_path, sheet = "baseCase_ECs")
  data_scenario2 <- read_excel(file_path, sheet = "highContagion_ECs")
  data_scenario3 <- read_excel(file_path, sheet = "highProf_ECs")
  data_scenario4 <- read_excel(file_path, sheet = "combined_ECs")
  
} else{
  data_scenario1 <- read_excel(file_path, sheet = "baseCase_projects")
  data_scenario2 <- read_excel(file_path, sheet = "highContagion_projects")
  data_scenario3 <- read_excel(file_path, sheet = "highProf_projects")
  data_scenario4 <- read_excel(file_path, sheet = "combined_projects")
}

# Historical data
historical_data <- read_excel(file_path_historic, sheet = "calibration_statistics")
if( graph_title == "Energy communities"){
  historical_data <- historical_data %>% select(year, 'ECs')
  historical_data <- historical_data %>% rename(median = 'ECs')
} else if( graph_title == "Projects"){
  historical_data <- historical_data %>% select(year, 'Projects')
  historical_data <- historical_data %>% rename(median = 'Projects')
} else if( graph_title == "Members"){
  historical_data <- historical_data %>% select(year, 'EC Members')
  historical_data <- historical_data %>% rename(median = 'EC Members')
} else if( graph_title == "Installed capacity"){
  historical_data <- historical_data %>% select(year, 'Installed cap (MW)')
  historical_data <- historical_data %>% rename(median = 'Installed cap (MW)')
}
# Add NA values for lower and upper bounds
historical_data <- historical_data %>% mutate(lower = NA, upper = NA)

# Extract the years column
years <- data_scenario1[[1]]

# Exclude the first column (years) from the data for calculations
data_without_years_scen1 <- data_scenario1[,-1]
data_without_years_scen2 <- data_scenario2[,-1]
data_without_years_scen3 <- data_scenario3[,-1]
data_without_years_scen4 <- data_scenario4[,-1]

if( graph_title == "Members"){
  data_without_years_scen1 <- data_without_years_scen1 * 99 / 8043443 * 100 #99hh per project, 8mil hh in netherlands
  data_without_years_scen2 <- data_without_years_scen2 * 99 / 8043443 * 100
  data_without_years_scen3 <- data_without_years_scen3 * 99 / 8043443 * 100
  data_without_years_scen4 <- data_without_years_scen4 * 99 / 8043443 * 100
  y-axis = "% of households"
} else if( graph_title == "Installed capacity"){
  data_without_years_scen1 <- data_without_years_scen1 * 518 / 1000         # 518kW per project to MW
  data_without_years_scen2 <- data_without_years_scen2 * 518 / 1000
  data_without_years_scen3 <- data_without_years_scen3 * 518 / 1000
  data_without_years_scen4 <- data_without_years_scen4 * 518 / 1000
  y-axis = "MW"
}

# Filter scenario data to start from 2023
data_scenario1_filtered <- subset(data_scenario1, year >= 2023)
data_scenario2_filtered <- subset(data_scenario2, year >= 2023)
data_scenario3_filtered <- subset(data_scenario3, year >= 2023)
data_scenario4_filtered <- subset(data_scenario4, year >= 2023)

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

# Filter the years to match the filtered data
years_filtered <- years[years >= 2023]

# Add the filtered years column to each scenario data frame
scenario1_stats$year <- years_filtered
scenario2_stats$year <- years_filtered
scenario3_stats$year <- years_filtered
scenario4_stats$year <- years_filtered

# Label the scenarios
scenario1_stats$scenario <- "Base line"
scenario2_stats$scenario <- "High contagion"
scenario3_stats$scenario <- "High professionalization"
scenario4_stats$scenario <- "Combined policies"

# Combine all scenario data
all_scenarios <- bind_rows(scenario1_stats, scenario2_stats, scenario3_stats, scenario4_stats)

# Combine historical data with scenario data
combined_data <- bind_rows(
  historical_data %>% mutate(scenario = "Historical"),
  all_scenarios
)
# Replace NA values in lower and upper columns with the median value for Historical scenario
combined_data <- combined_data %>%
  mutate(lower = ifelse(is.na(lower), median, lower),
         upper = ifelse(is.na(upper), median, upper))

# Ensure 'scenario' is a factor and reorder levels
combined_data$scenario <- factor(combined_data$scenario, levels = c("Historical", "Base line", "High contagion", "High professionalization", "Combined policies"))

# Get the default ggplot2 colors
default_colors <- scales::hue_pal()(4)
names(default_colors) <- c("Base line", "High contagion", "High professionalization", "Combined policies")

# Combine the colors into one vector
color_mapping <- c("Historical" = "darkgray", default_colors)


# Plot the combined data
ggplot(combined_data, aes(x = year, y = median, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.1, color = NA, na.rm = TRUE) +
  scale_color_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping) +
  labs(title = graph_title,
       x = "Year",
       y = y_axis,
       color = "Scenario",
       fill = "Scenario") +
  theme_minimal() +
  theme(legend.position = "right",
      plot.title = element_text(hjust = 0.5))  # Center-align the title

