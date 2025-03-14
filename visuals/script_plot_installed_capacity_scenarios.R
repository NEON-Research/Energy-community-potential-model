# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

# Set working directory and file paths
setwd("D:\\GitHub\\Energy-community-potential-model\\results")
file_path <- "_monte_carlo_results_final.xlsx"
file_path_historic <- "_EC_summary.xlsx"

# Define the start year
start_year <- 2020

# Modify the function to adjust legend width
create_installed_capacity_plot <- function(){  
  # Read scenario data
  data_scenario1 <- read_excel(file_path, sheet = "baseCase_projects")
  data_scenario2 <- read_excel(file_path, sheet = "highContagion_projects")
  data_scenario3 <- read_excel(file_path, sheet = "highProf_projects")
  data_scenario4 <- read_excel(file_path, sheet = "combined_projects")
  
  # Read historical data
  historical_data <- read_excel(file_path_historic, sheet = "calibration_statistics") %>%
    select(year, `Installed cap (MW)`) %>%
    rename(median = `Installed cap (MW)`) %>%
    mutate(lower = NA, upper = NA, mean = median)
  
  # Filter scenario data for years >= 2023
  filter_years <- function(data) subset(data, year >= 2023)
  data_scenario1_filtered <- filter_years(data_scenario1)
  data_scenario2_filtered <- filter_years(data_scenario2)
  data_scenario3_filtered <- filter_years(data_scenario3)
  data_scenario4_filtered <- filter_years(data_scenario4)
  
  # Convert projects to MW (518 kW per project)
  convert_to_mw <- function(df) df[, -1] * 518 / 1000
  data_scenario1_mw <- convert_to_mw(data_scenario1_filtered)
  data_scenario2_mw <- convert_to_mw(data_scenario2_filtered)
  data_scenario3_mw <- convert_to_mw(data_scenario3_filtered)
  data_scenario4_mw <- convert_to_mw(data_scenario4_filtered)
  
  # Compute mean and confidence intervals
  get_mean_and_ci <- function(df) {
    data.frame(
      mean = rowMeans(df),
      lower = apply(df, 1, function(x) quantile(x, probs = 0.05)),
      upper = apply(df, 1, function(x) quantile(x, probs = 0.95))
    )
  }
  
  # Compute statistics
  scenario1_stats <- get_mean_and_ci(data_scenario1_mw)
  scenario2_stats <- get_mean_and_ci(data_scenario2_mw)
  scenario3_stats <- get_mean_and_ci(data_scenario3_mw)
  scenario4_stats <- get_mean_and_ci(data_scenario4_mw)
  
  # Add year and scenario labels
  years_filtered <- data_scenario1_filtered$year
  scenarios <- c("Base line", "High social learning", "High professionalization", "Combined policies")
  scenario_list <- list(scenario1_stats, scenario2_stats, scenario3_stats, scenario4_stats)
  
  all_scenarios <- bind_rows(
    mapply(function(data, label) {
      data$year <- years_filtered
      data$scenario <- label
      data
    }, scenario_list, scenarios, SIMPLIFY = FALSE)
  )
  
  # Combine historical and scenario data
  combined_data <- bind_rows(
    historical_data %>% mutate(scenario = "Historical"),
    all_scenarios
  ) %>%
    mutate(lower = ifelse(is.na(lower), mean, lower),
           upper = ifelse(is.na(upper), mean, upper))
  
  # Define color mapping
  default_colors <- scales::hue_pal()(4)
  names(default_colors) <- scenarios
  color_mapping <- c("Historical" = "darkgray", default_colors)
  
  # Plot the data with the adjusted legend width
  plot <- ggplot(combined_data, aes(x = year, y = mean, color = scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA) +
    scale_x_continuous(limits = c(start_year, NA)) +
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    labs(title = "Installed Capacity",
         x = "Year",
         y = "Installed Capacity (MW)",
         color = "Scenario",
         fill = "Scenario") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = c(0, 0),  # Align legend to the left
      legend.box.just = "left",        # Ensure proper left alignment
      legend.spacing.x = unit(0.2, "cm"),
      legend.spacing.y = unit(0.4, "cm"),
      legend.key.width = unit(0.6, "cm"),
      legend.box.margin = margin(-5, -5, -5, -5),
      legend.text = element_text(size = 11)
    ) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE),
           fill = guide_legend(nrow = 2, byrow = TRUE))
  
  return(plot)
}

# Create and display the updated plot
plotInstalledCapacity <- create_installed_capacity_plot()
print(plotInstalledCapacity)
