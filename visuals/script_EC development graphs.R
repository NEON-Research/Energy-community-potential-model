# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)


# Load the Excel files
setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\results")
file_path <- "_monte_carlo_results_final.xlsx"
file_path_historic <- "_EC_summary.xlsx"

start_year <- 2020


create_plot <- function(graph_title){  
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
    y_axis = "# of ECs"
  } else if( graph_title == "Projects"){
    historical_data <- historical_data %>% select(year, 'Projects')
    historical_data <- historical_data %>% rename(median = 'Projects')
    y_axis = "# of projects"
  } else if( graph_title == "Members"){
    historical_data <- historical_data %>% select(year, 'EC Members')
    historical_data <- historical_data %>% rename(median = 'EC Members')
    y_axis = "# of members"
  } else if( graph_title == "Installed capacity"){
    historical_data <- historical_data %>% select(year, 'Installed cap (MW)')
    historical_data <- historical_data %>% rename(median = 'Installed cap (MW)')
    y_axis = "installed capacity (MW)"
  }
  # Add NA values for lower and upper bounds
  historical_data <- historical_data %>% mutate(lower = NA, upper = NA)
  historical_data <- historical_data %>% mutate(mean = median)       # Creating 'mean' column as a copy of 'median'
  
  # Filter scenario data to start from 2023
  data_scenario1_filtered <- subset(data_scenario1, year >= 2023)
  data_scenario2_filtered <- subset(data_scenario2, year >= 2023)
  data_scenario3_filtered <- subset(data_scenario3, year >= 2023)
  data_scenario4_filtered <- subset(data_scenario4, year >= 2023)
  
  # Extract the years column
  years <- data_scenario1[[1]]
  
  # Exclude the first column (years) from the data for calculations
  data_without_years_scen1 <- data_scenario1_filtered[,-1]
  data_without_years_scen2 <- data_scenario2_filtered[,-1]
  data_without_years_scen3 <- data_scenario3_filtered[,-1]
  data_without_years_scen4 <- data_scenario4_filtered[,-1]
  
  if( graph_title == "Members"){
    data_without_years_scen1 <- data_without_years_scen1 * 99 / 8043443 * 100 #99hh per project, 8mil hh in netherlands
    data_without_years_scen2 <- data_without_years_scen2 * 99 / 8043443 * 100
    data_without_years_scen3 <- data_without_years_scen3 * 99 / 8043443 * 100
    data_without_years_scen4 <- data_without_years_scen4 * 99 / 8043443 * 100
    historical_data <- historical_data %>%
      mutate(across(-year, ~ . / 8043443 * 100))
    historical_data <- subset(historical_data, year >= 2016)
    y_axis = "% of households"
  } else if( graph_title == "Installed capacity"){
    data_without_years_scen1 <- data_without_years_scen1 * 518 / 1000         # 518kW per project to MW
    data_without_years_scen2 <- data_without_years_scen2 * 518 / 1000
    data_without_years_scen3 <- data_without_years_scen3 * 518 / 1000
    data_without_years_scen4 <- data_without_years_scen4 * 518 / 1000
    y_axis = "MW"
  }
  
  
  # # Function to compute median and interquartile range (IQR)
  # get_median_and_iqr <- function(df) {
  #   median_vals <- apply(df, 1, median)  # Calculate median for each time point
  #   lower_iqr <- apply(df, 1, function(x) quantile(x, probs = 0.25))  # 25th percentile
  #   upper_iqr <- apply(df, 1, function(x) quantile(x, probs = 0.75))  # 75th percentile
  #   
  #   return(data.frame(median = median_vals, lower = lower_iqr, upper = upper_iqr))
  # }
  # 
  # # Calculate median and IQR for each scenario
  # scenario1_stats <- get_median_and_iqr(data_without_years_scen1)
  # scenario2_stats <- get_median_and_iqr(data_without_years_scen2)
  # scenario3_stats <- get_median_and_iqr(data_without_years_scen3)
  # scenario4_stats <- get_median_and_iqr(data_without_years_scen4)
  
  # Function to compute mean and confidence intervals (80%)
  get_mean_and_ci <- function(df) {
    mean_vals <- rowMeans(df)  # Calculate mean for each time point
    lower_ci <- apply(df, 1, function(x) quantile(x, probs = 0.05))  # 10th percentile
    upper_ci <- apply(df, 1, function(x) quantile(x, probs = 0.95))  # 90th percentile
    
    return(data.frame(mean = mean_vals, lower = lower_ci, upper = upper_ci))
  }
  
  # Calculate mean and confidence intervals for each scenario
  scenario1_stats <- get_mean_and_ci(data_without_years_scen1)
  scenario2_stats <- get_mean_and_ci(data_without_years_scen2)
  scenario3_stats <- get_mean_and_ci(data_without_years_scen3)
  scenario4_stats <- get_mean_and_ci(data_without_years_scen4)
  
  # Filter the years to match the filtered data
  years_filtered <- years[years >= 2023]
  
  # Add the filtered years column to each scenario data frame
  scenario1_stats$year <- years_filtered
  scenario2_stats$year <- years_filtered
  scenario3_stats$year <- years_filtered
  scenario4_stats$year <- years_filtered
  
  # Label the scenarios
  scenario1_stats$scenario <- "Base line"
  scenario2_stats$scenario <- "High social learning"
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
    mutate(lower = ifelse(is.na(lower), mean, lower),
           upper = ifelse(is.na(upper), mean, upper))
  
  # Ensure 'scenario' is a factor and reorder levels
  combined_data$scenario <- factor(combined_data$scenario, levels = c("Historical", "Base line", "High social learning", "High professionalization", "Combined policies"))
  
  # Get the default ggplot2 colors
  default_colors <- scales::hue_pal()(4)
  names(default_colors) <- c("Base line", "High social learning", "High professionalization", "Combined policies")
  
  # Combine the colors into one vector
  color_mapping <- c("Historical" = "darkgray", default_colors)
  
  combined_data <- combined_data %>% filter(year >= start_year)
  
  # Plot the combined data
  plot = ggplot(combined_data, aes(x = year, y = mean, color = scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA, na.rm = TRUE) +
    scale_x_continuous(limits = c(start_year, NA)) +  # Set x-axis to start at 2009
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    labs(title = graph_title,
         x = "Year",
         y = y_axis,
         color = "Scenario",
         fill = "Scenario") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),  # Remove y-axis title
          axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 8, family = "sans"),  # Vertical y-axis title
          axis.text = element_text(size = 10, family = "sans"),  # Axis text
          plot.title = element_text(hjust = 0.5, size = 12, family = "sans"))  # Plot title
  
  return(list(plot = plot, combined_data = combined_data))
}  

# Define color_mapping globally
default_colors <- scales::hue_pal()(4)
names(default_colors) <- c("Base line", "High social learning", "High professionalization", "Combined policies")
color_mapping <- c("Historical" = "darkgray", default_colors)

# Now call create_plot()
plotECs_data <- create_plot("Energy communities")
plotProjects_data <- create_plot("Projects")
plotMembers_data  <- create_plot("Members")
plotInstalledCapacity_data  <- create_plot("Installed capacity")

# Extract plots
plotECs <- plotECs_data$plot
plotProjects <- plotProjects_data$plot
plotMembers <- plotMembers_data$plot
plotInstalledCapacity <- plotInstalledCapacity_data$plot

# Extract combined_data
combined_data <- plotECs_data$combined_data

# Now use color_mapping in legend_plot
legend_plot <- ggplot(combined_data, aes(x = year, y = mean, color = scenario)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA, na.rm = TRUE) +
  scale_x_continuous(limits = c(start_year, NA)) +
  scale_color_manual(values = color_mapping) +
  scale_fill_manual(values = color_mapping) +
  labs(title = "Legend",
       x = "Year",
       y = "Value",
       color = "Scenario",
       fill = "Scenario") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.justification = c(0,0), # Align to the left
        legend.align = "l",
        legend.spacing.x = unit(0.5, "cm")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

# Extract and display legend
shared_legend <- get_legend(legend_plot)
grid.arrange(
  arrangeGrob(plotECs, plotProjects, plotMembers, plotInstalledCapacity, nrow = 2, ncol = 2),
  shared_legend,
  nrow = 2,
  heights = c(6, 1)
)

#


# plotECs_data <- create_plot("Energy communities")
# plotProjects_data <- create_plot("Projects")
# plotMembers_data  <- create_plot("Members")
# plotInstalledCapacity_data  <- create_plot("Installed capacity")
# 
# plotECs <- plotECs_data$plot
# plotProjects <- plotProjects_data$plot
# plotMembers <- plotMembers_data$plot
# plotInstalledCapacity <- plotInstalledCapacity_data$plot
# 
# combined_data <- plotECs_data$combined_data  # Save combined_data for legend plot
# 
# # Function to extract legend from a ggplot
# get_legend <- function(myplot) {
#   tmp <- ggplotGrob(myplot)
#   legend <- tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")]]
#   return(legend)
# }
# 
# # Extract the legend from one plot
# legend_plot <- ggplot(combined_data, aes(x = year, y = mean, color = scenario)) +
#   geom_line(linewidth = 0.8) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA, na.rm = TRUE) +
#   scale_x_continuous(limits = c(start_year, NA)) +  # Set x-axis to start at 2009
#   scale_color_manual(values = color_mapping) +
#   scale_fill_manual(values = color_mapping) +
#   labs(title = graph_title,
#        x = "Year",
#        y = y_axis,
#        color = "Scenario",
#        fill = "Scenario") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.direction = "horizontal",
#         legend.box = "horizontal",
#         legend.justification = c(0,0), #allign to the left
#         legend.align = "l", # left align legend items
#         legend.spacing.x = unit(0.5, "cm"),  # Adjust spacing between items
#         # legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Remove extra margins
#         # legend.key.width = unit(2, "cm")  # Set width for legend keys
#   ) +
#   guides(color = guide_legend(nrow = 2, byrow = TRUE))  # Allow wrapping in two rows
# 
# shared_legend <- get_legend(legend_plot)
# 
# # Arrange plots in a grid with the shared legend at the bottom
# grid.arrange(
#   arrangeGrob(plotECs, plotProjects, plotMembers, plotInstalledCapacity, nrow = 2, ncol = 2),  # Grid of plots
#   shared_legend,  # Shared legend at the bottom
#   nrow = 2,
#   heights = c(6, 1)  # Adjust space between the plot grid and legend
# )


