# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(tidyr)
library(RColorBrewer)


# Load the Excel files
setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\Energy community potential model_calibration")
file_path <- "_sensetivity_analysis.xlsx"

data_ECs <- read_excel(file_path, sheet = "sensetivity_params_ECs")
data_projects <- read_excel(file_path, sheet = "sensetivity_params_projects")

current_variable = 5
#currentSetting = "ECs"
currentSetting = "Projects"
if( currentSetting == "ECs"){
  graph_title = "Energy communities"
  variable_title = "ECs - Collective learning rate"
} else{
  variable_title = "Projects - Collective learning rate"
  graph_title = "Projects"
}


# Historical data
file_path_historic <- "_EC_summary.xlsx"
historical_data <- read_excel(file_path_historic, sheet = "calibration_statistics")
if( graph_title == "Energy communities"){
  historical_data <- historical_data %>% select(year, 'ECs')
  historical_data <- historical_data %>% rename(median = 'ECs')
} else if( graph_title == "Projects"){
  historical_data <- historical_data %>% select(year, 'Projects')
  historical_data <- historical_data %>% rename(median = 'Projects')
}
# Add NA values for lower and upper bounds
historical_data <- historical_data %>% mutate(lower = NA, upper = NA)
historical_data <- historical_data %>% mutate(mean = median)       # Creating 'mean' column as a copy of 'median'
historical_data <- historical_data %>%
  select(-median)
# Replace NA values in lower and upper columns with the median value for Historical scenario
historical_data <- historical_data %>%
  mutate(lower = ifelse(is.na(lower), mean, lower),
         upper = ifelse(is.na(upper), mean, upper))

# # Extract the years column
# years <- data_ECs[[1]]
# years <- head( years, 42)
years <- 2009:2050
output$year <- as.numeric(output$year)

# Filter columns where the value in row 57 == 1
filtered_columns_ECs <- which(data_ECs[57, ] == current_variable)
filtered_data_ECs <- data_ECs[, filtered_columns_ECs]
filtered_columns_projects <- which(data_projects[54, ] == current_variable)
filtered_data_projects <- data_projects[, filtered_columns_projects]

filtered_data_ECs <- filtered_data_ECs[,-1]
filtered_data_projects <- filtered_data_projects[,-1]


# # Exclude the first column (years) from the data for calculations
# data_ECs <- data_ECs[,-1]
# data_projects <- data_projects[,-1]

# Function to compute mean and confidence intervals (80%)
get_mean_and_ci <- function(df) {
  # Convert all columns to numeric
  df <- as.data.frame(lapply(df, as.numeric))
  
  mean_vals <- rowMeans(df, na.rm = TRUE)  # Calculate mean for each time point
  lower_ci <- apply(df, 1, function(x) quantile(x, probs = 0.1, na.rm = TRUE))  # 10th percentile
  upper_ci <- apply(df, 1, function(x) quantile(x, probs = 0.9, na.rm = TRUE))  # 90th percentile
  
  return(data.frame(mean = mean_vals, lower = lower_ci, upper = upper_ci))
}

output <- data.frame(percentage = integer(), mean = numeric(), lower = numeric(), upper = numeric())

for (i in 0:10) {
  # Filter columns where the value in row 57 == 1
  if( graph_title == "Energy communities"){
    filtered_columns_step <- which(filtered_data_ECs[56, ] == i)
    filtered_data_step <- filtered_data_ECs[, filtered_columns_step]
    filtered_data_step <- head(filtered_data_step, 42)
  } else if( graph_title == "Projects"){
    filtered_columns_step <- which(filtered_data_projects[53, ] == i)
    filtered_data_step <- filtered_data_projects[, filtered_columns_step]
    filtered_data_step <- head(filtered_data_step, 42)
  }
  summary_data <- get_mean_and_ci(filtered_data_step)
  if( i == 0){
    data_at_0 = filtered_data_step
  }
  
  # Add transformed group and corresponding year information to the summary_data
  transformed_group_value <- 90 + i * 2  # Group transformation: 90 + group value * 2
  
  # Add group information
  summary_data <- summary_data %>%
    mutate(percentage = transformed_group_value, year = years ) #1:nrow(summary_data))  # 'year' is the row index (1 to 42)
  
  # Append to output
  output <- bind_rows(output, summary_data)
}


# Add a column to distinguish historical and scenario data
historical_data <- historical_data %>%
  mutate(percentage = "Historical")  # Assign a label for the historical data

# Add 'data_type' to your existing scenario data to distinguish it from historical data
output <- output %>%
  mutate(percentage = as.factor(percentage))

# Combine historical data with scenario data
combined_data <- bind_rows(output, historical_data)
# Make sure percentage is a factor for proper mapping
combined_data$percentage <- factor(combined_data$percentage)


# Replace NA values in lower and upper columns with the median value for Historical scenario
combined_data <- combined_data %>%
  mutate(lower = ifelse(is.na(lower), mean, lower),
         upper = ifelse(is.na(upper), mean, upper))
# Make sure percentage is a factor for proper mapping
combined_data$percentage <- factor(combined_data$percentage)

# Generate a color palette with 11 distinct colors
palette_colors <- brewer.pal(n = 11, name = "Set3")  # Use "Greens" for a green color scale
palette_colors <- c("gray", palette_colors)  # Add gray for historical

# Add color names for scenarios
levels(combined_data$percentage) <- c("Historical", paste0("Scenario", 0:10))  # Change levels as needed

# 
# Create the plot
plot5Projects = ggplot(combined_data, aes(x = year, y = mean)) +
  # Scenario lines with color mapping based on percentage
  geom_line(data = combined_data %>% filter(percentage != "Historical"),
            aes(color = percentage), size = 0.8) +  # Scenario lines
  # Scenario confidence interval ribbons
  geom_ribbon(data = combined_data %>% filter(percentage != "Historical"),
              aes(ymin = lower, ymax = upper, fill = percentage),
              alpha = 0.12, linetype = 0) +  # Scenario CI ribbons
  # Historical data line in gray
  geom_line(data = historical_data, aes(color = "Historical"), size = 1) +  # Historical line
  # Historical confidence interval ribbon in gray
  geom_ribbon(data = historical_data, aes(ymin = lower, ymax = upper, fill = "Historical"),
              fill = "gray", alpha = 0.3) +  # Historical CI ribbon
  # Labels and theme
  labs(title = variable_title,
       x = "Year", y = "#",
       color = "Group", fill = "Group") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),   # Remove y-axis title
        panel.background = element_blank(),  # Set background to blank (white)
        plot.background = element_blank(),  # Set the plot area background to white
        panel.grid.major = element_line(color = "grey80"),  # Optional: Customize major grid lines
        panel.grid.minor = element_line(color = "grey90")) +   # Optional: Customize minor grid lines
  scale_x_continuous(breaks = seq(2009, 2050, 5)) +  # Adjust x-axis breaks for clarity
  scale_color_manual(values = c("Historical" = "gray")) +  # Explicitly set historical color
  scale_fill_manual(values = c("Historical" = "gray"))  # Explicitly set historical fill color
# 


# Arrange plots in a grid with the shared legend at the bottom
grid.arrange(
  arrangeGrob(plot1EC, plot1Projects, plot2EC, plot2Projects, plot3EC, plot3Projects, plot4EC, plot4Projects, plot5EC, plot5Projects,  nrow = 5, ncol = 2),  # Grid of plots
  nrow = 2,
  heights = c(6, 1)  # Adjust space between the plot grid and legend
)


# # Create the plot
# ggplot(combined_data, aes(x = year, y = mean)) +
#   # Scenario lines with color mapping based on percentage
#   geom_line(data = combined_data %>% filter(percentage != "Historical"), 
#             aes(color = percentage), size = 1) +  # Scenario lines
#   # Scenario confidence interval ribbons
#   geom_ribbon(data = combined_data %>% filter(percentage != "Historical"), 
#               aes(ymin = lower, ymax = upper, fill = percentage), 
#               alpha = 0.2, linetype = 0) +  # Scenario CI ribbons
#   # Historical data line in gray
#   geom_line(data = historical_data, aes(color = "Historical"), size = 1.2) +  # Historical line
#   # Historical confidence interval ribbon in gray
#   geom_ribbon(data = historical_data, aes(ymin = lower, ymax = upper, fill = "Historical"), 
#               alpha = 0.3, fill = "gray") +  # Historical CI ribbon
#   # Labels and theme
#   labs(title = "Mean with 90% Confidence Intervals by Group",
#        x = "Year", y = "#",
#        color = "Group", fill = "Group") +
#   theme_minimal() +  # Minimal theme for cleaner look
#   scale_x_continuous(breaks = seq(2009, 2050, 5)) +  # Adjust x-axis breaks for clarity
#   scale_color_manual(values = c("Historical" = "gray", palette_colors)) +  # Manual color scale
#   scale_fill_manual(values = c("Historical" = "gray", palette_colors)) +  # Manual fill scale
#   theme(legend.position = "bottom")  # Place legend at the bottom for clarity
