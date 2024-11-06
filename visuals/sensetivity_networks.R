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
setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\results")
file_path <- "_sensetivity_network.xlsx"

#----ECs----
data_ECs <- read_excel(file_path, sheet = "sens_network_ECs")

filtered_colums <- which(data_ECs[52, ] == "RANDOM_NETWORK")
random_network_ECs <- data_ECs[, filtered_colums]
filtered_colums <- which(data_ECs[52, ] == "SMALL_WORLD_GEOGRAPHICAL")
small_world_geographical_ECs <- data_ECs[, filtered_colums]
filtered_colums <- which(data_ECs[52, ] == "SMALL_WORLD_SIMILAR")
small_world_similar_ECs <- data_ECs[, filtered_colums]
random_network_ECs <- random_network_ECs[1:42, ]
small_world_geographical_ECs <- small_world_geographical_ECs[1:42, ]
small_world_similar_ECs <- small_world_similar_ECs[1:42, ]

# Step 3: Convert all columns to numeric
random_network_ECs <- as.data.frame(lapply(random_network_ECs, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))
small_world_geographical_ECs <- as.data.frame(lapply(small_world_geographical_ECs, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))
small_world_similar_ECs <- as.data.frame(lapply(small_world_similar_ECs, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))

years <- 2009:2050

# Function to compute mean and confidence intervals (80%)
get_mean_and_ci <- function(df) {
  mean_vals <- rowMeans(df)  # Calculate mean for each time point
  lower_ci <- apply(df, 1, function(x) quantile(x, probs = 0.05))  # 10th percentile
  upper_ci <- apply(df, 1, function(x) quantile(x, probs = 0.95))  # 90th percentile
  
  return(data.frame(mean = mean_vals, lower = lower_ci, upper = upper_ci))
}

# Calculate mean and confidence intervals for each scenario
scenario1_stats <- get_mean_and_ci(random_network_ECs)
scenario2_stats <- get_mean_and_ci(small_world_geographical_ECs)
scenario3_stats <- get_mean_and_ci(small_world_similar_ECs)

# Label the scenarios
scenario1_stats$scenario <- "Random network"
scenario2_stats$scenario <- "Small world geographical"
scenario3_stats$scenario <- "Small world similar"


# Create a year vector for each dataset
years_data1 <- 2009:(2009 + nrow(scenario1_stats) - 1)  # e.g., 2009, 2010, 2011
years_data2 <- 2009:(2009 + nrow(scenario2_stats) - 1)
years_data3 <- 2009:(2009 + nrow(scenario3_stats) - 1)

# Add year column to each dataset
scenario1_stats <- cbind(year = years_data1, scenario1_stats)
scenario2_stats <- cbind(year = years_data2, scenario2_stats)
scenario3_stats <- cbind(year = years_data3, scenario3_stats)

historic_years <- 2009:2023
historic_data <- random_network_ECs[1:length(historic_years), ]

# Calculate mean and confidence intervals for the historic data
historic_stats <- get_mean_and_ci(historic_data)
historic_stats$scenario <- "Historic"
historic_stats <- cbind(year = historic_years, historic_stats)

# Filter out the historic years from the other datasets
historic_years_min1 <- 2009:2022
scenario1_stats <- scenario1_stats[!(scenario1_stats$year %in% historic_years_min1), ]
scenario2_stats <- scenario2_stats[!(scenario2_stats$year %in% historic_years_min1), ]
scenario3_stats <- scenario3_stats[!(scenario3_stats$year %in% historic_years_min1), ]

# Combine all scenario data
all_scenarios <- bind_rows(scenario1_stats, scenario2_stats, scenario3_stats)
all_scenarios_ECs <- bind_rows(all_scenarios, historic_stats)



#----projectss----
data_projects <- read_excel(file_path, sheet = "sens_network_projects")

filtered_colums <- which(data_projects[52, ] == "RANDOM_NETWORK")
random_network_projects <- data_projects[, filtered_colums]
filtered_colums <- which(data_projects[52, ] == "SMALL_WORLD_GEOGRAPHICAL")
small_world_geographical_projects <- data_projects[, filtered_colums]
filtered_colums <- which(data_projects[52, ] == "SMALL_WORLD_SIMILAR")
small_world_similar_projects <- data_projects[, filtered_colums]
random_network_projects <- random_network_projects[1:42, ]
small_world_geographical_projects <- small_world_geographical_projects[1:42, ]
small_world_similar_projects <- small_world_similar_projects[1:42, ]

# Step 3: Convert all columns to numeric
random_network_projects <- as.data.frame(lapply(random_network_projects, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))
small_world_geographical_projects <- as.data.frame(lapply(small_world_geographical_projects, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))
small_world_similar_projects <- as.data.frame(lapply(small_world_similar_projects, function(x) {
  as.numeric(as.character(x))  # Convert to character first to handle factors
}))

# Calculate mean and confidence intervals for each scenario
scenario1_stats <- get_mean_and_ci(random_network_projects)
scenario2_stats <- get_mean_and_ci(small_world_geographical_projects)
scenario3_stats <- get_mean_and_ci(small_world_similar_projects)

# Label the scenarios
scenario1_stats$scenario <- "Random network"
scenario2_stats$scenario <- "Small world geographical"
scenario3_stats$scenario <- "Small world similar"


# Create a year vector for each dataset
years_data1 <- 2009:(2009 + nrow(scenario1_stats) - 1)  # e.g., 2009, 2010, 2011
years_data2 <- 2009:(2009 + nrow(scenario2_stats) - 1)
years_data3 <- 2009:(2009 + nrow(scenario3_stats) - 1)

# Add year column to each dataset
scenario1_stats <- cbind(year = years_data1, scenario1_stats)
scenario2_stats <- cbind(year = years_data2, scenario2_stats)
scenario3_stats <- cbind(year = years_data3, scenario3_stats)

historic_data <- random_network_projects[1:length(historic_years), ]

# Calculate mean and confidence intervals for the historic data
historic_stats <- get_mean_and_ci(historic_data)
historic_stats$scenario <- "Historic"
historic_stats <- cbind(year = historic_years, historic_stats)

# Filter out the historic years from the other datasets
historic_years_min1 <- 2009:2022
scenario1_stats <- scenario1_stats[!(scenario1_stats$year %in% historic_years_min1), ]
scenario2_stats <- scenario2_stats[!(scenario2_stats$year %in% historic_years_min1), ]
scenario3_stats <- scenario3_stats[!(scenario3_stats$year %in% historic_years_min1), ]

# Combine all scenario data
all_scenarios <- bind_rows(scenario1_stats, scenario2_stats, scenario3_stats)
all_scenarios_projects <- bind_rows(all_scenarios, historic_stats)


# Choose a color palette
palette <- brewer.pal(3, "Set1")  # Choose a palette for 3 scenarios
names(palette) <- c("Random network", "Small world geographical", "Small world similar")

# Add gray for the Historic scenario
palette <- c(palette, "Historic" = "dimgrey")

# Plot the data with the chosen palette and set the line width
plotECs = ggplot(all_scenarios_ECs, aes(x = year, y = mean, color = scenario, fill = scenario)) +
  geom_line(linewidth = 0.6) +  # Line plot for the mean values with specified line width
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.16, color = NA) +  # Shaded area for confidence intervals without border
  scale_color_manual(values = palette) +  # Custom colors for each scenario
  scale_fill_manual(values = palette) +  # Matching fill colors
  labs(title = "Sensetivity to network structure",
       x = NULL, #"Year",
       y = "#",
       color = "Scenario",
       fill = "Scenario") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Plot the data with the chosen palette and set the line width
plotProjects = ggplot(all_scenarios_projects, aes(x = year, y = mean, color = scenario, fill = scenario)) +
  geom_line(linewidth = 0.6) +  # Line plot for the mean values with specified line width
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.16, color = NA) +  # Shaded area for confidence intervals without border
  scale_color_manual(values = palette) +  # Custom colors for each scenario
  scale_fill_manual(values = palette) +  # Matching fill colors
  labs(title = "  " ,
       x = NULL, #"Year",
       y = " ",
       color = "Scenario",
       fill = "Scenario") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

# Plot legend
plotLegend = ggplot(all_scenarios_projects, aes(x = year, y = mean, color = scenario, fill = scenario)) +
  geom_line(linewidth = 0.6) +  # Line plot for the mean values with specified line width
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.16, color = NA) +  # Shaded area for confidence intervals without border
  scale_color_manual(values = palette) +  # Custom colors for each scenario
  scale_fill_manual(values = palette) +  # Matching fill colors
  labs(title = "Sensetivity to network structure - ECs",
       x = NULL, #"Year",
       y = "#",
       color = "Scenario",
       fill = "Scenario") +
  theme_minimal()# +
  #theme(legend.position = "bottom")  # Remove the legend

# Define the function to extract legend
get_legend <- function(myplot) {
  tmp <- ggplotGrob(myplot)
  legend <- tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

# Extract the legend from one of the plots
legend <- get_legend(plotLegend)  # You can use either plot

# Arrange the plots and the legend in two columns
combined_plot <- grid.arrange(
  plotECs,
  plotProjects,
  legend, 
  ncol = 2,  # Two columns for the plots
  heights = c(1, 0.5)  # Adjust space between the plots and the legend
)

# Display the combined plot
grid.draw(combined_plot)