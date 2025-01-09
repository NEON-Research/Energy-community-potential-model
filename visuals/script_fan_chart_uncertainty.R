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
file_path <- "_sensetivity_interactions.xlsx"

data_ECs <- read_excel(file_path, sheet = "sensetivity_ECs")
data_projects <- read_excel(file_path, sheet = "sensetivity_projects")


years <- 2009:2050


data_ECs <- data_ECs[,-1]
data_ECs <- data_ECs[1:42, ]
data_ECs <- cbind(years, data_ECs)
data_projects <- data_projects[,-1]
data_projects <- data_projects[1:42, ]
data_projects <- cbind(years, data_projects)

start_year = 2020
data_ECs <- data_ECs[years >= start_year, ]
data_projects <- data_projects[years >= start_year, ]


# ---- six quantiles -----
quantile_data_ECs <- data_ECs %>%
  pivot_longer(-years, names_to = "series", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%  # Convert 'value' to numeric if necessary
  group_by(years) %>%
  summarise(
    p0 = quantile(value, 0.0),
    p5 = quantile(value, 0.05),
    p10 = quantile(value, 0.10),
    p25 = quantile(value, 0.25),
    p35 = quantile(value, 0.35),   # Add 35th percentile
    p45 = quantile(value, 0.45),
    median = quantile(value, 0.50),
    p55 = quantile(value, 0.55),
    p65 = quantile(value, 0.65),   # Add 65th percentile
    p75 = quantile(value, 0.75),
    p90 = quantile(value, 0.90),
    p95 = quantile(value, 0.95),
    p100 = quantile(value, 1.0)
  )

ggplot(quantile_data_ECs, aes(x = years, y = median)) +
  geom_line()

# Create the first plot (without the legend)
plotEC <- ggplot(quantile_data_ECs, aes(x = years)) +
  # Outermost layer: 0-5 and 95-100 percentiles
  geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0% - 100% percentiles"), alpha = 0.2) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5% - 95% percentiles"), alpha = 0.4) +
  
  # Middle layers
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10% - 90% percentiles"), alpha = 0.3) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25% - 75% percentiles"), alpha = 0.4) +
  geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35% - 65% percentiles"), alpha = 0.5) +
  
  # Inner band: 45-55 percentiles (high confidence)
  geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45% - 55% percentiles"), alpha = 0.5) +
  
  # Add the median line (p50)
  geom_line(aes(y = median, color = "Median"), linewidth = 0.6) +
  
  # Labels and theme
  labs(title = "Senestivity to interaction effects in quantile ranges",
       x = NULL,
       y = "#",
       fill = NULL,  # Remove the fill legend title
       color = NULL) +  # Remove the color legend title
  scale_fill_manual(values = c(
    "0% - 100% percentiles" = "lightblue",
    "5% - 95% percentiles" = "lightblue",
    "10% - 90% percentiles" = "blue",
    "25% - 75% percentiles" = "blue",
    "35% - 65% percentiles" = "blue",
    "45% - 55% percentiles" = "darkblue"
  ),   
  breaks = c(
    "0% - 100% percentiles",
    "5% - 95% percentiles",
    "10% - 90% percentiles",
    "25% - 75% percentiles",
    "35% - 65% percentiles",
    "45% - 55% percentiles"
  )) +
  scale_color_manual(values = c("Median" = "dimgray")) +  # Custom colors for the median
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend from this plot


# ---- six quantiles -----
quantile_data_projects <- data_projects %>%
  pivot_longer(-years, names_to = "series", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%  # Convert 'value' to numeric if necessary
  group_by(years) %>%
  summarise(
    p0 = quantile(value, 0.0),
    p5 = quantile(value, 0.05),
    p10 = quantile(value, 0.10),
    p25 = quantile(value, 0.25),
    p35 = quantile(value, 0.35),   # Add 35th percentile
    p45 = quantile(value, 0.45),
    median = quantile(value, 0.50),
    p55 = quantile(value, 0.55),
    p65 = quantile(value, 0.65),   # Add 65th percentile
    p75 = quantile(value, 0.75),
    p90 = quantile(value, 0.90),
    p95 = quantile(value, 0.95),
    p100 = quantile(value, 1.0)
  )



# Create the second plot (similar structure to the first)
plotProjects <- ggplot(quantile_data_projects, aes(x = years)) +
  # Outermost layer: 0-5 and 95-100 percentiles
  geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0% - 100% percentiles"), alpha = 0.2) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5% - 95% percentiles"), alpha = 0.4) +
  
  # Middle layers
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10% - 90% percentiles"), alpha = 0.3) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25% - 75% percentiles"), alpha = 0.4) +
  geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35% - 65% percentiles"), alpha = 0.5) +
  
  # Inner band: 45-55 percentiles (high confidence)
  geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45% - 55% percentiles"), alpha = 0.5) +
  
  # Add the median line (p50)
  geom_line(aes(y = median, color = "Median"), linewidth = 0.6) +
  
  # Labels and theme
  labs(title = " ",
       x = NULL,
       y = " ",
       fill = NULL,  # Remove the fill legend title
       color = NULL) +  # Remove the color legend title
  scale_fill_manual(values = c(
    "0% - 100% percentiles" = "lightblue",
    "5% - 95% percentiles" = "lightblue",
    "10% - 90% percentiles" = "blue",
    "25% - 75% percentiles" = "blue",
    "35% - 65% percentiles" = "blue",
    "45% - 55% percentiles" = "darkblue"
  ),   
  breaks = c(
    "0% - 100% percentiles",
    "5% - 95% percentiles",
    "10% - 90% percentiles",
    "25% - 75% percentiles",
    "35% - 65% percentiles",
    "45% - 55% percentiles"
  )) +
  scale_color_manual(values = c("Median" = "dimgray")) +  # Custom colors for the median
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend from this plot

# # Extract the full legend from one of the plots
# legend <- # Create the second plot (similar structure to the first)
legend_plot_full <- ggplot(quantile_data_projects, aes(x = years)) +
  # Outermost layer: 0-5 and 95-100 percentiles
  geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0% - 100% percentiles"), alpha = 0.2) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5% - 95% percentiles"), alpha = 0.4) +
  
  # Middle layers
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10% - 90% percentiles"), alpha = 0.3) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25% - 75% percentiles"), alpha = 0.4) +
  geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35% - 65% percentiles"), alpha = 0.5) +
  
  # Inner band: 45-55 percentiles (high confidence)
  geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45% - 55% percentiles"), alpha = 0.5) +
  
  # Add the median line (p50)
  geom_line(aes(y = median, color = "Median"), linewidth = 0.6) +
  
  # Labels and theme
  labs(title = "Quantile range projects",
       x = NULL,
       y = "#",
       fill = "Quantile ranges",  # Remove the fill legend title
       color = "Statistics") +  # Remove the color legend title
  scale_fill_manual(values = c(
    "0% - 100% percentiles" = "lightblue",
    "5% - 95% percentiles" = "lightblue",
    "10% - 90% percentiles" = "blue",
    "25% - 75% percentiles" = "blue",
    "35% - 65% percentiles" = "blue",
    "45% - 55% percentiles" = "darkblue"
  ),   
  breaks = c(
    "0% - 100% percentiles",
    "5% - 95% percentiles",
    "10% - 90% percentiles",
    "25% - 75% percentiles",
    "35% - 65% percentiles",
    "45% - 55% percentiles"
  )) +
  scale_color_manual(values = c("Median" = "dimgray")) +  # Custom colors for the median
  theme_minimal() +
  theme(legend.position = "bottom")  # Remove the legend from this plot

# Create a dummy plot for the legend only
legend_plot_quantileRange <- ggplot(quantile_data_projects, aes(x = years)) +
  # Outermost layer: 0-5 and 95-100 percentiles
  geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0% - 100% percentiles"), alpha = 0.2) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5% - 95% percentiles"), alpha = 0.4) +
  
  # Middle layers
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10% - 90% percentiles"), alpha = 0.3) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25% - 75% percentiles"), alpha = 0.4) +
  geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35% - 65% percentiles"), alpha = 0.5) +
  
  # Inner band: 45-55 percentiles (high confidence)
  geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45% - 55% percentiles"), alpha = 0.5) +
  
  # Add the median line (p50)
 # geom_line(aes(y = median, color = "Median"), size = 0.8) +
  
  # Labels and theme
  labs(title = "Quantile range projects",
       x = NULL,
       y = "#",
       fill = "Quantile ranges", # Remove the fill legend title
       color = NULL) +  # Remove the color legend title
  scale_fill_manual(values = c(
    "0% - 100% percentiles" = "lightblue",
    "5% - 95% percentiles" = "lightblue",
    "10% - 90% percentiles" = "blue",
    "25% - 75% percentiles" = "blue",
    "35% - 65% percentiles" = "blue",
    "45% - 55% percentiles" = "darkblue"
  ),   
  breaks = c(
    "0% - 100% percentiles",
    "5% - 95% percentiles",
    "10% - 90% percentiles",
    "25% - 75% percentiles",
    "35% - 65% percentiles",
    "45% - 55% percentiles"
  )) +
  theme_minimal() +
  theme(legend.position = "right") 

# Create a dummy plot for the legend only
legend_plot_statistics <- ggplot(quantile_data_projects, aes(x = years)) +
  # Outermost layer: 0-5 and 95-100 percentiles
  geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0% - 100% percentiles"), alpha = 0.2) +
  geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5% - 95% percentiles"), alpha = 0.4) +
  
  # Middle layers
  geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10% - 90% percentiles"), alpha = 0.3) +
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25% - 75% percentiles"), alpha = 0.4) +
  geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35% - 65% percentiles"), alpha = 0.5) +
  
  # Inner band: 45-55 percentiles (high confidence)
  geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45% - 55% percentiles"), alpha = 0.5) +
  
  # Add the median line (p50)
  geom_line(aes(y = median, color = "Median"), size = 0.8) +
  
  # Labels and theme
  labs(title = "Quantile range projects",
       x = NULL,
       y = "#",
       fill = NULL,  # Remove the fill legend title
       color = "Statistics") +  # Remove the color legend title
  scale_fill_manual(values = c(
    "0% - 100% percentiles" = "lightblue",
    "5% - 95% percentiles" = "lightblue",
    "10% - 90% percentiles" = "blue",
    "25% - 75% percentiles" = "blue",
    "35% - 65% percentiles" = "blue",
    "45% - 55% percentiles" = "darkblue"
  ),   
  breaks = NULL) +
  scale_color_manual(values = c("Median" = "dimgray")) +  # Custom colors for the median
  theme_minimal() +
  theme(legend.position = "right") 

# Define the function to extract legend
get_legend <- function(myplot) {
  tmp <- ggplotGrob(myplot)
  legend <- tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box")]]
  return(legend)
}

# Extract the legend from one of the plots
legendQuantileRanges <- get_legend(legend_plot_quantileRange)  # You can use either plot
legendStatistics <- get_legend(legend_plot_statistics)  # You can use either plot
legendBottom <- get_legend(legend_plot_full)
# Arrange the plots and the legend in two columns
combined_plot <- grid.arrange(
  plotEC,
  plotProjects,
  legendQuantileRanges, 
  legendStatistics,
  ncol = 2,  # Two columns for the plots
  heights = c(1, 0.5)  # Adjust space between the plots and the legend
)

# Arrange the plots and the legend in two columns
combined_plot2 <- grid.arrange(
  plotEC,
  plotProjects,
  legendQuantileRanges,
  legendStatistics,
  ncol = 2,  # Two columns for the plots
  heights = c(1, 0.6)  # Adjust space between the plots and the legend
)

# Display the combined plot
grid.draw(combined_plot)
