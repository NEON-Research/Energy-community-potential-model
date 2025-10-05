# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(cowplot)

# Set working directory and file paths 

setwd("C:\\Users\\naudl\\Documents\\GitHub\\Energy-community-potential-model\\results")
#setwd("C:\\Users\\s124129\\Documents\\GitHub\\Energy-community-potential-model\\results")
file_path <- "_monte_carlo_results_transition_scen.xlsx"
file_path_historic <- "_EC_summary.xlsx"




# Modify the function to adjust legend width
create_plot <- function(title, column_historic, sheet, scale){  
  start_year = 2020
  
  scenario_numbers <- 1:5
  scenarios <- c(
    "Baseline",
    "Energy-as-a-service",
    "Organizational breakthroughs",
    "Combined",
    "Combined hybrid projects"
  )
  
  scenario_list <- list()
  
  historical_data <- read_excel(file_path_historic, sheet = "calibration_statistics") %>%
    select(year, column_historic) %>%
    rename(median = column_historic) %>%
    mutate(lower = NA, upper = NA, mean = median * scale)%>%
    filter(year >= 2020)
  
  # Loop through scenarios
  for (i in seq_along(scenario_numbers)) {
    scenario_number <- scenario_numbers[i] 
    
    # Read scenario data
    simulated_data <- read_excel(file_path, sheet = sheet)
    
    # Filter columns where the value in row 63 == scenarioNumber
    filtered_columns <- which(simulated_data[62, ] == scenario_number)
    filtered_data <- simulated_data[1:42, filtered_columns]
    
    # Ensure the data is numeric
    if(sheet == "installedCap"){
      numeric_data <- apply(filtered_data, 2, as.numeric)* scale / 1000
    }
    else {
      numeric_data <- apply(filtered_data, 2, as.numeric)* scale
    }
    
    # Function to compute mean and confidence intervals
    get_mean_and_ci <- function(df) {
      data.frame(
        mean  = rowMeans(df, na.rm = TRUE),
        lower = apply(df, 1, function(x) quantile(x, probs = 0.05, na.rm = TRUE)),
        upper = apply(df, 1, function(x) quantile(x, probs = 0.95, na.rm = TRUE))
      )
    }
    
    # Compute statistics
    scenario_stats <- get_mean_and_ci(numeric_data)
    
    # Add year column starting at 2020
    scenario_stats$year <- 2009 + seq_len(nrow(scenario_stats)) - 1
    scenario_stats <- scenario_stats %>%
      filter(year >= 2024)
    
    
    scenario_list[[scenarios[i]]] <- scenario_stats

  }
  
  # Bind all scenarios into a single dataframe with scenario labels
  all_scenarios <- bind_rows(
    lapply(seq_along(scenario_list), function(i) {
      df <- scenario_list[[i]]
      df$scenario <- names(scenario_list)[i]
      df
    })
  )
  
  
  # Combine historical and scenario data
  combined_data <- bind_rows(
    historical_data %>% mutate(scenario = "Historical"),
    all_scenarios
  ) %>%
    mutate(lower = ifelse(is.na(lower), mean, lower),
           upper = ifelse(is.na(upper), mean, upper),
           scenario = factor(scenario, levels = c("Historical", scenarios)))
  
  # Define color mapping
  default_colors <- scales::hue_pal()(length(scenarios))
  names(default_colors) <- scenarios
  color_mapping <- c("Historical" = "darkgray", default_colors)
  
  # Plot the data with the adjusted legend width
  plot <- ggplot(combined_data, aes(x = year, y = mean, color = scenario)) +
    geom_line(linewidth = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA) +
    scale_x_continuous(limits = c(start_year, NA)) +
    scale_color_manual(values = color_mapping) +
    scale_fill_manual(values = color_mapping) +
    labs(title = title,
         x = "Year",
         y = title,
         color = "Scenario",
         fill = "Scenario") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the title
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
plotECs <- create_plot("ECs","ECs", "ECs", 1)
plotProjects <- create_plot("Projects","Projects", "projects", 1)
plotInstalledCapacity <- create_plot("Installed capacity (GW)", "Installed cap (MW)", "installedCap", 1/1000)
plotPercMembers <- create_plot("Members (% of households)", "Members (% of households)", "percMembers", 1)

print(plotECs)
print(plotProjects)
print(plotInstalledCapacity)
print(plotPercMembers)



# Remove legends from individual plots
plotProjects_noleg         <- plotProjects + theme(legend.position = "none")
plotInstalledCapacity_noleg <- plotInstalledCapacity + theme(legend.position = "none")
plotPercMembers_noleg      <- plotPercMembers + theme(legend.position = "none")

# Remove axis text, ticks, and titles
plotProjects_noleg <- plotProjects_noleg +
  theme(
    axis.title = element_blank()
  )

plotInstalledCapacity_noleg <- plotInstalledCapacity_noleg +
  theme(
    axis.title = element_blank()
  )

plotPercMembers_noleg <- plotPercMembers_noleg +
  theme(
    axis.title = element_blank()
  )


# Extract the legend from one plot
legend <- cowplot::get_legend(plotProjects + 
  theme(legend.position = "bottom",
  legend.background = element_rect(fill = "white", color = NA)
  )
)

# Wrap the legend in a full-width panel
legend_fullwidth <- ggdraw() + 
  draw_grob(legend) +
  theme(plot.background = element_rect(fill = "white", color = NA))


# Combine plots in a row and legend below
combined <- cowplot::plot_grid(
  cowplot::plot_grid(
    plotProjects_noleg, plotInstalledCapacity_noleg, plotPercMembers_noleg,
    labels = c("A", "B", "C"), ncol = 3, align = "v", axis = "tb"
  ),
  legend_fullwidth,
  ncol = 1,
  rel_heights = c(1, 0.2)  # adjust legend height
)

# Save the figure with enough vertical space for the legend
ggsave("transition_scenarios.png", combined, width = 12, height = 4.5, dpi = 300)


