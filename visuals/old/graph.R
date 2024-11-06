# Combine all data into a single data frame for plotting
all_scenarios <- bind_rows(scenario1_stats, scenario2_stats, scenario3_stats, scenario4_stats)


# Combine historical data with simulation data
combined_data <- rbind(historical_data, all_scenarios)

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

# Combine all data into a single data frame for plotting
all_scenarios <- bind_rows(scenario1_stats, scenario2_stats, scenario3_stats, scenario4_stats)

#Check columns historical data, and set missing to NA
missing_cols <- setdiff(colnames(historical_data), colnames(all_scenarios))
all_scenarios[missing_cols] <- NA

missing_cols <- setdiff(colnames(all_scenarios), colnames(historical_data))
historical_data[missing_cols] <- NA



# Combine historical data with simulation data
combined_data <- rbind(historical_data, all_scenarios)

# Plot using ggplot2
# ggplot(combined_data, aes(x = Year, y = median)) +
#   geom_line(aes(color = scenario), linewidth = 1.2) +  # Main line for the median
#   geom_ribbon(data = subset(combined_data, scenario != "Historical"), 
#               aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA) +  # Shaded area for IQR
#   labs(title = "Installed capacity",
#        x = "Year", y = "Value",
#        color = "Scenario", fill = "Scenario") +
#   ylab("MW") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5))  # Center-align the title
# 
# ggplot(combined_data, aes(x = Year, y = median, color = scenario)) +
#   geom_line(linewidth = 1.2) +  # Main line for the mean
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA) +  # Shaded area for confidence intervals
#   labs(title = "Installed capacity ECs",
#        x = "Year", y = "Value",
#        color = "Scenario", fill = "Scenario") +
#   ylab("MW") +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5))  # Center-align the title


ggplot(combined_data, aes(x = Year, y = median, color = scenario)) +
  geom_line(data = subset(combined_data, scenario == "Historical"), aes(color = "Historical"), linewidth = 1.2) +  # Historical data line
  geom_line(data = subset(combined_data, scenario != "Historical"), linewidth = 1.2) +  # Scenario data lines
  geom_ribbon(data = subset(combined_data, scenario != "Historical"), 
              aes(ymin = lower, ymax = upper, fill = scenario), alpha = 0.12, color = NA) +  # Shaded area for IQR
  #scale_color_manual(values = c("Historical" = "black", "Base line" = "blue", "High contagion" = "red", "High professionalization" = "green", "Combined policies" = "purple")) +  # Custom colors
  labs(title = "Installed capacity",
       x = "Year", y = "Value",
       color = "Scenario", fill = "Scenario") +
  ylab("MW") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))  # Center-align the title