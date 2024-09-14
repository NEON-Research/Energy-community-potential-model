install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Example data
set.seed(123)
time <- seq(2020, 2030, by = 1)
median_forecast <- cumsum(rnorm(11, 2, 1))

# Different confidence intervals
ci_50_lower <- median_forecast - runif(11, 1, 2)
ci_50_upper <- median_forecast + runif(11, 1, 2)
ci_75_lower <- median_forecast - runif(11, 2, 3)
ci_75_upper <- median_forecast + runif(11, 2, 3)
ci_95_lower <- median_forecast - runif(11, 3, 4)
ci_95_upper <- median_forecast + runif(11, 3, 4)

data <- data.frame(time, median_forecast, ci_50_lower, ci_50_upper, ci_75_lower, ci_75_upper, ci_95_lower, ci_95_upper)

ggplot(data, aes(x = time)) +
  geom_ribbon(aes(ymin = ci_95_lower, ymax = ci_95_upper), fill = "lightblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = ci_75_lower, ymax = ci_75_upper), fill = "blue", alpha = 0.5) +
  geom_ribbon(aes(ymin = ci_50_lower, ymax = ci_50_upper), fill = "darkblue", alpha = 0.7) +
  geom_line(aes(y = median_forecast), color = "black", size = 1) +
  labs(title = "Fan Chart with Different Confidence Intervals",
       x = "Year",
       y = "Forecast Value") +
  theme_minimal()
