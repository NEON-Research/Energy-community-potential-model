# ============================================================
# LOAD LIBRARIES
# ============================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(cowplot)
library(RColorBrewer)
library(grid)

# ============================================================
# LOAD DATA
# ============================================================
setwd("C:\\Users\\naudl\\Documents\\GitHub\\Energy-community-potential-model\\results")
file_path <- "_sensitivity_interaction.xlsx"

data_ECs <- read_excel(file_path, sheet = "ECs")
data_projects <- read_excel(file_path, sheet = "projects")

years <- 2009:2050
start_year <- 2020

data_ECs <- data_ECs[,-1][1:42, ] |> cbind(years)
data_projects <- data_projects[,-1][1:42, ] |> cbind(years)

data_ECs <- data_ECs[years >= start_year, ]
data_projects <- data_projects[years >= start_year, ]

# ============================================================
# FUNCTION TO COMPUTE QUANTILES
# ============================================================
compute_quantiles <- function(df) {
  df %>%
    pivot_longer(-years, names_to = "series", values_to = "value") %>%
    mutate(value = as.numeric(value)) %>%
    group_by(years) %>%
    summarise(
      p0 = quantile(value, 0.0),
      p5 = quantile(value, 0.05),
      p10 = quantile(value, 0.10),
      p25 = quantile(value, 0.25),
      p35 = quantile(value, 0.35),
      p45 = quantile(value, 0.45),
      median = quantile(value, 0.50),
      p55 = quantile(value, 0.55),
      p65 = quantile(value, 0.65),
      p75 = quantile(value, 0.75),
      p90 = quantile(value, 0.90),
      p95 = quantile(value, 0.95),
      p100 = quantile(value, 1.0)
    )
}

quantile_data_ECs <- compute_quantiles(data_ECs)
quantile_data_projects <- compute_quantiles(data_projects)

# ============================================================
# FUNCTION TO CREATE QUANTILE PLOTS
# ============================================================
make_quantile_plot <- function(data, title = "", ylab = "", show_legend = FALSE) {
  p <- ggplot(data, aes(x = years)) +
    geom_ribbon(aes(ymin = p0, ymax = p100, fill = "0–100%"), alpha = 0.2) +
    geom_ribbon(aes(ymin = p5, ymax = p95, fill = "5–95%"), alpha = 0.4) +
    geom_ribbon(aes(ymin = p10, ymax = p90, fill = "10–90%"), alpha = 0.3) +
    geom_ribbon(aes(ymin = p25, ymax = p75, fill = "25–75%"), alpha = 0.4) +
    geom_ribbon(aes(ymin = p35, ymax = p65, fill = "35–65%"), alpha = 0.5) +
    geom_ribbon(aes(ymin = p45, ymax = p55, fill = "45–55%"), alpha = 0.5) +
    geom_line(aes(y = median, color = "Median"), linewidth = 0.6) +
    labs(
      title = title,
      x = NULL, y = ylab,
      fill = if (show_legend) "Quantile ranges" else NULL,
      color = if (show_legend) "Statistics" else NULL
    ) +
    scale_fill_manual(
      values = c(
        "0–100%" = "lightblue",
        "5–95%" = "lightblue",
        "10–90%" = "blue",
        "25–75%" = "blue",
        "35–65%" = "blue",
        "45–55%" = "darkblue"
      ),
      breaks = c("0–100%", "5–95%", "10–90%", "25–75%", "35–65%", "45–55%")  # Correct legend order
    ) +
    scale_color_manual(values = c("Median" = "dimgray")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      plot.title = element_text(hjust = 0.5, face = "plain"),  # centered title
      axis.title.y = element_text(face = "plain"),
      axis.title.x = element_text(face = "plain")
    )
  
  return(p)
}

# ============================================================
# CREATE PLOTS (NO LEGENDS IN MAIN ONES)
# ============================================================
plotEC <- make_quantile_plot(quantile_data_ECs, "ECs", "#")
plotProjects <- make_quantile_plot(quantile_data_projects, "Projects", " ")

# ============================================================
# CREATE SEPARATE LEGENDS (ONLY ONCE EACH)
# ============================================================

# Legend for Quantile ranges (fill)
legend_quantiles <- cowplot::get_legend(
  make_quantile_plot(quantile_data_projects, show_legend = TRUE) +
    guides(color = "none") +  # remove color (Median) from this legend
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "plain"),
      legend.background = element_rect(fill = "white", color = NA)
    )
)

# Legend for Statistics (Median line)
legend_statistics <- cowplot::get_legend(
  ggplot(quantile_data_projects, aes(x = years, y = median, color = "Median")) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Median" = "dimgray")) +
    labs(color = "Statistics") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_text(face = "plain"),
      legend.background = element_rect(fill = "white", color = NA)
    )
)

# Combine both legends side-by-side
legends_combined <- plot_grid(
  ggdraw() + draw_grob(legend_quantiles),
  ggdraw() + draw_grob(legend_statistics),
  ncol = 2,
  rel_widths = c(1, 1)
)

# Add a white background for the combined legend area
legend_fullwidth <- ggdraw() +
  draw_grob(ggplotGrob(legends_combined)) +
  theme(plot.background = element_rect(fill = "white", color = NA))

# ============================================================
# ARRANGE FINAL LAYOUT
# ============================================================
plots_with_labels <- plot_grid(
  plotEC,
  plotProjects,
  labels = c("A", "B"),
  label_size = 14,
  label_fontface = "plain",  # no bold labels
  ncol = 2,
  align = "v",
  axis = "tb"
)

combined_plot <- plot_grid(
  plots_with_labels,
  legend_fullwidth,
  ncol = 1,
  rel_heights = c(1, 0.25)
)

# ============================================================
# SAVE AND SHOW
# ============================================================
ggsave("plot_sensitivity_interaction.png", combined_plot, width = 8, height = 4, dpi = 300)
print(combined_plot)
