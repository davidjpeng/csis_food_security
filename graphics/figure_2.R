# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)

# Read the data
df <- read.csv("combined_aggregate_results/summary_table.csv")

# Calculate prevalence and handle missing values
df <- df %>%
  mutate(prevalence = people_estimate / gen_pop,
         prevalence_lower = lower_bound / gen_pop,
         prevalence_upper = upper_bound / gen_pop) %>%
  # Remove rows with NA values in key columns
  filter(!is.na(people_estimate) & !is.na(prevalence)) %>%
  # Reorder DEGURBA factor levels: Rural first, then Towns, then Cities
  mutate(DEGURBA = factor(DEGURBA, levels = c("Rural areas", "Towns and semi-dense areas", "Cities")))

# Filter data for moderate + severe and severe
moderate_severe_data <- df %>%
  filter(severity == "Moderate + Severe") %>%
  select(country, DEGURBA, people_estimate, prevalence, prevalence_lower, prevalence_upper, lower_bound, upper_bound)

severe_data <- df %>%
  filter(severity == "Severe") %>%
  select(country, DEGURBA, people_estimate, prevalence, prevalence_lower, prevalence_upper, lower_bound, upper_bound)

# Define custom colors
custom_colors <- c("Rural areas" = "#228B22",          
                   "Towns and semi-dense areas" = "#8FBC8F",  
                   "Cities" = "#A0522D")                

# Enhanced prevalence plot with consistent label positioning (always above error bars)
p_prevalence <- moderate_severe_data %>%
  mutate(
    label_y = prevalence_upper + 0.05,  
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = prevalence, fill = DEGURBA)) +
  annotate("rect", 
           xmin = 1.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "gray60", color = "gray60", linetype = "dashed", size = 0.3) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = prevalence_lower, ymax = prevalence_upper), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(prevalence * 100, 1), "%"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                     breaks = seq(0, 1, 0.2),  
                     limits = c(0, 1.1)) +  # Extended to 110% to accommodate labels above error bars
  labs(title = "A",
       x = "Area Type (DEGURBA)",
       y = "% of Population") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "none")


p_absolute <- moderate_severe_data %>%
  mutate(
    shared_max = max(c(moderate_severe_data$upper_bound, severe_data$upper_bound), na.rm = TRUE),
    label_y = upper_bound + (shared_max * 0.08),
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = people_estimate, fill = DEGURBA)) +
  annotate("rect", 
           xmin = 1.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "gray60", color = "gray60", linetype = "dashed", size = 0.3) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(people_estimate/1e6, 1), "M"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6, 1), "M"),
                     limits = c(0, max(c(moderate_severe_data$upper_bound, severe_data$upper_bound), na.rm = TRUE) * 1.25)) +
  labs(title = "B",
       x = "Area Type (DEGURBA)", 
       y = "People (Millions)") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "bottom",
        legend.justification = "center")

# Print individual plots
print(p_prevalence)
print(p_absolute)

# Remove legend from both individual plots for combined version
p_prevalence_no_legend <- p_prevalence + theme(legend.position = "none")
p_absolute_no_legend <- p_absolute + theme(legend.position = "none")

# Create combined plot
combined_plot <- p_prevalence_no_legend | p_absolute_no_legend
combined_plot <- combined_plot + 
  plot_annotation(
    #title = "Moderate or Severe Food Insecurity by Country",
    #subtitle = "Prevalence (%) vs Population Count (People, millions)",
    #caption = "Gray boxes highlight areas considered urban | Error bars show 95% CI"
    ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_plot)

##############################################################################################################
##############################################################################################################
### Severe Food Insecurity

p_prevalence_sev <- severe_data %>%
  mutate(
    label_y = prevalence_upper + 0.05,
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = prevalence, fill = DEGURBA)) +
  annotate("rect", 
           xmin = 1.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "gray60", color = "gray60", linetype = "dashed", size = 0.3) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = prevalence_lower, ymax = prevalence_upper), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(prevalence * 100, 1), "%"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                     breaks = seq(0, 1, 0.2),  
                     limits = c(0, 1.1)) +  # Extended to 110% to accommodate labels above error bars
  labs(title = "A",
       x = "Area Type (DEGURBA)",
       y = "% of Population") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "none")

p_absolute_sev <- severe_data %>%
  mutate(
    shared_max = max(c(moderate_severe_data$upper_bound, severe_data$upper_bound), na.rm = TRUE),
    label_y = upper_bound + (shared_max * 0.08),
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = people_estimate, fill = DEGURBA)) +
  annotate("rect", 
           xmin = 1.5, xmax = 3.5,
           ymin = -Inf, ymax = Inf,
           alpha = 0.08, fill = "gray60", color = "gray60", linetype = "dashed", size = 0.3) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(people_estimate/1e6, 1), "M"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6, 1), "M"),
                     limits = c(0, max(c(moderate_severe_data$upper_bound, severe_data$upper_bound), na.rm = TRUE) * 1.25)) +
  labs(title = "B",
       x = "Area Type (DEGURBA)", 
       y = "People (Millions)") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "bottom",
        legend.justification = "center")

# Remove legend from both individual plots for combined version
p_prevalence_sev_no_legend <- p_prevalence_sev + theme(legend.position = "none")
p_absolute_sev_no_legend <- p_absolute_sev + theme(legend.position = "none")

# Create combined plot for severe
combined_plot_sev <- p_prevalence_sev_no_legend | p_absolute_sev_no_legend
combined_plot_sev <- combined_plot_sev + 
  plot_annotation(
    #title = "Severe Food Insecurity by Country",
    #subtitle = "Prevalence (%) vs Population Count (People, millions)",
    #caption = "Gray boxes highlight areas considered urban | Error bars show 95% CI"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_plot_sev)

ggsave("graphics/output/Figure_2.1.jpeg", plot = combined_plot, 
       width = 180, height = 170, units="mm", dpi = 300, device = "jpeg")

ggsave("graphics/output/Figure_2.2.jpeg", plot = combined_plot_sev, 
       width = 180, height = 170, units="mm", dpi = 300, device = "jpeg")