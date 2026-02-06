library(tidyverse)
library(scales)
library(patchwork)

df <- read.csv("combined_aggregate_results/summary_table.csv")

df <- df %>%
  mutate(prevalence = people_estimate / gen_pop,
         prevalence_lower = lower_bound / gen_pop,
         prevalence_upper = upper_bound / gen_pop) %>%
  filter(!is.na(people_estimate) & !is.na(prevalence))

urban_rural_data_explicit <- df %>%
  mutate(area_type = ifelse(DEGURBA == "Rural areas", "Rural", "Urban")) %>%
  group_by(country, severity, area_type) %>%
  summarise(
    people_estimate = sum(people_estimate, na.rm = TRUE),
    gen_pop = sum(gen_pop, na.rm = TRUE),
    variance_absolute = sum((MOE_total / 1.96)^2, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    SE_absolute = sqrt(variance_absolute),
    MOE_total = 1.96 * SE_absolute,
    lower_bound = people_estimate - MOE_total,
    upper_bound = people_estimate + MOE_total,
    prevalence = people_estimate / gen_pop,
    prevalence_lower = lower_bound / gen_pop,
    prevalence_upper = upper_bound / gen_pop
  )

combined_totals <- urban_rural_data_explicit %>%
  group_by(country, severity) %>%
  summarise(
    total_people = sum(people_estimate, na.rm = TRUE),
    total_pop = sum(gen_pop, na.rm = TRUE),
    .groups = 'drop'
  )

# Reformat data for ggplot 
urban_rural_plot_data <- urban_rural_data_explicit %>%
  mutate(
    area_type = factor(area_type, levels = c("Rural", "Urban")),
    DEGURBA = area_type  
  ) %>%
  select(country, DEGURBA, severity, people_estimate, MOE_total, lower_bound, upper_bound, 
         gen_pop, prevalence, prevalence_lower, prevalence_upper)

# If you would like to save this in tabular format: 
# write.csv(urban_rural_plot_data, 
#           "graphics/output/summary_table_urban_rural.csv", 
#           row.names = FALSE)


# Filter data for moderate + severe and severe
moderate_severe_data_ur <- urban_rural_plot_data %>%
  filter(severity == "Moderate + Severe") %>%
  select(country, DEGURBA, people_estimate, prevalence, prevalence_lower, prevalence_upper, lower_bound, upper_bound)

severe_data_ur <- urban_rural_plot_data %>%
  filter(severity == "Severe") %>%
  select(country, DEGURBA, people_estimate, prevalence, prevalence_lower, prevalence_upper, lower_bound, upper_bound)

# Define custom colors for urban-rural (2 categories instead of 3)
custom_colors_ur <- c("Rural" = "#228B22",     
                      "Urban" = "#A0522D")      

p_prevalence_ur <- moderate_severe_data_ur %>%
  mutate(
    # Always position labels above error bars for consistency with better spacing
    label_y = prevalence_upper + 0.05,
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = prevalence, fill = DEGURBA)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = prevalence_lower, ymax = prevalence_upper), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(prevalence * 100, 1), "%"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors_ur, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                     breaks = seq(0, 1, 0.2),  
                     limits = c(0, 1.1)) +  # Extended to 110% to accommodate labels above error bars
  labs(title = "A",
       x = "Area Type",
       y = "% of Population") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, family = "sans"),  
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "none")

p_absolute_ur <- moderate_severe_data_ur %>%
  mutate(
    # Calculate shared max and spacing within the mutate call
    shared_max = max(c(moderate_severe_data_ur$upper_bound, severe_data_ur$upper_bound), na.rm = TRUE),
    label_y = upper_bound + (shared_max * 0.08),
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = people_estimate, fill = DEGURBA)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(people_estimate/1e6, 1), "M"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors_ur, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6, 1), "M"),
                     limits = c(0, max(c(moderate_severe_data_ur$upper_bound, severe_data_ur$upper_bound), na.rm = TRUE) * 1.25)) +
  labs(title = "B",
       x = "Area Type", 
       y = "People (Millions)") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, family = "sans"),  
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
print(p_prevalence_ur)
print(p_absolute_ur)

# Remove legend from both individual plots for combined version
p_prevalence_ur_no_legend <- p_prevalence_ur + theme(legend.position = "none")
p_absolute_ur_no_legend <- p_absolute_ur + theme(legend.position = "none")

# Create combined plot
combined_plot_ur <- p_prevalence_ur_no_legend | p_absolute_ur_no_legend
combined_plot_ur <- combined_plot_ur + 
  plot_annotation(
    #title = "Moderate or Severe Food Insecurity by Country (Urban-Rural)",
    #subtitle = "Prevalence (%) vs Population Count (People, millions)",
    #caption = "Left: Prevalence rates | Right: Population count | Error bars show 95% CI"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_plot_ur)

p_prevalence_sev_ur <- severe_data_ur %>%
  mutate(
    label_y = prevalence_upper + 0.05,
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = prevalence, fill = DEGURBA)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = prevalence_lower, ymax = prevalence_upper), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(prevalence * 100, 1), "%"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors_ur, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%"),
                     breaks = seq(0, 1, 0.2),  
                     limits = c(0, 1.1)) +  # Extended to 110% to accommodate labels above error bars
  labs(title = "A",
       x = "Area Type",
       y = "% of Population") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "none")

p_absolute_sev_ur <- severe_data_ur %>%
  mutate(
    shared_max = max(c(moderate_severe_data_ur$upper_bound, severe_data_ur$upper_bound), na.rm = TRUE),
    label_y = upper_bound + (shared_max * 0.08),
    label_color = "black"
  ) %>%
  ggplot(aes(x = DEGURBA, y = people_estimate, fill = DEGURBA)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), 
                width = 0.15, color = "gray30", linewidth = 0.5, alpha = 0.7) +
  geom_text(aes(y = label_y, label = paste0(round(people_estimate/1e6, 1), "M"), 
                color = label_color), 
            hjust = 0.5, size = 2.5, fontface = "bold", family = "sans") +
  scale_color_identity() +
  facet_wrap(~country, ncol = 1, strip.position = "right") +
  scale_fill_manual(values = custom_colors_ur, name = "Area Type") +
  scale_y_continuous(labels = function(x) paste0(round(x/1e6, 1), "M"),
                     limits = c(0, max(c(moderate_severe_data_ur$upper_bound, severe_data_ur$upper_bound), na.rm = TRUE) * 1.25)) +
  labs(title = "B",
       x = "Area Type", 
       y = "People (Millions)") +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 6, family = "sans"),
        axis.text.y = element_text(size = 6, family = "sans"),
        axis.title.x = element_text(size = 7, family = "sans"),
        axis.title.y = element_text(size = 7, family = "sans"),
        strip.text = element_text(size = 7, face = "bold", family = "sans"),
        plot.title = element_text(size = 8, face = "bold", family = "sans"),
        legend.text = element_text(size = 6, family = "sans"),
        legend.title = element_text(size = 7, family = "sans"),
        legend.position = "none",
        legend.justification = "center")

# Remove legend from both individual plots for combined version
p_prevalence_sev_ur_no_legend <- p_prevalence_sev_ur + theme(legend.position = "none")
p_absolute_sev_ur_no_legend <- p_absolute_sev_ur + theme(legend.position = "none")

# Create combined plot for severe
combined_plot_sev_ur <- p_prevalence_sev_ur_no_legend | p_absolute_sev_ur_no_legend
combined_plot_sev_ur <- combined_plot_sev_ur + 
  plot_annotation(
    #title = "Severe Food Insecurity by Country (Urban-Rural)",
    #subtitle = "Prevalence (%) vs Population Count (People, millions)",
    #caption = "Left: Prevalence rates | Right: Population count | Error bars show 95% CI"
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "none")

print(combined_plot_sev_ur)

# Save plots
ggsave("graphics/output/Figure_Supplemental_2.1.jpeg", plot = combined_plot_ur, 
       width = 180, height = 170, units="mm", dpi = 300, device = "jpeg")

ggsave("graphics/output/Figure_Supplemental_2.2.jpeg", plot = combined_plot_sev_ur, 
       width = 180, height = 170, units="mm", dpi = 300, device = "jpeg")

