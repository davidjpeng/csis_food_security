# FIES x DEGURBA Trends (Limitation: Adult, Regional)

#https://www.fao.org/faostat/en/#data/FS
#Bulk Download: https://bulks-faostat.fao.org/production/Food_Security_Data_E_All_Data.zip

library(tidyverse)

#We include the aforementioned data in the repo as it is public 
faostat <- read_csv("graphics/data/Food_Security_Data_E_All_Data.csv")

fao_urban_rural <-faostat %>%
  filter(str_detect(Item, "urban|rural|town")) %>%
  select(where(~!all(is.na(.)))) 

#Plot world and regional trendlines, as FAO does not release country-level figures: 

filtered_data <- fao_urban_rural %>%
  select(Area, Item, Element, Unit, Y2022, Y2023, Y2024) %>%
  filter(!is.na(Y2022) | !is.na(Y2023) | !is.na(Y2024))

# Transform from wide to long format for the years 2022-2024
plot_data <- filtered_data %>%
  pivot_longer(
    cols = c(Y2022, Y2023, Y2024),
    names_to = "Year",
    values_to = "Value",
    names_prefix = "Y"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(!is.na(Value)) %>%
  mutate(
    ItemShort = case_when(
      # Severe only (not moderate or severe)
      str_detect(Item, "^Prevalence of severe food insecurity in the rural") ~ "Severe - Rural",
      str_detect(Item, "^Prevalence of severe food insecurity in the urban") ~ "Severe - Urban", 
      str_detect(Item, "^Prevalence of severe food insecurity in the town") ~ "Severe - Town/Semi-dense",
      # Moderate or severe
      str_detect(Item, "^Prevalence of moderate or severe food insecurity in the rural") ~ "Moderate/Severe - Rural",
      str_detect(Item, "^Prevalence of moderate or severe food insecurity in the urban") ~ "Moderate/Severe - Urban",
      str_detect(Item, "^Prevalence of moderate or severe food insecurity in the town") ~ "Moderate/Severe - Town/Semi-dense",
      TRUE ~ Item
    )
  )

# Reshape to get Value, Lower, and Upper bounds in the same row
plot_ready <- plot_data %>%
  select(Area, ItemShort, Element, Year, Value) %>%
  pivot_wider(
    names_from = Element,
    values_from = Value
  ) %>%
  rename(
    Estimate = `Value`,
    Lower = `Confidence interval: Lower bound`,
    Upper = `Confidence interval: Upper bound`
  ) %>%
  filter(!is.na(Estimate))

key_areas <- c("World", "Low-income economies", "Lower-middle-income economies")

plot_data_filtered <- plot_ready %>%
  filter(Area %in% key_areas) %>%
  mutate(
    Estimate = as.numeric(Estimate),
    Lower = as.numeric(Lower),
    Upper = as.numeric(Upper)
  ) %>%
  # Create factor with World first, then alphabetical
  mutate(Area = factor(Area, levels = c("World", sort(key_areas[key_areas != "World"]))))

# PNAS prefers to not label, so use convention A, B, C etc. 
area_labels <- c(
  "World" = "A",
  "Low-income economies" = "B",
  "Lower-middle-income economies" = "C"
)

# Severe 
p_severe <- plot_data_filtered %>%
  filter(!str_detect(ItemShort, "Moderate/Severe -")) %>%
  mutate(
    UrbanRuralType = case_when(
      str_detect(ItemShort, "Rural") ~ "Rural",
      str_detect(ItemShort, "Town") ~ "Town/Semi-dense",
      str_detect(ItemShort, "Urban") ~ "Urban"
    ),
    UrbanRuralType = factor(UrbanRuralType, levels = c("Rural", "Town/Semi-dense", "Urban"))
  ) %>%
  # Remove rows with missing confidence intervals
  filter(!is.na(Lower) & !is.na(Upper)) %>%
  ggplot(aes(x = Year, y = Estimate, color = UrbanRuralType, group = UrbanRuralType)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = UrbanRuralType), 
              alpha = 0.2, color = NA) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  facet_wrap(~Area, scales = "fixed", ncol = 3, labeller=labeller(Area = area_labels)) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  scale_color_manual(
    values = c("Rural" = "#228B22", "Town/Semi-dense" = "#8FBC8F", "Urban" = "#A0522D"),
    name = "DEGURBA"
  ) +
  scale_fill_manual(
    values = c("Rural" = "#228B22", "Town/Semi-dense" = "#8FBC8F", "Urban" = "#A0522D"),
    name = "DEGURBA"
  ) +
  labs(
    #title = "Severe Food Insecurity Across the Urban-Rural Continuum (2022-2024)",
    #subtitle = "Point estimates with 95% confidence intervals",
    x = "Year",
    y = "Prevalence (%)"#,
    #caption = "Source: FAOSTAT Suite of Food Security Indicators"
  ) +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(
    strip.text = element_text(size = 7, face = "bold", family = "sans"),
    strip.background = element_rect(fill = "grey90", color = "black"), 
    panel.border = element_rect(color = "black", fill = NA),           
    legend.position = "none",
    legend.text = element_text(size = 6, family = "sans"),
    legend.title = element_text(size = 7, family = "sans"),
    plot.title = element_text(size = 8, face = "bold", family = "sans"),
    plot.subtitle = element_text(size = 7, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6, family = "sans"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Moderate or severe 
p_moderate <- plot_data_filtered %>%
  filter(str_detect(ItemShort, "Moderate/Severe -")) %>%
  mutate(
    UrbanRuralType = case_when(
      str_detect(ItemShort, "Rural") ~ "Rural",
      str_detect(ItemShort, "Town") ~ "Town/Semi-dense",
      str_detect(ItemShort, "Urban") ~ "Urban"
    ),
    UrbanRuralType = factor(UrbanRuralType, levels = c("Rural", "Town/Semi-dense", "Urban"))
  ) %>%
  # Remove rows with missing confidence intervals
  filter(!is.na(Lower) & !is.na(Upper)) %>%
  ggplot(aes(x = Year, y = Estimate, color = UrbanRuralType, group = UrbanRuralType)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = UrbanRuralType), 
              alpha = 0.2, color = NA) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  facet_wrap(~Area, scales = "fixed", ncol = 3, labeller=labeller(Area = area_labels)) +
  scale_x_continuous(breaks = c(2022, 2023, 2024)) +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  scale_color_manual(
    values = c("Rural" = "#228B22", "Town/Semi-dense" = "#8FBC8F", "Urban" = "#A0522D"),
    name = "DEGURBA"
  ) +
  scale_fill_manual(
    values = c("Rural" = "#228B22", "Town/Semi-dense" = "#8FBC8F", "Urban" = "#A0522D"),
    name = "DEGURBA"
  ) +
  labs(
    #title = "Moderate or Severe Food Insecurity Across the Urban-Rural Continuum (2022-2024)",
    #subtitle = "Point estimates with 95% confidence intervals", 
    x = "Year",
    y = "Prevalence (%)"#,
    #caption = "Source: FAOSTAT Suite of Food Security Indicators"
  ) +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(
    strip.text = element_text(size = 7, face = "bold", family = "sans"),
    strip.background = element_rect(fill = "grey90", color = "black"), 
    panel.border = element_rect(color = "black", fill = NA),           
    legend.position = "none",
    legend.text = element_text(size = 6, family = "sans"),
    legend.title = element_text(size = 7, family = "sans"),
    plot.title = element_text(size = 8, face = "bold", family = "sans"),
    plot.subtitle = element_text(size = 7, family = "sans"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6, family = "sans"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p_severe)
print(p_moderate)

ggsave("graphics/output/Figure_Supplemental_1.1.jpeg", plot = p_moderate, 
       width = 180, height = 120, units="mm", dpi = 300, device = "jpeg")

ggsave("graphics/output/Figure_Supplemental_1.2.jpeg", plot = p_severe, 
       width = 180, height = 120, units="mm", dpi = 300, device = "jpeg")

