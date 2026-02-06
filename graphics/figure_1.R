# Urban/Rural Trends - DEGURBA vs World Bank Development Indicators 

library(readxl)
library(tidyverse)

# We include the GHS_DUC_GLOBE results in our repo for reviewers/editors: 
file_path <- "graphics/data/GHS_DUC_GLOBE_R2023A_V1_0.xlsx"

# Countries of study
target_countries <- c("AFG", "COD", "ETH", "PAK", "ZMB")

# Define the sheets to read (1975-2030 in 5-year intervals)
sheet_names <- paste("Country DEGURBA", 
                     seq(1975, 2030, 5), 
                     sep = " ")

# Function to extract data from a single sheet
extract_urban_share <- function(sheet_name, file_path) {
  year <- as.numeric(sub(".*DEGURBA ", "", sheet_name))
  data <- read_excel(file_path, sheet = sheet_name, col_types = "text")
  filtered_data <- data %>%
    filter(`GADM ISO` %in% target_countries) %>%
    select(country = `GADM ISO`, 
           urban_share = `Share of Urban Population`) %>%
    mutate(
      year = year,
      urban_share = as.numeric(urban_share)
    ) %>%
    select(country, year, urban_share)
  
  return(filtered_data)
}


urban_population_data <- map_dfr(sheet_names, 
                                 ~ extract_urban_share(.x, file_path))


urban_population_data <- urban_population_data %>%
  arrange(country, year)

urban_population_wide <- urban_population_data %>%
  pivot_wider(names_from = year, 
              values_from = urban_share, 
              names_prefix = "year_")

urban_population_wide

#read in WBI which is already formatted from download
wbi <- read_csv('graphics/data/ba2ec1b8-cdc9-4f61-8cea-fb939e0215e6_Data.csv')
wbi <- wbi[1:5,]

#Plot

ghs_long <- urban_population_wide %>%
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    values_to = "urban_share",
    names_prefix = "year_"
  ) %>%
  mutate(
    year = as.numeric(year),
    urban_share = urban_share * 100,  # Convert to percentage
    source = "GHS DEGURBA"
  ) %>%
  select(country, year, urban_share, source)

wbi_long <- wbi %>%
  select(country = `Country Code`, contains("[YR")) %>%
  pivot_longer(
    cols = -country,
    names_to = "year_raw",
    values_to = "urban_share"
  ) %>%
  # Extract year from column names like "1975 [YR1975]"
  mutate(
    year = as.numeric(stringr::str_extract(year_raw, "\\d{4}")),
    source = "World Bank",
    urban_share = as.numeric(urban_share)
  ) %>%
  filter(!is.na(urban_share)) %>%
  select(country, year, urban_share, source)

combined_data <- bind_rows(ghs_long, wbi_long) %>%
  filter(country %in% c("AFG", "COD", "ETH", "PAK", "ZMB")) %>%
  arrange(country, source, year)

# Create country name mapping for better labels
country_names <- c(
  "AFG" = "Afghanistan",
  "COD" = "Congo, Dem. Rep.",
  "ETH" = "Ethiopia", 
  "PAK" = "Pakistan",
  "ZMB" = "Zambia"
)

combined_data <- combined_data %>%
  mutate(country_name = country_names[country])

urban_trends_plot <- ggplot(combined_data, aes(x = year, y = urban_share, color = source)) +
  geom_line(aes(linetype = source), linewidth = 1) +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_wrap(~country_name, ncol = 2) +
  scale_color_manual(values = c("GHS DEGURBA" = "#2E86AB", "World Bank" = "#A23B72")) +
  scale_linetype_manual(values = c("GHS DEGURBA" = "solid", "World Bank" = "dashed")) +
  labs(
    #title = "Urban Population Share Trends: GHS DEGURBA vs World Bank Data",
    #subtitle = "Comparison of urbanization rates from two different data sources (1975-2030)",
    x = "Year",
    y = "Urban Population Share (%)",
    color = "Data Source",
    linetype = "Data Source"
  ) +
  theme_minimal(base_size = 6, base_family = "sans") +
  theme(
    plot.title = element_text(size = 8, face = "bold", family = "sans"),
    plot.subtitle = element_text(size = 7, family = "sans"),
    strip.text = element_text(size = 7, face = "bold", family = "sans"),
    legend.position = "none",
    legend.title = element_text(size = 7, family = "sans"),
    axis.text = element_text(size = 6, family = "sans"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(1975, 2030, 10)) +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1))

print(urban_trends_plot)

ggsave("graphics/output/Figure_1.jpeg", plot = urban_trends_plot, 
       width = 180, height = 170, units="mm", dpi = 300, device = "jpeg")