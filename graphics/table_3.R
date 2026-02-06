#To Reviewers: you will not be able to run this, as our repo only allows for AFG to be fully reproduced from microdata due to needing approval from FAO and Gallup for additional countries. 
#However, we include this file to showcase code that led to Tables 3.1 and 3.2 in our manuscript. This code will also allow you to see how /combined_aggregate_results/summary_table.csv was generated
#, which we use as the base to producing all subsequent graphics for the manuscript. 

library(tidyverse)
library(plotly)
library(readr)
library(scales)
library(flextable)
library(officer)

setwd('~/Desktop/food_insecurity_v2/food_repo/') 

# Collect all of the relevant tables from the analysis, across countries of analysis

afg_people_estimates_totalpopulation <- read_csv("results/afg_people_estimates_totalpopulation.csv")
afg_degurba_population_by_agegrp <- read_csv("results/afg_degurba_population_by_agegrp.csv")


afg_people_estimates_totalpopulation$DEGURBA <- factor(
  afg_people_estimates_totalpopulation$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

afg_degurba_population_by_agegrp$DEGURBA <- factor(
  afg_degurba_population_by_agegrp$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

people_data_afg <- afg_people_estimates_totalpopulation %>%
  left_join(afg_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop), by = "DEGURBA") %>%
  mutate(country = "AFG")


pak_people_estimates_totalpopulation <- read_csv("results/pak_people_estimates_totalpopulation.csv")
pak_degurba_population_by_agegrp <- read_csv("results/pak_degurba_population_by_agegrp.csv")


pak_people_estimates_totalpopulation$DEGURBA <- factor(
  pak_people_estimates_totalpopulation$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
) 

pak_degurba_population_by_agegrp$DEGURBA <- factor(
  pak_degurba_population_by_agegrp$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

# people_data_pak <- pak_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop) %>%
#   left_join(pak_people_estimates_totalpopulation,by = "DEGURBA") %>%
#   mutate(severity = ifelse(is.na(severity), "Moderate + Severe", severity)) %>%  # Replace last NA in severity
#   bind_rows(tibble(
#     DEGURBA = "Rural areas",
#     gen_pop = pak_degurba_population_by_agegrp$gen_pop[pak_degurba_population_by_agegrp$DEGURBA == "Rural areas"][1],  # Copy gen_pop from the existing Rural areas row
#     severity = "Severe",
#     people_estimate = NA,
#     MOE_total = NA,
#     lower_bound = NA,
#     upper_bound = NA
#   )) %>%
#   mutate(country = "PAK")
# 
# people_data_pak$DEGURBA <- factor(
#   people_data_pak$DEGURBA,
#   levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
# )  

people_data_pak <- pak_people_estimates_totalpopulation %>%
  left_join(pak_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop), by = "DEGURBA") %>%
  mutate(country = "PAK")

eth_people_estimates_totalpopulation <- read_csv("results/eth_people_estimates_totalpopulation.csv")
eth_degurba_population_by_agegrp <- read_csv("results/eth_degurba_population_by_agegrp.csv")

eth_people_estimates_totalpopulation$DEGURBA <- factor(
  eth_people_estimates_totalpopulation$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

eth_degurba_population_by_agegrp$DEGURBA <- factor(
  eth_degurba_population_by_agegrp$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

people_data_eth <- eth_people_estimates_totalpopulation %>%
  left_join(eth_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop), by = "DEGURBA") %>%
  mutate(country = "ETH")


cod_people_estimates_totalpopulation <- read_csv("results/cod_people_estimates_totalpopulation.csv")
cod_degurba_population_by_agegrp <- read_csv("results/cod_degurba_population_by_agegrp.csv")

cod_people_estimates_totalpopulation$DEGURBA <- factor(
  cod_people_estimates_totalpopulation$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

cod_degurba_population_by_agegrp$DEGURBA <- factor(
  cod_degurba_population_by_agegrp$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

people_data_cod <- cod_people_estimates_totalpopulation %>%
  left_join(cod_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop), by = "DEGURBA") %>%
  mutate(country = "COD")


zmb_people_estimates_totalpopulation <- read_csv("results/zmb_people_estimates_totalpopulation.csv")
zmb_degurba_population_by_agegrp <- read_csv("results/zmb_degurba_population_by_agegrp.csv")


zmb_people_estimates_totalpopulation$DEGURBA <- factor(
  zmb_people_estimates_totalpopulation$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

zmb_degurba_population_by_agegrp$DEGURBA <- factor(
  zmb_degurba_population_by_agegrp$DEGURBA,
  levels = c("Rural areas", "Towns and semi-dense areas", "Cities")
)

people_data_zmb <- zmb_people_estimates_totalpopulation %>%
  left_join(zmb_degurba_population_by_agegrp %>% select(DEGURBA, gen_pop), by = "DEGURBA") %>%
  mutate(country = "ZMB")

#people_data_combined corresponds to /combined_aggregate_results/summary_table.csv in the repo. 
people_data_combined <- bind_rows(people_data_afg, people_data_pak, people_data_cod, people_data_zmb, people_data_eth)

#Export people_data_combined for all subsequent analysis/graphics 

write_csv(people_data_combined, "combined_aggregate_results/summary_table.csv")

# Tables formatted for .docx inclusion in manuscript 

pub_table <- people_data_combined %>%
  select(DEGURBA:country) %>%
  mutate(people_estimate = people_estimate/1000000) %>%
  mutate(MOE_total = MOE_total/1000000) %>%
  mutate(lower_bound = lower_bound/1000000) %>%
  mutate(upper_bound = upper_bound/1000000) %>%
  mutate(gen_pop = gen_pop/1000000)
  

# Helper function for formatting 
format_entry <- function(est, low, high, pop) {
  if (is.na(est)) {
    return("NA")
  } else {
    formatted <- sprintf("%.1f [%.1f – %.1f] (%.1fM)", est, low, high, pop)
    return(formatted)
  }
}

# Apply formatting and pivot for a given severity
create_flex_table <- function(data, severity_filter) {
  data %>%
    filter(severity == severity_filter) %>%
    mutate(display = mapply(format_entry, people_estimate, lower_bound, upper_bound, gen_pop)) %>%
    select(DEGURBA, country, display) %>%
    pivot_wider(names_from = country, values_from = display) %>%
    arrange(factor(DEGURBA, levels = c("Cities", "Towns and semi-dense areas", "Rural areas"))) %>%
    flextable() %>%
    set_caption(paste("People in Food Insecurity (", severity_filter, ")", sep = "")) %>%
    autofit()
}

# Create the two tables
table_mod_sev <- create_flex_table(pub_table, "Moderate + Severe")
table_sev <- create_flex_table(pub_table, "Severe")

# Display tables
table_mod_sev
table_sev

# Format and prepare the display column
formatted_df <- pub_table %>%
  mutate(
    ci = sprintf("%.1f [%.1f–%.1f]", people_estimate, lower_bound, upper_bound),
    gen_pop_display = paste0("Pop: ", format(round(gen_pop, 1), big.mark = ","), "M"),
    display = paste(ci, gen_pop_display, sep = "\n")
  ) %>%
  select(country, DEGURBA, severity, display) %>%
  pivot_wider(names_from = country, values_from = display) %>%
  arrange(factor(DEGURBA, levels = c("Cities", "Towns and semi-dense areas", "Rural areas")))

# Footnote text
note_text <- "Each cell shows the estimated number of food insecure people (in millions) followed by the 95% confidence interval. Below that, the general population (in millions) is listed for context according to our methodology."

# Create Moderate + Severe table with note
table_mod_sev <- formatted_df %>%
  filter(severity == "Moderate + Severe") %>%
  select(-severity) %>%
  flextable() %>%
  set_header_labels(DEGURBA = "Area Type") %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  add_footer_lines(values = note_text) %>%
  fontsize(i = 1, part = "footer", size = 9)

# Create Severe table with note
table_sev <- formatted_df %>%
  filter(severity == "Severe") %>%
  select(-severity) %>%
  flextable() %>%
  set_header_labels(DEGURBA = "Area Type") %>%
  align(align = "left", part = "all") %>%
  autofit() %>%
  add_footer_lines(values = note_text) %>%
  fontsize(i = 1, part = "footer", size = 9)

doc <- read_docx() %>%
  body_add_par("Table 1. Moderate + Severe Insecurity", style = "heading 2") %>%
  body_add_break() %>%
  body_add_flextable(table_mod_sev) %>%
  body_add_par("Table 2. Severe Insecurity", style = "heading 2") %>%
  body_add_break() %>%
  body_add_flextable(table_sev)

# Set the section to landscape (for Word)
doc <- body_end_section_landscape(doc)

print(doc, target = "graphics/output/food_insecurity_tables.docx")










