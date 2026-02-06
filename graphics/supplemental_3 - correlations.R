# Spearman Correlation Analysis: Population vs Prevalence

data <- read.csv("combined_aggregate_results/summary_table.csv", stringsAsFactors = FALSE)


# Function to run and create tables we can compare 
analyze_spearman_by_severity <- function(severity_level) {
  cat("\n", rep("=", 80), "\n")
  cat("SPEARMAN CORRELATION ANALYSIS FOR:", severity_level, "\n")
  cat(rep("=", 80), "\n")
  
  severity_data <- data[data$severity == severity_level, ]
  
  severity_data$prevalence_rate <- (severity_data$people_estimate / severity_data$gen_pop) * 100
  
  countries <- unique(severity_data$country)
  
  results_summary <- data.frame()
  
  for (country in countries) {
    country_data <- severity_data[severity_data$country == country, ]
    
    if (nrow(country_data) >= 2) {  
      
      cat("\n--- ", country, " ---\n")
      
      # Display the data
      display_data <- data.frame(
        Category = country_data$DEGURBA,
        Population_M = round(country_data$gen_pop / 1000000, 1),
        Food_Insecure_M = round(country_data$people_estimate / 1000000, 1),
        Prevalence_Rate = round(country_data$prevalence_rate, 1)
      )
      print(display_data)
      
      # Calculate Spearman correlation
      if (nrow(country_data) > 2) {
        spearman_result <- cor.test(country_data$gen_pop, country_data$prevalence_rate, 
                                    method = "spearman", exact = FALSE)
      } else {
        # For n=2, use exact test
        spearman_result <- cor.test(country_data$gen_pop, country_data$prevalence_rate, 
                                    method = "spearman")
      }
      
      cat("Spearman correlation =", round(spearman_result$estimate, 3), "\n")
      if (!is.na(spearman_result$p.value)) {
        cat("p-value =", format(spearman_result$p.value, scientific = TRUE, digits = 3), "\n")
      } else {
        cat("p-value = Cannot compute with n=2\n")
      }
      
      # Interpretation, although very low sample size, so these are just descriptive for collaborator interpretation 
      r_value <- spearman_result$estimate
      if (r_value > 0.7) {
        interpretation <- "Strong positive: Larger population areas have much higher prevalence"
      } else if (r_value > 0.3) {
        interpretation <- "Moderate positive: Larger areas tend to have higher prevalence"
      } else if (r_value > -0.3) {
        interpretation <- "Weak/No relationship: Population size unrelated to prevalence"
      } else if (r_value > -0.7) {
        interpretation <- "Moderate negative: Smaller areas tend to have higher prevalence"
      } else {
        interpretation <- "Strong negative: Smaller population areas have much higher prevalence"
      }
      
      cat("Interpretation:", interpretation, "\n")
      
      # Store results
      result_row <- data.frame(
        Country = country,
        Categories = nrow(country_data),
        Total_Population_M = round(sum(country_data$gen_pop) / 1000000, 1),
        Total_Food_Insecure_M = round(sum(country_data$people_estimate) / 1000000, 1),
        Spearman_r = round(r_value, 3),
        p_value = ifelse(is.na(spearman_result$p.value), NA, spearman_result$p.value),
        p_formatted = ifelse(is.na(spearman_result$p.value), "Cannot compute", 
                             format(spearman_result$p.value, scientific = TRUE, digits = 3)),
        Pattern = ifelse(r_value > 0.3, "Positive", 
                         ifelse(r_value < -0.3, "Negative", "Weak/None")),
        Interpretation = interpretation,
        stringsAsFactors = FALSE
      )
      
      results_summary <- rbind(results_summary, result_row)
    }
  }
  
  # Summary table
  cat("\n", rep("-", 80), "\n")
  cat("SUMMARY TABLE FOR", severity_level, "\n")
  cat(rep("-", 80), "\n")
  
  summary_table <- data.frame(
    Country = results_summary$Country,
    Categories = results_summary$Categories,
    Total_Pop_M = results_summary$Total_Population_M,
    Spearman_r = results_summary$Spearman_r,
    p_value = results_summary$p_formatted,
    Pattern = results_summary$Pattern
  )
  
  print(summary_table)
  
  return(results_summary)
}

# Run analysis for both severity levels
moderate_severe_results <- analyze_spearman_by_severity("Moderate + Severe")
severe_results <- analyze_spearman_by_severity("Severe")

# Compare results between severity 

if (nrow(moderate_severe_results) > 0 && nrow(severe_results) > 0) {
  comparison <- merge(moderate_severe_results[, c("Country", "Spearman_r", "Pattern")], 
                      severe_results[, c("Country", "Spearman_r", "Pattern")], 
                      by = "Country", suffixes = c("_ModSev", "_Sev"))
  
  comparison$Pattern_Change <- ifelse(comparison$Pattern_ModSev == comparison$Pattern_Sev, 
                                      "Same", "Different")
  
  print(comparison)
  
  cat("\nKey Insights:\n")
  cat("- Countries with consistent patterns across severity levels:", 
      paste(comparison$Country[comparison$Pattern_Change == "Same"], collapse = ", "), "\n")
  if (any(comparison$Pattern_Change == "Different")) {
    cat("- Countries with different patterns by severity:", 
        paste(comparison$Country[comparison$Pattern_Change == "Different"], collapse = ", "), "\n")
  }
}
