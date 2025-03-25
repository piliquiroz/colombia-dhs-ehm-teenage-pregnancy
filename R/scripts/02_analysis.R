# 02_analysis.R

# Source the preamble script
source("R/scripts/preamble.R")

# Load the sample
dat <- load_sample()

# Adding sampling weights-------------------------------------------------------

# Create the survey design
design <- svydesign(
  id = ~w_psu,
  strata = ~w_strat,
  weight = ~w_weight,
  data = dat,
  nest = TRUE
)

# Function to generate weighted table
weighted_table <- function(x) { 
  svytable(x, design = design, round = FALSE)
}

# Function to compute totals and percentages
compute_percentages <- function(var) {
  tab <- weighted_table(var)  # Get weighted table
  totals <- rowSums(tab)  # Compute totals per category
  has_ehm <- tab[, "At least one"]  # Extract HAS ehm column
  percentages <- (totals / total_sample) * 100  # Compute percentages
  has_ehm_percentage <- (has_ehm / totals) * 100  # Compute % (HAS ehm)
  
  # Convert to data frame for display
  results <- data.frame(
    Category = rownames(tab), 
    Total = round(totals), # Round total counts
    Has_ehm = round(has_ehm), # Round HAS ehm counts
    Percentage = percentages,
    Has_ehm_Percentage = has_ehm_percentage)
  return(results)
}

# Compute total sample size
total_sample <- sum(weighted_table(~n_age+f_has_ehm)) 

# Compute percentages for all variables
variable_results <- list(
  Age = compute_percentages(~n_age+f_has_ehm),
  Region = compute_percentages(~f_region+f_has_ehm),
  Urban = compute_percentages(~f_urban+f_has_ehm),
  Pregnancy = compute_percentages(~f_ever_preg+f_has_ehm),
  Wealth = compute_percentages(~f_wealth+f_has_ehm),
  Secondary = compute_percentages(~f_secondary+f_has_ehm)
)

# Print results with more informative output
for (name in names(variable_results)) {
  cat(paste(name, "Results:\n"))
  print(variable_results[[name]])
  cat("\n")
}

# EHM Frequency Analysis -------------------------------------------------------
# Get the weighted frequency of f_has_ehm
ehm_table <- weighted_table(~f_has_ehm)

# Calculate total N
total_n <- sum(ehm_table)

# Extract N that have ehm
n_with_ehm <- ehm_table["At least one"]

# Compute the percentage
pct_with_ehm <- (n_with_ehm / total_n) * 100

# Display
cat("Total N:", total_n, "\n")
cat("N with ehm:", n_with_ehm, "\n")
cat("Percent with ehm:", pct_with_ehm, "\n")

# Verify the sample size
total_teens <- sum(weights(design, na.rm = TRUE))
cat("Number of teens:", total_teens, "\n")

# Verify the percentage of pregnant teens
preg_teens <- svytable(~ f_ever_preg, design = design)
percent_preg <- (preg_teens / total_teens) * 100

# Round the counts while keeping the precise percentages
preg_teens_rounded <- round(preg_teens)

preg_teens_rounded
cat("Percentage of pregant teens:", percent_preg, "%\n")

# Relationship between teen pregnancy and having an EHM wealth and schooling----
# Function to process survey cross-tabulation with rounded counts and precise prevalence
process_cross_tab <- function(survey_table) {
  # Convert to data frame
  cross_tab_df <- as.data.frame.matrix(survey_table)
  
  # Round the counts
  cross_tab_rounded <- round(cross_tab_df)
  
  # Calculate prevalence using precise (unrounded) values
  row_totals <- rowSums(cross_tab_df)
  prevalence <- (cross_tab_df$`At least once` / row_totals) * 100
  
  # Create final dataframe with rounded counts and precise prevalence
  result_df <- cross_tab_rounded
  result_df$Prevalence <- prevalence
  
  return(result_df)
}

# Pregnancy across having an EHM
preg_ehm <- svytable(~ f_has_ehm + f_ever_preg, design = design)
preg_ehm_df <- process_cross_tab(preg_ehm)
preg_ehm_df

# Pregnancy across wealth
preg_wealth <- svytable(~ f_wealth + f_ever_preg, design = design)
preg_wealth_df <- process_cross_tab(preg_wealth)
preg_wealth_df

# Pregnancy across schooling
preg_school_prog <- svytable(~ f_secondary + f_ever_preg, design = design)
preg_school_prog_df <- process_cross_tab(preg_school_prog)
preg_school_prog_df

# Chi sq tests

# Pregnancy and having an EHM
svychisq(~f_ever_preg + f_has_ehm, design, statistic="Chisq") # Chi² test  

# Pregnancy and Wealth
svychisq(~f_ever_preg + f_wealth, design, statistic="Chisq") # Chi² test

# Pregnancy and Schooling
svychisq(~f_ever_preg + f_secondary, design, statistic="Chisq") # Chi² test

# Characteristics of the emigrants----------------------------------------------

# Distribution of Emigrants by Gender -------------------------------------------

# Function to process gender distribution with weighted calculations
process_gender_distribution <- function(data) {
  # Gather data to long format, multiplying by weights
  gen_dat <- data %>%
    tidyr::gather(key = "gender", value = "people", 
                  n_ehm_man, n_ehm_wom, n_ehm_trans) %>%
    mutate(weighted_people = people * w_weight)
  
  # Summarize by gender with weighted counts
  gender_counts <- gen_dat %>%
    group_by(gender) %>%
    summarise(w_count = sum(weighted_people))
  
  # Calculate total weighted count
  total_gender <- sum(gender_counts$w_count)
  
  # Calculate percentages (precise calculation)
  gender_counts$percentage <- gender_counts$w_count / total_gender * 100
  
  # Prepare final dataframe
  result <- data.frame(
    Characteristic = recode(gender_counts$gender,
                            "n_ehm_man" = "Man",
                            "n_ehm_trans" = "Transgender",
                            "n_ehm_wom" = "Woman"),
    Count = round(gender_counts$w_count),  # Round counts
    Percentage = gender_counts$percentage  # Keep precise percentages
  )
  
  # Additional metadata
  attr(result, "total_count") <- total_gender
  
  return(result)
}

# Process gender distribution
gender_counts <- process_gender_distribution(dat)

# Print results
print(gender_counts)

# Print total count
cat("Total Weighted Count of Emigrants:", 
    round(attr(gender_counts, "total_count")), "\n")

# Destination Country of Migrants -------------------------------------------

# Function to process country distribution with weighted calculations
process_country_distribution <- function(data) {
  # Gather data to long format, multiplying by weights
  long_dat <- data %>%
    tidyr::gather(key = "country", value = "people", 
                  n_venezuela, n_us, n_spain, n_ecuador, n_panama, n_canada,
                  n_chile, n_mexico, n_brazil, n_argentina, n_france, n_italy,
                  n_uk, n_australia, n_peru, n_germany, n_other_country) %>%
    mutate(weighted_people = people * w_weight)
  
  # Summarize by country with weighted counts
  country_counts <- long_dat %>%
    group_by(country) %>%
    summarise(weighted_count = sum(weighted_people))
  
  # Calculate total weighted count
  total_migrants <- sum(country_counts$weighted_count)
  
  # Calculate percentages (precise calculation)
  country_counts$percentage <- country_counts$weighted_count / total_migrants * 100
  
  # Prepare final dataframe
  result <- data.frame(
    Country = recode(country_counts$country,
                     "n_venezuela" = "Venezuela",
                     "n_us" = "United States",
                     "n_spain" = "Spain",
                     "n_ecuador" = "Ecuador",
                     "n_panama" = "Panama",
                     "n_canada" = "Canada",
                     "n_chile" = "Chile",
                     "n_mexico" = "Mexico",
                     "n_brazil" = "Brazil",
                     "n_argentina" = "Argentina",
                     "n_france" = "France",
                     "n_italy" = "Italy",
                     "n_uk" = "United Kingdom",
                     "n_australia" = "Australia",
                     "n_peru" = "Peru",
                     "n_germany" = "Germany",
                     "n_other_country" = "Other Countries"),
    Count = country_counts$weighted_count,  # Not round counts
    Percentage = country_counts$percentage  # Keep precise percentages
  ) %>%
    arrange(desc(Count))  # Sort in descending order
  
  # Additional metadata
  attr(result, "total_migrants") <- total_migrants
  
  return(result)
}

# Process country distribution
country_counts <- process_country_distribution(dat)

# Print results
print(country_counts)

# Print total count of migrants
cat("Total Weighted Count of Migrants:", 
    round(attr(country_counts, "total_migrants")), "\n")

# Destination Regions Analysis -------------------------------------------

# Function to process destination regions with weighted calculations
process_destination_regions <- function(data) {
  # Define country and super-region columns
  srl <- c("n_latam", "n_northamerica", "n_europe", "n_australia_other")
  
  # Function to apply weighting
  reweight <- function(x) { x * data$w_weight }
  
  # Compute weighted counts
  tc <- data %>%
    reframe(across(srl, reweight)) %>%  # Apply weights
    summarise(across(everything(), sum)) %>%  # Get totals
    pivot_longer(cols = everything(), names_to = "Characteristic", 
                 values_to = "Count")
  
  # Compute total count for percentage calculation
  total_count <- sum(tc$Count)
  
  # Prepare final dataframe
  result <- data.frame(
    Characteristic = recode(tc$Characteristic,
                            "n_latam" = "Latin America",
                            "n_northamerica" = "North America",
                            "n_europe" = "Europe",
                            "n_australia_other" = "Australia and Other"),
    Count = round(tc$Count),  # Round counts
    Percentage = (tc$Count / total_count) * 100  # Precise percentages
  ) %>%
    arrange(desc(Count))  # Sort in descending order
  
  # Additional metadata
  attr(result, "total_count") <- total_count
  
  return(result)
}

# Process destination regions
destination_regions <- process_destination_regions(dat)

# Print results
print(destination_regions)

# Print total count
cat("Total Weighted Count of Migrants:", 
    round(attr(destination_regions, "total_count")), "\n")

# Destination of Migrants by Wealth -------------------------------------------

# Function to process destination regions by wealth group
process_destination_by_wealth <- function(data) {
  # Define super-region columns with their labels
  srl <- c(
    "n_latam" = "Latin America", 
    "n_northamerica" = "North America", 
    "n_europe" = "Europe", 
    "n_australia_other" = "Australia and Other"
  )
  
  # Function to process a specific wealth group
  process_wealth_group <- function(wealth_condition) {
    # Subset data based on wealth condition
    subset_data <- data[wealth_condition,]
    
    # Reweight and sum columns
    result <- subset_data %>%
      reframe(across(names(srl), function(d) {d * subset_data$w_weight})) %>%
      summarise(across(everything(), sum)) %>%
      pivot_longer(cols = everything(), names_to = "region", values_to = "Count")
    
    # Calculate percentages
    total_count <- sum(result$Count)
    
    # Prepare final dataframe
    processed_result <- data.frame(
      Characteristic = srl[result$region],
      Count = round(result$Count),  # Round counts
      Percentage = (result$Count / total_count) * 100  # Precise percentages
    ) %>%
      arrange(desc(Count))
    
    # Add metadata
    attr(processed_result, "total_count") <- total_count
    
    return(processed_result)
  }
  
  # Process poor and non-poor groups
  poor_condition <- data$f_wealth %in% c("Poorer", "Poorest")
  
  list(
    Poor = process_wealth_group(poor_condition),
    NonPoor = process_wealth_group(!poor_condition)
  )
}

# Process destination regions by wealth
destination_by_wealth <- process_destination_by_wealth(dat)

# Print results
cat("Destination Regions for Poor Households:\n")
print(destination_by_wealth$Poor)
cat("\nTotal Weighted Count (Poor):", 
    round(attr(destination_by_wealth$Poor, "total_count")), "\n\n")

cat("Destination Regions for Non-Poor Households:\n")
print(destination_by_wealth$NonPoor)
cat("\nTotal Weighted Count (Non-Poor):", 
    round(attr(destination_by_wealth$NonPoor, "total_count")), "\n")

#Plots--------------------------------------------------------------------------
# Bar chart of destination countries
plot_destination_countries <- function(country_data) {
  # Ensure the data is in the format we expect
  plot_data <- country_data %>%
    mutate(Country = factor(Country, levels = Country[order(-Count)]))
  
  ggplot(plot_data, aes(x = Country, y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(
      title = "Destination Countries of Emigrants",
      x = "Country", 
      y = "Weighted Count"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      aspect.ratio = 0.5
    ) +
    scale_y_continuous(labels = scales::comma)
}

# Process country distribution
country_counts <- process_country_distribution(dat)

# Create the plot using the precise country_counts
destination_plot <- plot_destination_countries(country_counts)

# Display the plot
print(destination_plot)

# Destination Region by wealth
# Function to create pie chart for destination regions by wealth
plot_destination_regions_by_wealth <- function(wealth_data, title) {
  # Prepare data for plotting
  plot_data <- wealth_data %>%
    mutate(
      Characteristic = factor(
        Characteristic, 
        levels = Characteristic[order(-Count)]
      )
    )
  
  ggplot(plot_data, aes(x = "", y = Count, fill = Characteristic)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(
      title = title,
      fill = "Target Region", 
      x = NULL, 
      y = NULL
    ) +
    scale_fill_brewer(palette = "Set3") +  # Or choose another color palette
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    ) +
    geom_text(
      aes(label = paste0(round(Percentage, 1), "%")),
      position = position_stack(vjust = 0.5)
    )
}

# Create plots for poor and non-poor households
poor_regions_plot <- plot_destination_regions_by_wealth(
  destination_by_wealth$Poor, 
  "Destination Regions for Lower-Income Households"
)

non_poor_regions_plot <- plot_destination_regions_by_wealth(
  destination_by_wealth$NonPoor, 
  "Destination Regions for Middle and High-Income Households"
)

# Display plots
print(poor_regions_plot)
print(non_poor_regions_plot)

# # CLEAN UP  --------------------------------------------------------------------
# # Clear environment
# rm(list = ls())
# 
# # Clear packages
# detach("package:datasets", unload = TRUE)  # For base
# 
# # Clear plots
# graphics.off()  # Clears plots, closes all graphics devices
# 
# # Clear console
# cat("\014")  # ctrl+L

