# 02_analysis.R

# Source the data preparation script
source("R/scripts/preamble.R")

# Load the sample with factor conversions
dat <- load_sample()

# Create the survey design
design <- svydesign(id = ~w_psu,
                 strata = ~w_strat,
                 weight = ~w_weight,
                 data = dat,
                 nest = TRUE)

# Order a table with the appropriate columns
foo <- function(x) { svytable (x, design=design, round=FALSE)}
foo(~n_age+f_has_florp)
foo(~f_region+f_has_florp)
foo(~f_urban+f_has_florp)
foo(~f_ever_preg+f_has_florp)
foo(~f_wealth+f_has_florp)
foo(~f_secondary+f_has_florp)

# Hello Pili, so the above should print out all the basic data you need. To get 
# totals just add the 2 numbers together, to get percentages divide the 'At 
# least one' column by  the total you just calculated. To get the tables etc. 
# working in R would require a bunch of learning R stuff that is hard, but this 
# prints out everything you need to put the latex-table together manually.
# Hello David, this is me trying
# Function to compute totals and percentages

compute_percentages <- function(var) {
  tab <- foo(var)  # Get weighted table
  totals <- rowSums(tab)  # Compute totals per category
  has_florp <- tab[, "At least one"]  # Extract HAS FLORP column
  percentages <- (totals / total_sample) * 100  # Compute percentages
  has_florp_percentage <- (has_florp / totals) * 100  # Compute % (HAS FLORP)
  
  # Convert to data frame for display
  results <- data.frame(
    Category = rownames(tab), 
    Total = round(totals), # Round total counts
    Has_Florp = round(has_florp), # Round HAS FLORP counts
    Percentage = percentages,
    Has_Florp_Percentage = has_florp_percentage)
  return(results)
}

# Compute total sample size
total_sample <- sum(foo(~n_age+f_has_florp)) 

# Compute percentages for all variables
age_results <- compute_percentages(~n_age+f_has_florp)
region_results <- compute_percentages(~f_region+f_has_florp)
urban_results <- compute_percentages(~f_urban+f_has_florp)
ever_preg_results <- compute_percentages(~f_ever_preg+f_has_florp)
wealth_results <- compute_percentages(~f_wealth+f_has_florp)
secondary_results <- compute_percentages(~f_secondary+f_has_florp)


# Print results
print(age_results)
print(region_results)
print(urban_results)
print(ever_preg_results)
print(wealth_results)
print(secondary_results)

# Get the weighted frequency of f_has_florp
florp_table <- foo(~f_has_florp)

# Calculate total N
total_n <- sum(florp_table)

# Extract N that have florp
n_with_florp <- florp_table["At least one"]

# Compute the percentage
pct_with_florp <- (n_with_florp / total_n) * 100

# Display
cat("Total N:", total_n, "\n")
cat("N with florp:", n_with_florp, "\n")
cat("Percent with florp:", pct_with_florp, "\n")

# Verify the sample size
total_teens <- sum(weights(design, na.rm = TRUE))
cat("Number of teens:", total_teens, "\n")

# Print the results
total_teens

# Verify the percentage of pregnant subjects
preg_teens <- svytable(~ f_ever_preg, design = design)
percent_preg <- (preg_teens / total_teens) * 100

preg_teens
cat("Percentage of pregant teens:", percent_preg, "%\n")

# CLEAN UP  --------------------------------------------------------------------
# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L

