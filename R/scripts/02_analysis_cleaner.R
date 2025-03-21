# Load required scripts and data
source("R/scripts/preamble.R")

# Load the sample data with factor conversions
dat <- load_sample()

# Define survey design
design <- svydesign(
  id = ~w_psu,
  strata = ~w_strat,
  weight = ~w_weight,
  data = dat,
  nest = TRUE
)

# Function to generate weighted tables
weighted_table <- function(var) {
  svytable(var, design = design, round = FALSE)
}

# Compute total sample size
total_sample <- sum(weighted_table(~n_age + f_has_florp))

# Function to compute totals and percentages
compute_percentages <- function(var, var_name) {
  tab <- weighted_table(var)
  
  if (is.null(dim(tab))) {  # Handle single-category cases
    return(data.frame(
      Factor = var_name, n = sum(tab), `%` = NA, 
      `n (has florp)` = NA, `% (has florp)` = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  totals <- rowSums(tab)
  
  # Ensure "At least one" column exists; if not, set to 0
  has_florp <- if ("At least one" %in% colnames(tab)) {
    tab[, "At least one"]
  } else {
    rep(0, length(totals))  # Fill with 0s if missing
  }
  
  percentages <- (totals / total_sample) * 100
  has_florp_percentage <- (has_florp / totals) * 100
  
  results <- data.frame(
    Factor = var_name,  # Set factor name instead of row names
    n = round(totals),
    `%` = round(percentages, 2),
    `n (has florp)` = round(has_florp),
    `% (has florp)` = round(has_florp_percentage, 2),
    stringsAsFactors = FALSE
  )
  
  return(results)
}

# Compute percentages for all key variables
results_list <- list(
  compute_percentages(~n_age + f_has_florp, "Age"),
  compute_percentages(~f_region + f_has_florp, "Region"),
  compute_percentages(~f_urban + f_has_florp, "Urban/Rural"),
  compute_percentages(~f_ever_preg + f_has_florp, "Ever Pregnant"),
  compute_percentages(~f_wealth + f_has_florp, "Wealth"),
  compute_percentages(~f_secondary + f_has_florp, "Secondary Education")
)

# Debugging Step: Check column names before combining
lapply(results_list, colnames)

# Ensure all results have the same column names before combining
final_table <- do.call(rbind, results_list)

# Print table in a readable format
kable(final_table, format = "markdown")  # Change to "latex" for LaTeX output
# CLEAN UP  --------------------------------------------------------------------
# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # ctrl+L
