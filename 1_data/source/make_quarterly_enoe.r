# =============================================================================
# This script deletes unnecessary columns from the ENOE cohort data,
# keeps only a minimal set of variables, creates new variables 
# (like municipality code), and saves the cleaned dataset.
# =============================================================================

# =============================================================================
# Process ENOE quarterly files:
#  - Keep only essential columns
#  - Create new variables (municipality, weights, salaries, trim)
#  - Save processed file to output directory
# =============================================================================

# Load necessary libraries
library(tidyverse)
library(haven)

# Function to keep only required columns
delete_unnecessary_columns <- function(df, columns_to_keep) {
  df <- df %>% filter (r_def == 00 & c_res %in% c(1,3) & eda >= 15 & eda <= 98)
  df <- df[, intersect(columns_to_keep, names(df))]
  return(df)
}

# Function to create new variables
create_new_variables <- function(df, year, quarter) {
  # assign weights
  if ("fac" %in% names(df)) {
    df <- df %>% mutate(weights = fac)
  } else if ("fac_tri" %in% names(df)) {
    df <- df %>% mutate(weights = fac_tri)
  } else {
    stop("Neither fac nor fac_tri found in dataset")
  }
  
  # add derived variables
  df <- df %>%
    mutate(
      municipality = paste0(ent, "0", sprintf("%02s", mun)),
      hrly_salary = as.numeric(ing_x_hrs),
      monthly_salary = as.numeric(ingocup),
      trim = paste0(year, "-", quarter)
    )
  
  return(df)
}

main <- function(){
  download_path <- "/Users/wernerd/Desktop/Daniel Werner/Quarterly/"
  
  # Parse arguments
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("Please provide year and quarter as command line arguments")
  }
  year <- as.integer(args[1])
  quarter <- args[2]
  
  if (year == 2020 && quarter == "T2") {
    message("Skipping 2020 T2 due to missing data.")
    return(invisible(NULL))
  }
  
  cat(sprintf("Processing %d %s...\n", year, quarter))
  
  cohort_file <- sprintf("%d_%s.dta", year, quarter)
  
  tryCatch({
    # Read data
    cohort <- read_dta(file.path(download_path, cohort_file))
    columns_to_keep <- c("ent", "mun", "clase1", "clase2",
                         "fac", "fac_tri", "ing_x_hrs", "ingocup", "sex",
                         "pos_ocu", "seg_soc", "rama", "c_ocu11c", "tpg_p8a",
                         "p14apoyos", "scian", "emp_ppal", "tue_ppal", "eda")
    
    cohort <- delete_unnecessary_columns(cohort, columns_to_keep)
    cohort <- create_new_variables(cohort, year, quarter)
    
    # Save
    write_dta(cohort, paste0("/Users/wernerd/Desktop/Voting Shares Raw/vot share temp/",
                             sprintf("%d_%s.dta", year, quarter)))
    invisible(gc())
  }, error = function(e) {
    message(sprintf("Failed for %d %s: %s", year, quarter, e$message))
  })
}

# Execute
main()