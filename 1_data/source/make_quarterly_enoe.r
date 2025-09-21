# =============================================================================
# This script puts the five parts of the ENOE together for each quarter
# =============================================================================
suppressPackageStartupMessages({
  suppressMessages({
    library(tidyverse)
    library(haven)
    library(data.table)
    library(dplyr)
  })
})

options(warn = -1) # Suppress warnings
# =============================================================================
# ---- Function to fuse components into a single ENOE (YYYY-T#) ----
    # For more information on how to merge these datasets, refer to the document at the following link:
    # https://pueaa.unam.mx/uploads/materials/Escoto-A.-2021.pdf?v=1661541626 

fuse_quarter <- function(sdem, coe1, coe2, hog, viv){
    #Creating IDs for dwelling, home, and person (the three levels along which the merge occurs)
    id_viv <- c("cd_a", "ent", "con", "v_sel") 
    id_hog <- c("cd_a", "ent", "con", "v_sel","n_hog", "h_mud") 
    id_persona <- c("cd_a", "ent", "con", "v_sel","n_hog", "h_mud", "n_ren")
    
    #Fusing Survey of Occupation and Employment 1 and 2
    coe <- merge(coe1,coe2, by = id_persona, all = TRUE)
    rm(coe1,coe2)
    invisible(gc())
    #Rename a few variables so as to not confuse with socio-demographic variable names later on 
    coe <- coe %>% rename(p1coe = p1, p3coe = p3)
    #Eliminate repeated variables from merge
    coe <- coe %>%  select(-ends_with(".y")) %>% 
        rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$", "", .))) 

    #Fusing Info of Occupaion and Employment Survey (COE) w/ Socio-Demographic Survey (SDEM)
    sdemcoe <- merge(sdem, coe, by = id_persona, all = TRUE)
    rm(sdem, coe)
    invisible(gc())
    #Eliminate repeated variables from merge 
    sdemcoe <- sdemcoe %>% select(-ends_with(".y")) %>% 
        rename_at(.vars = vars(ends_with(".x")),  
            .funs = funs(sub("[.]x$", "", .))) 

    #Fusing Home Survey (HOG) and Dwelling Survey (VIV)
    vivhog <- merge(viv, hog, by = id_viv, all = TRUE)
    rm(viv, hog)
    invisible(gc())
    #Eliminate repeated variables from merge 
    vivhog <- vivhog %>%  select(-ends_with(".y")) %>% 
        rename_at(.vars = vars(ends_with(".x")),  
            .funs = funs(sub("[.]x$", "", .))) 

    #Fusing to make final database (all together)
    complete <- merge(vivhog, sdemcoe, by = id_hog, all = TRUE)
    rm(vivhog, sdemcoe)
    invisible(gc())
    #Eliminate repeated variables from merge 
    complete <- complete %>%  select(-ends_with(".y")) %>% 
        rename_at(.vars = vars(ends_with(".x")),  
            .funs = funs(sub("[.]x$", "", .))) 

    #Keep only valid survey entries (drop incomplete interviews and those with an absent condition of residence)
    complete <- complete %>% filter(r_def == 0) %>% filter(c_res != 2)

    return(complete)
}

# ---- Function to process a single year/quarter ----
process_quarter <- function(year, quarter) {
  #Set the download path to the local copy of dropbox folder
  dropbox_path <- "/Users/wernerd/Desktop/Daniel Werner"
  download_path <- file.path(dropbox_path, "ENOE", as.character(year), quarter)
  sdem <- read_dta(file.path(download_path, "sdem.dta")) %>%
    rename_with(tolower)
  coe1 <- read_dta(file.path(download_path, "coe1.dta")) %>%
    rename_with(tolower)
  coe2 <- read_dta(file.path(download_path, "coe2.dta")) %>%
    rename_with(tolower)
  hog  <- read_dta(file.path(download_path, "hog.dta")) %>%
    rename_with(tolower)
  viv  <- read_dta(file.path(download_path, "viv.dta")) %>%
    rename_with(tolower)
  
  # Fuse datasets
  year_quarter <- fuse_quarter(sdem, coe1, coe2, hog, viv)
  cat("Fusing Completed Succesfully for", year, quarter, "\n")
  rm(sdem, coe1, coe2, hog, viv)
  invisible(gc())
  
  # Save output to output folder --> Back into the repository
  save_path <- file.path("../output",
                         paste0(as.character(year), "_", quarter, ".dta"))
  write_dta(year_quarter, save_path)
  cat(paste0(as.character(year), "_", quarter, ".dta"),
      "has been saved to output folder", "\n")
  rm(year_quarter)
  invisible(gc())
}

# ---- Main loop over years and quarters ----
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 2) {
    stop("Please provide year and quarter as command line arguments")
  }
  
  year <- as.integer(args[1])
  quarter <- args[2]
  if (year == 2020 && quarter == "T2") {
    message("Skipping 2020 T2 due to missing data.")
  } else {
    cat(sprintf("Processing %d %s...\n", year, quarter))
    tryCatch({
      process_quarter(year, quarter)
      invisible(gc())
    }, error = function(e) {
      message(sprintf("Failed for %d %s: %s", year, quarter, e$message))
    })
  }
}

# ---- Execute ----
main()
cat("NEXT -------->.\n")
