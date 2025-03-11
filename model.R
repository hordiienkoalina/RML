#############################################
# 1. Install and Load Packages and Libraries
#############################################
packages <- c("dplyr", "tidyverse", "readr", "readxl", "purrr", "tidyr", "stringr")

installed <- installed.packages()[, "Package"]
for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(purrr)  
library(tidyr)
library(stringr)

#################################
# 2. Load and Process MHCLD Data for Multiple States
#################################

# Define the list of MHCLD .rdata files (one for each year)
mhcld_files <- list(
  "data/MHCLD/mhcld_puf_2013.rdata",
  "data/MHCLD/mhcld_puf_2014.rdata",
  "data/MHCLD/mhcld_puf_2015.rdata",
  "data/MHCLD/MHCLD_PUF_2016.rdata",
  "data/MHCLD/MHCLD_PUF_2017.rdata"
)

# Define a named list of states and their corresponding STATEFIP codes
state_ids <- list(
  oregon        = 41,
  california    = 6,
  colorado      = 8,
  maine         = 23,
  massachusetts = 25,
  nevada        = 32,
  washington    = 53
)

# Modified function: load a given file for a specific year and state
load_mhcld <- function(file_path, year, state_id) {
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) {
    stop("The loaded object is not a data frame")
  }
  # Filter by the given STATEFIP and add the year column
  df <- df %>%
    filter(STATEFIP == state_id) %>%
    mutate(year = year)
  return(df)
}

# For each state, bind rows from all years and add a Legalized flag 
mhcld_list <- list()  # to store data frames for each state
for (state in names(state_ids)) {
  state_id <- state_ids[[state]]
  # Combine the data for each year
  state_df <- bind_rows(
    load_mhcld(mhcld_files[[1]], 2013, state_id),
    load_mhcld(mhcld_files[[2]], 2014, state_id),
    load_mhcld(mhcld_files[[3]], 2015, state_id),
    load_mhcld(mhcld_files[[4]], 2016, state_id),
    load_mhcld(mhcld_files[[5]], 2017, state_id)
  ) %>%
    # Add a Legalized variable: 1 for years >= 2015, 0 otherwise
    mutate(Legalized = ifelse(year >= 2015, 1, 0))
  
  # Store the resulting data frame in the list with the state name as key
  mhcld_list[[state]] <- state_df
}

# Assign each state’s data frame to its own object
mhcld_oregon       <- mhcld_list[["oregon"]]
mhcld_california   <- mhcld_list[["california"]]
mhcld_colorado     <- mhcld_list[["colorado"]]
mhcld_maine        <- mhcld_list[["maine"]]
mhcld_massachusetts<- mhcld_list[["massachusetts"]]
mhcld_nevada       <- mhcld_list[["nevada"]]
mhcld_washington   <- mhcld_list[["washington"]]

#################################
# 3. Load and Process ACS Data
#################################

# 1) Identify all ACS CSV files
acs_files <- list.files(path = "data/ACS", pattern = "ACS-\\d{4}\\.csv", full.names = TRUE)

# 2) Create a function that returns two data frames:
#    (a) Health insurance coverage
#    (b) Household income (median & mean)
load_and_process_acs <- function(file) {
  # Extract the year from the filename (e.g., "ACS-2013.csv")
  year <- as.numeric(gsub(".*ACS-(\\d{4}).csv", "\\1", basename(file)))
  
  # Read the CSV file, treating all columns as character to handle formatting issues like commas
  acs_data <- read_csv(file, col_types = cols(.default = "c"))
  
  # Select the rows of interest by index:
  # Row 103: "With health insurance coverage"
  # Row 68: "Median household income (dollars)"
  # Row 69: "Mean household income (dollars)"
  selected_rows <- acs_data[c(103, 68, 69), ]
  
  # Identify columns with percent and estimate values
  percent_columns   <- names(acs_data)[grepl("!!Percent$", names(acs_data))]
  estimate_columns  <- names(acs_data)[grepl("!!Estimate$", names(acs_data))]
  
  # (a) Process Health Insurance Coverage (row 103)
  health_insurance <- selected_rows[1, c("Label (Grouping)", percent_columns)] %>%
    mutate(
      year = year,
      # Convert the percent columns from "XX.X%" to numeric proportion
      across(ends_with("!!Percent"), ~ as.numeric(gsub("%", "", .)) / 100)
    ) %>%
    rename(Metric = `Label (Grouping)`)
  
  # (b) Process Household Income (rows 68 & 69)
  income_data <- selected_rows[2:3, c("Label (Grouping)", estimate_columns)] %>%
    mutate(
      year = year,
      # Remove commas or any non-numeric characters, then convert to numeric
      across(ends_with("!!Estimate"), ~ as.numeric(gsub("[^0-9.]", "", .)))
    ) %>%
    rename(Metric = `Label (Grouping)`)
  
  # Return a list with both data frames
  list(
    insurance = health_insurance,
    income    = income_data
  )
}

# 3) Apply the function to each ACS file
acs_files <- lapply(acs_files, load_and_process_acs)

# 4) Combine all insurance rows into one data frame and all income rows into another
acs_insurance <- bind_rows(lapply(acs_files, `[[`, "insurance"))
acs_income    <- bind_rows(lapply(acs_files, `[[`, "income"))

# 5) Clean up column names: remove "!!Percent" and "!!Estimate" from the state columns
colnames(acs_insurance) <- str_replace_all(colnames(acs_insurance), "!!Percent|!!Estimate", "")
colnames(acs_income)    <- str_replace_all(colnames(acs_income),    "!!Percent|!!Estimate", "")

# 7) Transpose the data

# For insurance data:
acs_insurance <- acs_insurance %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  )
# For income data:
acs_income <- acs_income %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  )

# 8) Write out each data frame to a separate CSV file
write_csv(acs_insurance, "data/ACS/ACS_insurance.csv")
write_csv(acs_income,    "data/ACS/ACS_income.csv")

#################################
# 4. Load and Process KFF Data
#################################

# Load KFF dataset and convert 'Adopted' to 1, 'Not Adopted' to 0
kff <- read_csv("data/KFF.csv") %>%
  mutate(
    `Medicaid Adoption Status` = ifelse(`Medicaid Adoption Status` == "Adopted", 1, 0)
  )

#################################
# 4. Load and Process P2 Data
#################################

# Load P2 dataset (Urbanization, Single-year data) and transpose it
p2 <- read_csv("data/P2-urban-rural-2020.csv") %>%
  pivot_longer(
    cols = -`Label (Grouping)`,
    names_to = "State",
    values_to = "Value"
  ) %>%
  rename(Statistic = `Label (Grouping)`) %>%
  filter(!(Statistic %in% c("Total:"))) %>%
  select(State, Statistic, everything())

#################################
# 5. Load and Process NSDUH Data
#################################

# Helper function to determine number of rows to skip based on header row content
get_skip_value <- function(file, header_keywords = c("Order", "State")) {
  # Read the first 10 lines of the file
  lines <- readLines(file, n = 10)
  
  # Look for a line that starts with one of the header keywords
  header_line_index <- which(sapply(header_keywords, function(kw) {
    any(grepl(paste0("^", kw), lines))
  }))
  
  # Determine the actual line where the header appears
  # Here we search each line for any of the keywords
  header_line <- NA
  for (i in seq_along(lines)) {
    if (any(sapply(header_keywords, function(kw) grepl(paste0("^", kw), lines[i])))) {
      header_line <- i
      break
    }
  }
  
  # If a header was found, skip the rows before the header; otherwise, use a default value (e.g., 5)
  if (!is.na(header_line) && header_line > 1) {
    return(header_line - 1)
  } else {
    return(5)  # Default skip value if header not found
  }
}

# 1) Define the folders where NSDUH data for each year are located
nsduh_folders <- c(
  "data/NSDUH/NSDUHsaeTotalsCSVs2013",
  "data/NSDUH/NSDUHsaeTotalsCSVs2015",
  "data/NSDUH/NSDUHsaeTotalsCSVs2016",
  "data/NSDUH/NSDUHsaeTotalsCSVs2017"
)

# 2) Helper function to extract the year from the folder name (e.g., "NSDUHsaeTotalsCSVs2013" -> 2013)
extract_year_from_folder <- function(folder_path) {
  folder_name <- basename(folder_path)
  as.numeric(str_extract(folder_name, "\\d{4}"))
}

# 3) Define the states
relevant_states <- c("Oregon", "California", "Colorado", "Maine", "Massachusetts", "Nevada", "Washington")

# 4) Initialize empty lists to accumulate data for each state and measure
nsduh_mj_data_list <- list()  # For Marijuana Use
nsduh_mi_data_list <- list()  # For Mental Illness

# Initialize an empty data frame for each state in each list
for (st in relevant_states) {
  nsduh_mj_data_list[[st]] <- tibble()  # Using tibble for consistency with readr output
  nsduh_mi_data_list[[st]] <- tibble()
}

# 5) Loop over each folder (year) and process data files
for (folder in nsduh_folders) {
  
  # Determine the year from the folder name
  this_year <- extract_year_from_folder(folder)
  
  # Identify the two CSV files (one for Marijuana Use, one for Mental Illness)
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Pattern matching: "27" for Marijuana Use, "02" for Mental Illness
  mj_file <- csv_files[grepl("27", csv_files, ignore.case = TRUE)]
  mi_file <- csv_files[grepl("02", csv_files, ignore.case = TRUE)]
  
  # Determine dynamic skip values for each file
  skip_mj <- get_skip_value(mj_file)
  skip_mi <- get_skip_value(mi_file)
  
  # Read each CSV with dynamic skip rows (treat all columns as character)
  nsduh_mj <- read_csv(mj_file, skip = skip_mj, col_types = cols(.default = "c"))
  nsduh_mi <- read_csv(mi_file, skip = skip_mi, col_types = cols(.default = "c"))
  
  # Add a year column
  nsduh_mj <- nsduh_mj %>% mutate(year = this_year)
  nsduh_mi <- nsduh_mi %>% mutate(year = this_year)
  
  # 6) For each state, filter the data and append to the accumulated list
  for (st in relevant_states) {
    
    # For Marijuana Use
    st_mj <- nsduh_mj %>% filter(State == st)
    # For Mental Illness
    st_mi <- nsduh_mi %>% filter(State == st)
    
    # Append the data for this year
    nsduh_mj_data_list[[st]] <- bind_rows(nsduh_mj_data_list[[st]], st_mj)
    nsduh_mi_data_list[[st]] <- bind_rows(nsduh_mi_data_list[[st]], st_mi)
  }
}

nsduh_mj <- bind_rows(nsduh_mj_data_list)
nsduh_mi <- bind_rows(nsduh_mi_data_list)

nsduh_mj <- nsduh_mj %>% select(-Order)
nsduh_mi <- nsduh_mi %>% select(-Order)

rm(st_mj, st_mi)

#################################
# 6. Run Model
#################################
model <- glm(SCHIZOFLG ~ Legalized, data = mhcld_oregon, family = binomial)
summary(model)
