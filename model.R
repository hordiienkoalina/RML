#############################################
# 1. Load Required Packages and Libraries
#############################################

# List of required packages
packages <- c("dplyr", "tidyverse", "readr", "readxl", "purrr", "tidyr", "stringr")

# Install any packages that are not already installed
installed <- installed.packages()[, "Package"]
for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

# Load libraries
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(purrr)
library(tidyr)
library(stringr)

#############################################
# 2. Load and Process MHCLD Data
#############################################

# Define file paths for MHCLD .rdata files for each year
mhcld_files <- list(
  "data/MHCLD/mhcld_puf_2013.rdata",
  "data/MHCLD/mhcld_puf_2014.rdata",
  "data/MHCLD/mhcld_puf_2015.rdata",
  "data/MHCLD/MHCLD_PUF_2016.rdata",
  "data/MHCLD/MHCLD_PUF_2017.rdata"
)

# Define state names with corresponding STATEFIP codes
state_ids <- list(
  oregon        = 41,
  california    = 6,
  colorado      = 8,
  maine         = 23,
  massachusetts = 25,
  nevada        = 32,
  washington    = 53
)

# Define cannabis legalization year for each state
legalization_years <- list(
  washington    = 2014,
  colorado      = 2014,
  oregon        = 2015,
  massachusetts = 2016,
  california    = 2017,
  nevada        = 2017,
  maine         = 2017
)

# Function to load a given MHCLD file, filter by state, and add a year column
load_mhcld <- function(file_path, year, state_id) {
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) {
    stop("The loaded object is not a data frame")
  }
  df <- df %>%
    filter(STATEFIP == state_id) %>%
    mutate(year = year)
  return(df)
}

# Create a list to store data frames for each state
mhcld_list <- list()

# Loop through each state and combine data across years
for (state in names(state_ids)) {
  state_id <- state_ids[[state]]
  legal_year <- legalization_years[[state]]
  
  # Combine data for all years for the current state
  state_df <- bind_rows(
    load_mhcld(mhcld_files[[1]], 2013, state_id),
    load_mhcld(mhcld_files[[2]], 2014, state_id),
    load_mhcld(mhcld_files[[3]], 2015, state_id),
    load_mhcld(mhcld_files[[4]], 2016, state_id),
    load_mhcld(mhcld_files[[5]], 2017, state_id)
  ) %>%
    mutate(
      Legalized = ifelse(year >= legal_year, 1, 0),
      State = state
    )
  
  mhcld_list[[state]] <- state_df
}

# Combine data for all states and remove the STATEFIP column
mhcld_all <- bind_rows(mhcld_list) %>% 
  select(-STATEFIP) %>%
  # Replace invalid entries (-9) with NA
  mutate(
    across(where(is.numeric), ~ na_if(., -9)),
    across(where(is.character), ~ na_if(., "-9"))
  ) %>%
  # Remove rows where AGE is 1, 2, or 3 (indicating under 18 years old)
  filter(!(AGE %in% c(1, 2, 3)))

#############################################
# 3. Load and Process ACS Data
#############################################

# Identify all ACS CSV files
acs_files <- list.files(path = "data/ACS", pattern = "ACS-\\d{4}\\.csv", full.names = TRUE)

# Function to process an ACS CSV file and extract:
# (a) Health insurance coverage data
# (b) Household income data (median & mean)
load_and_process_acs <- function(file) {
  # Extract year from filename (e.g., "ACS-2013.csv")
  year <- as.numeric(gsub(".*ACS-(\\d{4}).csv", "\\1", basename(file)))
  
  # Read the CSV file, treating all columns as character
  acs_data <- read_csv(file, col_types = cols(.default = "c"))
  
  # Select rows of interest:
  # Row 103: Health insurance coverage
  # Rows 68 and 69: Median and Mean household income
  selected_rows <- acs_data[c(103, 68, 69), ]
  
  # Identify columns for percent and estimate values
  percent_columns  <- names(acs_data)[grepl("!!Percent$", names(acs_data))]
  estimate_columns <- names(acs_data)[grepl("!!Estimate$", names(acs_data))]
  
  # Process Health Insurance Coverage (row 103)
  health_insurance <- selected_rows[1, c("Label (Grouping)", percent_columns)] %>%
    mutate(
      year = year,
      across(ends_with("!!Percent"), ~ as.numeric(gsub("%", "", .)) / 100)
    ) %>%
    rename(Metric = `Label (Grouping)`)
  
  # Process Household Income (rows 68 and 69)
  income_data <- selected_rows[2:3, c("Label (Grouping)", estimate_columns)] %>%
    mutate(
      year = year,
      across(ends_with("!!Estimate"), ~ as.numeric(gsub("[^0-9.]", "", .)))
    ) %>%
    rename(Metric = `Label (Grouping)`)
  
  list(
    insurance = health_insurance,
    income = income_data
  )
}

# Apply processing function to each ACS file
acs_list <- lapply(acs_files, load_and_process_acs)

# Combine insurance and income data across files
acs_insurance <- bind_rows(lapply(acs_list, `[[`, "insurance"))
acs_income    <- bind_rows(lapply(acs_list, `[[`, "income"))

# Clean column names by removing suffixes from state columns
colnames(acs_insurance) <- str_replace_all(colnames(acs_insurance), "!!Percent|!!Estimate", "")
colnames(acs_income)    <- str_replace_all(colnames(acs_income),    "!!Percent|!!Estimate", "")

# Transpose insurance data to long format
acs_insurance <- acs_insurance %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(State = tolower(State))

# Transpose income data to long format
acs_income <- acs_income %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(State = tolower(State))

# Write the processed ACS data to CSV files
write_csv(acs_insurance, "data/ACS/ACS_insurance.csv")
write_csv(acs_income, "data/ACS/ACS_income.csv")

#############################################
# 4. Load and Process KFF Data
#############################################

# Load KFF dataset and convert Medicaid adoption status to numeric (1 for Adopted, 0 for Not Adopted)
kff <- read_csv("data/KFF.csv") %>%
  mutate(`Medicaid Adoption Status` = ifelse(`Medicaid Adoption Status` == "Adopted", 1, 0)) %>%
  mutate(State = tolower(State))

#############################################
# 5. Load and Process P2 Data
#############################################

# Load P2 dataset (Urbanization data), transpose, and calculate urban and rural proportions
p2 <- read_csv("data/P2-urban-rural-2020.csv") %>%
  pivot_longer(
    cols = -`Label (Grouping)`,
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(State = tolower(State)) %>%
  rename(Statistic = `Label (Grouping)`) %>%
  filter(!grepl("Not defined for this file", Statistic)) %>%
  mutate(Statistic = if_else(Statistic == "Total:", "Total", Statistic)) %>%
  mutate(Statistic = str_trim(Statistic)) %>%
  mutate(Value = as.numeric(Value)) %>%
  group_by(State) %>%
  mutate(total_value = Value[Statistic == "Total"]) %>%
  mutate(Proportion = if_else(Statistic %in% c("Urban", "Rural"), Value / total_value, NA_real_)) %>%
  ungroup() %>%
  select(-total_value)

#############################################
# 6. Load and Process NSDUH Data
#############################################

# Function to determine the number of rows to skip based on header keywords
get_skip_value <- function(file, header_keywords = c("Order", "State")) {
  lines <- readLines(file, n = 10)
  header_line <- NA
  for (i in seq_along(lines)) {
    if (any(sapply(header_keywords, function(kw) grepl(paste0("^", kw), lines[i])))) {
      header_line <- i
      break
    }
  }
  if (!is.na(header_line) && header_line > 1) {
    return(header_line - 1)
  } else {
    return(5)  # Default skip value if header not found
  }
}

# Define folders for NSDUH data by year
nsduh_folders <- c(
  "data/NSDUH/NSDUHsaeTotalsCSVs2013",
  "data/NSDUH/NSDUHsaeTotalsCSVs2015",
  "data/NSDUH/NSDUHsaeTotalsCSVs2016",
  "data/NSDUH/NSDUHsaeTotalsCSVs2017"
)

# Extract year from folder name
extract_year_from_folder <- function(folder_path) {
  folder_name <- basename(folder_path)
  as.numeric(str_extract(folder_name, "\\d{4}"))
}

# Define relevant states for NSDUH data processing
relevant_states <- c("oregon", "california", "colorado", "maine", "massachusetts", "nevada", "washington")

# Initialize empty lists to accumulate NSDUH data for Marijuana Use and Mental Illness
nsduh_mj_data_list <- vector("list", length(relevant_states))
nsduh_mi_data_list <- vector("list", length(relevant_states))
names(nsduh_mj_data_list) <- names(nsduh_mi_data_list) <- relevant_states

# Loop over each NSDUH folder (year) and process the corresponding files
for (folder in nsduh_folders) {
  this_year <- extract_year_from_folder(folder)
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  # Identify CSV files for Marijuana Use (pattern "27") and Mental Illness (pattern "02")
  mj_file <- csv_files[grepl("27", csv_files, ignore.case = TRUE)]
  mi_file <- csv_files[grepl("02", csv_files, ignore.case = TRUE)]
  
  skip_mj <- get_skip_value(mj_file)
  skip_mi <- get_skip_value(mi_file)
  
  nsduh_mj <- read_csv(mj_file, skip = skip_mj, col_types = cols(.default = "c")) %>%
    mutate(State = tolower(State), year = this_year)
  nsduh_mi <- read_csv(mi_file, skip = skip_mi, col_types = cols(.default = "c")) %>%
    mutate(State = tolower(State), year = this_year)
  
  # Filter and accumulate data for each relevant state
  for (st in relevant_states) {
    nsduh_mj_data_list[[st]] <- bind_rows(nsduh_mj_data_list[[st]], nsduh_mj %>% filter(State == st))
    nsduh_mi_data_list[[st]] <- bind_rows(nsduh_mi_data_list[[st]], nsduh_mi %>% filter(State == st))
  }
}

nsduh_mj <- bind_rows(nsduh_mj_data_list) %>% select(-Order)
nsduh_mi <- bind_rows(nsduh_mi_data_list) %>% select(-Order)

#############################################
# 7. Merge All Datasets
#############################################

# Load 18+ population data based on 2017 estimates
# Note: the population estimate is constant for all years
pop18 <- read_csv("data/scprc-est2017-18+pop-res.csv") %>%
  # Convert state names to lowercase for consistency using the "STATE" column
  mutate(State = tolower(NAME)) %>%
  # Select and rename the relevant column; using "POPEST18PLUS2017" as the population estimate
  select(State, pop_18plus = POPEST18PLUS2017)

# Check that each state appears only once (ensuring a one-to-many join later)
if(any(duplicated(pop18$State))) {
  warning("Duplicate state entries found in pop18. Please verify your CSV data.")
}

# Create a base grid of state-year combinations (2013-2017)
base_df <- expand.grid(State = relevant_states, year = 2013:2017, stringsAsFactors = FALSE)

# Process ACS data: pivot insurance and income data wider so each metric becomes a column
acs_insurance_wide <- acs_insurance %>%
  pivot_wider(names_from = Metric, values_from = Value, names_prefix = "ins_")
acs_income_wide <- acs_income %>%
  pivot_wider(names_from = Metric, values_from = Value, names_prefix = "inc_")
acs_all <- full_join(acs_insurance_wide, acs_income_wide, by = c("State", "year"))

# Merge NSDUH data for Marijuana Use and Mental Illness
nsduh_all <- full_join(nsduh_mj, nsduh_mi, by = c("State", "year"), suffix = c("_mj", "_mi"))

# Process KFF data: ensure one row per state and expand to state-year level
kff_unique <- kff %>% distinct(State, .keep_all = TRUE)
kff_expanded <- base_df %>% left_join(kff_unique, by = "State")

# Process P2 data: pivot wider and expand to state-year level
p2_wide <- p2 %>%
  pivot_wider(names_from = Statistic, values_from = c(Value, Proportion), names_sep = "_")
p2_unique <- p2_wide %>% group_by(State) %>% slice(1) %>% ungroup()
p2_expanded <- base_df %>% left_join(p2_unique, by = "State")

# Merge all datasets together on the base state-year grid,
# then merge the 18+ population data using a left join (one-to-many join on State)
final_df <- base_df %>%
  left_join(mhcld_all, by = c("State", "year")) %>%
  left_join(acs_all, by = c("State", "year")) %>%
  left_join(nsduh_all, by = c("State", "year")) %>%
  left_join(kff_expanded, by = c("State", "year")) %>%
  left_join(p2_expanded, by = c("State", "year")) %>%
  left_join(pop18, by = "State")

# Convert NSDUH variables to numeric after removing commas
final_df <- final_df %>%
  mutate(`18 or Older Estimate_mi` = as.numeric(gsub(",", "", as.character(`18 or Older Estimate_mi`))),
         `18 or Older Estimate_mj` = as.numeric(gsub(",", "", as.character(`18 or Older Estimate_mj`))))

# Clean column names by trimming whitespace
names(final_df) <- str_squish(names(final_df))

# Rename selected columns for clarity
final_df <- final_df %>%
  rename(
    MentalIllnessYr = `18 or Older Estimate_mi`,
    MarijuanaUseYr  = `18 or Older Estimate_mj`,
    Medicaid        = `Medicaid Adoption Status`,
    UrbanPop        = Proportion_Urban,
    MedianIncome    = `inc_ Median household income (dollars)`,
    MeanIncome      = `inc_ Mean household income (dollars)`,
    Insurance       = `ins_ With health insurance coverage`
  )

# Subset to include only columns used in the regression model
final_df <- final_df %>%
  select(State, year, SCHIZOFLG, Legalized, AGE, GENDER, ETHNIC, RACE, EDUC,
         MentalIllnessYr, MarijuanaUseYr, MedianIncome, MeanIncome, Insurance, UrbanPop, Medicaid, pop_18plus)

# Represent MentalIllnessYr and MarijuanaUseYr as a proportion of the state population
final_df <- final_df %>%
  mutate(
    MentalIllnessYr = (MentalIllnessYr * 1000) / pop_18plus,
    MarijuanaUseYr  = (MarijuanaUseYr * 1000)  / pop_18plus
  )

# Print column names for verification
print(names(final_df))

# Write the final integrated dataset to a CSV file
write_csv(final_df, "data/final_df.csv")

# Write the first 500,000 rows to a CSV file
write_csv(final_df %>% slice_head(n = 500000), "data/final_df_truncated.csv")

#############################################
# 8. Fit Logistic Regression Model
#############################################

# Fit a logistic regression model using the cleaned dataset
model <- glm(SCHIZOFLG ~ 
               Legalized + 
               AGE + 
               GENDER + 
               ETHNIC +
               # RACE + # Omitted due to collinearity w/ ETHNIC
               EDUC +
               MentalIllnessYr + 
               MarijuanaUseYr +
               MedianIncome +
               # MeanIncome + # Omitted due to collinearity w/ MedianIncome
               # Medicaid + # Omitted since unique(final_df$Medicaid) = 1
               Insurance + 
               UrbanPop,
             data = final_df,
             family = binomial)

# Display model summary
summary(model)
