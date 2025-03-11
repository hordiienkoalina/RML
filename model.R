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
# 2. Load and Process MHCLD Data
#################################

load_mhcld <- function(file_path, year) {
  env <- new.env()
  load(file_path, envir = env)
  df_name <- ls(env)[1]
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) {
    stop("The loaded object is not a data frame")
  }
  # Filter by STATEFIP = 41 (Oregon)
  df <- df %>%
    filter(STATEFIP == 41) %>%
    mutate(year = year)
  return(df)
}

mhcld_files <- list(
  "data/MHCLD/mhcld_puf_2013.rdata",
  "data/MHCLD/mhcld_puf_2014.rdata",
  "data/MHCLD/mhcld_puf_2015.rdata",
  "data/MHCLD/MHCLD_PUF_2016.rdata",
  "data/MHCLD/MHCLD_PUF_2017.rdata"
)
mhcld_oregon <- bind_rows(
  load_mhcld(mhcld_files[[1]], 2013),
  load_mhcld(mhcld_files[[2]], 2014),
  load_mhcld(mhcld_files[[3]], 2015),
  load_mhcld(mhcld_files[[4]], 2016),
  load_mhcld(mhcld_files[[5]], 2017)
) %>%
  mutate(Legalized = ifelse(year >= 2015, 1, 0))

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
# 5. Run Model
#################################
model <- glm(SCHIZOFLG ~ Legalized, data = mhcld_oregon, family = binomial)
summary(model)
