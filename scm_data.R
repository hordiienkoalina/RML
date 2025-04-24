#############################################
# 1. Load Required Packages and Libraries
#############################################

# List of required packages
packages <- c(
  "dplyr", "tidyverse", "readr", "readxl", "purrr", "tidyr",
  "stringr", "MatchIt", "car", "stargazer", "ggplot2", "devtools", "Synth"
)

# Install any packages that are not already installed
installed_pkgs <- installed.packages()[, "Package"]
for (pkg in packages) {
  if (!(pkg %in% installed_pkgs)) {
    install.packages(pkg)
  }
}

# Load libraries (note: some packages are part of tidyverse)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(purrr)
library(tidyr)
library(stringr)
library(MatchIt)
library(car)
library(stargazer)
library(ggplot2)
library(devtools)
library(Synth)
library(janitor)

#############################################
# 2. Load and Process MHCLD Data
#############################################

# Define treatment states with corresponding STATEFIP codes
treatment_states <- list(
  california    = 6,
  maine         = 23,
  nevada        = 32,
  michigan      = 26,
  arizona       = 4
)

# Define additional control states (these did not legalize during 2013-2017)
control_states <- list(
  alabama        =  1,
  arkansas       =  5,
  delaware       = 10,
  florida        = 12,
  georgia        = 13,
  idaho          = 16,
  indiana        = 18,
  iowa           = 19,
  kansas         = 20,
  kentucky       = 21,
  louisiana      = 22,
  mississippi    = 28,
  missouri       = 29,
  nebraska       = 31,
  north_carolina = 37,
  north_dakota   = 38,
  ohio           = 39,
  oklahoma       = 40,
  pennsylvania   = 42,
  south_carolina = 45
)

# Combine treatment and control states
all_states <- c(treatment_states, control_states)

# Legalization years for treatment states
legalization_years <- list(
  california    = 2017,
  nevada        = 2017,
  maine         = 2017,
  michigan      = 2018,
  arizona       = 2020
)

# Set legalization years for control states to NA
control_legalization_years <- lapply(control_states, function(x) NA)
all_legalization_years <- c(legalization_years, control_legalization_years)

# Define expected years for MHCLD data
years_mhcld <- 2013:2022
mhcld_files <- sapply(years_mhcld, function(yr) {
  sprintf("data/MHCLD/mhcld_puf_%d.rdata", yr)
}, USE.NAMES = FALSE)

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

# Create a list to store data frames for each state (both treatment and control)
mhcld_list <- list()

# Loop through each state in the combined list and load data across all available years.
for (state in names(all_states)) {
  state_id <- all_states[[state]]
  legal_year <- all_legalization_years[[state]]
  
  state_data <- purrr::map2_dfr(mhcld_files, years_mhcld, function(file, yr) {
    if (file.exists(file)) {
      load_mhcld(file, yr, state_id)
    } else {
      message(sprintf("File for year %s not found: %s. Skipping this year.", yr, file))
      NULL
    }
  })
  
  state_df <- state_data %>%
    mutate(
      Legalized = ifelse(!is.na(legal_year) & (year >= legal_year), 1, 0),
      State = state
    )
  
  mhcld_list[[state]] <- state_df
}

# Combine data for all states and further process the data
mhcld_all <- bind_rows(mhcld_list) %>% 
  select(-STATEFIP) %>%
  mutate(
    across(where(is.numeric), ~ na_if(., -9)),
    across(where(is.character), ~ na_if(., "-9"))
  ) %>%
  filter(!(AGE %in% c(1, 2, 3)))

#############################################
# 3. Load and Process ACS Data
#############################################

# Identify all ACS CSV files (should now include 2013-2023)
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
  
  # Identify percent and estimate columns
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

# Clean column names by removing suffixes
colnames(acs_insurance) <- str_replace_all(colnames(acs_insurance), "!!Percent|!!Estimate", "")
colnames(acs_income)    <- str_replace_all(colnames(acs_income), "!!Percent|!!Estimate", "")

# Pivot insurance data to long format
acs_insurance <- acs_insurance %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(State = tolower(State))

# Pivot income data to long format
acs_income <- acs_income %>%
  pivot_longer(
    cols = -c(Metric, year),
    names_to = "State",
    values_to = "Value"
  ) %>%
  mutate(State = tolower(State))

# Write processed ACS data to CSV files
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
  mutate(Statistic = if_else(Statistic == "Total:", "Total", Statistic),
         Statistic = str_trim(Statistic),
         Value = as.numeric(Value)) %>%
  group_by(State) %>%
  mutate(total_value = Value[Statistic == "Total"],
         Proportion = if_else(Statistic %in% c("Urban", "Rural"), Value / total_value, NA_real_)) %>%
  ungroup() %>%
  select(-total_value)

#######################
# 6. Merge All Datasets
#######################

# Load 18+ population data based on 2017 estimates
pop18 <- read_csv("data/scprc-est2017-18+pop-res.csv") %>%
  mutate(State = tolower(NAME)) %>%
  select(State, pop_18plus = POPEST18PLUS2017)

if (any(duplicated(pop18$State))) {
  warning("Duplicate state entries found in pop18. Please verify your CSV data.")
}

# Create a base grid of state-year combinations for all states (treatment and control) covering 2013-2023
all_states_vec <- tolower(names(all_states))
base_df <- expand.grid(State = all_states_vec, year = 2013:2023, stringsAsFactors = FALSE)

# Process ACS data: pivot insurance and income data wider so each metric becomes a column
acs_insurance_wide <- acs_insurance %>%
  pivot_wider(names_from = Metric, values_from = Value, names_prefix = "ins_")
acs_income_wide <- acs_income %>%
  pivot_wider(names_from = Metric, values_from = Value, names_prefix = "inc_")
acs_all <- full_join(acs_insurance_wide, acs_income_wide, by = c("State", "year"))

# Process KFF data: ensure one row per state and expand to state-year level
kff_unique <- kff %>% distinct(State, .keep_all = TRUE)
kff_expanded <- base_df %>% left_join(kff_unique, by = "State")
kff_ready <- kff_expanded %>%
  rename(Medicaid = `Medicaid Adoption Status`)

# Process P2 data: pivot wider and expand to state-year level
p2_wide <- p2 %>%
  pivot_wider(names_from = Statistic, values_from = c(Value, Proportion), names_sep = "_")
p2_unique <- p2_wide %>% group_by(State) %>% slice(1) %>% ungroup()
p2_expanded <- base_df %>% left_join(p2_unique, by = "State")

# ----- Aggregate MHCLD Data -----
mhcld_agg <- mhcld_all %>%
  group_by(State, year) %>%
  summarize(SCHIZOFLG_prev = mean(SCHIZOFLG, na.rm = TRUE), .groups = "drop")

# ----- Prepare ACS Data -----
names(acs_all) <- str_squish(names(acs_all))
print(names(acs_all))
acs_ready <- acs_all %>%
  rename(
    Insurance    = `ins_ With health insurance coverage`,
    MedianIncome = `inc_ Median household income (dollars)`,
    MeanIncome   = `inc_ Mean household income (dollars)`
  )

# ----- Prepare P2 Data -----
p2_ready <- p2_expanded %>%
  rename(UrbanPop = Proportion_Urban)

# ----- Merge All Datasets Using the Base State-Year Grid -----
pop18_extrapolated <- base_df %>% left_join(pop18, by = "State")

# Remove the undesired "year" column from p2_ready before joining
p2_ready_clean <- p2_ready %>% select(-year)
p2_extrapolated <- base_df %>% left_join(p2_ready_clean, by = "State")

distinct_years <- unique(p2_extrapolated$year)
print(distinct_years)

aggregated_state_df <- base_df %>%
  left_join(acs_ready, by = c("State", "year")) %>%
  left_join(p2_extrapolated, by = c("State", "year")) %>%
  left_join(pop18_extrapolated, by = c("State", "year")) %>%
  left_join(kff_ready, by = c("State", "year")) %>%
  left_join(mhcld_agg, by = c("State", "year")) %>%
  distinct(State, year, .keep_all = TRUE)

# ----- Prepare Race Data ------
df <- read_csv("data/race.csv")

# Pivot longer so that each row is (Label (Grouping), State, Count)
df_long <- df %>%
  pivot_longer(
    cols = -`Label (Grouping)`,
    names_to = "State",
    values_to = "Count"
  ) %>%
  rename(RaceCategory = `Label (Grouping)`) %>%
  mutate(State = tolower(State))

# Pivot wider so that each race category becomes its own column
df_wide <- df_long %>%
  pivot_wider(
    names_from  = RaceCategory,
    values_from = Count
  )

names(df_wide) <- names(df_wide) %>%
  stringr::str_squish()

# Rename columns for simplicity
race_processed <- df_wide %>%
  rename(
    white = `White alone`,
    black = `Black or African American alone`,
    american_indian = `American Indian and Alaska Native alone`,
    asian = `Asian alone`,
    native_hawaiian = `Native Hawaiian and Other Pacific Islander alone`,
    some_other = `Some Other Race alone`,
    total = `Total:`
  )%>%
  select(State, white, black, american_indian, asian, native_hawaiian, some_other, total)

race_processed <- race_processed %>%
  mutate(prop_white = white / total,
         prop_black = black / total,
         prop_american_indian = american_indian / total,
         prop_asian = asian / total,
         prop_native_hawaiian = native_hawaiian / total,
         prop_some_other = some_other / total)

# ------- Prepare Age Data -----------------
age_df <- read_csv("data/age.csv")

# Clean Column Names
# Remove extra spaces (leading/trailing and internal) from column names.
names(age_df) <- names(age_df) %>% str_squish()

# Optional: inspect the cleaned names
cat("Cleaned column names:\n")
print(names(age_df))

# Filter to Keep Only Columns Ending with "!!total!!estimate"
# We assume the first column is "Label (Grouping)" (the age group labels).
# We use a case‑insensitive regex to match the desired columns.
pattern <- regex("!!total!!estimate$", ignore_case = TRUE)
matching_cols <- names(age_df)[-1][str_detect(names(age_df)[-1], pattern)]

if (length(matching_cols) == 0) {
  warning("No columns matching the pattern '!!total!!estimate' were found. Please verify your column names or adjust the pattern.")
} else {
  # Keep the label column plus the matching state columns.
  cols_to_keep <- c("Label (Grouping)", matching_cols)
  age_df <- age_df %>% select(all_of(cols_to_keep))
}

cat("\nColumn names after filtering for '!!total!!estimate':\n")
print(names(age_df))

# Pivot Longer (Transpose the Table)
# At this point, the remaining state columns are expected to be named like 
# "Alabama!!Total!!Estimate", "Alaska!!Total!!Estimate", etc.
# We pivot longer and split these names into three parts.
age_long <- age_df %>%
  pivot_longer(
    cols = -`Label (Grouping)`,             # Exclude the age group label column
    names_to = c("State", "Measure", "Metric"),
    names_pattern = "^(.*)!!(.*)!!(.*)$",     # Split the column names on "!!"
    values_to = "Count"
  ) %>%
  rename(AgeGroup = `Label (Grouping)`) %>%  # Rename the grouping column for clarity
  mutate(
    State = tolower(str_squish(State)),
    AgeGroup = str_squish(AgeGroup)
  )

# Check the first few rows and unique age groups
cat("\nFirst few rows of age_long:\n")
print(head(age_long))
cat("\nDistinct AgeGroup values:\n")
print(sort(unique(age_long$AgeGroup)))

# Parse the Count Column as Numbers
age_long <- age_long %>% 
  mutate(Count = parse_number(Count))

# Filter for Desired Age Groups
desired_age_groups <- c(
  "20 to 24 years", "25 to 29 years", "30 to 34 years", "35 to 39 years",
  "40 to 44 years", "45 to 49 years", "50 to 54 years", "55 to 59 years",
  "60 to 64 years", "65 to 69 years", "70 to 74 years", "75 to 79 years",
  "80 to 84 years", "85 years and over"
)

age_long_filtered <- age_long %>%
  filter(AgeGroup %in% desired_age_groups)

# Check how many rows remain
cat("\nNumber of rows after filtering for desired age groups: ", nrow(age_long_filtered), "\n")

# Pivot Wider: One Row per State with Each Age Group as a Column
age_wide <- age_long_filtered %>%
  pivot_wider(
    id_cols    = State,         # Each row represents one State
    names_from = AgeGroup,      # Each age group becomes a column
    values_from = Count,        # Fill with the Count values
    values_fn  = first          # In case of duplicates, take the first value
  )

# Clean the resulting column names for safety.
names(age_wide) <- str_squish(names(age_wide))
cat("\nColumn names after pivoting wider:\n")
print(names(age_wide))

# Rename the Age Group Columns for Simplicity and Process Numerically
age_processed <- age_wide %>%
  rename(
    age20_24   = "20 to 24 years",
    age25_29   = "25 to 29 years",
    age30_34   = "30 to 34 years",
    age35_39   = "35 to 39 years",
    age40_44   = "40 to 44 years",
    age45_49   = "45 to 49 years",
    age50_54   = "50 to 54 years",
    age55_59   = "55 to 59 years",
    age60_64   = "60 to 64 years",
    age65_69   = "65 to 69 years",
    age70_74   = "70 to 74 years",
    age75_79   = "75 to 79 years",
    age80_84   = "80 to 84 years",
    age85_over = "85 years and over"
  ) %>%
  # Optionally, select only the State and age columns
  select(State, age20_24, age25_29, age30_34, age35_39, age40_44, 
         age45_49, age50_54, age55_59, age60_64, age65_69, age70_74,
         age75_79, age80_84, age85_over) %>%
  # At this point the columns should already be numeric (after parse_number),
  # but we can force conversion in case some values are still character.
  mutate(across(starts_with("age"), ~ as.numeric(.))) %>%
  # Calculate total and proportions (optional)
  mutate(total_age = rowSums(across(starts_with("age")), na.rm = TRUE)) %>%
  mutate(
    prop_20_24   = age20_24 / total_age,
    prop_25_29   = age25_29 / total_age,
    prop_30_34   = age30_34 / total_age,
    prop_35_39   = age35_39 / total_age,
    prop_40_44   = age40_44 / total_age,
    prop_45_49   = age45_49 / total_age,
    prop_50_54   = age50_54 / total_age,
    prop_55_59   = age55_59 / total_age,
    prop_60_64   = age60_64 / total_age,
    prop_65_69   = age65_69 / total_age,
    prop_70_74   = age70_74 / total_age,
    prop_75_79   = age75_79 / total_age,
    prop_80_84   = age80_84 / total_age,
    prop_85_over = age85_over / total_age
  )

# Merge age data and race data into the aggregated_state_df
aggregated_state_df <- aggregated_state_df %>%
  left_join(age_processed, by = "State") %>%
  left_join(race_processed, by = "State")

#################################
# 7. Load and Process NSDUH Data
#################################

# 7.1 Find all Tab02 & Tab27 CSVs under data/NSDUH (recursively)
nsduh_files <- list.files(
  path       = "data/NSDUH",
  pattern    = "Tab(02|27)-\\d{4}\\.csv$",
  full.names = TRUE,
  recursive  = TRUE
)
cat(">> Found NSDUH files:\n"); print(nsduh_files)

# 7.2 Helper to read one CSV and pull “18+” or “18 or Older” Estimate
load_18plus <- function(path) {
  cat("\n----\nReading:", path, "\n")
  
  # detect header row (quoted or unquoted)
  lines <- readLines(path, n = 20, encoding = "latin1")
  hdr_i <- which(str_detect(
    lines,
    regex('^"?Order"?\\s*,\\s*"?State"?', ignore_case = TRUE)
  ))
  if (length(hdr_i) != 1) {
    stop("✖ Cannot detect header row in ", basename(path))
  }
  skip_n <- hdr_i - 1
  cat("Detected header at line", hdr_i, "→ skip =", skip_n, "\n")
  
  # peek at columns
  tmp <- read_csv(path, skip = skip_n, n_max = 0, col_types = cols(.default = "c"))
  cat(" Columns after skip:\n"); print(names(tmp))
  
  # load & clean
  df <- read_csv(path, skip = skip_n, col_types = cols(.default = "c")) %>%
    clean_names()
  cat(" Cleaned names:\n"); print(names(df))
  
  # pick the right “18+” column
  possible <- c("x18_or_older_estimate", "x18_estimate")
  key_col  <- intersect(possible, names(df))
  if (length(key_col) != 1) {
    stop("✖ Neither '18 or Older' nor '18+ Estimate' found in ", basename(path))
  }
  cat("Using column:", key_col, "\n")
  
  # extract
  out <- df %>%
    select(state, raw_value = all_of(key_col)) %>%
    mutate(
      state = tolower(state),
      year  = as.integer(str_extract(basename(path), "\\d{4}")),
      Value = na_if(as.numeric(str_remove_all(raw_value, ",")), 
                    -9) * 1000   # convert from thousands to units
                      ) %>%
    select(state, year, Value)
  
  cat(" First few rows:\n"); print(head(out))
  return(out)
}

# 7.3 Load & join Marijuana use (Tab27)
mar_files <- grep("Tab27", nsduh_files, value = TRUE)
cat("\n>> Tab27 (MarijuanaUse) files:\n"); print(mar_files)

mar_long <- map_dfr(mar_files, load_18plus)
cat("\n>> Combined MarijuanaUse rows:\n"); print(head(mar_long))

# 7.4 Load & join Mental illness (Tab02)
ment_files <- grep("Tab02", nsduh_files, value = TRUE)
cat("\n>> Tab02 (MentalIllness) files:\n"); print(ment_files)

ment_long <- map_dfr(ment_files, load_18plus)
cat("\n>> Combined MentalIllness rows:\n"); print(head(ment_long))

# ---- NSDUH Values as Fractions ------
# Rename the NSDUH counts
mar_counts <- mar_long  %>% rename(mar_count  = Value)
ment_counts <- ment_long %>% rename(ment_count = Value)

# Join them both to pop18_extrapolated
nsduh_frac <- pop18_extrapolated %>%
  # pop18_extrapolated has State, year, pop_18plus
  left_join(mar_counts,  by = c("State" = "state",  "year")) %>%
  left_join(ment_counts, by = c("State" = "state",  "year")) %>%
  
  # Compute fractions (no longer in thousands)
  mutate(
    mar_frac  = mar_count  / pop_18plus,
    ment_frac = ment_count / pop_18plus
  ) %>%
  
  select(State, year, mar_frac, ment_frac)

aggregated_state_df <- aggregated_state_df %>%
  left_join(
    nsduh_frac %>% select(State, year, mar_frac, ment_frac),
    by = c("State", "year")
  )

aggregated_state_df %>% 
  select(State, year, mar_frac, ment_frac) %>% 
  head()

#####################################
# 8. Load and Process Education Data
#####################################

# Read the education CSV
educ_df <- read_csv("data/educ.csv")

# 1. Clean column names
names(educ_df) <- names(educ_df) %>% str_squish()

# 2. Identify columns ending with "!!total!!estimate"
pattern <- regex("!!total!!estimate$", ignore_case = TRUE)
matching_cols <- names(educ_df)[-1][str_detect(names(educ_df)[-1], pattern)]

if (length(matching_cols) == 0) {
  warning("No columns matching '!!total!!estimate' found in educ.csv")
} else {
  cols_to_keep <- c("Label (Grouping)", matching_cols)
  educ_df <- educ_df %>% select(all_of(cols_to_keep))
}

# 3. Pivot longer: split names into State / Measure / Metric
educ_long <- educ_df %>%
  pivot_longer(
    cols       = -`Label (Grouping)`,
    names_to   = c("State", "Measure", "Metric"),
    names_pattern = "^(.*)!!(.*)!!(.*)$",
    values_to  = "Count"
  ) %>%
  rename(EducationLevel = `Label (Grouping)`) %>%
  mutate(
    State = tolower(str_squish(State)),
    EducationLevel = str_squish(EducationLevel),
    Count = parse_number(Count)
  )

# 4. Pivot wider: one row per state, one column per education level
educ_wide <- educ_long %>%
  pivot_wider(
    id_cols    = State,
    names_from = EducationLevel,
    values_from = Count,
    values_fn  = first
  )

# Final processed education data
educ_processed <- educ_wide

# Removing unnecessary data
educ_processed <- educ_processed[, -2] 
educ_processed <- educ_processed[, 1:14]
educ_processed <- educ_processed[, -c(2:6)]

educ_processed <- educ_processed %>%
  mutate(
    frac_9th_12_nodiploma = `9th to 12th grade, no diploma` / `Population 25 years and over`,
    frac_associate       = `Associate's degree`               / `Population 25 years and over`,
    frac_bachelor        = `Bachelor's degree`                / `Population 25 years and over`,
    frac_grad_prof       = `Graduate or professional degree`  / `Population 25 years and over`,
    frac_hs_or_higher    = `High school graduate or higher`   / `Population 25 years and over`,
    frac_less9           = `Less than 9th grade`              / `Population 25 years and over`
  )

aggregated_state_df <- aggregated_state_df %>%
  left_join(
    educ_processed %>% 
      select(
        State,
        frac_9th_12_nodiploma,
        frac_associate,
        frac_bachelor,
        frac_grad_prof,
        frac_hs_or_higher,
        frac_less9
      ),
    by = "State"
  )

#####################################################
# 9. Final Changes to the Aggregated State Dataframe
#####################################################

aggregated_state_df <- aggregated_state_df %>% 
  select(-Proportion_Total, -total)

aggregated_state_df <- aggregated_state_df %>%
  mutate(
    state_id = as.numeric(factor(State)),
    year     = as.numeric(year)
  )

write_csv(aggregated_state_df, "scm_data.csv")