# Install necessary packages (if not already installed)
packages <- c("dplyr", "tidyverse", "readr", "readxl")
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

### Step 1: Load and Process MHCLD Data (Psychotic Disorder Hospitalizations) ###
load_mhcld <- function(file_path, year) {
  env <- new.env()
  load(file_path, envir = env)  # Load .rdata file into a new environment
  df_name <- ls(env)[1]         # Get the first object from that environment
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) {
    stop("The loaded object is not a data frame")
  }
  # Use the MH-CLD codebook: filter where STATEFIP equals 41 (Oregon)
  df <- df %>%
    filter(STATEFIP == 41) %>%
    mutate(year = year)
  return(df)
}

# Load and merge MHCLD data for Oregon (2013–2017)
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
)