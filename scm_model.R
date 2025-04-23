library(readr)
library(dplyr)
library(Synth)

data <- read_csv("scm_data.csv") %>% 
  as.data.frame()

# Identify treated unit (California)
treatment_id <- data %>%
  filter(State == "california") %>%
  pull(state_id) %>%
  unique()

# Pre/post windows
pre_treatment  <- 2013:2016
post_treatment <- 2017:2022

# Predictor list
predictors <- c(
  "ment_frac",
  "mar_frac",
  "Insurance",
  "MedianIncome",
  "MeanIncome",
  "UrbanPop",
  "pop_18plus",
  "Medicaid",
  # age props
  paste0("prop_", c(
    "20_24","25_29","30_34","35_39","40_44",
    "45_49","50_54","55_59","60_64","65_69",
    "70_74","75_79","80_84","85_over"
  )),
  # race props
  c("prop_white","prop_black","prop_american_indian","prop_asian",
    "prop_native_hawaiian","prop_some_other"),
  # education fractions
  c("frac_9th_12_nodiploma","frac_associate","frac_bachelor",
    "frac_grad_prof","frac_hs_or_higher","frac_less9")
)

# Prepare data for Synth
dp.out <- dataprep(
  foo                   = data,
  predictors            = predictors,
  predictors.op         = "mean",
  time.predictors.prior = pre_treatment,
  dependent             = "SCHIZOFLG_prev",
  unit.variable         = "state_id",
  unit.names.variable   = "State",
  time.variable         = "year",
  treatment.identifier  = treatment_id,
  controls.identifier   = c("alabama",
                            "arkansas",
                            # "georgia", # omitted due to missing data
                            "idaho",
                            "indiana",
                            "iowa",
                            #"kansas", # omitted due to missing data
                            "kentucky",
                            "louisiana",
                            "mississippi",
                            "missouri",
                            "nebraska",
                            #"north_dakota", # omitted due to missing data
                            "oklahoma",
                            #"south_carolina", # omitted due to missing data
                            #"south_dakota", # omitted due to missing data
                            "tennessee",
                            "texas",
                            "virginia",
                            #"west_virginia", # omitted due to missing data
                            "utah"),
  time.optimize.ssr     = pre_treatment,
  time.plot             = c(pre_treatment, post_treatment)
)

# Run Synth & view results
synth.out   <- synth(dp.out)
synth.tables <- synth.tab(dataprep.res = dp.out, synth.res = synth.out)
print(synth.tables)

# Plot
path.plot(
  synth.res    = synth.out,
  dataprep.res = dp.out,
  Ylab         = "SCHIZOFLG_prev",
  Xlab         = "Year",
  Legend       = c("California", "Synthetic"),
  tr.intake    = 2017
)