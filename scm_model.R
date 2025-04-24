#── 0) Load libraries ──────────────────────────────────────────────────────────
library(Synth)
library(zoo)

#── 1) Read in the CSV with base R to guarantee plain data.frame ────────────
data <- read.csv("scm_data.csv", stringsAsFactors = FALSE)

#── 2) Diagnostic check: ensure there is a numeric 'state_id' column ─────────
#    Print any columns with “state” in their name
cat("Columns matching /state/ in the data:\n")
print(names(data)[grep("state", names(data), ignore.case = TRUE)])

#    Stop early if state_id is missing or non‐numeric
if (!"state_id" %in% names(data)) {
  stop("ERROR: No column named 'state_id' found in the data.frame.")
}
data$state_id <- as.numeric(data$state_id)
if (!is.numeric(data$state_id)) {
  stop("ERROR: 'state_id' exists but is not numeric after coercion.")
}

#── 3) Lowercase the state names for consistent matching ────────────────────
data$State <- tolower(as.character(data$State))

#── 4) Define treatment & control sets and time windows ──────────────────────
treatment_state <- "arizona"
control_states  <- c(
  "alabama","arkansas","delaware","florida",
  "idaho","indiana","iowa","kentucky",
  "louisiana","mississippi","missouri","nebraska",
  "ohio","oklahoma","pennsylvania"
)

pre_treatment  <- 2013:2020
post_treatment <- 2021:2022

treatment_id <- unique(data$state_id[data$State == treatment_state])
control_ids  <- unique(data$state_id[data$State %in% control_states])

#── 5) Impute one‐off missing SCHIZOFLG_prev values via linear interpolation ─
#    This will fill each state's single NA in the modeling window
data <- data[order(data$State, data$year), ]
for (st in unique(data$State)) {
  idx <- data$State == st
  data$SCHIZOFLG_prev[idx] <- na.approx(
    data$SCHIZOFLG_prev[idx],
    x    = data$year[idx],
    na.rm = FALSE
  )
}

#── 6) Confirm no more NAs in the modeling period for controls & treatment ──
check_na <- subset(data,
                   state_id %in% c(treatment_id, control_ids) &
                     year %in% c(pre_treatment, post_treatment) &
                     is.na(SCHIZOFLG_prev),
                   select = c(State, year)
)
if (nrow(check_na) > 0) {
  cat("WARNING: These rows still have missing SCHIZOFLG_prev:\n")
  print(check_na)
  stop("Please resolve these before running dataprep().")
}

#── 7) Specify the predictors ─────────────────────────────────────────────────
predictors <- c(
  "ment_frac", "mar_frac", "Insurance",
  "MedianIncome", "MeanIncome", "UrbanPop",
  "pop_18plus", "Medicaid",
  paste0("prop_", c(
    "20_24","25_29","30_34","35_39","40_44",
    "45_49","50_54","55_59","60_64","65_69",
    "70_74","75_79","80_84","85_over"
  )),
  c("prop_white","prop_black","prop_american_indian",
    "prop_asian","prop_native_hawaiian","prop_some_other"),
  c("frac_9th_12_nodiploma","frac_associate","frac_bachelor",
    "frac_grad_prof","frac_hs_or_higher","frac_less9")
)

#── 8) Prepare data for the Synth package ────────────────────────────────────
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
  controls.identifier   = control_ids,
  time.optimize.ssr     = pre_treatment,
  time.plot             = c(pre_treatment, post_treatment)
)

#── 9) Fit the synthetic control model & view results ─────────────────────────
synth.res    <- synth(dp.out)
synth.tables <- synth.tab(dataprep.res = dp.out, synth.res = synth.res)
print(synth.tables)

#── 10) Plot the actual vs. synthetic series ─────────────────────────────────
path.plot(
  synth.res, dp.out,
  Ylab   = "SCHIZOFLG_prev",
  Xlab   = "Year",
  Legend = c("Treated Arizona", "Synthetic Arizona")
)
abline(v = max(pre_treatment), lty = 2, col = "red")

#── 11) Plot the gaps (actual − synthetic) ───────────────────────────────────
gaps.plot(
  synth.res, dp.out,
  Ylab = "Gap (Actual − Synthetic)",
  Xlab = "Year"
)
abline(v = max(pre_treatment), lty = 2, col = "red")

#── 12) Compute & print RMSPE for pre‐ and post‐ periods ──────────────────────
time_pts <- dp.out$tag$time.plot
pre_idx  <- time_pts %in% pre_treatment
post_idx <- time_pts %in% post_treatment

pre_rmspe  <- sqrt(mean((dp.out$Y0plot[pre_idx,1] - dp.out$Y1plot[pre_idx,1])^2, na.rm = TRUE))
post_rmspe <- sqrt(mean((dp.out$Y0plot[post_idx,1] - dp.out$Y1plot[post_idx,1])^2, na.rm = TRUE))

cat("Pre-treatment RMSPE: ",  pre_rmspe,  "\n",
    "Post-treatment RMSPE:", post_rmspe, "\n")

#── 13) In-space placebo tests & plot ─────────────────────────────────────────

placebo_ids <- control_ids
time_pts    <- dp.out$tag$time.plot
n_t         <- length(time_pts)
n_p         <- length(placebo_ids)

# matrix to hold each placebo gap series
gaps_matrix <- matrix(NA, nrow = n_t, ncol = n_p,
                      dimnames = list(time_pts, as.character(placebo_ids)))

for (i in seq_along(placebo_ids)) {
  pid      <- placebo_ids[i]
  ctrl_set <- setdiff(c(treatment_id, control_ids), pid)
  
  dp_p <- dataprep(
    foo                   = data,
    predictors            = predictors,
    predictors.op         = "mean",
    time.predictors.prior = pre_treatment,
    dependent             = "SCHIZOFLG_prev",
    unit.variable         = "state_id",
    unit.names.variable   = "State",
    time.variable         = "year",
    treatment.identifier  = pid,
    controls.identifier   = ctrl_set,
    time.optimize.ssr     = pre_treatment,
    time.plot             = time_pts
  )
  synth_p <- synth(dp_p)
  
  # compute synthetic series for this placebo
  synth_y_p <- dp_p$Y0plot %*% synth_p$solution.w
  # actual series
  actual_p  <- dp_p$Y1plot[,1]
  # gap = actual − synthetic
  gaps_matrix[, i] <- actual_p - synth_y_p
}

# now do the same for Arizona
synth_y_az   <- dp.out$Y0plot %*% synth.res$solution.w
actual_az    <- dp.out$Y1plot[,1]
treated_gap  <- actual_az - synth_y_az

# determine y-limits over *all* gaps
all_gaps <- c(as.vector(gaps_matrix), treated_gap)
ylim     <- range(all_gaps, na.rm = TRUE)

# plot
plot(time_pts, gaps_matrix[,1], type = "n",
     ylab = "Gap (Actual − Synthetic)", xlab = "Year",
     ylim = ylim)
for (i in seq_len(n_p)) {
  lines(time_pts, gaps_matrix[,i], col = "grey", lty = 1)
}
lines(time_pts, treated_gap, col = "black", lwd = 2)

abline(v = max(pre_treatment), lty = 2, col = "red")
legend("bottomleft",
       legend = c("Placebo Gaps", "Treated Arizona"),
       col    = c("grey","black"),
       lty    = c(1,1),
       lwd    = c(1,2))

#── 14) Plot pre/post MSPE ratios for Arizona and placebos with state names ─────

# logical flags for pre‐ and post‐treatment periods
pre_flag  <- time_pts %in% pre_treatment
post_flag <- time_pts %in% post_treatment

# 1) Compute MSPE for each placebo
pre_mspe_p  <- colMeans(gaps_matrix[pre_flag, ]^2, na.rm = TRUE)
post_mspe_p <- colMeans(gaps_matrix[post_flag, ]^2, na.rm = TRUE)
ratio_p     <- post_mspe_p / pre_mspe_p

# 2) Compute MSPE for Arizona
pre_mspe_az  <- mean(treated_gap[pre_flag]^2, na.rm = TRUE)
post_mspe_az <- mean(treated_gap[post_flag]^2, na.rm = TRUE)
ratio_az     <- post_mspe_az / pre_mspe_az

# 3) Combine into one named vector
ratios <- c(ratio_p, ratio_az)
names(ratios)[length(ratios)] <- treatment_state

# 4) Sort the ratios
ratios_sorted <- sort(ratios)

# 5) Build a lookup of state_id → lowercase name, then capitalize
id_name_map <- unique(data.frame(
  id   = data$state_id,
  name = data$State,
  stringsAsFactors = FALSE
))
id_name_map$name_cap <- sapply(id_name_map$name, function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
})

# 6) Create x-axis labels: map IDs to names, and capitalize 'arizona'
bar_labels <- sapply(names(ratios_sorted), function(nm) {
  if (nm == treatment_state) {
    paste0(toupper(substring(nm, 1, 1)), substring(nm, 2))
  } else {
    id_val <- as.numeric(nm)
    id_name_map$name_cap[id_name_map$id == id_val]
  }
})

# 7) Set bar colors
bar_colors <- ifelse(names(ratios_sorted) == treatment_state, "red", "grey")

# 8) Plot with state names on x-axis
barplot(
  ratios_sorted,
  names.arg = bar_labels,
  col       = bar_colors,
  las       = 2,
  ylab      = "MSPE Ratio (Post / Pre)",
  main      = "Post- vs Pre-Treatment MSPE Ratios",
  cex.names = 0.8
)

# 9) Add reference line for Arizona’s ratio
abline(h = ratio_az, col = "red", lty = 2)

# 10) Add legend
legend(
  "topright",
  legend = c("Treated Arizona", "Placebos"),
  fill   = c("red", "grey"),
  border = NA
)

#── 15) Leave-One-Out Synthetic Control Estimates ──────────────────────────────
looc_donors <- control_ids
time_pts   <- dp.out$tag$time.plot
n_looc     <- length(looc_donors)

# matrix to store each LOOC synthetic series
looc_synth <- matrix(NA, nrow = length(time_pts), ncol = n_looc,
                     dimnames = list(time_pts, as.character(looc_donors)))

for (j in seq_along(looc_donors)) {
  # drop one donor
  drop_id      <- looc_donors[j]
  donors_minus <- setdiff(control_ids, drop_id)
  
  dp_looc <- dataprep(
    foo                   = data,
    predictors            = predictors,
    predictors.op         = "mean",
    time.predictors.prior = pre_treatment,
    dependent             = "SCHIZOFLG_prev",
    unit.variable         = "state_id",
    unit.names.variable   = "State",
    time.variable         = "year",
    treatment.identifier  = treatment_id,
    controls.identifier   = donors_minus,
    time.optimize.ssr     = pre_treatment,
    time.plot             = time_pts
  )
  synth_looc <- synth(dp_looc)
  looc_synth[, j] <- dp_looc$Y0plot %*% synth_looc$solution.w
}

# full-set synthetic and actual
full_synth <- dp.out$Y0plot %*% synth.res$solution.w
actual     <- dp.out$Y1plot[,1]

# plot
plot(time_pts, actual,
     type = "l", lwd = 2, col = "black",
     xlab = "Year", ylab = "SCHIZOFLG_prev")
# each LOOC fit in grey
apply(looc_synth, 2, function(z) lines(time_pts, z, col = "grey", lty = 1))
# full donor‐set fit as dashed
lines(time_pts, full_synth, lty = 2, lwd = 2, col = "black")
# vertical line at treatment
abline(v = max(pre_treatment), lty = 2, col = "red")

legend("bottomleft",
       legend = c("Treated Arizona", "Synthetic Arizona", "LOO Synthetic Control Estimates"),
       col    = c("black",   "black",         "grey"),
       lty    = c(1,         2,               1),
       lwd    = c(2,         2,               1),
       bty    = "n")

#── 16) % difference between treated & synthetic Arizona in post-treatment ────

# Compute synthetic series for Arizona
synth_y_az <- dp.out$Y0plot %*% synth.res$solution.w
actual_az  <- dp.out$Y1plot[,1]

# Locate 2021 and 2022 in the time.plot vector
year_idx <- match(c(2021, 2022), dp.out$tag$time.plot)

# Extract actual and synthetic values for those years
actual_vals <- actual_az[year_idx]
synth_vals  <- synth_y_az[year_idx]

# Compute percentage difference: (Actual − Synthetic) / Synthetic × 100
pct_diff <- (actual_vals - synth_vals) / synth_vals * 100
names(pct_diff) <- paste0("pct_diff_", c("2021", "2022"))

# Display the results
print(pct_diff)

#── 17) Cohen’s d for treated vs. synthetic Arizona ───────────────────────────

# Compute the difference series for all time points
diff_all <- actual_az - synth_y_az

# Calculate the mean and standard deviation of the differences
mean_diff <- mean(diff_all, na.rm = TRUE)
sd_diff   <- sd(diff_all,   na.rm = TRUE)

# Compute Cohen’s d (paired) as mean difference divided by SD of differences
cohens_d_all <- mean_diff / sd_diff

# Display the result
print(cohens_d_all)