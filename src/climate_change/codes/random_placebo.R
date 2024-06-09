#======================================================================================================================#
### Packages
#======================================================================================================================#
library(tidyverse)
# library(statar)
library(fixest)
# library(did)
# library(car)
library(ggplot2)
#======================================================================================================================#
## Working Directory
#======================================================================================================================#
setwd(dir = "C:/Users/david/OneDrive/Documents/ULMS/PhD/")
#======================================================================================================================#
### Loading Data
#======================================================================================================================#
file <- "./Data_PhD/US/BLS/onsite/triQc_on.rds"
triQc <- read_rds(file = file)
set.seed(seed = 12345)
#======================================================================================================================#
### Randomizing treatment assignments---Temporal
#======================================================================================================================#
randomize_treatment_temporal <- function(df) {
  # Define the function to randomize ch.year except Inf values
  # Identify rows with and without Inf values
  inf_indices <- which(is.infinite(df$ch.year))
  non_inf_indices <- which(!is.infinite(df$ch.year))

  # Randomize the non-Inf values
  randomized_non_inf_values <- sample(df$ch.year[non_inf_indices], replace = T)

  # Create a new ch.year column and fill with randomized values
  df$random.ch.year <- df$ch.year
  df$random.ch.year[non_inf_indices] <- randomized_non_inf_values

  return(df)
}

triQc <- randomize_treatment_temporal(df = triQc)
table(triQc$year, triQc$ch.year)
table(triQc$year, triQc$random.ch.year)

calculate_placebo_effect <- function(df, dep_var, random, FEs) {
  # Construct the formula
  formula <- as.formula(paste(dep_var, " ~ sunab(", random, ", year) +
    gdppc.1 + annual.avg.estabs.1 + cpi.1 + federal.facility +
    produced.chem.facility + imported.chem.facility +
    chemical.formulation.component + chemical.article.component +
    chemical.manufacturing.aid + chemical.ancilliary.use +
    production.ratio.activity.index + maxnum.chem.onsite +
    clean.air.act.chems + hap.chems + pbt.chems | ", FEs))

  # Fit the model
  model <- feols(formula, data = df, cluster = ~chemical.id + naics.code + facility.state)

  # Extract the coefficient of interest
  summary_model <- summary(model, agg = "ATT", digits = 3, digits.stats = 3)
  model_att <- coef(summary_model)[1]
  p_value_att <- summary_model$coeftable[1, "Pr(>|t|)"]

  # Return the results as a list
  return(list(model_att = model_att, p_value_att = p_value_att))
}

# Calculate placebo effects
calculate_placebo_effect(
  df = triQc,
  dep_var = "l.total.releases.onsite.intensity",
  random = "random.ch.year",
  FEs = "year + facility.id.fe + border.county.fe + chemical.id.fe + chemical.year.fe"
)

start_time <- Sys.time()
# Calculate placebo effects
n_replicates <- 500
placebo_results <- replicate(n_replicates, {
  randomized_data <- randomize_treatment_temporal(df = triQc)
  calculate_placebo_effect(
    df = randomized_data,
    dep_var = "l.total.releases.onsite.intensity",
    random = "random.ch.year",
    FEs = "year + facility.id.fe + border.county.fe + chemical.id.fe + chemical.year.fe"
  )
}, simplify = FALSE)
end_time <- Sys.time()
end_time - start_time

# Extract estimates and p-values
placebo_estimates <- sapply(placebo_results, function(x) x$model_att)
placebo_p_values <- sapply(placebo_results, function(x) x$p_value_att)
sort(unique(placebo_estimates))
sort(unique(placebo_p_values))

# Calculate the number of p-values below thresholds
p_values_below_0_01 <- sum(placebo_p_values < 0.01)
p_values_below_0_05 <- sum(placebo_p_values < 0.05)
p_values_below_0_10 <- sum(placebo_p_values < 0.10)

# Print the counts of p-values below the thresholds
cat("Number of p-values < 0.01:", p_values_below_0_01, "\n")
cat("Number of p-values < 0.05:", p_values_below_0_05, "\n")
cat("Number of p-values < 0.10:", p_values_below_0_10, "\n")
#======================================================================================================================#
### Randomizing treatment assignments---Spatial
#======================================================================================================================#
randomize_treatment_spatial <- function(df, temp_var) {
  df %>%
    group_by(df$temp_var) %>%
    mutate(
      random.state = sample(x = treated.match, replace = T),
      random.county.id = sample(x = treated.cluster.id, replace = T),
      random.treated.state = ifelse(test = year >= ch.year, yes = 1, no = 0),
      # Getting the FEs while keeping the control groups unchanged
      random.border.state.fe = as.numeric(as.factor(random.state)) * as.numeric(as.factor(control.match)),
      random.border.state.year.fe = random.border.state.fe * year,
      random.border.county.fe = as.numeric(as.factor(random.county.id)) * as.numeric(as.factor(control.cluster.id)),
      random.border.county.year.fe = random.border.county.fe * year
    ) %>%
    ungroup()
}

triQc <- randomize_treatment_spatial(df = triQc, temp_var = ~ch.year)
table(triQc$random.state, triQc$ch.year)

#======================================================================================================================#
### Randomizing treatment assignments---Temporal and Spatial
#======================================================================================================================#
randomize_treatment <- function(df, spatial_var, temp_var) {
  df %>%
    group_by(df$spatial_var) %>%
    mutate(
      random.ch.year = sample(x = ch.year, replace = T),
      random.treated.time = ifelse(test = year >= random.ch.year, yes = 1, no = 0)
    ) %>%
    ungroup()

  df %>%
    group_by(df$temp_var) %>%
    mutate(
      random.state = sample(x = treated.match, replace = T),
      random.county.id = sample(x = treated.cluster.id, replace = T),
      random.treated.state = ifelse(test = year >= ch.year, yes = 1, no = 0),
      # Getting the FEs while keeping the control groups unchanged
      random.border.state.fe = as.numeric(as.factor(random.state)) * as.numeric(as.factor(control.match)),
      random.border.state.year.fe = random.border.state.fe * year,
      random.border.county.fe = as.numeric(as.factor(random.county.id)) * as.numeric(as.factor(control.cluster.id)),
      random.border.county.year.fe = random.border.county.fe * year
    ) %>%
    ungroup()
}

triQc <- randomize_treatment(df = triQc, spatial_var = ~facility.state, temp_var = ~ch.year)
table(triQc$year, triQc$random.ch.year)
#======================================================================================================================#
### Run randomisation and collect placebo effects
#======================================================================================================================#
# Simulate example data
n <- 1000
df_ex <- data.frame(
  county_id = rep(1:10, each = 100),
  year = rep(2011:2020, times = 100),
  ch.year = sample(c(2014, 2015, 2017, Inf), n, replace = TRUE),
  l.total.releases.onsite.intensity = rnorm(n),
  gdppc.1 = rnorm(n),
  annual.avg.estabs.1 = rnorm(n),
  cpi.1 = rnorm(n),
  federal.facility = sample(0:1, n, replace = TRUE),
  produced.chem.facility = sample(0:1, n, replace = TRUE),
  imported.chem.facility = sample(0:1, n, replace = TRUE),
  chemical.formulation.component = sample(0:1, n, replace = TRUE),
  chemical.article.component = sample(0:1, n, replace = TRUE),
  chemical.manufacturing.aid = sample(0:1, n, replace = TRUE),
  chemical.ancilliary.use = sample(0:1, n, replace = TRUE),
  production.ratio.activity.index = rnorm(n),
  maxnum.chem.onsite = rnorm(n),
  clean.air.act.chems = rnorm(n),
  hap.chems = rnorm(n),
  pbt.chems = rnorm(n),
  chemical.id = sample(1:100, n, replace = TRUE),
  naics.code = sample(1:20, n, replace = TRUE),
  facility.state = sample(1:50, n, replace = TRUE),
  facility.id.fe = sample(1:200, n, replace = TRUE),
  border.county.fe = sample(1:50, n, replace = TRUE),
  chemical.year.fe = sample(1:200, n, replace = TRUE)
)

n_placebos <- 1000
for (i in 1:n_placebos) {
  randomized_data <- randomize_treatment_temporal(df = df_ex)
  placebo_effects[i] <- calculate_placebo_effect(
    df = randomized_data,
    dep_var = "l.total.releases.onsite.intensity",
    random = "random.ch.year",
    FEs = "year + facility.id.fe + border.county.fe + chemical.year.fe"
  )[1]
}