library(tidycensus)
library(tidyverse)

# Set ACS year
acs_year <- 2022

# === STEP 1: Load ACS Summary Data (one call) ===
acs_vars <- c(
  "B01001_001",  # Total population
  "B01001_026",  # Female population
  "B02001_002",  # White alone
  "B02001_003",  # Black alone
  "B15003_001",  # Education base: population 25+
  "B15003_022",  # Bachelor's
  "B15003_023",  # Master's
  "B15003_024",  # Professional
  "B15003_025"   # Doctorate
)

acs_data <- get_acs(
  geography = "state",
  variables = acs_vars,
  year = acs_year,
  survey = "acs5",
  output = "wide"
)

# === STEP 2: Load age-by-sex data to compute Voting-Age Population (18+) ===
age_sex_data <- get_acs(
  geography = "state",
  table = "B01001",
  year = acs_year,
  survey = "acs5"
)

vap_data <- age_sex_data %>%
  mutate(age_bucket = as.integer(str_sub(variable, -3))) %>%
  filter(age_bucket >= 8) %>%  # Age 18+ starts at line 8
  group_by(NAME) %>%
  summarise(voting_age_pop = sum(estimate), .groups = "drop") %>%
  rename(state_full = NAME)

# === STEP 3: Calculate Percentages and Join ===
acs_pct <- acs_data %>%
  transmute(
    state_full = NAME,
    pct_female = 100 * B01001_026E / B01001_001E,
    pct_black = 100 * B02001_003E / B01001_001E,
    pct_college = 100 * (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E
  ) %>%
  left_join(vap_data, by = "state_full") %>%
  mutate(
    state = state.abb[match(state_full, state.name)]
  ) %>%
  drop_na(state)

# === STEP 4: Save to CSV ===
dir.create("data", showWarnings = FALSE)

write_csv(acs_pct, file = "data/state_level_acs_summary_2022.csv")

message("✅ Saved ACS summary to data/state_level_acs_summary_2022.csv")