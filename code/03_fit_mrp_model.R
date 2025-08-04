library(rstanarm)
library(tidyverse)

year = 2008
prev_year = "2004"
exit_data <- read_csv(paste0("../data/exit_poll_combined_", year,".csv"))

prev_vote <- read_csv(paste0("../data/census/", prev_year, "_pres_dem_share_by_state.csv"))

exit_data <- exit_data %>%
  left_join(prev_vote %>% rename(prev_vote_share = dem_share), by = "state")

age_levels  <- c("18-29", "30-44", "45-65", "65+")
sex_levels  <- c("Male", "Female")
race_levels <- c("White", "Black", "Hispanic/Latino", "Asian", "Other")
educ_levels <- c("College graduate", "No college degree")


exit_data <- exit_data %>%
  filter(!is.na(vote), !is.na(prev_vote_share)) %>%
  mutate(
    age   = factor(age, levels = age_levels),
    sex   = factor(sex, levels = sex_levels),
    race  = factor(race, levels = race_levels),
    educ  = factor(educ, levels = educ_levels)
  )

fit <- stan_glmer(
  vote_dem ~ age + sex + race + educ + prev_vote_share + (1 | state),
  data = exit_data,
  family = binomial(link = "logit"),
  weights = weight,
  chains = 4,
  warmup = 1000,
  iter = 4000,
  cores = 4,
  refresh = 1
)

# === Save model ===
saveRDS(fit, paste0("../models/mrp_model_with_prev_vote_", year, ".rds"))