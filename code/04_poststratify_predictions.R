library(tidyverse)
library(rstanarm)
library(glue)

year <- "2008"
year_prev <- "2004"
n_draws <- 2000
chunk_size <- 5000

fit <- readRDS(paste0("../models/mrp_model_with_prev_vote_", year, ".rds"))
prev_vote <- read_csv(paste0("../data/past results/", year_prev, "_pres_dem_share_by_state.csv"), show_col_types = FALSE) %>%
  rename(prev_vote_share = dem_share)

poststrat_files <- list.files(paste0("data/", year, "/"), pattern = "^poststrat_frame_.*\\.csv$", full.names = TRUE)

for (file in poststrat_files) {
  poststrat <- read_csv(file, show_col_types = FALSE)
  
  state_name <- unique(poststrat$state)
  vote_row <- prev_vote %>% filter(state == state_name)
  
  if (length(state_name) != 1 || nrow(vote_row) == 0) {
    warning(glue("⚠️ Skipping {state_name} — invalid state name or no prior vote share."))
    next
  }
  
  poststrat <- poststrat %>%
    mutate(
      prev_vote_share = vote_row$prev_vote_share,
      row_id = row_number()  
    )
  
  poststrat$weight <- poststrat$weight / sum(poststrat$weight)
  
  poststrat <- poststrat %>%
    mutate(chunk_id = ceiling(row_number() / chunk_size))
  
  posterior_vote_share <- rep(0, n_draws)
  all_pred_probs <- numeric(nrow(poststrat))
  
  for (chunk in unique(poststrat$chunk_id)) {
    chunk_data <- poststrat %>% filter(chunk_id == chunk)
    
    pred_draws <- posterior_epred(fit, newdata = chunk_data, re.form = NA, draws = n_draws)
    
    col_means <- colMeans(pred_draws)
    all_pred_probs[chunk_data$row_id] <- col_means
    
    weighted_draws <- sweep(pred_draws, 2, chunk_data$weight, `*`)
    posterior_vote_share <- posterior_vote_share + rowSums(weighted_draws)
  }
  
  point_estimate <- sum(all_pred_probs * poststrat$weight)
  ci <- quantile(posterior_vote_share, probs = c(0.025, 0.5, 0.975))
  
  res <- tibble(
    state = state_name,
    vote_share = point_estimate,
    lower_95 = ci[1],
    median = ci[2],
    upper_95 = ci[3]
  )
  
  out_path <- file.path("output", as.character(year))
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  write_csv(res, file.path(out_path, str_replace(basename(file), "poststrat_frame", "state_pred")))
  
  rm(poststrat, all_weighted_draws, pred_draws)
  gc(verbose = FALSE)
}

result_files <- list.files(
  file.path("output", as.character(year)),
  pattern = "^state_pred_.*\\.csv$",
  full.names = TRUE
)

state_predictions <- result_files %>%
  purrr::map_dfr(read_csv, show_col_types = FALSE)

write_csv(state_predictions, paste0("../output/all_state_predictions_", year, ".csv"))
message(glue("Saved combined predictions to output/all_state_predictions_{year}.csv"))







