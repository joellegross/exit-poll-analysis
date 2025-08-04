# === Libraries ===
library(tidyverse)
library(glue)
library(broom)
library(stringr)
library(knitr)
library(kableExtra)

# === Global container for regression p-values ===
regression_pvals <- tibble()

# === Function: Analyze MRP Accuracy and Run Regression ===
analyze_mrp_accuracy <- function(year, exit_poll_states) {
  truth_file <- glue("data/{year}_pres_dem_share_by_state.csv")
  preds_file <- glue("output/all_state_predictions_{year}.csv")
  output_csv <- glue("output/mrp_regression_results_{year}.csv")
  
  truth <- read_csv(truth_file, show_col_types = FALSE) %>%
    rename(state_abbr = state, actual_dem = dem_share)
  
  preds <- read_csv(preds_file, show_col_types = FALSE) %>%
    rename(state_abbr = state, predicted_dem = vote_share)
  
  if (year == "2024") {
    truth <- truth %>%
      mutate(actual_dem = if_else(actual_dem > 1, actual_dem / 100, actual_dem))
  }
  
  comparison <- truth %>%
    inner_join(preds, by = "state_abbr") %>%
    mutate(
      actual_scaled = if_else(actual_dem > 1, actual_dem / 100, actual_dem),
      abs_error = abs(predicted_dem - actual_scaled),
      squared_error = (predicted_dem - actual_scaled)^2,
      exit_poll_conducted = state_abbr %in% exit_poll_states
    )
  
  # === Summary metrics ===
  mae <- mean(comparison$abs_error)
  rmse <- sqrt(mean(comparison$squared_error))
  r2 <- cor(comparison$predicted_dem, comparison$actual_scaled)^2
  
  cat(glue("\n📊 MRP Accuracy Metrics for {year}:\n"))
  cat(glue("• MAE  = {round(mae, 3)}\n"))
  cat(glue("• RMSE = {round(rmse, 3)}\n"))
  cat(glue("• R²    = {round(r2, 3)}\n"))
  
  # === Regression: Exit Poll → Absolute Error ===
  model <- lm(abs_error ~ exit_poll_conducted, data = comparison)
  model_summary <- summary(model)
  
  # Safely extract coefficient table
  coefs <- coef(model_summary)
  
  # Check if the variable exists in the model
  if ("exit_poll_conductedTRUE" %in% rownames(coefs)) {
    p_val <- coefs["exit_poll_conductedTRUE", "Pr(>|t|)"]
  } else {
    p_val <- NA_real_
    warning(glue("⚠️ 'exit_poll_conducted' has no variation in {year} — skipping p-value."))
  }
  
  # Append to global results
  regression_pvals <<- bind_rows(regression_pvals, tibble(
    year = year,
    p_value = p_val
  ))
  
  cat("\n🧪 Regression Results:\n")
  print(model_summary)
  
  write_csv(tidy(model), output_csv)
  cat(glue("\n✅ Regression results saved to {output_csv}\n"))
  
  return(comparison)
}

# === Get Exit Poll States for a Year ===
get_exit_poll_states <- function(year) {
  csv_files <- list.files(
    path = file.path("/Users/joellegr/Documents/DATS /Classes/Thesis:Practicum/exit poll project/roper", year, "General", "State"),
    pattern = "\\.csv$",
    full.names = TRUE
  )
  state_abbrs <- str_extract(basename(csv_files), "^[A-Z]{2}")
  return(sort(unique(state_abbrs)))
}

# === Run MRP Analysis for All Years ===
years <- c("2008", "2012", "2016", "2020", "2024")

for (yr in years) {
  state_list <- get_exit_poll_states(yr)
  cat(glue("\n📅 Exit poll states for {yr}: {paste(state_list, collapse = ', ')}\n"))
  
  comparison_df <- analyze_mrp_accuracy(year = yr, exit_poll_states = state_list)
  
  write_csv(comparison_df, glue("output/mrp_regression_results_{yr}.csv"))
  message(glue("✅ Results saved to output/mrp_regression_results_{yr}.csv"))
}

# === Display Regression Summary Table ===
kable(regression_pvals, format = "latex", booktabs = TRUE, digits = 4,
      caption = "P-values for Exit Poll Effect on MRP Absolute Error by Year") %>%
  kable_styling(latex_options = c("hold_position"))

# === Build Exit Poll Tracker ===
exit_poll_tracker <- tibble()
for (yr in years) {
  state_list <- get_exit_poll_states(yr)
  temp <- tibble(
    year = yr,
    state_abbr = state_list,
    exit_poll_conducted = TRUE
  )
  exit_poll_tracker <- bind_rows(exit_poll_tracker, temp)
}

write_csv(exit_poll_tracker, "output/exit_poll_conducted_by_state.csv")
message("✅ Saved exit poll tracker to output/exit_poll_conducted_by_state.csv")

# === Summarize and Display Exit Poll States Table ===
exit_poll_data <- read_csv("output/exit_poll_conducted_by_state.csv", show_col_types = FALSE)

state_lists <- exit_poll_data %>%
  group_by(year) %>%
  summarise(states = paste(sort(state_abbr), collapse = ", ")) %>%
  arrange(year)

kable(state_lists, format = "latex", booktabs = TRUE,
      col.names = c("Year", "States with Exit Polls"),
      caption = "States with Exit Polls by Year") %>%
  kable_styling(latex_options = c("hold_position"))