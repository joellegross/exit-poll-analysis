library(tidyverse)
library(readr)
library(scales)
library(glue)

evaluate_mrp <- function(year) {
  # === File paths ===
  truth_file <- glue("data/{year}_pres_dem_share_by_state.csv")
  preds_file <- glue("output/all_state_predictions_{year}.csv")
  plot_file <- glue("plots/mrp_vs_truth_plot_{year}.png")
  metrics_file <- glue("plots/mrp_metrics_{year}.csv")
  
  # === Load data ===
  truth <- read_csv(truth_file, show_col_types = FALSE)   # expected: state_abbr, dem_pct
  preds <- read_csv(preds_file, show_col_types = FALSE) 
  
  if (year == "2024") {
    truth <- truth %>%
      mutate(dem_share = if_else(dem_share > 1, dem_share / 100, dem_share))
  }# expected: state, vote_share
  
  # === Clean and join ===
  comparison <- truth %>%
    rename(state_abbr = state, actual_dem = dem_share) %>%
    inner_join(
      preds %>% rename(state_abbr = state, predicted_dem = vote_share),
      by = "state_abbr"
    )
  
  if (nrow(comparison) == 0) {
    stop(glue("❌ No overlapping states found between truth and predictions for {year}"))
  }
  
  # === Compute errors ===
  comparison <- comparison %>%
    mutate(
      actual_scaled = actual_dem,
      abs_error = abs(predicted_dem - actual_scaled),
      squared_error = (predicted_dem - actual_scaled)^2
    )
  
  # === Summary metrics ===
  mae <- mean(comparison$abs_error)
  rmse <- sqrt(mean(comparison$squared_error))
  r2 <- cor(comparison$predicted_dem, comparison$actual_scaled)^2
  
  # === Save metrics ===
  metrics <- tibble(
    year = year,
    MAE = mae,
    RMSE = rmse,
    R2 = r2
  )
  write_csv(metrics, metrics_file)
  
  # === Output to console ===
  cat(glue("📊 Model Evaluation Metrics for {year}:\n"))
  cat(glue("• Mean Absolute Error (MAE): {round(mae, 3)}\n"))
  cat(glue("• Root Mean Square Error (RMSE): {round(rmse, 3)}\n"))
  cat(glue("• R-squared (Correlation^2): {round(r2, 3)}\n"))
  
  # === Plot prediction vs actual ===
  p <- ggplot(comparison, aes(x = actual_scaled, y = predicted_dem, label = state_abbr)) +
    geom_point(color = "steelblue", size = 2) +
    geom_text(nudge_y = 0.01, size = 5) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.2, 0.8)) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.2, 0.8)) +
    labs(
      x = glue("Actual Democratic Vote Share ({year})"),
      y = "Predicted Vote Share (MRP Model)",
      title = glue("MRP Model Accuracy vs. {year} Results")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12)
    )
  
  ggsave(plot_file, plot = p, width = 7, height = 5, dpi = 300)
}

# === Run for 2024 or any other year ===
for (year in list("2008", "2012", "2024", "2020", "2016")){
  evaluate_mrp(year)
}
