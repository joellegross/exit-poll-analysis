library(tidyverse)
library(knitr)


# Set your directory path
metrics_dir <- "/Users/joellegr/Documents/DATS /Classes/Thesis:Practicum/exit poll project/exit poll project analysis/plots/"  # ← change this to your actual path

# List all CSV files in the directory that contain metrics
metric_files <- list.files(metrics_dir, pattern = "mrp_metrics_\\d{4}\\.csv$", full.names = TRUE)

# Read and combine all files
metrics_df <- metric_files %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# Optional: Round values
metrics_df <- metrics_df %>%
  mutate(across(c(MAE, RMSE, R2), ~ round(.x, 3)))

kable(metrics_df, format = "latex", digits = 3, booktabs = TRUE,
      caption = "Model Evaluation Metrics by Year")
