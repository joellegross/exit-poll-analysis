library(tidyverse)
library(knitr)


metrics_dir <- "../output"  
metric_files <- list.files(metrics_dir, pattern = "mrp_metrics_\\d{4}\\.csv$", full.names = TRUE)

metrics_df <- metric_files %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

metrics_df <- metrics_df %>%
  mutate(across(c(MAE, RMSE, R2), ~ round(.x, 3)))

kable(metrics_df, format = "latex", digits = 3, booktabs = TRUE,
      caption = "Model Evaluation Metrics by Year")
