library(tidyverse)
library(ggplot2)


actual_path <- "../data/past results"
predicted_path <- "../output"  
actual_files <- list.files(actual_path, pattern = "_pres_dem_share_by_state\\.csv$", full.names = TRUE)

actual_df <- map_dfr(actual_files, function(file) {
  year <- str_extract(file, "\\d{4}")
  read_csv(file) %>%
    mutate(year = year)
})

predicted_files <- list.files(predicted_path, pattern = "all_state_predictions_\\d{4}.*\\.csv$", full.names = TRUE)

predicted_df <- map_dfr(predicted_files, function(file) {
  year <- str_extract(file, "\\d{4}")
  read_csv(file) %>%
    mutate(year = year)
})

actual_df <- map_dfr(actual_files, function(file) {
  year <- str_extract(file, "\\d{4}")
  read_csv(file) %>%
    mutate(year = year)
              }) %>%
  rename(state = state, actual = dem_share) %>%
  filter(year != "2004")

predicted_df <- predicted_df %>%
  rename(state = state, predicted = vote_share)  
combined_df <- left_join(predicted_df, actual_df, by = c("state", "year"))

combined_df <- combined_df %>%
  mutate(
    predicted = ifelse(year == "2024" & predicted > 1, predicted / 100, predicted),
    actual    = ifelse(actual > 1, actual / 100, actual),  # optional safeguard
    error     = predicted - actual,
    abs_error = abs(error)
  )

combined_df <- combined_df %>%
  mutate(error = predicted - actual,
         abs_error = abs(error),
         year = factor(year))

combined_df <- combined_df %>%
  mutate(year = relevel(factor(year), ref = "2020"))

model <- lm(abs_error ~ year, data = combined_df)
summary(model)

anova(model)

combined_df <- combined_df %>%
  mutate(year = factor(year, levels = sort(unique(as.numeric(as.character(year)))))
  )

p1 <- ggplot(combined_df, aes(x = year, y = abs_error)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Prediction Error by Year",
    subtitle = "Absolute difference between predicted and actual Democratic vote share",
    x = "Election Year",
    y = "Absolute Error"
  ) +
  theme_minimal(base_size = 14)

ggsave("../plots/abs_error_by_year_boxplot.png", plot = p1, width = 8, height = 5)

