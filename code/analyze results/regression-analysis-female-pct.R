# === Load Libraries ===
library(tidycensus)
library(tidyverse)
library(glue)
library(ggrepel)
library(scales)
library(broom)

years_to_run <- c("2008", "2012", "2016", "2020", "2024")
acs_year <- 2022
predicted_path <- "output"
dir.create("plots", showWarnings = FALSE)

acs_df <- read_csv("../data/census/state_level_acs_summary_2022.csv", show_col_types = FALSE)

sex_pct <- acs_df %>%
  select(state, pct_female)

all_results <- list()
model_stats <- list()

for (year in years_to_run) {
  message(glue("📊 Processing {year}..."))
  
  file <- list.files(predicted_path, pattern = glue("all_state_predictions_{year}.*\\.csv$"), full.names = TRUE)[1]
  
  predicted_df <- read_csv(file, show_col_types = FALSE) %>%
    transmute(
      state = state,
      predicted = ifelse(vote_share > 1, vote_share / 100, vote_share),
      year = as.character(year)
    )
  
  merged_df <- left_join(predicted_df, sex_pct, by = "state") %>%
    mutate(region = state.region[match(state, state.abb)]) %>%
    drop_na()
  
  all_results[[year]] <- merged_df
  
  model <- lm(predicted ~ pct_female, data = merged_df)
  pval <- summary(model)$coefficients["pct_female", "Pr(>|t|)"]
  model_stats[[year]] <- tibble(year = year, p_value = pval)
}

combined_all <- bind_rows(all_results) %>%
  mutate(year = factor(year, levels = years_to_run))

model_stats_df <- bind_rows(model_stats)

facet_labels <- model_stats_df %>%
  mutate(
    year = factor(year, levels = years_to_run),
    label = glue("p = {pvalue(p_value, accuracy = 0.001)}")
  )

facet_plot <- ggplot(combined_all, aes(x = pct_female, y = predicted, color = region)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1, show.legend = FALSE) +
  facet_wrap(~ year) +
  geom_text(
    data = facet_labels,
    mapping = aes(x = 48, y = 0.25, label = label),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(
    title = "Predicted Democratic Vote Share vs. % Female Population",
    subtitle = glue("Colored by Region • ACS {acs_year} 5-Year Estimates"),
    x = "Percent Female (ACS)",
    y = "Predicted Democratic Vote Share",
    color = "Region"
  ) +
  theme_minimal(base_size = 14)

ggsave("../plots/facet_female_vs_predicted.png", plot = facet_plot, width = 12, height = 7)
