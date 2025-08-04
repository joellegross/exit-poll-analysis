library(tidyverse)
library(usmap)
library(ggplot2)
library(readr)
library(glue)

plot_mrp_predictions <- function(year) {
  preds_file <- glue("../output/all_state_predictions_{year}.csv")
  truth_file <- glue("../data/{year}_pres_dem_share_by_state.csv")
  bar_plot_file <- glue("../plots/mrp_barplot_{year}.png")
  map_plot_file <- glue("../plots/mrp_map_{year}.png")
  
  preds <- read_csv(preds_file, show_col_types = FALSE)
  truths <- read_csv(truth_file, show_col_types = FALSE)
  if(year == "2024"){
    truths$dem_share <- truths$dem_share/100
    truths$rep_share <- truths$rep_share/100
  }
  merged <- preds %>%
    left_join(truths, by = "state")
  
  
  bar_plot <- ggplot(merged, aes(x = reorder(state, vote_share))) +
    geom_col(aes(y = vote_share, fill = "Predicted"), position = "dodge", width = 0.6) +
    geom_col(aes(y = dem_share, fill = "Actual"), position = "dodge", width = 0.6) +
    coord_flip() +
    labs(
      title = glue("Predicted vs Actual Democratic Vote Share by State ({year})"),
      x = "State",
      y = "Democratic Vote Share",
      fill = ""
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("Predicted" = "skyblue", "Actual" = "navy")) +
    theme_minimal()
  
  ggsave(bar_plot_file, plot = bar_plot, width = 8, height = 10, dpi = 300)
  message(glue("Saved bar plot to {bar_plot_file}"))
  
  preds <- preds %>%
    left_join(truths, by = "state") %>%
    mutate(
      predicted_winner = if_else(vote_share >= rep_share, "Democrat", "Republican"),
      actual_winner = if_else(dem_share >= rep_share, "Democrat", "Republican"),
      correct = predicted_winner == actual_winner,
      fill_group = case_when(
        predicted_winner == "Democrat" & correct ~ "Correct Democrat",
        predicted_winner == "Democrat" & !correct ~ "Incorrect Democrat",
        predicted_winner == "Republican" & correct ~ "Correct Republican",
        predicted_winner == "Republican" & !correct ~ "Incorrect Republican"
      )
    )
  
  map_plot <- plot_usmap(data = preds, values = "fill_group", regions = "states") +
    scale_fill_manual(
      name = "Prediction",
      values = c(
        "Correct Democrat" = "#0000FF",       # dark blue
        "Incorrect Democrat" = "#bdd7e7",     # light blue
        "Correct Republican" = "#FF0000",     # dark red
        "Incorrect Republican" = "#fcbba1"    # light red
      ), na.translate = FALSE
    ) +
    labs(title = glue("Predicted {year} Winner")) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 14)
    )
  
  ggsave(map_plot_file, plot = map_plot, width = 8, height = 6, dpi = 300)
  message(glue("🗺️  Saved map plot to {map_plot_file}"))
}

for (year in list("2024", "2020", "2016", "2012", "2008")){
    plot_mrp_predictions(year)
}

