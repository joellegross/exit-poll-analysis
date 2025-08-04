# === Load Required Libraries ===
library(tidyverse)
library(rvest)

# ================================
# === 2020 PRESIDENTIAL RESULTS ===
# ================================
election_data <- read_csv(
  "data/1976-2020-president.csv",
  col_types = cols(
    year = col_integer(),
    office = col_character(),
    state_po = col_character(),
    candidatevotes = col_double(),
    party_detailed = col_character(),
    .default = col_guess()
  )
)

years = list("2020", "2016","2012", "2008", "2004")
# Filter 2020 general election for President
  
for (year_var in years){  
pres_prev <- election_data %>%
  filter(year == year_var, office == "US PRESIDENT", !is.na(candidatevotes))

# Summarize total and Democratic votes per state
total_votes_prev <- pres_prev %>%
  group_by(state_po) %>%
  summarise(total_votes = sum(candidatevotes), .groups = "drop")

dem_votes_prev <- pres_prev %>%
  filter(party_detailed == "DEMOCRAT") %>%
  group_by(state_po) %>%
  summarise(dem_votes = sum(candidatevotes), .groups = "drop")

rep_votes_prev <- pres_prev %>%
  filter(party_detailed == "REPUBLICAN") %>%
  group_by(state_po) %>%
  summarise(rep_votes = sum(candidatevotes), .groups = "drop")

# Merge and calculate Democratic vote share
state_vote_share_prev_year <- left_join(dem_votes_prev, total_votes_prev, by = "state_po") %>%
  left_join(rep_votes_prev, by ="state_po")  %>%
  mutate(
    dem_share = dem_votes / total_votes,
    rep_share = rep_votes / total_votes
  ) %>%
  rename(state = state_po) %>%
  arrange(desc(dem_share))

# Save results
  write_csv(state_vote_share_prev_year, paste0("data/", year_var, "_pres_dem_share_by_state.csv"))

  # Preview
  print(paste0("completed:", year_var))

}
# ================================
# === 2024 PRESIDENTIAL RESULTS ===
# ================================

# Wikipedia results page
url <- "https://en.wikipedia.org/wiki/2024_United_States_presidential_election#Results"
page <- read_html(url)

# Extract all wikitable elements
tables <- page %>% html_nodes("table.wikitable")

# Find the correct results table (the one with "Alabama" in column 1)
table_index <- which(sapply(tables, function(tbl) any(grepl("Alabama", html_text(tbl)))))
results_table <- html_table(tables[[table_index]], fill = TRUE)

# Select and clean relevant columns (adjust column numbers as needed)
results_clean <- results_table %>%
  select(State = 1, Harris_pct = 6, Trump_pct = 3) %>%
  mutate(
    Harris_pct = as.numeric(gsub("%", "", Harris_pct)),
    Trump_pct = as.numeric(gsub("%", "", Trump_pct)),
    State = str_remove(State, "\\[.*?\\]") %>% str_trim()
  ) %>%
  filter(
    !is.na(State),
  ) %>%
  mutate(dem_share = Harris_pct) %>%
  mutate(rep_share = Trump_pct) %>%
  select(State, dem_share, rep_share)

state_full <- c(state.name, "District of Columbia")
state_abbr <- c(state.abb, "DC")

results_clean <- results_clean %>%
  mutate(
    State = State %>% 
      str_remove("\\[.*?\\]") %>%  
      str_trim()                   
  ) %>%
  mutate(
    state = state_abbr[match(State, state_full)]
  ) %>%
  filter(!is.na(state)) %>%
  select(state, dem_share, rep_share)

# Save results
write_csv(results_clean, "data/2024_pres_dem_share_by_state.csv")

# Preview
print(results_clean)