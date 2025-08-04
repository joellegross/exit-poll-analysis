library(tidyverse)

## specify year here
year = "2024"
exit_files <- list.files(
  paste0("../data/exit polls/", year),
  full.names = TRUE
)

exit_list <- list()

for (file in exit_files) {
  message("Processing file: ", file)
  
  exit_poll_file <- read_csv(file, show_col_types = FALSE) %>%
    rename_with(~ toupper(str_trim(.)))

  weight_var <- names(exit_poll_file)[str_detect(names(exit_poll_file), "WGHT|WEIGHT")] %>% first()
  race_var <- names(exit_poll_file)[names(exit_poll_file) %in% c("RACE", "QRACEAI")] %>% first()

  is_national <- str_detect(file, "National")
  year_suffix <- substr(year, nchar(year)-1, nchar(year))
  vote_var <- grep(paste0("^PRS[A-Z]{2}", year_suffix, "$"), names(exit_poll_file), value = TRUE)
  if(is_national){
    vote_var <- "PRES"
    if (year == "2008"){
      vote_var <- "PRSUS08"
    }
    }
  
  if (year == "2020"){
    dem_cand<-"JOE BIDEN"
  }
  else if (year == 2024){
    dem_cand<-"KAMALA HARRIS"
  }
  else if (year == 2016) {
    dem_cand<-"HILLARY CLINTON"
  }
  else if (year == 2012 || year == 2008) {
    dem_cand<-"BARACK OBAMA"
  }
  
  # Skip if vote_var not found
  if (is.null(vote_var) || length(vote_var) == 0) {
    message("Vote variable not found, skipping: ", file)
    next
  }
  print(vote_var)
  exit_poll_file <- exit_poll_file %>%
    rename(
      age = "AGE",
      race = race_var,
      sex = "SEX",
      educ = "EDUCCOLL",
      vote = vote_var,
      weight = all_of(weight_var)
    ) %>%
    mutate(
      vote = toupper(vote),
      vote_dem = if_else(vote == dem_cand, 1, 0),
      state = if (is_national) {
        state.abb[match(toupper(STANUM), toupper(state.name))]
      } else {
        str_extract(basename(file), "^[A-Z]{2}")
      },
      GEOCODE = as.character("GEOCODE")
    )
  

  exit_list[[file]] <- exit_poll_file
}

cur_fle<-exit_list[[file]]

exit_combined <- bind_rows(exit_list)


write_csv(exit_combined, paste0("../data/exit polls/combined/exit_poll_combined_", year, ".csv"))

