library(tidyverse)
library(tidycensus)
library(glue)

all_states <- state.abb
years = list(2024, 2020)

for (year in years){
  for (state in all_states) {
    message(glue::glue("🔄 Processing {state} in {year}..."))
   
    try({
      pums <- get_pums(
        variables = c("AGEP", "SEX", "SCHL", "RAC1P", "HISP", "PWGTP"),
        state = state,
        survey = "acs5",
        year = year-1,
        recode = TRUE
      )
      
      poststrat <- pums %>%
        filter(AGEP >= 18) %>%
        mutate(
          age = case_when(
            AGEP >= 18 & AGEP <= 29 ~ "18-29",
            AGEP >= 30 & AGEP <= 44 ~ "30-44",
            AGEP >= 45 & AGEP <= 65 ~ "45-65",
            AGEP > 65               ~ "65+",
            TRUE ~ NA_character_
          ),
          sex = case_when(
            SEX_label == "Male" ~ "Male",
            SEX_label == "Female" ~ "Female",
            TRUE ~ NA_character_
          ),
          race = case_when(
            HISP_label != "Not Spanish/Hispanic/Latino" ~ "Hispanic/Latino",
            RAC1P_label == "White alone" ~ "White",
            RAC1P_label == "Black or African American alone" ~ "Black",
            RAC1P_label == "Asian alone" ~ "Asian",
            TRUE ~ "Other"
          ),
          educ = case_when(
            SCHL_label %in% c(
              "Bachelor's degree",
              "Master's degree",
              "Doctorate degree",
              "Professional degree beyond a bachelor's degree"
            ) ~ "College graduate",
            SCHL_label == "N/A (less than 3 years old)" ~ NA_character_,
            TRUE ~ "No college degree"
          ),
          state = state,
          weight = PWGTP
        ) %>%
        select(age, sex, race, educ, state, weight) %>%
        drop_na() %>%
        mutate(
          age   = factor(age, levels = c("18-29", "30-44", "45-65", "65+")),
          sex   = factor(sex, levels = c("Male", "Female")),
          race  = factor(race, levels = c("White", "Black", "Hispanic/Latino", "Asian", "Other")),
          educ  = factor(educ, levels = c("College graduate", "No college degree"))
        )
      
      # Write to CSV
      output_path <- file.path("data/", paste0(year,"/", "poststrat_frame_", state, ".csv"))
      write_csv(poststrat, output_path)
      message(glue::glue("✅ Saved poststrat frame for {state}"))
    }, silent = TRUE)
  }
}


## need to set the year explicitly
year <- "2008"
output_dir <- file.path("data", year)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (state in all_states) {
  message(glue("🔄 Processing {state} in {year}..."))
  
  try({
    pums <- get_pums(
      variables = c("AGEP", "SEX", "SCHL", "RAC1P", "HISP", "PWGTP"),
      state = state,
      survey = "acs5",
      year = 2009,
    )
    
    if (nrow(pums) == 0) {
      warning(glue("⚠️ No data returned for {state}, skipping."))
      next
    }
    
    poststrat <- pums %>%
      filter(AGEP >= 18) %>%
      mutate(
        age = case_when(
          AGEP >= 18 & AGEP <= 29 ~ "18-29",
          AGEP >= 30 & AGEP <= 44 ~ "30-44",
          AGEP >= 45 & AGEP <= 65 ~ "45-65",
          AGEP > 65               ~ "65+",
          TRUE ~ NA_character_
        ),
        sex = case_when(
          SEX == 1 ~ "Male",
          SEX == 2 ~ "Female",
          TRUE ~ NA_character_
        ),
        race = case_when(
          HISP != 1 ~ "Hispanic/Latino",
          RAC1P == 1 ~ "White",
          RAC1P == 2 ~ "Black",
          RAC1P == 6 ~ "Asian",
          TRUE ~ "Other"
        ),
        educ = case_when(
          SCHL >= 20 ~ "College graduate",
          SCHL == 1 ~ NA_character_,
          SCHL <= 19 ~ "No college degree"
        ),
        state = state,
        weight = PWGTP
      ) %>%
      select(age, sex, race, educ, state, weight) %>%
      drop_na() %>%
      mutate(
        across(c(age, sex, race, educ), as.factor)
      )
    
    output_path <- file.path(output_dir, paste0("..data/census/poststrat_frame_", state, ".csv"))
    write_csv(poststrat, output_path)
    message(glue("Saved poststrat frame for {state}"))
    
  }, silent = TRUE)
}