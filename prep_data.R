# Aggregate the raw NYC collision records into daily counts by borough.
# Run once (from the ShinyApp folder) whenever the raw CSV is updated.
# Output: data/daily_borough_counts.csv (the only data file the app needs).

library(tidyverse)

raw_file <- file.path(
  "data",
  "Motor_Vehicle_Collisions_-_Crashes_20261114.csv"
)

read_csv(
  raw_file,
  col_select = c(`CRASH DATE`, BOROUGH),
  show_col_types = FALSE
) |>
  mutate(
    day = as.Date(`CRASH DATE`, format = "%m/%d/%Y"),
    borough = str_to_title(replace_na(BOROUGH, "Unknown"))
  ) |>
  filter(!is.na(day), day >= as.Date("2021-01-01")) |>
  count(borough, day, name = "crashes") |>
  arrange(borough, day) |>
  write_csv(file.path("data", "daily_borough_counts.csv"))
