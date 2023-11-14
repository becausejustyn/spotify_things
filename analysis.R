
library(arrow, warn.conflicts = FALSE)
library(dplyr)

df <- open_dataset("data/parq/") |>
  select(-c(skipped, offline_timestamp)) |>
  collect()

df <- df |>
  mutate(
    date = lubridate::as_date(ts),
    time = format(lubridate::as_datetime(ts), "%H:%M:%S"),
  ) |>
  select(-ts)

df |> 
  group_by(album_artist_name) |> 
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60),
  ) |>
  ungroup() |>
  arrange(-total_duration)


df |> 
  group_by(track_name, album_artist_name) |> 
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60),
  ) |>
  ungroup() |>
  arrange(-total_duration)


df |> 
  summarise(across(album_artist_name, ~n_distinct(.)))

df |>
  # filter rows where year is 2022
  filter(lubridate::year(date) == 2022) |>
  group_by(album_artist_name) |> 
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60),
  ) |>
  ungroup() |>
  arrange(-total_duration)



df |> 
  filter(lubridate::year(date) == 2022) |> 
  select(date) |> 
  arrange(date) |>
  slice(c(1, n()))