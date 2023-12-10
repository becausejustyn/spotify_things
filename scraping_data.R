
library(arrow, warn.conflicts = FALSE)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

library(spotifyr)

access_token <- get_spotify_access_token()

df <- open_dataset("data/parq/") |>
  select(-c(skipped, offline_timestamp)) |>
  filter(!is.na(album_artist_name)) |>
  collect()

# have to do this after you load it since lubridate doesn't work with arrow

df <- df |>
  mutate(
    date = lubridate::as_date(ts),
    time = format(lubridate::as_datetime(ts), "%H:%M:%S"),
  ) |>
  select(-ts)

# this is for scraping track data later on
track_df <- df |>
  distinct(spotify_track_uri, .keep_all = TRUE) |>
  # this allows spotifyr to work with the track ids
  mutate(track_id = gsub('^spotify:track:', '', spotify_track_uri)) |>
  select(track_name, album_artist_name, album_album_name, track_id) |>
  rename_with(~stringr::str_replace(., 'album_', ''), starts_with('album_'))

track_ids <- track_df |> pull(track_id)

scrape_track_audio_features <- function(track_lst, access_token) {
  track_lst |> 
    map(~get_track_audio_features(ids = .x, authorization = access_token)) |>
    map_df(~as_tibble(.x))
}

# https://msmith7161.github.io/what-is-speechiness/
# https://www.caitlinhudon.com/posts/2017/12/22/blue-christmas
# https://developer.spotify.com/dashboard/052dec7a99bd42e888548b0f32f5d444/settings


mkdir("data/audio_features", showWarnings = FALSE, recursive = TRUE)

# track_audio_features_1 <- open_dataset("data/track_audio_features_1.parquet") |> collect()

track_audio_features <- open_dataset("data/audio_features/") |> collect()


test <- df |>
  mutate(id = gsub('^spotify:track:', '', spotify_track_uri)) |>
  left_join(track_audio_features, by = 'id', relationship = 'many-to-many') 

test2 <- test |>
  distinct(id, .keep_all = TRUE)

df_greater <- test |>
  group_by(album_artist_name, track_name, id) |>
  summarise(n = n()) |>
  ungroup() |>
  filter(n > 1)

library(ggplot2)



test2 |> 
  filter(id %in% df_greater[['id']]) |>
  ggplot(aes(x = valence, y = energy)) +
  geom_point(position = 'jitter') +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_label(x = 0.25 / 2, y = 0.95, label = "Angry / Turbulent", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.95, label = "Joyful / Happy", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.05, label = "Peace / Chill", color = "black", fill = "aliceblue") +
  geom_label(x = 0.25 / 2, y = 0.05, label = "Depressing / Sad", color = "black", fill = "aliceblue") +
  labs(x = "Valence", y = "Energy") 


test |>
  group_by(album_artist_name) |>
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60)
  ) |>
  ungroup() |>
  View()
  #  arrange(-total_duration) 


test |>
  group_by(album_artist_name, track_name) |>
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60)
  ) |>
  ungroup() |>
  View()
#  arrange(-total_duration) 

# https://developer.spotify.com/documentation/web-api/reference/get-several-audio-features
# A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. 
# Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).


test |>
  mutate(
    mode = case_when(
      mode == 1 ~ "Major",
      mode == 0 ~ "Minor"),
    key = case_when(
      key == 0 ~ "C",
      key == 1 ~ "C♯, D♭",
      key == 2 ~ "D",
      key == 3 ~ "D♯, E♭",
      key == 4 ~ "E",
      key == 5 ~ "F",
      key == 6 ~ "F♯, G♭",
      key == 7 ~ "G",
      key == 8 ~ "G♯, A♭",
      key == 9 ~ "A",
      key == 10 ~ "A♯, B♭",
      key == 11 ~ "B",
      key == -1 ~ NA)
  ) |>
  count(key)


test |>
  filter(
    lubridate::year(date) == 2023, 
    lubridate::month(date) != 11
  ) |>
  group_by(album_artist_name) |>
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60)
  ) |>
  ungroup() |>
  arrange(-n)


test2 |> 
  filter(album_artist_name == 'Tom Waits') |>
  ggplot(aes(x = valence, y = energy)) +
  #geom_point(position = 'jitter') +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_label(x = 0.25 / 2, y = 0.95, label = "Angry / Turbulent", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.95, label = "Joyful / Happy", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.05, label = "Peace / Chill", color = "black", fill = "aliceblue") +
  geom_label(x = 0.25 / 2, y = 0.05, label = "Depressing / Sad", color = "black", fill = "aliceblue") +
  geom_point(position = 'jitter') +
  labs(x = "Valence", y = "Energy") 


test2 |> 
  filter(album_artist_name %in% c('Tom Waits', 'The Gaslight Anthem', 'The Afghan Whigs', 'Bruce Springsteen', 'The Airborne Toxic Event')) |>
  ggplot(aes(x = valence, y = energy, colour = album_artist_name)) +
  #geom_point(position = 'jitter') +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_label(x = 0.25 / 2, y = 0.95, label = "Angry / Turbulent", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.95, label = "Joyful / Happy", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75 / 2, y = 0.05, label = "Peace / Chill", color = "black", fill = "aliceblue") +
  geom_label(x = 0.25 / 2, y = 0.05, label = "Depressing / Sad", color = "black", fill = "aliceblue") +
  geom_point(position = 'jitter') +
  labs(x = "Valence", y = "Energy") 


library(ggridges)

test2 |> 
  filter(album_artist_name == 'Tom Waits') |>
  ggplot(aes(x = valence, y = album_album_name, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0, 1) +
  theme(legend.position = "none")

artist_valence_plot <- function(df, artist_name, ...){
  df |> 
    filter(album_artist_name == artist_name) |>
    # use after_stat to access the computed value of ..x..
    ggplot(aes(x = valence, y = album_album_name, fill = after_stat(..x..))) +
    geom_density_ridges_gradient(scale = 0.9) + 
    scale_fill_gradient(low = "white", high = "maroon3") + 
    theme(panel.background = element_rect(fill = "white")) +
    theme(plot.background = element_rect(fill = "white")) +
    xlim(0, 1) +
    theme(legend.position = "none")
}

artist_valence_plot(test2, artist_name = 'Tom Waits')