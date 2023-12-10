
library(arrow, warn.conflicts = FALSE)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

df <- open_dataset("data/parq/") |>
  select(-c(skipped, offline_timestamp)) |>
  filter(!is.na(album_artist_name)) |>
  collect() |>
  # have to do this after you load it since lubridate doesn't work with arrow
  mutate(
    date = lubridate::as_date(ts),
    time = format(lubridate::as_datetime(ts), "%H:%M:%S"),
  ) |>
  select(-ts)

# what proportion of the songs are unique?
df |> 
  summarise(n_distinct = n_distinct(spotify_track_uri) / n())

df |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise(
    n = n(),
    n_distinct = n_distinct(spotify_track_uri) / n)




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



df |> 
  filter(lubridate::year(date) == 2023) |> 
  group_by(album_artist_name) |> 
  summarise(
    n = n(),
    total_duration = sum(ms_played) / (1000 * 60 * 60)
  ) |>
  ungroup() |>
  arrange(-total_duration)


# this is for scraping track data later on
track_df <- df |>
  distinct(spotify_track_uri, .keep_all = TRUE) |>
  # this allows spotifyr to work with the track ids
  mutate(track_id = gsub('^spotify:track:', '', spotify_track_uri)) |>
  select(track_name, album_artist_name, album_album_name, track_id) |>
  rename_with(~stringr::str_replace(., 'album_', ''), starts_with('album_'))


library(spotifyr)

access_token <- get_spotify_access_token()


ggplot(data = top_4, aes(x = valence, y = energy, color = artist_name)) +
  geom_point(position = "jitter") +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_label(x = 0.25/2, y = 0.95, label = "Angry / Turbulent", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75/2, y = 0.95, label = "Joyful / Happy", color = "black", fill = "aliceblue") +
  geom_label(x = 1.75/2, y = 0.05, label = "Peace / Chill", color = "black", fill = "aliceblue") +
  geom_label(x = 0.25/2, y = 0.05, label = "Depressing / Sad", color = "black", fill = "aliceblue") +
  labs(x= "Valence", y= "Energy") +
  ggtitle("Emotional Quadrant - Top Four Artists", "Energy vs Valence")  

# Function to get track audio features for a batch
get_batch_audio_features <- function(batch_df, access_token) {
  batch_df |>
    mutate(
      track_info = map(track_id, ~get_track_audio_features(ids = .x, authorization = access_token))
    ) |>
    select(-track_id) |>
    unnest(track_info)
}

# Function to process the dataframe in batches
process_in_batches <- function(df, batch_size, access_token, delay_seconds) {
  total_batches <- ceiling(nrow(df) / batch_size)
  
  result <- map(1:total_batches, ~{
    start_idx <- (.-1) * batch_size + 1
    end_idx <- min(. * batch_size, nrow(df))
    
    batch_df <- df[start_idx:end_idx, , drop = FALSE]
    
    start_time <- Sys.time()
    batch_result <- get_batch_audio_features(batch_df, access_token)
    end_time <- Sys.time()
    
    # Print progress information
    cat(sprintf("Batch %d/%d completed in %.2f seconds\n", ., total_batches, as.numeric(difftime(end_time, start_time, units = "secs"))))
    
    Sys.sleep(delay_seconds)  # Add delay between batches
    
    batch_result
  })
  
  bind_rows(result)
}

# batch_size is how many rows will be done at a time
# since there is a delay_seconds of 60, this means we will limit how many requests we make to 1 per minute
BATCH_SIZE <- 20 
DELAY_SECONDS <- 60

track_df_final <- process_in_batches(track_df, BATCH_SIZE, access_token, DELAY_SECONDS)

# https://msmith7161.github.io/what-is-speechiness/
# https://www.caitlinhudon.com/posts/2017/12/22/blue-christmas
# https://developer.spotify.com/dashboard/052dec7a99bd42e888548b0f32f5d444/settings