# spotify_things


```r
stream_hist <- purrr::map_df(c(0:2), function(x) {
  jsonlite::fromJSON(
    glue::glue('data/endsong_{x}.json')
  )
}) |> 
  select(-c(username, platform, conn_country:user_agent_decrypted, episode_name:spotify_episode_uri, offline, incognito_mode)) |> 
  rename_with(~ gsub('master_metadata_', '', .x)) 
```