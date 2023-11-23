
#list.files('data')

library(jsonlite)
library(dplyr)
library(purrr)
library(glue)
library(arrow)

# Create the 'parq' directory if it doesn't exist
dir.create("data/parq", showWarnings = FALSE, recursive = TRUE)

FILE_PATHS <- list.files('data/extended', full.names = TRUE)

FILE_PATHS |>
  walk(~{
    df <- jsonlite::fromJSON(.x) |>
      dplyr::select(-c(username, platform, conn_country:user_agent_decrypted, episode_name:spotify_episode_uri, offline, incognito_mode)) |>
      dplyr::rename_with(~ gsub('master_metadata_', '', .x))
    
    # Remove the ".json" extension from the output file name
    output_file <- gsub("\\.json$", "", basename(.x))
    
    # Write the Parquet file to the 'data/parq' directory
    arrow::write_parquet(df, glue::glue('data/parq/{output_file}.parquet'))
  })

