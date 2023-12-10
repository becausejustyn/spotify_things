
library(ggplot2)


df1 <- df %>%
  filter(lubridate::year(date) == 2023) %>%
  group_by(album_artist_name, date) %>%
  summarise(
    ms_played = sum(ms_played)
  ) %>%
  ungroup()

tom <- df1 %>%
  filter(album_artist_name == "Tom Waits")

# circular plot of Tom Waits listening by month

ggplot(tom, aes(x = date, y = ms_played, group = 1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_polar(start = 0) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

tom$date <- as.POSIXct(tom$date, format = "%Y-%m-%d")

ggplot(tom, aes(x = date, y = ms_played, group = 1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_polar(start = 0) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    # hide y axis
    axis.text.y = element_blank(),
  )


df1 <- df %>%
  filter(lubridate::year(date) == 2023) %>%
  group_by(album_artist_name, date) %>%
  summarise(
    ms_played = sum(ms_played)
  ) %>%
  ungroup()

tom <- df1 %>%
  filter(album_artist_name == "Tom Waits")

ggplot(tom, aes(x = date, y = ms_played)) +
  coord_polar(start = 0, direction = 1) +
  geom_bar(stat = "identity", fill = "steelblue") 

df1 %>% 
  distinct(date) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = date, y = 1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_polar(start = 0, direction = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") 


df1 %>% 
  filter(album_artist_name == 'The Afghan Whigs') %>%
  # distinct(date) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
  ggplot(aes(x = date)) +
  geom_histogram() +
  coord_polar(start = 0, direction = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b") 