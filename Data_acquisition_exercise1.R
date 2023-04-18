library(devtools)
library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(ggridges)


Sys.setenv(SPOTIFY_CLIENT_ID = '5964d300017d4643a56fedb1e42b561e')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '307841367f1c4457baea58d6bf6584ff')

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')%>%
  select(artist_name,artist_id, album_id,album_type, album_release_date, album_release_year, album_release_date_precision, danceability,artists ,track_uri,external_urls.spotify , album_name, key_name, mode_name ,key_mode)%>%
  head(5)

beatles

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(10) %>% 
  as_tibble()


joy <- get_artist_audio_features('joy division')
joy %>% 
  arrange(-valence) %>% 
  select(track_name, valence) %>% 
  head(10) %>% 
  as_tibble()



