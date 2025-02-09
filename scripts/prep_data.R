library(tidyverse)
library(httr2)

# Get currently open exhibitions.
today_str <- today() |> as.character()

# All open exhibitions. Includes exhibitions with no entries in the artwork API.
exhibits_0 <-
  request("https://openaccess-api.clevelandart.org/api/exhibitions/") |>
  req_url_query(
    opened_before = today_str, 
    closed_after = today_str, 
    venues = "The Cleveland Museum of Art"
  ) |>
  req_perform() |> 
  resp_body_json(simplifyVector = TRUE) |>
  pluck("data") |>
  filter(is_venue_cma == TRUE)

# For each exhibition, get the artworks.

artwork_list <- list()

for (i in 1:nrow(exhibits_0)) {
  artwork_list[[i]] <-
    request("https://openaccess-api.clevelandart.org/api/artworks/") |>
    req_url_query(exhibition_id = exhibits_0$id[i]) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    pluck("data")
  Sys.sleep(1)
}

names(artwork_list) <- exhibits_0$id

artworks_0 <-
  bind_rows(artwork_list, .id = "exhibition_id") |>
  mutate(exhibition_id = as.numeric(exhibition_id))

artwork_url <-
  artworks_0 |> 
  select(id, images) |>
  unnest(images) |>
  unnest(web) |>
  select(id, image_url = url) |>
  filter(!is.na(image_url)) |>
  slice_head(by = id, n = 1) # just in case

artworks <- inner_join(artworks_0, artwork_url, by = "id")

# Open exhibitions _with artwork_.
exhibits <- semi_join(exhibits_0, artworks, by = c("id" = "exhibition_id"))

# Save for results for app.
save(exhibits, artworks, file = "app_data.Rdata")
