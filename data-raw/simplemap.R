## code to prepare `simplemap` dataset goes here

simplemap <- maps::map("world", fill = TRUE,
                 col = "transparent", plot = FALSE, wrap = c(0, 360))
simplemap <- sf::st_as_sf(simplemap)
simplemap <- rmapshaper::ms_simplify(simplemap, keep = 0.015, weighting = 0.7)

usethis::use_data(simplemap, overwrite = TRUE, internal = TRUE)
