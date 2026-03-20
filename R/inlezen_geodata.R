
library(ggplot2)
library(sf)

geo_nuts3 <- readRDS("data/geo_nuts3.rds")

geo_nuts3 |>
  ggplot() +
  geom_sf()

geo_nuts3 |>
  filter(nuts3 %in% c("Noord-Drenthe",
                      "Noord-Friesland",
                      "Oost-Groningen",
                      "Overig Groningen",
                      "Delfzijl en omgeving",
                      "Zuidoost-Drenthe",
                      "Zuidoost-Friesland",
                      "Zuidwest-Drenthe",
                      "Zuidwest-Friesland")) |>
  ggplot() +
  geom_sf()

geo_nuts3 |>
  filter(nuts3 %in% c("Noord-Drenthe",
                      "Noord-Friesland",
                      "Oost-Groningen",
                      "Overig Groningen",
                      "Delfzijl en omgeving",
                      "Zuidoost-Drenthe",
                      "Zuidoost-Friesland",
                      "Zuidwest-Drenthe",
                      "Zuidwest-Friesland")) |>
  ggplot() +
  geom_sf(aes(fill = oppervlakte_km2)) +
  geom_sf_text(aes(label = nuts3), colour = "white", size = 3) +
  theme_void()
