#!/usr/bin/env Rscript
# Generate static map images for PDF/EPUB using ggplot2 + sf

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(rnaturalearth)

setwd("/Users/rodney/Desktop/ENVS543-Book")

# Get US states for background
states <- ne_states(country = "united states of america", returnclass = "sf")
mexico <- ne_states(country = "mexico", returnclass = "sf")

# --- Map 1: Beetle sampling sites (narrative_points) ---
cat("Generating map for narrative_points (Araptus beetle sites)...\n")

url1 <- "https://raw.githubusercontent.com/dyerlab/ENVS-Lectures/master/data/Araptus_Disperal_Bias.csv"
data <- read_csv(url1, show_col_types = FALSE) |>
  select(Site, Longitude, Latitude, everything()) |>
  arrange(Latitude)

p1 <- ggplot() +
  geom_sf(data = states, fill = "antiquewhite", color = "gray70") +
  geom_sf(data = mexico, fill = "antiquewhite", color = "gray70") +
  geom_point(data = data, aes(x = Longitude, y = Latitude),
             color = "darkred", size = 3, alpha = 0.8) +
  geom_text(data = data, aes(x = Longitude, y = Latitude, label = Site),
            hjust = -0.15, vjust = 0.5, size = 2.5, color = "gray30") +
  coord_sf(xlim = range(data$Longitude) + c(-1.5, 2),
           ylim = range(data$Latitude) + c(-0.5, 0.5)) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray90"),
    panel.background = element_rect(fill = "aliceblue")
  ) +
  labs(x = "Longitude", y = "Latitude",
       title = "Araptus Beetle Sampling Sites",
       subtitle = "Interactive map available in online version")

ggsave("media/map_points_leaflet.png", p1, width = 10, height = 6, dpi = 150, bg = "white")
cat("  Saved: media/map_points_leaflet.png\n")

# --- Map 2: Beetles data (narrative_containers) ---
cat("Generating map for narrative_containers (beetles data)...\n")

url2 <- "https://raw.githubusercontent.com/DyerlabTeaching/Data-Containers/main/data/arapat.csv"
beetles <- read_csv(url2, show_col_types = FALSE)

p2 <- ggplot() +
  geom_sf(data = states, fill = "antiquewhite", color = "gray70") +
  geom_sf(data = mexico, fill = "antiquewhite", color = "gray70") +
  geom_point(data = beetles, aes(x = Longitude, y = Latitude, color = Stratum),
             size = 3, alpha = 0.8) +
  coord_sf(xlim = range(beetles$Longitude, na.rm = TRUE) + c(-1, 1),
           ylim = range(beetles$Latitude, na.rm = TRUE) + c(-0.5, 0.5)) +
  theme_minimal() +
  theme(
    panel.grid = element_line(color = "gray90"),
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "bottom"
  ) +
  labs(x = "Longitude", y = "Latitude",
       title = "Beetle Collection Locations by Stratum",
       subtitle = "Interactive map available in online version",
       color = "Stratum")

ggsave("media/map_containers_leaflet.png", p2, width = 10, height = 6, dpi = 150, bg = "white")
cat("  Saved: media/map_containers_leaflet.png\n")

cat("\nDone! Static maps generated.\n")
