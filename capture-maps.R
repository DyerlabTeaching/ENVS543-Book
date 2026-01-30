#!/usr/bin/env Rscript
# Capture leaflet maps using webshot2

# Install webshot2 if needed
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2", repos = "https://cloud.r-project.org")
}

library(webshot2)

# Set working directory
setwd("/Users/rodney/Desktop/ENVS543-Book")

# Capture maps from rendered HTML files
# The maps are rendered as htmlwidgets within the page

cat("Capturing narrative_points.html map...\n")
webshot2::webshot(
  "docs/narrative_points.html",
  "media/map_points_leaflet.png",
  selector = ".leaflet",
  delay = 5,
  zoom = 2
)

cat("Capturing narrative_containers.html map...\n")
webshot2::webshot(
  "docs/narrative_containers.html",
  "media/map_containers_leaflet.png",
  selector = ".leaflet",
  delay = 5,
  zoom = 2
)

cat("Done!\n")
