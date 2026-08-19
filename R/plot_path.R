library(sf)
library(ggplot2)
library(tidyverse)
library(rnaturalearthdata)
library(plotly)
library(httr)


# helper function to construct url address from date and type of eclipse
base_url <- "http://xjubier.free.fr/download/GE/en/"
eclipse_abbs <- c(
  "Total" = "TSE",
  "Annular" = "ASE",
  "Partial" = "PSE",
  "Hybrid" = "HSE"
)
format <- ".kmz"

construct_url <- function(eclipse_date, type) {
  eclipse_date <- gsub(pattern = "-", replacement = "_", x = eclipse_date)
  constructed_url <- file.path(
    base_url,
    paste0(eclipse_abbs[type], "_", eclipse_date, format)
  )

  return(constructed_url)
}

# function to plot umbral paths and max eclipse point in an interactive map using `plotly` and `sf`

data("countries110", package = "rnaturalearthdata")
world <- countries110

plot_path <- function(url, destfile = file.path(tempdir(), basename(url))) {
  # check if the url is reachable
  tryCatch(download.file(url, destfile), error = function(e) {
    message("Failed to download file: ", e$message)
    return(NULL)
  })

  # read layers and take max eclipse point and umbral path
  # check if it's readable by sf::st_layers()
  tryCatch(layers <- st_layers(destfile), error = function(e) {
    message("Failed to read layers from file: ", e$message)
  })

  max_point <- st_read(destfile, layer = layers$name[1])
  path_layer <- st_read(destfile, layer = layers$name[2])

  # Total/Annular/Hybrid eclipses have a filled "Umbra" polygon in this layer.
  # Partial eclipses never do (the umbra doesn't reach Earth's surface), so
  # fall back to the penumbral limit line(s) that bound where the eclipse
  # was visible.
  path <- path_layer |> filter(grepl("Umbra", Name))
  if (nrow(path) < 1) {
    path <- path_layer |> filter(grepl("Penumbra.*Limit", Name))
  }

  if (nrow(path) < 1) {
    stop("No eclipse path found in the file")
  }

  # plot

  p <- plot_ly() |>
    add_sf(
      data = world,
      color = I("white"),
      line = list(color = "black", width = .5)
    ) |>
    add_sf(
      data = path,
      split = ~Name,
      color = I("orange"),
      alpha = 0.3,
      line = list(width = 1)
    ) |>
    add_sf(
      data = max_point,
      color = I("black"),
      text = "Max point"
    ) |>
    layout(showlegend = F)

  return(p)
}
