library(sf)
library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(plotly)
library(httr)

# helper function to construct url address from date and type of eclipse
base_url <- "http://xjubier.free.fr/download/GE/en/"
eclipse_abbs <- c(
  "Total" = "TSE",
  "Anular" = "ASE",
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

world <- ne_countries(returnclass = "sf")

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
  path <- st_read(destfile, layer = layers$name[2])
  path <- path |> filter(Name == "Umbra")

  if (nrow(path) < 1) {
    stop("No `Umbra` path found in the file")
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
      split = ~ seq_len(nrow(path)),
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
