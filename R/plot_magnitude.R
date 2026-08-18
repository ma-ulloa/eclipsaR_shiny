library(ggplot2)
library(tidyverse)
library(ggforce)

# ------
# auxiliary functions to find the position of the moon for a partial eclipse given its overlap
# ------

# --- Area of intersection for two EQUAL circles of radius r, centers distance d apart ---
circle_intersection_area <- function(d, r) {
  if (d >= 2 * r) {
    return(0)
  } # no overlap
  if (d <= 0) {
    return(pi * r^2)
  } # fully overlapping

  2 * r^2 * acos(d / (2 * r)) - (d / 2) * sqrt(4 * r^2 - d^2)
}

# --- Solve for distance d given target overlap area ---
solve_for_distance <- function(target_area, r) {
  max_area <- pi * r^2
  if (target_area > max_area) {
    stop("Target area exceeds a full circle's area.")
  }

  f <- function(d) circle_intersection_area(d, r) - target_area * pi * r**2
  uniroot(f, lower = 1e-6, upper = 2 * r - 1e-6)$root
}


# --------

plot_magnitude <- function(magnitude, type) {
  if (!(type %in% c("Total", "Hybrid", "Annular", "Partial"))) {
    stop('`type` has to be one of c("Total", "Hybrid", "Annular", "Partial")')
  }
  r <- 1
  sun <- tibble(x0 = 1, y0 = 1, r = r)
  if (type %in% c("Total", "Hybrid")) {
    moon <- circle1
    if (!is.null(magnitude)) {
      warning(paste0("Magnitude will be ignore because the eclipse is ", type))
    }
  }

  if (type == "Annular") {
    moon <- tibble(x0 = 1, y0 = 1, r = sqrt(magnitude) * r)
  }

  if (type == "Partial") {
    d <- solve_for_distance(target_area = magnitude, r = r)
    moon <- tibble(x0 = 1 - d, y0 = 1, r = r)
  }

  p <- ggplot() +
    geom_circle(
      data = sun,
      aes(x0 = x0, y0 = y0, r = r),
      fill = "yellow",
      alpha = 0.5,
      color = NA
    ) +
    geom_circle(
      data = moon,
      aes(x0 = x0, y0 = y0, r = r),
      fill = "lightgrey",
      color = NA
    ) +
    coord_fixed() +
    theme_void()

  return(p)
}
