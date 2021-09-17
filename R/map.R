# library(rnaturalearth)
# library(rnaturalearthdata)
# library(ggplot2)
# library(rgeos)
# library(dplyr)
# library(sf)

#' plot a geographical map
#'
#' @param values - a data frame with countries
#' @param latitude - the latitude range
#' @param longitude - the longitude range
#' @param colorRange - color range
#' @param breaks - breaks for color range
#' @return print a map
#' @export
plotMap <- function(data,
                    latitude = c(-45, 15),
                    longitude = c(-150, 180),
                    colorRange = c("#ffeeee", "#ff0000"),
                    breaks = c(100, 200, 500, 1000, 3000)) {
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- world[world$subregion != "Antarctica",]

  theme_map <- theme(axis.text         = element_blank(),
                     axis.title        = element_blank(),
                     panel.background  = element_rect(fill = "aliceblue"),
                     panel.grid.major  = element_line(colour = "#dddddd"),
                     panel.grid.minor  = element_line(colour = "#dddddd"))

  F <- match(data$countries, world$name_sort)
  colCol = rep(NA, nrow(world))
  colCol[F[!is.na(F)]] <- data$values[!is.na(F)]
  world$colCol <- colCol
  SEA_centroid <- st_centroid(world %>% select(name))
  SEA_centroid$name[-F[!is.na(F)]] <- NA

  p <- ggplot(data = world, aes(fill = colCol)) +
    geom_sf() +
    geom_sf_label(data=SEA_centroid,aes(label=name)) +
    #geom_point(data = cities, aes(x = lng, y = lat), size = 2,
    #shape = 23, fill = "darkred") +
    coord_sf(crs = "+proj=laea +lat_0=70 +lon_0=130 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
    # coord_sf(crs = "+proj=laea +lat_0=-20 +lon_0=130 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
    # coord_sf(xlim = longitude, ylim = latitude, expand = FALSE) +
    theme_map +
    #
    scale_fill_continuous(low = colorRange[1],
                          high = colorRange[2],
                          na.value = "white",
                          trans = "log",
                          breaks = breaks, name = "N")
  return(p)
}

# theme_map <- theme(axis.text         = element_blank(),
#                    axis.title        = element_blank(),
#                    panel.background  = element_rect(fill = "aliceblue"),
#                    panel.grid.major  = element_blank(),
#                    panel.grid.minor  = element_blank(),
#                    axis.ticks.length = grid::unit(0, "cm"),
#                    panel.spacing     = grid::unit(0, "lines"),
#                    plot.margin       = grid::unit(c(0, 0, 0, 0), "lines"),
#                    complete          = TRUE)
#
# countries <- c("Malaysia", "United Kingdom", "Spain", "Australia", "United States of America")
# values <- c(160, 440, 525, 100+2000, 2900)
# data <- data.frame(countries = countries, values = values)

# p <- plotMap(data, latitude = c(-43, 70), longitude = c(-145, 149)) +
#   labs(title = "Geographical distribution of cohorts", caption = "COVID-19 Cohorts", x = "longitude", y = "latitude")
# p


