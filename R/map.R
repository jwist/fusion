# library(rnaturalearth)
# library(rnaturalearthdata)
# library(ggplot2)
# library(rgeos)
# library(dplyr)
# library(sf)

#' plot a geographical map
#'
#' @param data - a data frame with countries
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
                    breaks = c(10, 20, 50, 100, 150)) {
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

  path <- data.frame(x = c(0,20), y = c(0,40))
  p <- ggplot(data = world, aes(fill = colCol)) +
    geom_sf() +
    geom_sf_label(data=SEA_centroid,aes(label=name), cex = 2.5) +
    #geom_point(data = cities, aes(x = lng, y = lat), size = 2,
    #shape = 23, fill = "darkred") +
    # coord_sf(crs = "+proj=laea +lat_0=70 + lon_0=130 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
    # coord_sf(crs = "+proj=laea +lat_0=-20 +lon_0=130 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
    coord_sf(crs = "+proj=laea +lat_0=70 +lon_0=40 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
    geom_path(data = path, aes(x, y)) +
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
#

# country <- read.csv2("~/Downloads/Scopus-1276-Analyze-Country.csv", sep = ",", header = FALSE)
# data <- data.frame(countries = country$V1, values = country$V2)
# data$countries[1] <- "United States of America"
# data$countries[19] <- "Korea, Dem. Rep."
# data$countries[35] <- "Egypt, Arab Rep."
# data$countries[36] <- "Hong Kong SAR, China"
# data$countries[37] <- "Iran, Islamic Rep."
# data$countries[56] <- "Czechia"
# data$countries[57] <- "Macao SAR, China"
# data$countries[62] <- "Slovak Republic"
# data$countries[65] <- "Vietnam"
# data$countries[67] <- "Unknown"
#
#
# p <- plotMap(data, latitude = c(-40, 70), longitude = c(-149, 149)) +
#      labs(title = "Most prolific countries", caption = "Gut simulation", x = "longitude", y = "latitude")
# p
#
# svglite::svglite(filename = "round1.svg")
# p
# dev.off()
#
