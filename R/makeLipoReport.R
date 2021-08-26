
printIndicator <- function(lip, row, column, options = list()) {
  refTextColor <- "black"
  refTextAlpha <- 0.7
  unitTextColor <- "black"
  unitTextAlpha <- 0.7
  valueTextColor <- "black"
  valueTextAlpha <- 1
  indBgColor <- "black"
  indBgAlpha <- 0.1

  if ("dotColor" %in% names(options)) {
    dotColor <- options$dotColor
  } else {
    dotColor <- "black"
  }
  print(options$dotPch)
  if ("dotPch" %in% names(options)) {
    dotPch <- options$dotPch
  } else {
    dotPch <- 22
  }

  if (options$fold) {
    indicatorWidth <- 1
  } else {
    indicatorWidth <- 0.5
  }

  min <- min(lip$value, lip$refMin)
  max <- max(lip$value, lip$refMax)

  m <- (max + min)/2
  d <- dist(c(max, min))

  vp <- viewport(x = unit(2+column-0.75, "native"),
                 y = unit(row -.4, "native"),
                 width = unit(indicatorWidth, "native"),
                 height = unit(0.8, "native"),
                 xscale = c(min, max),
                 yscale = c(0, 2))
  pushViewport(vp)

  if (!options$add) {
    # draw indicator line
    grid.lines(x = unit(c(lip$refMin, lip$refMax), "native"),
               y = unit(c(1, 1), "native"),
               gp = gpar(lwd=8, col = indBgColor, alpha = indBgAlpha))

    # draw value
    if (lip$value < m){
      pos <- lip$value + d/20
      just <- "left"
    } else {
      pos <- lip$value - d/20
      just <- "right"
    }
    grid.text(round(lip$value, 2),
              gp=gpar(col = valueTextColor, alpha = valueTextAlpha, cex=0.5),
              just = just,
              x=unit(pos, "native"))

    # draw line
    if (options$fold) {
      lineColor <- "black"
      folds <- seq(round(min), floor(max), by = 1)
      for (f in folds) {
        if (f == 0) {
          lineAlpha <- 1
          lwd <- 3
        } else {
          lineAlpha <- 0.3
          lwd <- 1
        }

        grid.lines(x = unit(c(f, f), "native"),
                   y = unit(c(0.5, 1.5), "native"),
                   gp=gpar(col = lineColor,
                           alpha = lineAlpha,
                           lwd = lwd))
      }
    } else {
      grid.lines(x = unit(c(lip$value,lip$value), "native"),
                 y = unit(c(0.5, 1.5), "native"))
    }
    # print upper and lower bounds
    if (!options$fold) {
      grid.text(lip$refMin,
                gp=gpar(col = refTextColor, alpha = refTextAlpha, cex = 0.5),
                just = "left",
                x = -0.35)
      grid.text(lip$refMax,
                gp=gpar(col = refTextColor, alpha = refTextAlpha, cex = 0.5),
                just = "right",
                x = 1.35)
      grid.text(paste0(" [", lip$refUnit,"]"),
                gp=gpar(col = unitTextColor, alpha = unitTextAlpha, cex=0.5),
                just = "left",
                x = 1.35)
    }
  }
  # draw dot
  if (lip$value > lip$refMin & lip$value < lip$refMax){
    if (options$fold & lip$value < 0) {
      dotColor <- "red"
    } else {
      dotColor <- dotColor
    }
  } else {
    dotColor <- "red"
  }
  grid.points(lip$value,
              1,
              pch = dotPch,
              gp=gpar(fill = dotColor,
                      col=NA,
                      cex = 0.5))

  upViewport()
}
# lip$refMin <- runif(112, -12, -11)
# lip$refMax <- runif(112, 6, 8)
# lip$value <- runif(112, -10, 5)
# makeLipoReport(lip, options = list(fold = TRUE))

printStrip <- function(lip, row, column, options = list()) {

  if ("textColor" %in% names(options)) {
    stripTextColor <- options$textColor
  } else {
    stripTextColor <- "black"
  }

  if ("bgColor" %in% names(options)) {
    stripBgColor <- options$bgColor
  } else {
    stripBgColor <- "black"
  }

  abbrTextAlpha <- 0.7
  stripTextAlpha <- 1
  stripBgAlpha <- 0.1

  name <- paste0(lip$name)
  vp<-viewport(x = unit(1 + column - 0.65, "native"),
               y = unit(row - 0.4, "native"),
               width = unit(0.7, "native"),
               height = unit(0.75, "native"))
  pushViewport(vp)

  grid.rect(gp = gpar(fill = stripBgColor, col = NA, alpha = stripBgAlpha))
  grid.text(name, gp = gpar(col = stripTextColor, alpha = stripTextAlpha, cex = 0.5),
            just = "left",
            x = unit(0, "native"))
  grid.text(paste0(" (", lip$id, ") "),
            gp=gpar(col = stripTextColor, cex=0.5, alpha = abbrTextAlpha),
            just = "right",
            x = unit(1, "native"))
  upViewport()
}


printCaption <- function(caption, row, column) {
  png <- logo
  vp<-viewport(x=unit(1+column, "native"),
               y=unit(row, "native"),
               width=unit(2,"native"),
               height=unit(4,"native"))
  pushViewport(vp)
  grid.text(caption,
            gp=gpar(cex=0.7, alpha = 0.6),
            just = "left",
            x = unit(0.2, "native"),
            y = unit(0.1, "native"))
  grid.raster(png[1:620, 1:900,],
              just = "left",
              x = unit(0, "native"),
              y = unit(-.1, "native"),
              width = 0.2)
  upViewport()
  popViewport(1)
}

printTitle <- function(title, titleBoxPosition, titlePosition) {

  vp<-viewport(x=titleBoxPosition[1],
               y=titleBoxPosition[2],
               width=1,
               height=0.05)
  pushViewport(vp)

  grid.text(title,
            gp=gpar(cex=0.7, alpha = 0.6),
            just = "left",
            x = unit(titlePosition[1], "native"),
            y = unit(titlePosition[2], "native"))
  popViewport(1)
}

#' print lipoprotein  quantification report
#'
#' @param lip - a lipoprotein report
#' @param options - a list of options
#' @param options$fold - if true use design for fold change (default = FALSE)
#' @param options$scale - if true center and scale (align) the output (default = FALSE)
#' @return print a report
#'
#' @export
#' @importFrom grid grid.xaxis grid.yaxis pushViewport viewport grid.rect
#' @importFrom grid grid.newpage grid.text gEdit upViewport gpar unit
#' @importFrom grid grid.circle grid.points grid.lines grid.raster
makeLipoReport <- function(lip, options = list()) {
  if ("dotColor" %in% names(options)) {
    dotColor <- options$dotColor
  } else {
    dotColor <- "black"
  }

  if ("dotPch" %in% names(options)) {
    dotPch <- options$dotPch
  } else {
    dotPch <- 21
  }

  if ("title" %in% names(options)) {
    title <- options$title
  } else {
    title <- ""
  }

  if ("caption" %in% names(options)) {
    caption <- options$caption
  } else {
    caption <- "Lipoprotein Report
    Powered by phenocare/fusion ©2021"
  }

  if ("ncol" %in% names(options)) {
    ncol <- options$ncol
  } else {
    ncol <- 2
  }

  if (ncol == 2) {
    width <- 0.95
    height <- 0.9
    cols <- c(58, 54 + 4)
    nRows = nrow(lip) + 4
  } else if (ncol == 3) {
    width <- 0.95
    height <- 0.675
    cols <- c(38, 39, 38)
    nRows = nrow(lip) + 6
  } else {
    stop("fusion::makeLipoReport -> only 2 and 3 columns are supported")
  }

  grid.newpage()
  pushViewport(viewport(width = width,
                        height = height,
                        xscale=c(0, ncol * 2),
                        yscale=c(0, (nRows %/% ncol))))
  # grid.rect()
  i <- 1
  c <- 1
  r <- 0
  frac <- ""
  while(i < (nrow(lip)+1)){
    column <- 2 * (c - 1)
    row <- nRows %/% ncol - r

    if (row == nRows %/% ncol - cols[c]) {
      c <- c + 1
      r <- 0
      row <- nRows %/% ncol - r
      column <- 2 * (c - 1)
    }

    if (lip$fraction[i] != frac) {
      width <- min(table(lip$fraction)[lip$fraction[i]],
                   row)

      vp<-viewport(x = unit(1+column-1.05,"native"),
                   y = unit(row-width/2,"native"),
                   width = unit(0.05,"native"),
                   height = unit(0.99 * width,"native"))
      pushViewport(vp)
      grid.rect(gp=gpar(fill="gray75", col=NA))
      upViewport()

      vp<-viewport(x = unit(1+column-1.05,"native"),
                   y = unit(row-width/2,"native"),
                   width = unit(1,"native"),
                   height = unit(0.5,"native"),
                   angle = 90)
      pushViewport(vp)
      grid.text(lip$fraction[i],
                gp = gpar(cex=0.5),
                just = "center",
                x=unit(0.5, "native"))
      upViewport()

      frac <- lip$fraction[i]
    }

    if (lip$fraction[i] == frac & r == 0) {
      width <- table(lip$fraction[i:nrow(lip)])[lip$fraction[i]]

      vp<-viewport(x = unit(1+column-1.05,"native"),
                   y = unit(row-width/2,"native"),
                   width = unit(0.05,"native"),
                   height = unit(0.99 * width,"native"))
      pushViewport(vp)
      grid.rect(gp=gpar(fill="gray75", col=NA))
      upViewport()

      vp<-viewport(x = unit(1+column-1.05,"native"),
                   y = unit(row-width/2,"native"),
                   width = unit(1,"native"),
                   height = unit(0.5,"native"),
                   angle = 90)
      pushViewport(vp)
      grid.text(lip$fraction[i],
                gp = gpar(cex=0.5),
                just = "center",
                x=unit(0.5, "native"))
      upViewport()

      frac <- lip$fraction[i]
    }

    # print strip
    printStrip(lip[i,],
               row,
               column,
               options = list(bgColor = "red",
                              textColor = "red"))

    # print indicator
    if ("fold" %in% names(options)) {
      fold = options$fold
      if ("scale" %in% names(options)) {
        if (options$scale) {
          lip$refMin <- min(lip$value, lip$refMin)
          lip$refMax <- max(lip$value, lip$refMax)
        }
      } else {
        lip$refMin <- min(lip$value, lip$refMin)
        lip$refMax <- max(lip$value, lip$refMax)
      }
    } else {
      fold = FALSE
    }
    printIndicator(lip[i,],
                   row,
                   column,
                   options = list(fold = fold,
                                  add = FALSE,
                                  dotColor = dotColor,
                                  dotPch = dotPch))

    i <- i + 1
    r <- r + 1
  }

  printCaption(caption, 3, column)

  if (ncol == 2) {
    printTitle(title, titleBoxPosition = c(0.5, 0.975),
               titlePosition = c(0.1, 0.5))
  } else if (ncol == 3) {
    printTitle(title, titleBoxPosition = c(0.5, 0.875),
               titlePosition = c(0.1, 0.5))
  }

}

# lip <- getLipoprotein("./inst/HB-COVID0001/10")
# lip$id <- seq_along(lip$abbr)
# makeLipoReport(lip, options = list(title = "Lipo", ncol = 3))
# makeLipoReport(lip, options = list(fold = FALSE, dotColor = "blue"))
# makeLipoReport(lip, options = list(fold = TRUE, scale = FALSE))

#' add a serie of value to an existing lipoprotein  quantification report
#'
#' @param lip - a lipoprotein report with new values
#' @param options - a list of options (should be as existing report)
#' @param options$fold - if true use design for fold change (default = FALSE)
#' @param options$scale - if true center and scale (align) the output (default = FALSE)
#' @return print a report with overlayed values
#' @export
addValues <- function(lip, options = list()) {

  if ("dotColor" %in% names(options)) {
    dotColor <- options$dotColor
  } else {
    dotColor <- "black"
  }

  if ("dotPch" %in% names(options)) {
    dotPch <- options$dotPch
  } else {
    dotPch <- 21
  }

  if ("ncol" %in% names(options)) {
    ncol <- options$ncol
  } else {
    ncol <- 2
  }

  if (ncol == 2) {
    width <- 0.95
    height <- 0.9
    cols <- c(58, 54 + 4)
    nRows = nrow(lip) + 4
  } else if (ncol == 3) {
    width <- 0.95
    height <- 0.675
    cols <- c(38, 39, 38)
    nRows = nrow(lip) + 6
  } else {
    stop("fusion::makeLipoReport -> only 2 and 3 columns are supported")
  }

  pushViewport(viewport(width = width,
                        height = height,
                        xscale=c(0, ncol * 2),
                        yscale=c(0, (nRows %/% ncol))))
  # grid.rect()
  i <- 1
  c <- 1
  r <- 0
  frac <- ""
  while(i < (nrow(lip)+1)){
    column <- 2 * (c - 1)
    row <- nRows %/% ncol - r

    if (row == nRows %/% ncol - cols[c]) {
      c <- c + 1
      r <- 0
      row <- nRows %/% ncol - r
      column <- 2 * (c - 1)
    }

    # print indicator
    if ("fold" %in% names(options)) {
      fold = options$fold
      if ("scale" %in% names(options)) {
        if (options$scale) {
          lip$refMin <- min(lip$value, lip$refMin)
          lip$refMax <- max(lip$value, lip$refMax)
        }
      } else {
        lip$refMin <- min(lip$value, lip$refMin)
        lip$refMax <- max(lip$value, lip$refMax)
      }
    } else {
      fold = FALSE
    }
    printIndicator(lip[i,],
                   row,
                   column,
                   options = list(fold = fold,
                                  add = TRUE,
                                  dotColor = dotColor,
                                  dotPch = dotPch))

    i <- i + 1
    r <- r + 1
  }

}

# addValues(lip, options = list(fold = TRUE, scale = FALSE, dotColor = "red"))
# makeLipoReport(lip, options = list(fold = FALSE, dotColor = "blue", dotPch = 21))
# addValues(lip, options = list(dotColor = "red", dotPch = 22))
