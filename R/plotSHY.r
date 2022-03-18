
#' convert a matrix to a color matrix
#' @param mat - a matrix
#' @export
#' @importFrom scales rescale
#'
corToColor <- function(mat) {
  if (max(mat, na.rm = TRUE) > 1 | min(mat, na.rm = TRUE) < -1) {
    stop("input values should be between -1 and 1")
  }
  mat <- rescale(mat, to = c(0, 1), from = c(-1,1))
  tictoc::tic("mean")
  row <- nrow(mat)
  col <- ncol(mat)
  corp<- colorRamp(c("blue", "white", "red"), alpha = TRUE)
  av <- corp(mat)
  res <- array(dim = c(row, col, 4))
  res[,,1] <- matrix(av[,1], row, col)
  res[,,2] <- matrix(av[,2], row, col)
  res[,,3] <- matrix(av[,3], row, col)
  res[,,4] <- matrix(av[,1], row, col)
  tictoc::toc()
  return(res)
}

#' plot correlation between NMR trace (part of) and MS data with segments
#' @param cor - a matrix with correlation values
#' @param sig - a matrix with significance values
#' @param txt - a matrix with text
#' @param labels - the name of the MS values (x rows)
#' @param trace - the NMR trace to plot (values)
#' @param xaxis - the xaxis for the NMR trace
#' @param options - options
#'
#' @export
#' @importFrom grid grid.xaxis grid.yaxis pushViewport viewport grid.rect
#' @importFrom grid grid.newpage grid.text gEdit upViewport gpar unit popViewport
#' @importFrom grid grid.circle calcStringMetric grid.roundrect grid.convertY
#' @importFrom grDevices rgb colorRamp
plotSHY <- function(cor, sig, txt, labels, trace, xaxis, options = list()) {
  tictoc::tic("init")

  x <- cor
  Ncol <- length(trace)

  if ("alpha" %in% names(options)) {
    alpha <- options$alpha
  } else {
    alpha <- 1
  }

  if ("columns" %in% names(options)) {
    columns <- TRUE
    columnColors <- options$colors
    columnName <- names(options$columns)
    columnCenter <- Ncol - sapply(options$columns, function(x) sum(xaxis < mean(x)))
    columnWidth <- sapply(options$columns, function(x) sum(xaxis >= x[1] & xaxis <= x[2] ))
    columnMin <- min(columnCenter - columnWidth / 2)
    columnMax <- max(columnCenter + columnWidth / 2)
    print(columnMin)
    print(columnMax)
  } else {
    columns <- FALSE
  }

  if ("xlab" %in% names(options)) {
    xlab <- options$xlab
  } else {
    xlab  <- ""
  }

  if ("ylab" %in% names(options)) {
    ylab <- options$ylab
  } else {
    ylab  <- ""
  }


  grid.newpage()
  pushViewport(viewport(width = 0.9,
                        height = 0.9,
                        xscale=c(0, 4),
                        yscale=c(0, 3)))


  # ploting color matrix
  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(2, "native"),
                        x = unit(2.5, "native"),
                        y = unit(1, "native"),
                        xscale=c(0, Ncol),
                        yscale=c(0, nrow(x))))

  maxTextWidth <- grid.convertY(unit(max(calcStringMetric(labels)$width), "inches"), "native", valueOnly = TRUE)

  corp<- colorRamp(c("blue", "white", "red"))

  for (i in c(1:nrow(x))) {
    if (!missing(labels) & !missing(xaxis)) {
      grid.text(x = unit(-maxTextWidth + columnMin - (0.05 * Ncol), "native"),
                y = unit(nrow(x) + 1 - i - 0.5, "native"),
                label = labels[i], gp = gpar(cex = 0.8), just = "right")
    }
    for (j in c(1:length(options$columns))) {
      if (length(dim(x)) > 2) {
        if (is.na(x[i,j,1]) | is.na(x[i,j,2]) | is.na(x[i,j,3])) {
          fill <- "black"
          pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                                y = unit(nrow(x) + 1 - i - 0.5, "native"),
                                width = unit(columnWidth[[j]], "native"),
                                height = unit(1, "native"),
                                xscale = c(0,1),
                                yscale = c(0,1)))
          grid.rect(gp = gpar(fill = NA, alpha = alpha))
          upViewport()
        } else {
          fill <- rgb(array(data = c(x[i,j,1], x[i,j,2], x[i,j,3]), dim = c(1,4))/256)
          pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                                y = unit(nrow(x) + 1 - i - 0.5, "native"),
                                width = unit(columnWidth[[j]], "native"),
                                height = unit(1, "native"),
                                xscale = c(0,1),
                                yscale = c(0,1)))
          if (numToSignificance(sig[i, j]) != "NS" & !is.na(sig[i, j])) {
            grid.rect(gp = gpar(fill = NA, alpha = alpha))
            grid.circle(x = 0.5, y = 0.55, r = 0.3, gp = gpar(fill = fill, col = NA))
            grid.text(x = 0.5, y = 0.15, label = round(txt[i,j], digits = 2), gp = gpar(cex = 1, just = "center"))
            grid.text(x = 0.5, y = 0.875, label = numToSignificance(sig[i, j]), gp = gpar(cex = 1, just = "center"))
          } else {
            # grid.rect(gp = gpar(fill = NA, alpha = alpha))
          }
          upViewport()
        }
      } else {
        fill <- rgb(corp(x[i,j])/255)
      }


    }
  }

  # labels
  if (!missing(labels) & !missing(xaxis)) {
    pushViewport(viewport(width = unit(Ncol, "native"),
                          height = 0.1,
                          x = columnMax / Ncol + 0.03,
                          y = 0.5,
                          angle = 90))
    grid.text(label = ylab)
    upViewport()

    pushViewport(viewport(width = unit(Ncol, "native"),
                          height = 0.1,
                          x = 0.96,
                          y = 0.5,
                          angle = 90))
    grid.text(label = "color scale")
    upViewport()

    pushViewport(viewport(width = 1,
                          height = 0.1,
                          x = 0.5,
                          y = -0.035))
    grid.text(label = xlab)
    upViewport()
  }

  upViewport()

  # ploting trace
  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(0.7, "native"),
                        x = unit(2.5, "native"),
                        y= unit(2.6, "native"),
                        xscale=c(0, Ncol),
                        yscale=c(min(trace), max(trace))))

  if (!missing(labels) & !missing(xaxis)) {
    tick <- seq(1, Ncol, length.out = 15)
    grid.xaxis(at=tick - 0.5,
               label = round(xaxis[tick], 2),
               gp = gpar(cex = 0.6),
               edits = gEdit(gPath="labels", rot=90))

    grid.yaxis(at=round(c(min(trace), max(trace)), 2),
               gp = gpar(cex = 0.6))
  }

  # draw points for trace
  for (i in c(1:Ncol)) {
    pushViewport(viewport(x = unit(i - 0.5, "native"),
                          y = unit(trace[i], "native"),
                          height = unit(1, "native"),
                          width = unit(1, "native")))

      grid.circle(gp = gpar(alpha = 0.8))
    upViewport()
  }
  upViewport()

  # draw segment shade
  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(0.7, "native"),
                        x = unit(2.5, "native"),
                        y= unit(2.6, "native"),
                        xscale=c(0, Ncol),
                        yscale=c(0, 1)))


  for (j in c(1:length(options$columns))) {
    pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                          y = unit(0.5, "native"),
                          height = unit(1, "native"),
                          width = unit(columnWidth[[j]], "native")))
    grid.rect( gp = gpar(alpha = 0.1, col = NA, fill = columnColors[j]))
    grid.lines(c(0, 0), c(0, 1), gp = gpar(alpha = 0.5, col = "black", lwd = 2, lty = 2))
    grid.lines(c(1, 1), c(0, 1), gp = gpar(alpha = 0.5, col = "black", lwd = 2, lty = 2))
    if (columns) {
      grid.text(label = columnName[j], x = 0.5, y = max(trace) * 1.2)
    }
    upViewport()
  }
  upViewport()

  # draw segment titles
  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(1, "native"),
                        x = unit(2.5, "native"),
                        y= unit(2.6, "native"),
                        xscale=c(0, Ncol),
                        yscale=c(0, 1)))


  for (j in c(1:length(options$columns))) {
    pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                          y = unit(0.525, "native"),
                          height = unit(1, "native"),
                          width = unit(columnWidth[[j]], "native")))
    if (columns) {
      grid.text(label = columnName[j], x = 0.5, y = 0.9)
    }
    upViewport()
  }
  upViewport()


  # ploting color scale
  colorScale <- rev(seq(-1,1, length.out = nrow(x)))
  sc <- corToColor(matrix(colorScale))
  pushViewport(viewport(width = unit(0.05, "native"),
                        height = unit(2, "native"),
                        x = unit(4, "native"),
                        y = unit(1, "native"),
                        xscale=c(0, 1),
                        yscale=c(0, nrow(x))))
  for (i in c(1:nrow(x))) {
    pushViewport(viewport(x = unit(0.5, "native"),
                          y = unit(nrow(x) + 1 - i - 0.5, "native"),
                          width = unit(1, "native"),
                          height = unit(1, "native"),
                          xscale = c(0,1),
                          yscale = c(0,1)))
    grid.rect(gp = gpar(fill =  rgb(array(data = c(sc[i,1,1], sc[i,1,2], sc[i,1,3]), dim = c(1,4))/256)))
    grid.text(label = round(colorScale[i], 1), x = 2.5, y = 0.5, gp = gpar(cex = 0.7))
    upViewport()
  }
  upViewport()

  tictoc::toc()
  upViewport()
}

# plotSHY(cor = corToColor(rescale(matrixList[[1]][,1:3], to = c(-1,1))),
#         sig = matrixList[[1]][,1:3]/100,
#         txt = rescale(matrixList[[1]][,1:3], to = c(-1,1)),
#         labs,
#         trace * 2,
#         xaxis,
#         options = list(alpha = 0.1,
#                        xlab = "xlab",
#                        ylab = "ylab",
#                        colors = c("blue", "red", "green"),
#                        columns = list("seg1" = c(3.13, 3.19),
#                                       "seg2" = c(3.19, 3.21),
#                                       "seg3" = c(3.21, 3.24))))
# #
# # #
# matrixList <- list()
# for (i in 1:9) {
#   x <- (matrix(rnorm(12000, 0, 1), 10, 40))
#   x[2,2] <- NA
#   x[2,3] <- 1
#   x[3,3] <- 1
#   x[3,1] <- -0.1
#   trace <- x[1,]
#   matrixList[[i]] <- x
# }
# #
# labs <- paste("variable", c(1:nrow(x)))
# trace <- x[1,]
# xaxis <- seq(3.3, 3.1, length.out = ncol(x))
# # # plotSHY(meanColor(matrixList), labs, trace, xaxis, options = list(segments = c(3.13, 3.16, 3.2, 3.22)))
# #
#
# # png(filename = paste0("test_", j, ".png"), width = 2000, height = 600)


