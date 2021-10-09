#' plot correlation between NMR trace (part of) and MS data
#' @param x - a matrix with correlation values
#' @param labels - the name of the MS values (x rows)
#' @param trace - the NMR trace to plot (values)
#' @param xaxis - the xaxis for the NMR trace
#'
#' @export
#' @importFrom grid grid.xaxis grid.yaxis pushViewport viewport grid.rect
#' @importFrom grid grid.newpage grid.text gEdit upViewport gpar unit
#' @importFrom grid grid.circle calcStringMetric grid.roundrect
#' @importFrom grDevices rgb colorRamp
plotSHY <- function(cor, sig, txt, labels, trace, xaxis, options = list()) {
  tictoc::tic("init")
  if ("alpha" %in% names(options)) {
    alpha <- options$alpha
  } else {
    alpha <- 1
  }

  if ("columns" %in% names(options)) {
    columns <- TRUE
    columnColors <- rev(options$colors)
    columnName <- rev(names(options$columns))
    columnCenter <- sapply(options$columns, function(x) sum(xaxis < mean(x)))
    columnWidth <- sapply(options$columns, function(x) sum(xaxis > x[1] & xaxis < x[2] ))
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
  x <- cor
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
                        xscale=c(0, ncol(x)),
                        yscale=c(0, nrow(x))))

  maxTextWidth <- max(calcStringMetric(labels)$width) * 0.8 #cex

  corp<- colorRamp(c("blue", "white", "red"))

  for (i in c(1:nrow(x))) {
    if (!missing(labels) & !missing(xaxis)) {
      grid.text(x = unit(-maxTextWidth, "inches"),
                y = unit(nrow(x) + 1 - i - 0.5, "native"),
                label = labels[i], gp = gpar(cex = 0.8), just = "center")
    }
    for (j in c(1:length(options$columns))) {
      if (length(dim(x)) > 2) {
        if (is.na(x[i,j,1]) | is.na(x[i,j,2]) | is.na(x[i,j,3])) {
          fill <- "black"
        } else {
          fill <- rgb(array(data = c(x[i,j,1], x[i,j,2], x[i,j,3]), dim = c(1,4))/256)
        }
      } else {
        fill <- rgb(corp(x[i,j])/255)
      }

      pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                            y = unit(nrow(x) + 1 - i - 0.5, "native"),
                            width = unit(columnWidth[[j]], "native"),
                            height = unit(1, "native"),
                            xscale = c(0,1),
                            yscale = c(0,1)))
      if (numToSignificance(sig[i, j]) != "NS" & !is.na(sig[i, j])) {
        grid.rect(gp = gpar(fill = NA, alpha = alpha))
        grid.circle(x = 0.5, y = 0.55, r = 0.3, gp = gpar(fill = fill, col = NA))
        grid.text(x = 0.5, y = 0.15, label = round(txt[i,j], digits = 2), gp = gpar(cex = 0.7, just = "center"))
        grid.text(x = 0.5, y = 0.875, label = numToSignificance(sig[i, j]), gp = gpar(cex = 0.7, just = "center"))
      } else {
        grid.rect(gp = gpar(fill = NA, alpha = alpha))
      }
      upViewport()
    }
  }

  if (!missing(labels) & !missing(xaxis)) {
    pushViewport(viewport(width = unit(ncol(x), "native"),
                          height = 0.1,
                          x = 1.025,
                          y = 0.5,
                          angle = 90))
    grid.text(label = ylab)
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
                        xscale=c(0, ncol(x)),
                        yscale=c(min(trace), max(trace))))

  if (!missing(labels) & !missing(xaxis)) {
    tick <- seq(1, ncol(x), length.out = 15)
    grid.xaxis(at=tick - 0.5,
               label = round(xaxis[tick], 2),
               gp = gpar(cex = 0.6),
               edits = gEdit(gPath="labels", rot=90))

    grid.yaxis(at=round(c(min(trace), max(trace)), 2),
               gp = gpar(cex = 0.6))
  }
  # draw segment shade
  for (j in c(1:length(options$columns))) {
    pushViewport(viewport(x = unit(columnCenter[[j]], "native"),
                          y = unit(0.525, "native"),
                          height = unit(0.65, "native"),
                          width = unit(columnWidth[[j]], "native")))
    grid.rect(gp = gpar(alpha = 0.1, col = NA, fill = columnColors[j]))
    grid.lines(c(0, 0), c(0, 1), gp = gpar(alpha = 0.5, col = "black", lwd = 2, lty = 2))
    grid.lines(c(1, 1), c(0, 1), gp = gpar(alpha = 0.5, col = "black", lwd = 2, lty = 2))
    if (columns) {
      grid.text(label = columnName[j], x = 0.5, y = 1.1)
    }
    upViewport()
  }
  # draw points for trace
  for (i in c(1:ncol(x))) {
    pushViewport(viewport(x = unit(i - 0.5, "native"),
                          y = unit(trace[i], "native"),
                          height = unit(1, "native"),
                          width = unit(1, "native")))

      grid.circle(gp = gpar(alpha = 0.8))
    upViewport()
  }
  upViewport()
  tictoc::toc()

  upViewport()
}
# plotSHY(cor = corToColor(matrixList[[1]]),
#         sig = matrixList[[1]]/100,
#         txt = matrixList[[1]],
#         labs,
#         trace,
#         xaxis,
#         options = list(alpha = 0.1,
#                        xlab = "xlab",
#                        ylab = "ylab",
#                        colors = c("blue", "red", "green"),
#                        columns = list("seg1" = c(3.13, 3.18),
#                                       "seg2" = c(3.18, 3.22),
#                                       "seg3" = c(3.22, 3.27))))
#
#
# matrixList <- list()
# for (i in 1:9) {
#   x <- (matrix(rnorm(12000, 0.5, 0.1), 10, 200))
#   x[9,9] <- rnorm(1, 0.9, 0.05)
#   x[8,8] <- rnorm(1, 0.5, 0.05)
#   x[7,7] <- max(0, rnorm(1, 0.2, 0.05))
#   x[2,2] <- NA
#   trace <- x[1,]
#   matrixList[[i]] <- x
# }
#
# labs <- paste("variable", c(1:nrow(x)))
# trace <- x[1,]
# xaxis <- seq(3.3, 3.1, length.out = ncol(x))
# plotSHY(meanColor(matrixList), labs, trace, xaxis, options = list(segments = c(3.13, 3.16, 3.2, 3.22)))


# png(filename = paste0("test_", j, ".png"), width = 2000, height = 600)
corToColor <- function(mat) {
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

