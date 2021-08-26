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
plotCorrelation <- function(x, labels, trace, xaxis, options = list()) {
  tictoc::tic("init")
  if ("alpha" %in% names(options)) {
    alpha <- options$alpha
  } else {
    alpha <- 1
  }

  if (!missing(labels) & !missing(xaxis)) {
    grid.newpage()
  }
  pushViewport(viewport(width = 0.9,
                        height = 0.9,
                        xscale=c(0, 4),
                        yscale=c(0, 3)))

  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(2, "native"),
                        x = unit(2.5, "native"),
                        y= unit(1, "native"),
                        xscale=c(0, ncol(x)),
                        yscale=c(0, nrow(x))))

  corp<- colorRamp(c("blue", "white", "red"))

  for (i in c(1:nrow(x))) {
    grid.text(x = unit(-10, "inches"),
              y = unit(nrow(x) + 1 - i - 0.5, "native"),
              label = labels[i], gp = gpar(cex = 0.5), just = "center")
    for (j in c(1:ncol(x))) {
      grid.rect(x = unit(j - 0.5, "native"),
                y = unit(nrow(x) + 1 - i - 0.5, "native"),
                width = unit(1, "native"),
                height = unit(1, "native"),
                gp = gpar(col = NA,
                          fill = rgb(corp(x[i,j])/255),
                          alpha = alpha))
    }
  }

  if (!missing(labels) & !missing(xaxis)) {
    pushViewport(viewport(width = unit(ncol(x), "native"),
                          height = 0.1,
                          x = 1.025,
                          y = 0.5,
                          angle = 90))
    grid.text(label = "spc")
    upViewport()

    pushViewport(viewport(width = 1,
                          height = 0.1,
                          x = 0.5,
                          y = -0.035))
    grid.text(label = "spc")
    upViewport()
  }

  upViewport()

  pushViewport(viewport(width = unit(3, "native"),
                        height = unit(0.7, "native"),
                        x = unit(2.5, "native"),
                        y= unit(2.6, "native"),
                        xscale=c(0, ncol(x)),
                        yscale=c(min(x), max(x))))

  if (!missing(labels) & !missing(xaxis)) {
    tick <- seq(1, ncol(x), length.out = 30)
    grid.xaxis(at=tick - 0.5,
               label = round(xaxis[tick], 2),
               gp = gpar(cex = 0.6),
               edits = gEdit(gPath="labels", rot=90))

    grid.yaxis(at=round(c(min(trace), max(trace)), 2),
               gp = gpar(cex = 0.6))
  }

  for (i in c(1:ncol(x))) {
    pushViewport(viewport(x = unit(i - 0.5, "native"),
                          y = unit(trace[i], "native"),
                          height = unit(1, "native"),
                          width = unit(1, "native")))
    grid.circle()
    upViewport()
  }
  upViewport()
  tictoc::toc()
  tictoc::tic("label")
  if (!missing(labels) & !missing(xaxis)) {
    pushViewport(viewport(width = unit(1, "native"),
                          height = unit(2, "native"),
                          x = unit(0.5, "native"),
                          y= unit(1, "native"),
                          xscale=c(0, 1),
                          yscale=c(0, nrow(x))))

    for (i in c(1:nrow(x))) {
      maxTextWidth <- max(calcStringMetric(labels)$width)*0.7

      if (i %% 2 == 0) {
        grid.roundrect(x = unit(1 + maxTextWidth/2, "inches"),
                       y = unit(i - 0.5, "native"),
                       height = unit(0.8, "native"),
                       width = unit(maxTextWidth, "inches"),
                       gp = gpar(col = NA, fill = "black", alpha = 0.1))
        grid.text(x = unit(1 + maxTextWidth/2, "inches"),
                  y = unit(i - 0.5, "native"),
                  label = labels[i], gp = gpar(cex = 0.5), just = "center")
      } else {
        grid.text(x = unit(1 - maxTextWidth/2, "inches"),
                  y = unit(i - 0.5, "native"),
                  label = labels[i], gp = gpar(cex = 0.5), just = "center")
      }
    }
    upViewport()
  }
  tictoc::toc()
  upViewport()
}


x <- (matrix(rnorm(1000, 0.5, 0.1), 10, 100))
labs <- paste("variable", c(1:nrow(x)))
trace <- x[1,]
xaxis <- seq(3.3, 3.1, length.out = ncol(x))
plotCorrelation(x, labs, trace, xaxis, options = list(alpha = 0.1))

for (i in 1:9) {
  x <- (matrix(rnorm(1000, 0.5, 0.1), 10, 100))
  x[9,9] <- rnorm(1, 0.9, 0.05)
  x[8,8] <- rnorm(1, 0.5, 0.05)
  x[7,7] <- max(0, rnorm(1, 0.2, 0.05))
  trace <- x[1,]
  plotCorrelation(x = x, trace = trace, options = list(alpha = 0.1))

}






