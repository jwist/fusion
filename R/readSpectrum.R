#' read a spectrum (processed) from a Bruker expno folder
#'
#' @param path - the path to the expNo folder
#' @param procs - the name of the folder with experiments
#' @param options - options
#' @return a vector with spectra (real part and x axis)
#'
#' @export
readSpectrum <- function(path, procs = TRUE, options = list()){
  path1r <- file.path(path, "pdata", "1", "1r")

  if (file.exists(path1r)) {
    if (is.logical(procs) && isTRUE(procs)) {
      pathProcs <- file.path(path, "pdata", "1", "procs")
    } else {
      pathProcs <- procs
    }
    pathAcqus <- file.path(path, "acqus")

    # reading important parameters
    if(readParam(pathProcs, "BYTORDP") == 0) {
      endian <- "little"
    } else {
      endian = "big"
    }

    nc <- readParam(pathProcs, "NC_proc")
    size <- readParam(pathProcs, "FTSIZE") # it should be equivalent to use SI
    sf <-readParam(pathProcs, "SF") # SF is equal to acqus/BF1
    sw <- readParam(pathProcs, "SW_p") / sf # SW_p is equal to acqus/SW_h
    offset <- readParam(pathProcs, "OFFSET")

    # removing SR (useful for JEDI experiments)
    if ("uncalibrate" %in% names(options)) {
      if (uncalibrate) {
        BF1 <- readParam(pathAcqus, "BF1")
        SR_p <- (SF - BF1)
        SR <- (SF - BF1) * 1e6
        offset <- offset - SR_p

        cat(crayon::blue("fusion::readSpectrum >> calibration (SR) removed:",
                         SR_p,
                         "\n"))
      }
    }

    # computing increment, ppm axis and reading spectra
    y <- read1r(path1r, size, nc, endian)
    inc <- sw / (length(y) - 1) # ok
    x <- seq(from = offset, to = (offset - sw), by = -inc)


    # applying eretic correction if provided
    if ("ereticFactor" %in% names(options)) {
      y <- y / options$ereticFactor

      cat(crayon::blue("fusion::readSpectrum >> spectra corrected for eretic:",
                       options$ereticFactor,
                       "\n"))
    }

    # if upper and lower bounds are provided the spectra is extrapolated to fit
    # those boundaries. If no length.out is provided, them similar length is
    # used.
    if ("fromTo" %in% names(options)) {
      from <- options$fromTo[1]
      to <- options$fromTo[2]

      if (from > to) {
        cat(crayon::blue("fusion::readSpectrum >> from should be smaller than to\n"))
      }

      if ("length.out" %in% names(options)) {
        length.out <- options$length.out
      } else {
        fi <- s$x > from & s$x < to
        length.out <- sum(fi)
      }

      newX <- seq(from,
                  to,
                  length.out = length.out)

      y <- interp1(x = x,
                     y = y,
                     xi = newX,
                     method = "spline")

      x <- newX

      cat(crayon::blue("fusion::readSpectrum >> spectra in common grid (from:",
                       from,
                       "to:",
                       to,
                       "dimension:",
                       length.out,
                       ")\n"))
    }

    spec <- data.table(x, y)
    return(spec)

  } else {
    cat(crayon::yellow("fusion::readSpectrum >> data not found for", path, "\n"))
  }
}

