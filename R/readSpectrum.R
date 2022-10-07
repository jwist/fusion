#' read a spectrum (processed) from a Bruker expno folder
#'
#' @param path - the path to the expNo folder
#' @param procs - the name of the folder with experiments
#' @param options - options
#' @param options$uncalibrate - if true, calibration will be removed (SR set to 0)
#' @param options$eretic - provide eretic correction to be applied
#' @param options$fromTo - provide lower and upper bound for common grid
#' @param options$length.out -set the length of the final data
#' @return a vector with spectra (real part and x axis)
#' @importFrom signal interp1
#' @importFrom data.table data.table
#' @export
readSpectrum <- function(path, procs = TRUE, options = list()){
  path1r <- file.path(path, "pdata", "1", "1r")
  path1i <- file.path(path, "pdata", "1", "1i")

  if (is.logical(procs) && isTRUE(procs)) {
    pathProcs <- file.path(path, "pdata", "1", "procs")
  } else {
    pathProcs <- procs
  }
  pathAcqus <- file.path(path, "acqus")

  # checking that file is not empty
  if (file.exists(pathProcs)) {
    if (is.null(readParam(pathProcs, "NC_proc"))) {
      cat(crayon::yellow("fusion::readSpectrum >> empty procs file for", path, "\n"))
      return(NULL)
    }
  } else {
    cat(crayon::yellow("fusion::readSpectrum >> procs file not found for", path, "\n"))
    return(NULL)
  }

  if (file.exists(pathAcqus)) {
    if (is.null(readParam(pathAcqus, "BF1"))) {
      cat(crayon::yellow("fusion::readSpectrum >> empty acqus for", path, "\n"))
      return(NULL)
    }
  } else {
    cat(crayon::yellow("fusion::readSpectrum >> acqus file not found for", path, "\n"))
    return(NULL)
  }

  if (file.exists(path1r)) {
    if ("im" %in% names(options)) {
      im <- options$im
    } else {
      im = FALSE
    }

    if ("uncalibrate" %in% names(options)) {
      uncalibrate <- options$uncalibrate
    } else {
      uncalibrate <- FALSE
    }

    # reading important parameters
    if(readParam(pathProcs, "BYTORDP") == 0) {
      endian <- "little"
    } else {
      endian = "big"
    }
    nc <- readParam(pathProcs, "NC_proc")
    size <- readParam(pathProcs, "FTSIZE") # it should be equivalent to use SI
    sf <-readParam(pathProcs, "SF") # SF is equal to acqus/BF1
    sw_p <- readParam(pathProcs, "SW_p")
    sw <- sw_p / sf # SW_p is equal to acqus/SW_h
    offset <- readParam(pathProcs, "OFFSET")

    # read additional information for output
    phc0 <- readParam(pathProcs, "PHC0")
    phc1 <- readParam(pathProcs, "PHC1")

    if (phc1 != 0) {
      cat(crayon::yellow("fusion::readSpectrum >> phc1 is expected to be 0 in IVDr experiments,\n",
                         "instead phc1 was found to be:",
                         phc1,
                         "\n"))
    }

    # removing SR (useful for JEDI experiments)

    bf1 <- readParam(pathAcqus, "BF1")
    SR_p <- (sf - bf1) * 1e6 / sf
    SR <- (sf - bf1) * 1e6
    if (uncalibrate) {
      # a negative SR value means an uncalibrated signal on the right of 0
      offset <- offset + SR_p

      cat(crayon::blue("fusion::readSpectrum >> calibration (SR) removed:",
                       SR_p, "ppm", SR, "Hz",
                       "\n"))
    }

    # computing increment, ppm axis and reading spectra
    y <- read1r(path1r, size, nc, endian)
    inc <- sw / (length(y) - 1) # ok
    x <- seq(from = offset, to = (offset - sw), by = -inc)

    # reading imaginary data if necessary
    if (im) {
      yi <- read1r(path1i, size, nc, endian)
    }

    # applying eretic correction if provided
    if ("eretic" %in% names(options)) {
      y <- y / options$eretic
      if (im) {
        yi <- yi / options$eretic
      }

      cat(crayon::blue("fusion::readSpectrum >> spectra corrected for eretic:",
                       options$eretic,
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
        fi <- x > from & x < to
        length.out <- sum(fi)
      }

      newX <- seq(from,
                  to,
                  length.out = length.out)

      y <- interp1(x = x,
                   y = y,
                   xi = newX,
                   method = "spline")

      xr <- newX

      if (im) {
        yi <- interp1(x = x,
                      y = yi,
                      xi = newX,
                      method = "spline")
      }

      cat(crayon::blue("fusion::readSpectrum >> spectra in common grid (from:",
                       from,
                       "to:",
                       to,
                       "dimension:",
                       length.out,
                       ")\n"))
    }

    info <- c(SF = sf,
              PHC0 = phc0,
              PHC1 = phc1,
              SR = SR)
    if (im) {
      spec <- list(info  = info,
                   spec = data.table(xr, y, yi))
    } else {
      spec <- list(info  = info,
                   spec = data.table(xr, y))
    }

    return(spec)

  } else {
    cat(crayon::yellow("fusion::readSpectrum >> data not found for", path, "\n"))
  }
}
