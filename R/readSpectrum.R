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

    if(readParam(pathProcs, "BYTORDP") == 0) {
      endian <- "little"
    } else {
      endian = "big"
    }

    nc <- readParam(pathProcs, "NC_proc")
    size <- readParam(pathProcs, "FTSIZE")
    y <- read1r(path1r, size, nc, endian)
    sf <-readParam(pathProcs, "SF")
    sw <- readParam(pathProcs, "SW_p") / sf
    inc <- sw/(length(y) - 1)
    offset <- readParam(pathProcs, "OFFSET")
    x <- seq(from = offset, to = (offset - sw), by = -inc)

    # applying eretic correction if provided
    if ("ereticFactor" %in% names(options)) {
      y <- y / options$ereticFactor

      cat(crayon::blue("fusion::readSpectrum >> spectra corrected for eretic:",
                       options$ereticFactor,
                       "\n"))
    }

    # correcting for offset if provided (useful for JEDI experiments)
    if ("offset" %in% names(options)) {
      x <- x + options$offset

      cat(crayon::blue("fusion::readSpectrum >> spectra corrected for offset:",
                       options$offset,
                       "\n"))
    }

    # if upper and lower bounds are provided the spectra is extrapolated to fit
    # those boundaries. If no length.out is provided, them similar length is
    # used.
    if ("fromTo" %in% names(options)) {
      from <- options$fromTo[1]
      to <- options$fromTo[2]

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

