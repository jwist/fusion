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
    } else {
      if(getParam(procs, "BYTORDP") == 0) {
        endian <- "little"
      } else {
        endian = "big"
      }
      nc <- getParam(procs, "NC_proc")
      size <- getParam(procs, "FTSIZE")
      y <- read1r(path1r, size, nc, endian)
      sf <- getParam(procs, "SF")
      sw <- getParam(procs, "SW_p") / sf
      inc <- sw/(length(y) - 1)
      offset <- getParam(procs, "OFFSET")
      x <- seq(from = offset, to = (offset - sw), by = -inc)
    }
    if ("ereticFactor" %in% names(options)) {
      spec <- data.frame(x, y / options$ereticFactor)
      cat(crayon::blue("fusion::readSpectrum >> spectra corrected for eretic (",
                       options$ereticFactor,
                       ")\n"))
    } else {
      spec <- data.frame(x, y)
    }
    return(spec)
  } else {
    cat(crayon::yellow("fusion::readSpectrum >> data not found for", path, "\n"))
  }
}


