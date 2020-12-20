#' extract a spectrum (processed) from a folder
#'
#' @param path - the path to the expName folder
#' @param expName - the name of the folder with experiments
#' @param expNo - the number of experiment folder
#' @return a vector with spectra (real part and x axis)
#'
#' @export

getSpectra <- function(path, expName, expNo){
  procFile <- file.path(path,
                        expName,
                        expNo,
                        "/pdata/1/procs")
  if(getParam(procFile, "BYTORDP") == 0) {
    endian <- "little"
  } else {
    endian = "big"
  }
  nc <- getParam(procFile, "\\$NC_proc")
  size <- getParam(procFile, "\\$FTSIZE")
  y <- read1r(paste0(expName, "/", expNo, "/pdata/1/1r"), size, nc, endian)
  sf <- getParam(procFile, "\\$SF=")
  sw <- getParam(procFile, "\\$SW_p") / sf
  inc <- sw/(length(y) - 1)
  offset <- getParam(procFile, "\\$OFFSET")
  x <- seq(from = offset, to = (offset - sw), by = -inc)

  spec <- data.frame(x, y)
  return(spec)
}


