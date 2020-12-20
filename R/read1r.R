#' read spectrum (processed)
#'
#' @param filePath - the path to the spectrum (processed)
#' @param numberOfPoints - the number of points that should be read
#' @param nc - power factor
#' @param endian - endianness
#' @return the spectrum
#'
#' @export

read1r <- function(filePath, numberOfPoints, nc = 0, endian = "little"){
  spec <- readBin(filePath,
                  what = "int",
                  n = numberOfPoints,
                  size = 4,
                  signed = TRUE,
                  endian = endian)
  spec <- (2^nc) * spec
}


