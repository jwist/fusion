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

# x <-
#
# ser <- read1r("~/data/MRMS-TEST/covid19_biogune_SER_MS-MRMSP_MRMS01_COVp20_090421_PQC_11_1-1-12_1_881.d/ser", 8000000*2)
# spec <- fft(ser)
# plot((Re(spec) + Im(spec))^2, type = "l")
# DW <- 0.1 # sec
# SW <- 2e6 # Hz
# lf <- 71656.9 # Hz
