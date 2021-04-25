#' extract eretic factor from eretic data frame
#'
#' @param eretic - eretic data frame
#' @return the eretic factor
#'
#' @export
getEreticFactor <- function(eretic){
  idx <- which(eretic$path == "/Quant_Factor_Sample/Eretic_Methods/Application/Application_Parameter/Eretic_Factor")
  fac <- as.numeric(eretic$value[idx])
  idx <- which(eretic$path == "/Quant_Factor_Sample/Eretic_Methods/Eretic/Eretic_Calibration/Eretic_Factor")
  ref <- as.numeric(eretic$value[idx])
  return(list(c(fac = fac, ref = ref)))
}