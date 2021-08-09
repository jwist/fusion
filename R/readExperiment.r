#' read experiment from a bruker folder (expno)
#'
#' @param path - the path to the expNo folder
#' @return a list with all read elements
#'
#' @export
readExperiment <- function(path, acqus = TRUE, procs = TRUE) {
  if (is.logical(acqus) && isFALSE(acqus)) {
    acqus <- readParams(file.path(path, "acqus"))
  }
  if (is.logical(procs) && isFALSE(procs)) {
    procs <- readParams(file.path(path, "pdata", "1", "procs"))
  }
  title <- getTitle(file.path(path, "pdata", "1", "title"))

  spec <- readSpectrum(path, procs)
  if (!is.null(spec)) {
    res <- list(path = path,
                spec = spec,
                acqus = acqus,
                procs = procs,
                title = title)

    if (file.exists(file.path(path, "QuantFactorSample.xml"))) {
      eretic <- getEretic(path)
      if (!is.null(eretic)) {
        res <- c(res, list(eretic = eretic))
      }
    }

    if (file.exists(file.path(path, "pdata", "1", "lipo_results.xml"))) {
      lipoproteins <- getLipoprotein(path)
      if (!is.null(lipoproteins)) {
        res <- c(res, list(lipoproteins = lipoproteins))
      }
    }

    if (file.exists(file.path(path, "pdata", "1", "plasma_quant_report.xml")) |
        file.exists(file.path(path, "pdata", "1", "urine_quant_report_e.xml"))) {
      ivdr <- getIvdr(path)
      if (!is.null(ivdr)) {
        res <- c(res, list(ivdr = ivdr))
      }
    }
    return(res)
  }
}

# path <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/11"
# exp <- readExperiment(path)
