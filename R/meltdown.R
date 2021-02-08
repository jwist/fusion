

meltdown <- function() {
  melt <- list(
    "NMR" = list(method = c("noesy",
                            "cpmg",
                            "jres",
                            "dire",
                            "diff"),
                 sampleType = c("sample",
                                "pqc",
                                "standard",
                                "ltr")),
    "T-MS" =  list(method = c("tryptophan",
                              "aminoAcids"),
                   sampleType = c("sample",
                                  "qc",
                                  "pqc",
                                  "blank",
                                  "standard",
                                  "ltr"))
  )
  cat("replicates goes with # (we don't use _ or -)")
  return(melt)
}
