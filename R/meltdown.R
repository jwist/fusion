

meltdown <- function(type) {
  type = 1
  switch(type,
         "NMR" = c("noesy", "cpmg", "jres", "dire", "diff"),
         "MS" = 2)
  cat("replicates goes with # (we don't use _ or -)")
}
