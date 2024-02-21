
# B <- data.frame(path = c(1, 2, 3), value = c(10, 20, NA))
# qc <- data.frame(path = c(1, 2, 3), value = c(NA, NA, NA))
# A <- data.frame(path = c(1, 2, 3, 4), value = c(50, 3, 2, 2), test = c(34, 5, 22, 12))
# leftJoin(A, B, by = "path")

leftJoinWithAcqus <- function(A, B, by) {
  merged <- merge(A, B, by, all.x = TRUE, suffixes = c(".AAA",".BBB"))

  setDT(merged)
  matched <- merged[,.SD,.SDcols = c(names(merged)[!grepl("acqus", names(merged))])]
  setnames(matched, names(matched), gsub("\\.BBB$", "", names(matched)))
  return(as.data.frame(matched))
}



