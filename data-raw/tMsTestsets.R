

file = "./inst/tryptophanPathwayMolecularWeights.txt"
mw = read.table(file, header = TRUE, sep = "\t", dec = ".")

tMsTestsets <- list(mw = mw)
usethis::use_data(tMsTestsets, overwrite = TRUE, internal = TRUE)
