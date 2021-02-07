## code to prepare `tMS-testsets` dataset goes here

file = "./tests/testthat/plate_1_export.txt"
tryptophan <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))
# save(tryptophan, file = "./data/tryptophan.rda")

file = "./tests/testthat/cambridge_aa_PAI-03_plate_4.csv"
aminoAcids <- parseTargetedMS(file, method = "aminoAcids", options = list(codePosition = 8))
# save(aminoAcids, file = "./data/aminoAcids.rda")

file = "./inst/tryptophanPathwayMolecularWeights.txt"
mw = read.table(file, header = TRUE, sep = "\t", dec = ".")

tMsTestsets <- list(tryptophan = tryptophan,
                    aminoAcids = aminoAcids,
                    mw = mw)
usethis::use_data(tMsTestsets, overwrite = TRUE, internal = TRUE)
