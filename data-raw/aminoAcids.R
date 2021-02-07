## code to prepare `aminoAcids` dataset goes here

usethis::use_data(aminoAcids, overwrite = TRUE)
file = "./inst/cambridge_aa_PAI-03_plate_4.csv"
aminoAcids <- parseTargetedMS(file, method = "aminoAcids", options = list(codePosition = 8))
save(aminoAcids, file = "./data/aminoAcids.rda")
