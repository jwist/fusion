## code to prepare `tryptophan` dataset goes here

usethis::use_data(tryptophan, overwrite = TRUE)
file = "./inst/plate_1_export.txt"
tryptophan <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))
save(tryptophan, file = "./data/tryptophan.rda")
