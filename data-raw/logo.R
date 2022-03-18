file = "./inst/anpcLogo.png"

logo <- readPNG(file)
usethis::use_data(logo, overwrite = TRUE, internal = TRUE)
