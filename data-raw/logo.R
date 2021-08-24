file = "./inst/anpc logo2.png"


logo <- readPNG(file)
usethis::use_data(logo, overwrite = TRUE, internal = TRUE)
