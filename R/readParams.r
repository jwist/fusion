
readParams <- function(filePath) {
  if (file.exists(filePath)) {
    buf <- file(filePath, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)
    content <- list()
    counter <- 1 # line counter
    while (counter < length(txt)) {
      line <- txt[[counter]]
      if (grepl("^##END=", line)) {
        break
      }
      path <- strsplit(filePath, "/")[[1]]
      # get titles
      if (grepl("^##[A-Z]", line)) {
        param <- strsplit(line, "= ")[[1]]
        content <- c(content, list(c(path = path[length(path)],
                                     name = gsub("##", "", param[1]),
                                     value = param[2])))
      }
      # get params
      if (grepl("^##\\$", line)) {
        param <- strsplit(line, "= ")[[1]]
        # value <- gsub("[<->]", "", param[2])
        value <- param[2]
        # look for vectors
        if (grepl("\\([0-9]+..[0-9]+\\)", value)) {
          counter <- counter + 1
          vect <- txt[[counter]]
          value <- strsplit(vect, " ")
          for (i in 1:length(value[[1]])) {
            content <- c(content, list(c(path = path[length(path)],
                                         name = paste0(gsub("##\\$", "", param[1]), "_", i - 1),
                                         value = value[[1]][i])))
          }
        } else {
          content <- c(content, list(c(path = path[length(path)],
                                       name = gsub("##\\$", "", param[1]),
                                       value = gsub("[<->]", "", value))))
        }
      }
      counter <- counter + 1
    }
    ret <- data.frame(do.call(rbind, content))
    fi <- sapply(ret$value, function(x) identical(x, character(0)))
    ret$value[fi] <- NA

    return(data.frame(path = unlist(ret$path),
                      name = unlist(ret$name),
                      value = unlist(ret$value)))
    # return(ret)
  } else {
    cat(crayon::yellow("fusion::getParams >>", filePath, " file not found\n"))
  }
}

# filePath <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/10/acqus"
# readParams(filePath)
