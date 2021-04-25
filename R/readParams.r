
readParams <- function(filePath) {
  if (file.exists(filePath)) {
  buf <- file(filePath, open = "r")
  txt <- readLines(buf, n = -1, warn = FALSE)
  close(buf)
  
  content <- list()
  counter <- 1
  while (counter < length(txt)) {
    line <- txt[[counter]]
    if (grepl("^##END=", line)) {
      break
    }
    path <- strsplit(filePath, "/")[[1]]
    if (grepl("^##[A-Z]", line)) {
      param <- strsplit(line, "= ")[[1]]
      content <- c(content, list(c(path = path[length(path)],
                                   name = gsub("##", "", param[1]),
                                   value = param[2])))
    }
    if (grepl("^##\\$", line)) {
      param <- strsplit(line, "= ")[[1]]
      value <- gsub("[<->]", "", param[2])
      subcounter <- 1
      while (!grepl("^##", txt[[counter + 1]])) {
        counter <- counter + 1
        value <- paste(value, txt[[counter]])
      }
      value <- strsplit(value, " ")
      if (length(value[[1]]) > 1) {
        for (i in 2:length(value[[1]])) {
          content <- c(content, list(c(path = path[length(path)],
                                       name = paste0(gsub("##\\$", "", param[1]), "_", i - 2),
                                       value = value[[1]][i])))
        }
      } else {
        content <- c(content, list(c(path = path[length(path)],
                                     name = gsub("##\\$", "", param[1]),
                                     value = value)))
      }
    }
    counter <- counter + 1
  }
  return(data.frame(do.call(rbind, content)))
  } else {
    cat(crayon::yellow("fusion::getParams >>", filePath, " file not found\n"))
  }
}

# filePath <- "/home/rstudio/data/imports/data2/BIOGUNE/HB-COVID0001/10/acqus"
# readParams(filePath)