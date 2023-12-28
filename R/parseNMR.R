#' reads all information from bruker folder structures
#' @param folder - the root folder to read from
#' @param options - options (what, specOpts(uncalibrate, fromTo, length.out))
#' @export
#' @importFrom nmr.parser scanFolder readExperiment

parseNMR <- function(folder,
                     options = list(what = c("spec"),
                                    projectName = "",
                                    cohortName = "",
                                    runID = "",
                                    method = "",
                                    sampleMatrixType = "",
                                    specOpts = list(uncalibrate = FALSE,
                                                    fromTo = c(-0.1, 10),
                                                    length.out = 44079))) {

  if (!("what" %in% names(options))) {
    options$what <- "spec"
  }
  if (!("specOpts" %in% names(options))) {
    options$specOpts <- list(uncalibrate = FALSE,
                             fromTo = c(-0.1, 10),
                             length.out = 44079)
  }
  if (!("projectName" %in% names(options))) {
    options$projectName <- ""
  }
  if (!("cohortName" %in% names(options))) {
    options$cohortName <- ""
  }
  if (!("runID" %in% names(options))) {
    options$runID <- ""
  }
  if (!("method" %in% names(options))) {
    options$method <- ""
  }
  if (!("sampleMatrixType" %in% names(options))) {
    options$sampleMatrixType <- ""
  }



  lof <- scanFolder(folder, options)
  EXP <- nmr.parser::cleanNames(lof$EXP[1])
  if (!is.na(lof$USERA2[1])) {
    sampleID <- lof$USERA2
    sampleID <- sapply(sampleID, function(x) gsub("SLTR", "sltr", x))
    sampleID <- sapply(sampleID, function(x) gsub("LTR", "ltr", x))
    sampleID <- sapply(sampleID, function(x) gsub("PQC", "pqc", x))
    sampleID <- sapply(sampleID, function(x) gsub("QC", "qc", x))
    sampleID <- unname(sampleID)
    sampleType <- sampleID
    sampleType[!grepl("ltr|qc", sampleID)] <- "sample"
  } else {
    choices <- strsplit(lof$file[1], "/")[[1]]
    choices <- c("a timeStamp", choices)
    choice <- menu(choices, title = "Select what part of the path to use as sampleID.")
    if (choice == 1) {
      stamp(length(lof))
    } else {
      sampleID <- sapply(lof$file, function(x) strsplit(x, "/")[[1]][choice - 1])
      sampleType <- rep("sample", nrow(lof))
    }
  }

  # making sampleID unique (in case of repetitions in non anpc folders)
  sampleID <- makeUnique(sampleID)

  loe <- data.frame(dataPath = lof$file,
                    sampleID,
                    sampleType)

  if (options$sampleMatrixType == "") {
    req <- rldx_get("matrices", "")
    choices <- paste0(req$content$matrixName,
                                 ": ", req$content$matrixDescription)
    choice <- menu(choices, title = "Choose matrix.")
    sampleMatrixType <- req$content$matrixName[choice]
  } else {
    sampleMatrixType <- options$sampleMatrixType
  }

  if (options$projectName == "") {
    req <- rldx_get("projects", "")
    choices <- paste0(req$content$name,
                      ": ", req$content$prefix)
    choice <- menu(choices, title = "Choose a project.")
    projectName <- req$content$name[choice]
    projectId <- req$content$id[choice]
    prefix <- req$content$prefix[choice]
  } else {
    projectName <- options$projectName
  }

  if (options$cohortName == "") {
    req <- rldx_get("projects", paste0(projectId, "/cohorts"))
    choices <- paste0(req$content$cohortName,
                      ": ", req$content$description)
    choice <- menu(choices, title = "Choose a cohort.")
    cohortName <- req$content$cohortName[choice]
  } else {
    cohortName <- options$cohortName
  }

  if (options$runID == "") {
    choice <- readline(prompt = "runID: ")
    choice <- nmr.parser::cleanNames(choice)
    runID <- paste0(prefix, "r", "XX", choice)
  } else {
    runID <- options$runID
  }

  if ("spec" %in% options$what) {
    options$what = c("spec")

    if (options$method == "") {
      choice <- menu(suppressMessages(meltdown()$NMR$method))
      method <- suppressMessages(meltdown()$NMR$method[choice])
    } else {
      method <- options$method
    }

    # we need to append the EXP to method to make it unique name
    method <- paste0(method, "-", EXP)

    spec <- readExperiment(loe$dataPath, options)

    ppm <- seq(from=options$specOpts$fromTo[1],
               to=options$specOpts$fromTo[2],
               length.out = options$specOpts$length.out)
    varName <- as.character(ppm)

    dat <- as.matrix(do.call("rbind",
                             lapply(spec$spec$spec,
                                    function(x) x$spec$y)))

    type <- "NMR"
  }

  if ("brxlipo" %in% options$what) {
    options$what = c("lipo")
    lipo <- readExperiment(loe$dataPath, options)

    dat <- as.matrix(lipo$lipo[,.SD, .SDcols = grep("value", names(lipo$lipo))])
    varName <- gsub("value.", "", colnames(dat))
    method <- "brxlipo"
    type <- "QUANT"
  }

  if ("brxpacs" %in% options$what) {
    options$what = c("pacs")
    brxpacs <- readExperiment(loe$dataPath, options)

    dat <- as.matrix(brxpacs$pacs[,.SD, .SDcols = grep("value", names(brxpacs$pacs))])
    varName <- gsub("value.", "", colnames(dat))
    method <- "brxpacs"
    type <- "QUANT"
  }

  if ("brxsm" %in% options$what) {
    options$what = c("quant")
    brxsm <- readExperiment(loe$dataPath, options)

    dat <- as.matrix(brxsm$quant[,.SD, .SDcols = grep("value", names(brxsm$quant))])
    varName <- gsub("value.", "", colnames(dat))
    method <- "brxsm"
    type <- "QUANT"
  }

  options$what = c("acqus")
  acqus <- readExperiment(loe$dataPath, options)
  options$what = c("qc")
  qc <- readExperiment(loe$dataPath, options)

  info <- list(info = loe,
               procs = do.call("rbind",
                               lapply(spec$spec$spec,
                                      function(x) x$info)),
               params = acqus$acqus,
               test_tests_name = do.call("rbind",
                                         lapply(qc[[1]]$tests,
                                                function(x) x$name)),
               test_tests_comment = do.call("rbind",
                                            lapply(qc[[1]]$tests,
                                                   function(x) x$comment)),
               test_tests_value = do.call("rbind",
                                          lapply(qc[[1]]$tests,
                                                 function(x) x$value)))


  # create dataElement
  da <- new("dataElement",
            .Data = dat,
            obsDescr = info,
            varName = varName,
            type = type,
            method = method)

  fileName <- paste(c(projectName,
                      cohortName,
                      sampleMatrixType,
                      runID,
                      method), collapse = "_")

  assign(fileName, da)

  save(list=(fileName),
       file = file.path(".", paste0(fileName, ".daE")))
  txt <- paste0("daE can be loaded as var <- local(get(load(",
                file.path(".", paste0(fileName, ".daE")),
                "))) to rename them on the fly")
  message(cat(crayon::blue(txt)))
}
