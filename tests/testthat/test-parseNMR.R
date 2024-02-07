test_that("test parsing lipo anpc from local folders", {

  folder <- c("~/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp294/",
              "~/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp295/")

  parseNMR(folder,
           opts = list(what = c("brxlipo"),
                       projectName = "covid19",
                       cohortName = "mauritius",
                       runID = "COVr49",
                       method = "noesy",
                       sampleMatrixType = "SER",
                       specOpts = list(uncalibrate = FALSE,
                                       fromTo = c(-0.1, 10),
                                       length.out = 44079),
                       outputDir = "."))

  var <- local(get(load("./covid19_mauritius_SER_COVr49@local_brxlipo.daE")))



  expect_equal(nrow(var), 134)
  expect_equal(ncol(var), 112)
  expect_length(var@obsDescr, 6)
  expect_equal(names(var@obsDescr$test_tests_value)[22], "isopropanol-in-mmol-l")
  expect_length(names(var@obsDescr$test_tests_value), 22)
  expect_equal(names(var@obsDescr[[5]])[22], "isopropanol-in-mmol-l")
  expect_length(names(var@obsDescr[[5]]), 22)
  expect_equal(names(var@obsDescr[[6]])[22], "lb-test")
  expect_length(names(var@obsDescr[[6]]), 24)
  expect_equal(var@obsDescr[[2]], data.frame())
  expect_equal(var@obsDescr[[1]]$sampleID[1], "COV17110")
  expect_equal(var@obsDescr[[1]]$dataPath[1], "/Users/jul/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp294/10")
  expect_equal(as.numeric(var[1,1]), 128.89)
  expect_equal(as.numeric(var[1,112]), 15.87)
  expect_equal(var@obsDescr[[1]]$sampleID[134], "sltr#7")
  expect_equal(var@obsDescr[[1]]$dataPath[134], "/Users/jul/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp295/90")
  expect_equal(as.numeric(var[134,1]), 96.8)
  expect_equal(as.numeric(var[134,112]), 15.99)

})

test_that("test parsing lipo anpc from rolodex link", {

  loe = rldx::rldx_get("link", "?runName=COVr49")

  loe$content$list$noesygppr1d$dataPath <- gsub("/exports/nmr/IVDR05/data/",
                                                "/Users/jul/Downloads/F80_ANPC/",
                                                loe$content$list$noesygppr1d$dataPath)

  parseNMR(loe, list(what = "brxlipo"))

  var <- local(get(load("./covid19_mauritius_SER_COVr49_brxlipo.daE")))

  which(var@obsDescr[[1]]$dataPath == "/Users/jul/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp294/10")
  which(var@obsDescr[[1]]$dataPath == "/Users/jul/Downloads/F80_ANPC/covid19_mauritius_SER_NMR-PACS_COVr49_COVp295/90")


  expect_equal(nrow(var), 134)
  expect_equal(ncol(var), 112)
  expect_length(var@obsDescr, 6)
  expect_equal(var@obsDescr[[2]], list())
  expect_equal(as.numeric(var[1,1]), 128.89)
  expect_equal(as.numeric(var[1,112]), 15.87)
  expect_equal(as.numeric(var[91,1]), 96.8)
  expect_equal(as.numeric(var[91,112]), 15.99)

})


test_that("test parsing lipo brx from local folder", {

  folder <- c("~/Downloads/BRUKER_600_80/600/")

  parseNMR(folder,
           opts = list(what = c("brxlipo"),
                          projectName = "external",
                          cohortName = "F80Bruker",
                          runID = "EXTr12",
                          method = "noesy",
                          sampleMatrixType = "SER",
                          specOpts = list(uncalibrate = FALSE,
                                          fromTo = c(-0.1, 10),
                                          length.out = 44079),
                          outputDir = "."))

  var <- local(get(load("./external_F80Bruker_SER_EXTr12@local_brxlipo.daE")))

  expect_equal(nrow(var), 135)
  expect_equal(ncol(var), 112)
  expect_length(var@obsDescr, 6)
  expect_equal(names(var@obsDescr[[4]])[22], "isopropanol-in-mmol-l")
  expect_length(names(var@obsDescr[[4]]), 22)
  expect_equal(names(var@obsDescr[[5]])[22], "isopropanol-in-mmol-l")
  expect_length(names(var@obsDescr[[5]]), 22)
  expect_equal(names(var@obsDescr[[6]])[22], "lb-test")
  expect_length(names(var@obsDescr[[6]]), 24)
  expect_equal(var@obsDescr[[2]], data.frame())
  expect_equal(var@obsDescr[[1]]$sampleID[1], "32565")
  expect_equal(var@obsDescr[[1]]$dataPath[1], "/Users/jul/Downloads/BRUKER_600_80/600/32565/10")
  expect_equal(as.numeric(var[1,1]), 304.05)
  expect_equal(as.numeric(var[1,112]), 11.02)
  expect_equal(var@obsDescr[[1]]$sampleID[134], "Biomex_756514")
  expect_equal(var@obsDescr[[1]]$dataPath[134], "/Users/jul/Downloads/BRUKER_600_80/600/Biomex_756514/10")
  expect_equal(as.numeric(var[134,1]), 134)
  expect_equal(as.numeric(var[134,112]), 23.41)

})

test_that("test parsing lipo brx from rolodex link", {

  loe = rldx::rldx_get("link", "?runName=EXTr12")

  loe$content$list$noesygppr1d$dataPath <- gsub("/exports/nmr/EXTERNAL/F80Bruker/",
                                                "~/Downloads/BRUKER_600_80/",
                                                loe$content$list$noesygppr1d$dataPath)

  parseNMR(loe, list(what = "brxlipo"))

  var <- local(get(load("./external_F80Bruker_SER_EXTr12_brxlipo.daE")))

  which(var@obsDescr[[1]]$dataPath == "~/Downloads/BRUKER_600_80/600/Biomex_756514/10")
  which(var@obsDescr[[1]]$dataPath == "~/Downloads/BRUKER_600_80/600/32565/10")

  expect_equal(nrow(var), 135)
  expect_equal(ncol(var), 112)
  expect_length(var@obsDescr, 6)
  expect_equal(var@obsDescr[[2]], list())
  expect_equal(var@obsDescr[[1]]$sampleID[97], "EXT07086")
  expect_equal(var@obsDescr[[1]]$dataPath[97], "~/Downloads/BRUKER_600_80/600/32565/10")
  expect_equal(as.numeric(var[97,1]), 304.05)
  expect_equal(as.numeric(var[97,112]), 11.02)
  expect_equal(var@obsDescr[[1]]$sampleID[103], "EXT07091")
  expect_equal(as.numeric(var[103,1]), 134)
  expect_equal(as.numeric(var[103,112]), 23.41)
})


test_that("test rbinding", {
  A <- local(get(load("./external_F80Bruker_SER_EXTr12@local_brxlipo.daE")))

  B <- local(get(load("./covid19_mauritius_SER_COVr49@local_brxlipo.daE")))

  expect_equal(names(A@obsDescr[[4]])[22], "isopropanol-in-mmol-l")
  expect_length(names(A@obsDescr[[4]]), 22)
  expect_equal(names(A@obsDescr[[5]])[22], "isopropanol-in-mmol-l")
  expect_length(names(A@obsDescr[[5]]), 22)
  expect_equal(names(A@obsDescr[[6]])[22], "lb-test")
  expect_length(names(A@obsDescr[[6]]), 24)

  expect_equal(names(B@obsDescr[[4]])[22], "isopropanol-in-mmol-l")
  expect_length(names(B@obsDescr[[4]]), 22)
  expect_equal(names(B@obsDescr[[5]])[22], "isopropanol-in-mmol-l")
  expect_length(names(B@obsDescr[[5]]), 22)
  expect_equal(names(B@obsDescr[[6]])[22], "lb-test")
  expect_length(names(B@obsDescr[[6]]), 24)

  m <- list(A@obsDescr[[3]], B@obsDescr[[3]])
  merged <- rbindlist(m, use.names = TRUE, fill = TRUE)
  m <- list(A@obsDescr[[1]], B@obsDescr[[1]])
  merged <- rbindlist(m, use.names = TRUE, fill = TRUE)
  m <- list(A@obsDescr[[2]], B@obsDescr[[2]])
  merged <- rbindlist(m, use.names = TRUE, fill = TRUE)
  m <- list(A@obsDescr[[4]], B@obsDescr[[4]])
  merged <- rbindlist(m, use.names = TRUE, fill = TRUE)
  M <- rbind(A, B)
  expect(dim(M)[1], 269)
  expect(dim(M@obsDescr[[1]])[1], 269)
  expect_identical(M@obsDescr[[1]]$dataPath, M@obsDescr[[3]]$path)
  expect_length(M@obsDescr, 6)
  expect_equal(names(M@obsDescr[[4]])[22], "isopropanol-in-mmol-l")
  expect_length(names(M@obsDescr[[4]]), 22)
  expect_equal(names(M@obsDescr[[5]])[22], "isopropanol-in-mmol-l")
  expect_length(names(M@obsDescr[[5]]), 22)
  expect_equal(names(M@obsDescr[[6]])[22], "lb-test")
  expect_length(names(M@obsDescr[[6]]), 24)
})
