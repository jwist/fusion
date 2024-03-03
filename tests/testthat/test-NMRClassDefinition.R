shape1 = list("name" = "pseudoVoigt", "params" = list("mu"=1,
                                                      "fwhm"=2,
                                                      "base" = 0))

shape2 = list("name" = "pseudoVoigt", "params" = list("mu"=1.1,
                                                      "fwhm"=2.2,
                                                      "base" = 0))

peak1 = new("NMRPeak1D", x = 1.1, y = 1.2, fwhm = 1.3)
peak2 = new("NMRPeak1D", x = 2.1, y = 2.2, fwhm = 2.3)
peak3 = new("NMRPeak1D", x = 3.1, y = 3.2, fwhm = 3.3)


signal1 <- new("NMRSignal1D",
               peaks=list(peak1, peak2),
               nbAtoms = 1,
               multiplicity = "d",
               shiftRange = 10.1,
               heightRangePer = 20.1,
               analyte="A",
               shape=shape1)

signal2 <- new("NMRSignal1D",
               peaks=list(peak1, peak2, peak3),
               nbAtoms = 2,
               multiplicity = "t",
               shiftRange = 10.2,
               heightRangePer = 20.2,
               analyte="A",
               shape=shape2)

test_that("creation of NMRPeak1D", {
  shape = list("name" = "pseudoVoigt", "params" = list("mu"=1,
                                                       "fwhm"=2,
                                                       "minmu"=3,
                                                       "maxmu"=4,
                                                       "minfwhm"=5,
                                                       "maxfwhm"=6,
                                                       "base" = 0))
  peak1 = new("NMRPeak1D", x = 1.1, y = 2.2, fwhm = 3.3)

  expect_equal(peak1@type, "NMRPeak1D")
  expect_equal(peak1@x, 1.1)
  expect_equal(peak1@y, 2.2)
  expect_equal(peak1@fwhm, 3.3)
  expect_equal(peak1@shape, list())

  peak11 = new("NMRPeak1D", x = 1.1, y = 2.2, fwhm = 3.3, shape=shape)
  expect_equal(peak11@type, "NMRPeak1D")
  expect_equal(peak11@x, 1.1)
  expect_equal(peak11@y, 2.2)
  expect_equal(peak11@fwhm, 3.3)
  expect_equal(peak11@shape, shape)
})

test_that("creation of NMRSignal1D", {

  peak1 = new("NMRPeak1D", x = 1.1, y = 1.2, fwhm = 1.3, shape=shape1)
  peak2 = new("NMRPeak1D", x = 2.1, y = 2.2, fwhm = 2.3, shape=shape2)
  peak3 = new("NMRPeak1D", x = 3.1, y = 3.2, fwhm = 3.3)

  signal1 <- new("NMRSignal1D",
                   peaks=list(peak1, peak2),
                   nbAtoms = 1,
                   multiplicity = "d",
                   shiftRange = 10.1,
                   heightRangePer = 20.1)

  expect_equal(signal1@type, "NMRSignal1D")
  expect_equal(signal1@nbAtoms, 1)
  expect_equal(signal1@peaks, list(peak1, peak2))
  expect_equal(signal1@shiftRange, 10.1)
  expect_equal(signal1@heightRangePer, 20.1)
  expect_equal(signal1@multiplicity, "d")
  expect_equal(signal1@analyte, NA_character_)

  signal2 <- new("NMRSignal1D",
                 peaks=list(peak1, peak2, peak3),
                 nbAtoms = 2,
                 multiplicity = "t",
                 shiftRange = 10.2,
                 heightRangePer = 20.2)

  expect_equal(signal2@type, "NMRSignal1D")
  expect_equal(signal2@nbAtoms, 2)
  expect_equal(signal2@peaks, list(peak1, peak2, peak3))
  expect_equal(signal2@shiftRange, 10.2)
  expect_equal(signal2@multiplicity, "t")
  expect_equal(signal2@heightRangePer, 20.2)
  expect_equal(signal2@analyte, NA_character_)
  expect_equal(signal2@shape, list())


  signal3 <- new("NMRSignal1D",
                 peaks=list(peak3),
                 nbAtoms = 3,
                 multiplicity = "s",
                 shiftRange = 10.2,
                 heightRangePer = 20.2,
                 shape=shape2)

  expect_equal(signal3@type, "NMRSignal1D")
  expect_equal(signal3@nbAtoms, 3)
  expect_equal(signal3@peaks, list(peak3))
  expect_equal(signal3@shiftRange, 10.2)
  expect_equal(signal3@multiplicity, "s")
  expect_equal(signal3@heightRangePer, 20.2)
  expect_equal(signal3@analyte, NA_character_)
  expect_equal(signal3@shape, shape2)
})

test_that("creation of NMRSignalModel", {

  ppm=1:10
  model1 = new("NMRSignalModel", signalsInput = list(signal1, signal2),
              from = ppm[[1]],
              to =  ppm[[10]],
              ppm = ppm,
              experimental = ppm*3)

  expect_equal(model1@type, "NMRSignalModel")
  expect_equal(model1@signalsInput, list(signal1, signal2))
  expect_equal(model1@signalsOutput, list())
  expect_equal(model1@from, ppm[[1]])
  expect_equal(model1@to, ppm[[10]])
  expect_equal(model1@ppm, ppm)
  expect_equal(model1@experimental, ppm*3)
  expect_equal(model1@fitted, NA_real_)
  expect_equal(model1@shape, list())
  expect_equal(model1@error, list())

})

test_that("creation of Analyte", {
  analyte1 = new("Analyte", signals = list(signal1, signal2),
               name = "A")

  expect_equal(analyte1@type, "Analyte")
  expect_equal(analyte1@signals, list(signal1, signal2))
  expect_equal(analyte1@name, "A")
  expect_equal(analyte1@category, NA_character_)
  expect_equal(analyte1@inchiKey, NA_character_)
  expect_equal(analyte1@id, NA_character_)
  expect_equal(analyte1@diaID, NA_character_)
})

test_that("Check that toJSONFile file works on NMRPeak1D", {
  peak1_json <- paste(capture.output(toJSONFile(peak1, NA, "")), collapse = "")
  expect_equal(peak1_json, "{\"x\":1.1,\"y\":1.2,\"fwhm\":1.3,\"type\":\"NMRPeak1D\"}")

  peak11 = new("NMRPeak1D", x = 1.7, y = 2.2, fwhm = 3.3, shape=shape1)
  peak11_json <- paste(capture.output(toJSONFile(peak11, NA, "")), collapse = "")
  peak11_json_res <- "{\"x\":1.7,\"y\":2.2,\"fwhm\":3.3,\"shape\":{\"name\":\"pseudoVoigt\",\"params\":{\"mu\":1,\"fwhm\":2,\"base\":0}},\"type\":\"NMRPeak1D\"}"
  expect_equal(peak11_json, peak11_json_res)

  fileName = "/tmp/test.json"
  file.create(fileName)
  fileConn<-file(fileName, "wb")
  toJSONFile(peak11, control=c(no_xy=TRUE), con=fileConn)

  fileLines <-readLines(fileName, encoding="UTF-8")
  expect_equal(paste(fileLines, collapse = ""), peak11_json_res)

  close(fileConn)
})

test_that("Check that toJSONFile file works on NMRSignal1D", {
  signal1_json <- paste(capture.output(toJSONFile(signal1, NA, "")), collapse = "")
  signal1_json_res <- "{\"peaks\":[{\"x\":1.1,\"y\":1.2,\"fwhm\":1.3,\"type\":\"NMRPeak1D\"},{\"x\":2.1,\"y\":2.2,\"fwhm\":2.3,\"type\":\"NMRPeak1D\"}],\"nbAtoms\":1,\"integration\":0,\"multiplicity\":\"d\",\"shiftRange\":10.1,\"heightRangePer\":20.1,\"widthFactor\":1,\"shape\":{\"name\":\"pseudoVoigt\",\"params\":{\"mu\":1,\"fwhm\":2,\"base\":0}},\"analyte\":\"A\",\"validated\":0,\"type\":\"NMRSignal1D\"}"
  expect_equal(signal1_json, signal1_json_res)

  fileName = "/tmp/test1.json"
  file.create(fileName)
  fileConn<-file(fileName, "wb")
  toJSONFile(signal1, control=c(no_xy=TRUE), con=fileConn)

  fileLines <-readLines(fileName, encoding="UTF-8")
  expect_equal(paste(fileLines, collapse = ""), signal1_json_res)

  close(fileConn)
})

test_that("Check that toJSONFile file works on NMRSignalModel", {
  ppm=1:10
  model1 = new("NMRSignalModel", signalsInput = list(signal1, signal2),
               from = ppm[[1]],
               to =  ppm[[10]],
               ppm = ppm,
               experimental = ppm*3)

  model1_json <- paste(capture.output(toJSONFile(model1, NA, "")), collapse = "")
  model1_resurrected <- fromVector(jsonlite::fromJSON(model1_json, simplifyVector = FALSE))

  expect_equal(model1, model1_resurrected)
})

test_that("Check that toJSONFile file works on Analyte", {
  analyte1 = new("Analyte", signals = list(signal1, signal2),
                 name = "A")

  analyte1_json <- paste(capture.output(toJSONFile(analyte1, NA, "")), collapse = "")
  analyte1_resurrected <- fromVector(jsonlite::fromJSON(analyte1_json, simplifyVector = FALSE))

  expect_equal(analyte1, analyte1_resurrected)
})

test_that("Check that toJSONFile file works on vector", {

})

test_that("Check that toJSONFile file works on list", {

})

test_that("Check that toJSONFile file works on numeric", {

})

test_that("Check that toJSONFile file works on logical", {

})

test_that("Check that toJSONFile file works on character", {

})

test_that("Check that toJSONFile file works on matrix", {

})

test_that("Check that writeToJSON and fromVector works for a NMRSignalModel", {
  ppm=1:10
  model1 = new("NMRSignalModel", signalsInput = list(signal1, signal2),
               from = ppm[[1]],
               to =  ppm[[10]],
               ppm = ppm,
               experimental = ppm*3)
  fileName = "/tmp/model1.json"
  writeToJSON(model1, fileName)
  model1_resurrected <- fromVector( jsonlite::fromJSON(fileName, simplifyVector = FALSE))

  model1@ppm = NA_real_
  model1@experimental = NA_real_
  expect_equal(model1, model1_resurrected)
})

test_that("Check that writeToJSON works for lists of list with NAs", {
  input <- list(1, 2, NA, 4, list(NA, 1, "2"))
  input_expected <- list(1, 2, NULL, 4, list(NULL, 1, "2"))
  
  fileName = "/tmp/list.json"
  writeToJSON(input, fileName)
  input_resurrected <- jsonlite::fromJSON(fileName, simplifyVector = FALSE)
  
  expect_equal(input_expected, input_resurrected)
})



