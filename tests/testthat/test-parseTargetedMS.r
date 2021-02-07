test_that("parse targeted MS works ... Tr", {
  # file = file.path("tests/testthat/plate_1_export.txt")
  file = file.path("plate_1_export.txt")

  tryptophan <- tMsTestsets[[1]]
  da <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))

  expect_equal(unname(da[1,1]), 1.026494, tolerance = 1e-6)
  expect_equal(getID(da), getID(tryptophan))
  expect_equal(getType(da), getType(tryptophan))
  expect_equal(da@.Data, tryptophan@.Data)
  expect_equal(da@obsDescr, tryptophan@obsDescr)
  expect_equal(da@varName, tryptophan@varName)
  expect_equal(da@type, tryptophan@type)
  expect_equal(da@method, tryptophan@method)
})

test_that("parse targeted MS works ... AA", {
  # file <- file.path("tests/testthat/cambridge_aa_PAI-03_plate_4.csv")
  file <- file.path("cambridge_aa_PAI-03_plate_4.csv")
  columnsList <- c("Analyte Name",
                   "Data Set",
                   "SampleType",
                   "m/z expected",
                   "RT [min]",
                   "mSigma",
                   "Area of PI",
                   "Quantity [units]")

  da <- parseTargetedMS(file,
                        method = "aminoAcids",
                        options = list(codePosition = 8))
  aminoAcids <- tMsTestsets[[2]]
  expect_equal(unname(da[1,1]), 3.68)
  expect_equal(getID(da), getID(aminoAcids))
  expect_equal(getType(da), getType(aminoAcids))
  expect_equal(da@.Data, aminoAcids@.Data)
  expect_equal(da@obsDescr, aminoAcids@obsDescr)
  expect_equal(da@varName, aminoAcids@varName)
  expect_equal(da@type, aminoAcids@type)
  expect_equal(da@method, aminoAcids@method)
})

