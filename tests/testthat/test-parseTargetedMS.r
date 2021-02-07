test_that("parse targeted MS works ... Tr", {
  file = file.path("plate_1_export.txt")
  print(paste("PATHPATH", getwd()))
  print(paste("PATHPATH", file))
  da <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))
  expect_snapshot(getID(da))
  expect_snapshot(da@.Data)
  expect_snapshot(da@varName)
  expect_snapshot(da@obsDescr)
})


test_that("parse targeted MS works ... AA", {
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
                        options = list(codePosition = 8,
                                       columnsList = columnsList))
  expect_snapshot(getID(da))
  expect_snapshot(da@.Data)
  expect_snapshot(da@varName)
  expect_snapshot(da@obsDescr)
})

