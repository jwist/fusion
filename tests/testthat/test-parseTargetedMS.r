test_that("parse targeted MS works ... Tr", {
  file = "data/plate_1_export.txt"
  da <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))
  expect_snapshot(getID(da))
  expect_snapshot(da@.Data)
  expect_snapshot(da@varName)
  expect_snapshot(da@obsDescr)
})
#
# test_that("parse targeted MS works ... AA", {
#   pathToData <- file.path(Sys.getenv()['DATASETS'], "covid19", "cambridge", "datasets")
#   columnsList <- c("Analyte Name",
#                    "Data Set",
#                    "SampleType",
#                    "m/z expected",
#                    "RT [min]",
#                    "mSigma",
#                    "Area of PI",
#                    "Quantity [units]")
#
#   da <- parseTargetedMS(file.path(pathToData, "cambridge_aa_PAI-03_plate_4.csv"),
#                         method = "aminoAcids",
#                         options = list(codePosition = 8,
#                                        columnsList = columnsList))
#   expect_snapshot(getID(da))
#   expect_snapshot(da@.Data)
#   expect_snapshot(da@varName)
#   expect_snapshot(da@obsDescr)
# })

