# test_that("parse targeted MS works ... Tr", {
#   # file = file.path("tests/testthat/plate_1_export.txt")
#   file = file.path("plate_1_export.txt")
#
#   tryptophan <- tMsTestsets[[1]]
#   da <- parseTargetedMS(file, method = "tryptophan", options = list(codePosition = 8))
#
#   expect_equal(unname(da[1,1]), 1.026494, tolerance = 1e-6)
#   expect_equal(unname(da[114,1]), 63.60825, tolerance = 1e-6)
#   expect_equal(unname(da[33,15]), 3.140806, tolerance = 1e-6) #serotonin
#   expect_equal(da@obsDescr[[15]]$RT[33], 2.04)
#   expect_equal(unname(da[114,32]), 0.153, tolerance = 1e-6) #IL beta-nicotinamide mononucleotide D3
#   expect_equal(da@obsDescr[[32]]$RT[114], 0.47)
#   expect_equal(getID(da), getID(tryptophan))
#   expect_equal(getType(da), getType(tryptophan))
#   expect_equal(da@.Data, tryptophan@.Data)
#   expect_equal(da@obsDescr, tryptophan@obsDescr)
#   expect_equal(da@varName, tryptophan@varName)
#   expect_equal(da@type, tryptophan@type)
#   expect_equal(da@method, tryptophan@method)
# })
#
# test_that("parse targeted MS works ... AA", {
#   # file <- file.path("tests/testthat/cambridge_aa_PAI-03_plate_4.csv")
#   file <- file.path("cambridge_aa_PAI-03_plate_4.csv")
#   columnsList <- c("Analyte Name",
#                    "Data Set",
#                    "SampleType",
#                    "m/z expected",
#                    "RT [min]",
#                    "mSigma",
#                    "Area of PI",
#                    "Quantity [units]")
#
#   da <- parseTargetedMS(file,
#                         method = "aminoAcids",
#                         options = list(codePosition = 8))
#   aminoAcids <- tMsTestsets[[2]]
#   expect_equal(unname(da[1,1]), 3.68)
#   expect_equal(unname(da[1,14]), 6.64)
#   expect_equal(unname(da[121,14]), 6.12)
#   expect_equal(unname(da[1,58]), 277.46)
#   expect_equal(unname(da[79,58]), 164.66)
#   expect_equal(da@obsDescr[[58]]$`Area of PI`[79], "54,758.00")
#   expect_equal(da@obsDescr[[59]]$`Area of PI`[123], "8,762.00")
#   expect_equal(getID(da), getID(aminoAcids))
#   expect_equal(getType(da), getType(aminoAcids))
#   expect_equal(da@.Data, aminoAcids@.Data)
#   expect_equal(da@obsDescr, aminoAcids@obsDescr)
#   expect_equal(da@varName, aminoAcids@varName)
#   expect_equal(da@type, aminoAcids@type)
#   expect_equal(da@method, aminoAcids@method)
# })
#
# test_that("test AA with whole export", {
#   file=file.path(Sys.getenv()['DATASETS'], "covid19", "bioGune", "datasets", "PAI-05_Biogune_COVID_AA-Day2_Plate4and5.txt")
#   da <- parseTargetedMS(file,
#                         method = "aminoAcids",
#                         options = list(codePosition = 8))
# })
