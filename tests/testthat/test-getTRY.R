
#
# test_that("data and descriptions are aligned", {
#   da <- getTRY("tests/testthat/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P43.xml")
#   expect_identical(unname(sapply(da@obsDescr, function(x) unique(x$name))), da@varName)
# })
#
#
# test_that("data and descriptions are aligned", {
#   da <- getTRY("tests/testthat/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P43.xml")
#   # path <- "tests/testthat/xml_test_cambridge_plate_1.xml"
#   # da <- getTRY(path = path)
#   # identical(unname(sapply(da@obsDescr, function(x) unique(x$name))), da@varName)
#   # da[9,1] == 101.3523
#   # #COMPOUND$analconc[COMPOUND$name == "tryptophan"][9]
#
#   expect_equal(round(da[9,1], 2), 311.85)
# })
#
