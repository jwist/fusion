test_that("filtering is working", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  fi <- seq(1, 10) > 1
  filteredDa <- filterWith(da, fi)

  expect_equal(length(getID(filteredDa)), 9)
  expect_equal(getID(filteredDa), seq(2, 10))
  expect_equal(filteredDa[1,], rep(2, 10))
})

# test_that("filtering is working with single column data.frame", {
#   param = data.frame(sampleID = seq(1, 10))
#   da = new("dataElement",
#            type = "ANN",
#            obsDescr = list(param))
#   fi <- seq(1, 10) > 1
#   filteredDa <- filterWith(da, fi)
#
#   expect_equal(length(getID(filteredDa)), 9)
#   expect_equal(getID(filteredDa), seq(2, 10))
# })
