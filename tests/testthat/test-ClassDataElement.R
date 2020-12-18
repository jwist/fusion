x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement", x,
         varName = as.character(seq(1, 10)),
         type = "NMR",
         obsDescr = param)

test_that("creation of dataElement", {
  expect_equal(length(da@obsDescr$sampleID), 10)
})

param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement",
         varName = as.character(seq(1, 10)),
         type = "ANN",
         obsDescr = param)

test_that("creation of annotations", {
  expect_equal(length(da@obsDescr$sampleID), 10)
})
