test_that("check dataElement", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  expect_equal(check(da), TRUE)
})

test_that("check annotations", {
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement",
           type = "ANN",
           obsDescr = list(param))
  expect_equal(check(da), TRUE)
})

