test_that("sampleID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10))
  da = new("dataElement", x,
          variableName = seq(1, 10),
          type = "NMR",
          experimentalParameter = param)
  sampleID <- getID(da)
  expect_equal(length(sampleID), 10)
  expect_equal(sampleID, seq(1, 10))
})
