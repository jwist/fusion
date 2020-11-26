test_that("ordering is working", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           variableName = seq(1, 10),
           type = "NMR",
           experimentalParameter = param)
  sampleID <- getID(da)
  idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
  orderedDa <- order(da, idx)
  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
})

test_that("ordering is working even with a single column data frame", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10))
  da = new("dataElement", x,
           variableName = seq(1, 10),
           type = "NMR",
           experimentalParameter = param)
  sampleID <- getID(da)
  idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
  orderedDa <- order(da, idx)
  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
})
