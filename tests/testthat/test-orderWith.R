x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10), testAnn = LETTERS[1:10])
da = new("dataElement", x,
         varName = seq(1, 10),
         type = "NMR",
         obsDescr = param)
sampleID <- getID(da)
idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
orderedDa <- orderWith(da, idx)

test_that("ordering is working", {
  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
  expect_equal(orderedDa[1,], rep(10, 10))
})

x = matrix(rep(c(1:10), 10), 10, 10)
param = data.frame(sampleID = seq(1, 10))
da = new("dataElement", x,
         varName = seq(1, 10),
         type = "NMR",
         obsDescr = param)
sampleID <- getID(da)
idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
orderedDa <- orderWith(da, idx)

test_that("ordering is working even with a single column data frame", {
  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
  expect_equal(orderedDa[1,], rep(10, 10))
})
