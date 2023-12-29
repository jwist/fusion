test_that("ordering is working", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10],
                     dataPath = paste0("/test/", seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  sampleID <- getID(da)
  idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
  orderedDa <- orderWith(da, idx)

  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
  expect_equal(orderedDa[1,], rep(10, 10))
})


test_that("ordering throw error", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10],
                     dataPath = paste0("/test/", seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  sampleID <- getID(da)
  idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix

  expect_error(orderWith(da, idx[-1]), "*length*")
})

test_that("ordering is working with multi dim obsDescr", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10],
                     dataPath = paste0("/test/", seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param, param))
  sampleID <- getID(da)
  idx <- sort(sampleID, decreasing = TRUE, index.return = TRUE)$ix
  orderedDa <- orderWith(da, idx)

  expect_equal(length(sampleID), 10)
  expect_equal(getID(orderedDa), seq(10, 1))
  expect_equal(orderedDa[1,], rep(10, 10))
  expect_equal(orderedDa@obsDescr[[1]]$sampleID, orderedDa@obsDescr[[2]]$sampleID)
})
