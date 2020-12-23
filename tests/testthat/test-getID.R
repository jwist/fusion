test_that("sampleID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = seq(1, 10)))
  da = new("dataElement", x,
          varName = as.character(seq(1, 10)),
          type = "NMR",
          method = "1D",
          obsDescr = param)
  sampleID <- getID(da)
  expect_equal(length(sampleID), 10)
  expect_equal(sampleID, seq(1, 10))
})

test_that("UID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(UID = seq(1, 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  UID <- getID(da, using = "UID")
  expect_equal(length(UID), 10)
  expect_equal(UID, seq(1, 10))
})
