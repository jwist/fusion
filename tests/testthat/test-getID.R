test_that("sampleID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = list(data.frame(sampleID = seq(1, 10),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
          varName = as.character(seq(1, 10)),
          type = "NMR",
          method = "1D",
          obsDescr = param)
  sampleID <- getID(da)
  expect_equal(length(sampleID), 10)
  expect_equal(sampleID, seq(1, 10))
})

test_that("sampleID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  ID <- as.character(c(1, seq(1, 9)))
  param = list(data.frame(sampleID = makeUnique(ID),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  sampleID <- getID(da)
  expect_equal(length(sampleID), 10)
  expect_equal(sampleID, makeUnique(ID))
})

test_that("sampleID is retrieved", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  ID <- as.character(c(1, seq(1, 9)))
  param = list(data.frame(sampleID = makeUnique(ID),
                          sampleType = rep("sample", 10)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  sampleID <- getID(da, re.rm = TRUE)
  expect_equal(length(sampleID), 10)
  expect_equal(sampleID, ID)
})

test_that("sampleID is retrieved for sample", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  ID <- as.character(c(1, seq(1, 9)))
  param = list(data.frame(sampleID = makeUnique(ID),
                          sampleType = rep(c("sample", "qc"), 5)))
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = param)
  sampleID <- getID(da, type = "sample")
  expect_equal(length(sampleID), 5)
  expect_equal(sampleID, as.character(c(1,2,4,6,8)))
  sampleID <- getID(da, type = "qc")
  expect_equal(length(sampleID), 5)
  expect_equal(sampleID, as.character(c("1#1",3,5,7,9)))
})


# test_that("UID is retrieved", {
#   x = matrix(rep(c(1:10), 10), 10, 10)
#   param = list(data.frame(UID = seq(1, 10),
#                           sampleType = rep("sample", 10)))
#   da = new("dataElement", x,
#            varName = as.character(seq(1, 10)),
#            type = "NMR",
#            method = "1D",
#            obsDescr = param)
#   UID <- getID(da, using = "UID")
#   expect_equal(length(UID), 10)
#   expect_equal(UID, seq(1, 10))
# })
