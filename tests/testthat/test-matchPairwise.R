
test_that("matching two, same dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))
  sampleID <- getID(da)

  idx <- sort(sampleID, decreasing = TRUE,
              index.return = TRUE)$ix
  oDa <- orderWith(da, idx)
  mDa <- matchPairwise(oDa, da)

  expect_equal(mDa[[1]], mDa[[2]])
})

test_that("matching two, different dimension, bigger first", {
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
  fDa <- filterWith(da, fi)
  mDa <- matchPairwise(fDa, da)
  expect_identical(mDa[[1]], mDa[[2]])
})

test_that("matching two, different dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  mDa <- matchPairwise(da, fDa)

  expect_equal(mDa[[1]], mDa[[2]])
})

test_that("matching two, different dimension, bigger first", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)

  mDa <- matchPairwise(fDb, fDa)
  expect_equal(mDa[[1]], mDa[[2]])
})

test_that("matching annotations", {
  x = matrix(rep(c(1:10), 10), 10, 10)
  param = data.frame(sampleID = seq(1, 10),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[1:10])
  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  param = data.frame(sampleID = seq(10, 1),
                     sampleType = rep("sample", 10),
                     testAnn = LETTERS[10:1])
  ann = new("dataElement",
           type = "ANN",
           obsDescr = list(param))

  mDa <- matchPairwise(ann, da)
  expect_equal(mDa[[1]]@obsDescr[[1]]$sampleID,
               mDa[[2]]@obsDescr[[1]]$sampleID)
})
