
param = data.frame(sampleID = seq(1, 10),
                   sampleType = rep("sample", 10),
                   testAnn = LETTERS[1:10],
                   dataPath = paste0("/test/", seq(1, 10)))

test_that("matching two, same dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)

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
  expect_equal(nrow(mDa[[1]]), 10)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 1)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "A")

  mDa <- matchPairwise(da, oDa)
  expect_equal(mDa[[1]], mDa[[2]])
  expect_equal(nrow(mDa[[1]]), 10)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 10)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "J")
})

test_that("matching two, different dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)

  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 1
  fDa <- filterWith(da, fi)
  mDa <- matchPairwise(fDa, da)
  expect_identical(mDa[[1]], mDa[[2]])
  expect_equal(nrow(mDa[[1]]), 9)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 2)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "B")

  mDb <- matchPairwise(da, fDa)
  expect_equal(mDb[[1]], mDb[[2]])
  expect_equal(nrow(mDb[[1]]), 9)
  expect_equal(ncol(mDb[[1]]), 10)
  expect_equal(mDb[[1]][1,1], 2)
  expect_equal(mDb[[1]]@obsDescr[[1]]$testAnn[1], "B")

  expect_equal(mDa, mDb)
})

test_that("matching two, different dimension", {
  x = matrix(rep(c(1:10), 10), 10, 10)

  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param))

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  mDa <- matchPairwise(da, fDa)
  expect_identical(mDa[[1]], mDa[[2]])
  expect_equal(nrow(mDa[[1]]), 7)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 4)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "D")

  mDb <- matchPairwise(da, fDa)
  expect_equal(mDb[[1]], mDb[[2]])
  expect_equal(nrow(mDb[[1]]), 7)
  expect_equal(ncol(mDb[[1]]), 10)
  expect_equal(mDb[[1]][1,1], 4)
  expect_equal(mDb[[1]]@obsDescr[[1]]$testAnn[1], "D")

  expect_equal(mDa, mDb)
})

test_that("matching two, different dimension, bigger first", {
  x = matrix(rep(c(1:10), 10), 10, 10)

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
  expect_equal(nrow(mDa[[1]]), 3)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 4)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "D")

  mDb <- matchPairwise(fDa, fDb)
  expect_equal(mDb[[1]], mDb[[2]])
  expect_equal(nrow(mDb[[1]]), 3)
  expect_equal(ncol(mDb[[1]]), 10)
  expect_equal(mDb[[1]][1,1], 4)
  expect_equal(mDb[[1]]@obsDescr[[1]]$testAnn[1], "D")

  expect_equal(mDa, mDb)
})

test_that("matching annotations", {
  x = matrix(rep(c(1:10), 10), 10, 10)

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

test_that("matching two, list of observation", {
  x = matrix(rep(c(1:10), 10), 10, 10)

  da = new("dataElement", x,
           varName = as.character(seq(1, 10)),
           type = "NMR",
           method = "1D",
           obsDescr = list(param, param))

  fi <- seq(1, 10) > 1
  fDa <- filterWith(da, fi)
  fDa <- orderWith(fDa, c(9,8,7,6,5,4,3,2,1))
  mDa <- matchPairwise(fDa, da)
  expect_identical(mDa[[1]], mDa[[2]])
  expect_equal(nrow(mDa[[1]]), 9)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 2)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "B")
  expect_equal(mDa[[1]]@obsDescr[[2]]$testAnn[1], "B")

  mDb <- matchPairwise(da, fDa)
  expect_equal(mDb[[1]], mDb[[2]])
  expect_equal(nrow(mDb[[1]]), 9)
  expect_equal(ncol(mDb[[1]]), 10)
  expect_equal(mDb[[1]][1,1], 10)
  expect_equal(mDb[[1]][9,1], 2)
  expect_equal(mDb[[1]]@obsDescr[[1]]$testAnn[1], "J")
  expect_equal(mDb[[1]]@obsDescr[[2]]$testAnn[1], "J")
})

