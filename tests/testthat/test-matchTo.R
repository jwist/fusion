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
  mDa <- matchTo(oDa, da)

  expect_equal(mDa, da)
})

test_that("matching two, different dimension, smaller first", {
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
  mDa <- matchTo(fDa, da)

  expect_equal(mDa, fDa)
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
  mDa <- matchTo(da, fDa)

  expect_equal(mDa, fDa)
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
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)

  mDa <- matchTo(fDb, fDa)

  expect_equal(getID(mDa), c(4:6))
})

test_that("matching with annotations", {
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

  mDa <- matchTo(ann, da)

  expect_equal(getID(mDa), c(1:10))
})

