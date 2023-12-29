test_that("matching two, same dimension", {
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

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)
  fi <- seq(1, 10) < 7 & seq(1, 10) > 3
  fDc <- filterWith(da, fi)

  mDa <- matchAll(list(fDa, fDb, fDc))
  expect_equal(mDa[[1]], mDa[[2]])
  expect_equal(mDa[[2]], mDa[[3]])

  expect_equal(nrow(mDa[[1]]), 3)
  expect_equal(ncol(mDa[[1]]), 10)
  expect_equal(mDa[[1]][1,1], 4)
  expect_equal(mDa[[1]]@obsDescr[[1]]$testAnn[1], "D")

  mDb <- matchAll(list(fDb, fDa, fDc))
  mDc <- matchAll(list(fDb, fDc, fDa))
  mDd <- matchAll(list(fDc, fDb, fDa))
  mDe <- matchAll(list(fDc, fDa, fDb))
  mDf <- matchAll(list(fDa, fDc, fDb))

  expect_equal(mDa, mDb)
  expect_equal(mDa, mDc)
  expect_equal(mDa, mDd)
  expect_equal(mDa, mDe)
  expect_equal(mDa, mDf)
  IDs <- getID(fDc)
  test <- all(sapply(list(mDa[[1]], mDa[[2]], mDa[[3]]),
                     function(x) identical(getID(x), IDs)))
  expect_true(test)
})

test_that("matching two, same dimension", {
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

  fi <- seq(1, 10) > 3
  fDa <- filterWith(da, fi)
  fDa@type = "NMR"
  fi <- seq(1, 10) < 7
  fDb <- filterWith(da, fi)
  fDb@type = "MS"

  da = new("dataElement",
           type = "ANN",
           obsDescr = list(param))

  fi <- seq(1, 10) < 7 & seq(1, 10) > 3
  fDc <- filterWith(da, fi)
  fDc@type = "ANN"

  mDa <- matchAll(list(fDc, fDa, fDb))
  IDs <- getID(fDc)
  test <- all(sapply(list(mDa[[1]], mDa[[2]], mDa[[3]]),
                     function(x) identical(getID(x), IDs)))
  expect_true(test)
  expect_equal(mDa[[1]]@type, "ANN")
  expect_equal(mDa[[2]]@type, "NMR")
  expect_equal(mDa[[3]]@type, "MS")

  mDa <- matchAll(list(fDb, fDc, fDa))
  IDs <- getID(fDc)
  test <- all(sapply(list(mDa[[1]], mDa[[2]], mDa[[3]]),
                     function(x) identical(getID(x), IDs)))
  expect_true(test)

  mDa <- matchAll(list(fDa, fDc, fDb))
  IDs <- getID(fDc)
  test <- all(sapply(list(mDa[[1]], mDa[[2]], mDa[[3]]),
                     function(x) identical(getID(x), IDs)))
  expect_true(test)
})


